using CompilerPluginTools: rm_code_coverage_effect
using IBMQClient.Schema
using YaoCompiler
using YaoCompiler.Intrinsics
using YaoTargetQobj
using Test


@device function basic()
    1 => X
    2 => Y
end

@testset "basic" begin
    ci, type = @yao_code_typed(basic())[1]
    ci = rm_code_coverage_effect(ci)
    @test measure_ssa_uses(ci) == Set{Int}()
    mi = MemoryInfo(ci)
    display(mi)
    @test mi.qubits == Dict(1=>0, 2=>1)
    @test mi.n_qubits == 2

    @test code_qobj(typeof(basic())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(basic), Tuple{}}",
            "n_qubits" => 2,
        ),
        config = ExpConfig(;
            memory_slots = 0,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "x",
                qubits = [0],
            ),
            Gate(;
                name = "y",
                qubits = [1],
            ),
        ],
    )
end

@device function just_measure()
    1 => H
    c = @measure 1
    return c
end

@testset "just_measure" begin
    ci, type = @yao_code_typed(just_measure())[1]
    ci = rm_code_coverage_effect(ci)
    @test measure_ssa_uses(ci) == Set{Int}()
    mi = MemoryInfo(ci)
    display(mi)
    @test isempty(mi.registers)

    @test code_qobj(typeof(just_measure())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(just_measure), Tuple{}}",
            "n_qubits" => 1,
        ),
        config = ExpConfig(;
            memory_slots = 1,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "h",
                qubits = [0],
            ),
            Measure(;
                qubits = [0],
                memory = [0],
            ),
        ],
    )
end

@device function reset_qubit()
    1 => H
    c = @measure 1
    if c == 1
        1 => X
    end
    return
end

@testset "reset_qubit" begin
    ci, type = @yao_code_typed(reset_qubit())[1]
    ci = rm_code_coverage_effect(ci)
    @test measure_ssa_uses(ci) == Set(2)
    @test measure_ssa_returns(ci) == Set()

    mi = MemoryInfo(ci)
    display(mi)
    @test mi.registers == Dict(2=>[0])
    @test mi.n_qubits == 1
    @test mi.qubits == Dict(1=>0)
    @test code_qobj(typeof(reset_qubit())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(reset_qubit), Tuple{}}",
            "n_qubits" => 1,
        ),
        config = ExpConfig(;
            memory_slots = 0,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "h",
                qubits = [0],
            ),
            Reset(;
                qubits = [0],
            ),
        ],
    )
end

@device function reset_qubit_return()
    1 => H
    c = @measure 1
    if c == 1
        1 => X
    end
    return c
end

@testset "reset_qubit_return" begin
    ci, type = @yao_code_typed(reset_qubit_return())[1]
    ci = rm_code_coverage_effect(ci)
    @test measure_ssa_uses(ci) == Set(2)
    # @test measure_ssa_returns(ci) == Set(2)
    @test code_qobj(typeof(reset_qubit_return())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(reset_qubit_return), Tuple{}}",
            "n_qubits" => 1,
        ),
        config = ExpConfig(;
            memory_slots = 1,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "h",
                qubits = [0],
            ),
            Measure(;
                qubits = [0],
                memory = [0],
                register = [0],
            ),
            Gate(;
                name = "x",
                qubits = [0],
                conditional = 0,
            ),
        ],
    )
end

@device function reset_with_return()
    1 => H
    m1 = @measure 2
    m2 = @measure 1
    if m1 == 1
        1 => X
    end
    return m1, m2
end

@testset "reset_with_return" begin
    ci, type = @yao_code_typed(reset_with_return())[1]
    ci = rm_code_coverage_effect(ci)
    @test measure_ssa_uses(ci) == Set(2)
    # @test measure_ssa_returns(ci) == Set([2, 3])
    mi = MemoryInfo(ci)
    @test mi.n_qubits == 2
    @test mi.registers == Dict(2=>[0])
    @test mi.qubits == Dict(1=>0, 2=>1)
    @test code_qobj(typeof(reset_with_return())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(reset_with_return), Tuple{}}",
            "n_qubits" => 2,
        ),
        config = ExpConfig(;
            memory_slots = 2,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "h",
                qubits = [0],
            ),
            Measure(;
                qubits = [1],
                memory = [0],
                register = [0],
            ),
            Measure(;
                qubits = [0],
                memory = [1],
            ),
            Gate(;
                name = "x",
                qubits = [0],
                conditional = 0,
            ),
        ],
    )
end

@device function multi_qubit_measure()
    1 => H
    3 => X
    c = @measure (1, 2)
    return (c=c)
end

@testset "multi_qubit_measure" begin
    ci, type = @yao_code_typed(multi_qubit_measure())[1]
    ci = rm_code_coverage_effect(ci)
    mi = MemoryInfo(ci)
    @test mi.n_qubits == 3
    @test isempty(mi.registers)
    @test mi.qubits == Dict(1=>0, 2=>2, 3=>1)
    @test code_qobj(typeof(multi_qubit_measure())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(multi_qubit_measure), Tuple{}}",
            "n_qubits" => 3,
        ),
        config = ExpConfig(;
            memory_slots = 2,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "h",
                qubits = [0],
            ),
            Gate(;
                name = "x",
                qubits = [1],
            ),
            Measure(;
                qubits = [0, 2],
                memory = [0, 1],
            ),
        ],
    )
end


@device function multi_if_measure()
    1 => H
    3 => X
    c = @measure 1
    if c == 1
        1 => Y
        2 => Z
        3 => H
    end
    return
end

@testset "multi if stmt" begin
    @test code_qobj(typeof(multi_if_measure())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(multi_if_measure), Tuple{}}",
            "n_qubits" => 3,
        ),
        config = ExpConfig(;
            memory_slots = 1,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "h",
                qubits = [0],
            ),
            Gate(;
                name = "x",
                qubits = [1],
            ),
            Measure(;
                qubits = [0],
                memory = [0],
                register = [0],
            ),
            Gate(;
                name = "y",
                qubits = [0],
                conditional = 0,
            ),
            Gate(;
                name = "z",
                qubits = [2],
                conditional = 0,
            ),
            Gate(;
                name = "h",
                qubits = [1],
                conditional = 0,
            ),
        ],
    )
end

@device function conditional_ctrl()
    1 => H
    3 => X
    c = @measure 1
    if c == 1
        @ctrl 2 1 => Y
    end
    return
end

@testset "conditional ctrl" begin
    @test code_qobj(typeof(conditional_ctrl())) == Experiment(;
        header = Dict{String, Any}(
            "description" => "Operation{typeof(conditional_ctrl), Tuple{}}",
            "n_qubits" => 3,
        ),
        config = ExpConfig(;
            memory_slots = 1,
            seed = 1,
            max_credits = 3,
        ),
        instructions = [
            Gate(;
                name = "h",
                qubits = [0],
            ),
            Gate(;
                name = "x",
                qubits = [1],
            ),
            Measure(;
                qubits = [0],
                memory = [0],
                register = [0],
            ),
            Gate(;
                name = "cy",
                qubits = [0, 2],
                conditional = 0,
            ),
        ],
    )
end
