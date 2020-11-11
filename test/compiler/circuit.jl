module TestMacros

using YaoCompiler
using YaoCompiler.Intrinsics
using Test

@device function qft(n::Int)
    1 => H
    for k in 2:n
        @ctrl k 1 => shift(2π / 2^k)
    end

    if n > 1
        2:n => qft(n - 1)
    end
end

@device function qft4()
    1 => H
    @ctrl 2 1 => shift(π / 2)
    @ctrl 3 1 => shift(π / 4)
    @ctrl 4 1 => shift(π / 8)

    2 => H
    @ctrl 3 2 => shift(π / 2)
    @ctrl 4 2 => shift(π / 4)

    3 => H
    @ctrl 4 3 => shift(π / 2)
    4 => H
end

@device function hadamard()
    1 => H
end

@testset "tracing" begin
    tape1 = YaoCompiler.@trace qft(4)
    tape2 = YaoCompiler.@trace qft4()
    @test tape1 == tape2    
end

struct Foo
    a::Int
    b::Int
end

@device function (b::Foo)(theta)
    @ctrl b.a b.b => shift(theta)
    return theta
end

@testset "callable" begin
    m = Foo(1, 2)
    tape = YaoCompiler.@trace m(0.1)
    @test length(tape) == 1
    @test tape[1] == Expr(:call, YaoCompiler.Semantic.ctrl, shift(0.1), Locations(2), CtrlLocations(1))
end

qasm"""OPENQASM 2.0;
include "qelib1.inc";
"""

@device function adder3()
    @ctrl 1 4=>X
    @ctrl 2 4=>X
    @ctrl 3 4=>X
    (1,2,5) => ccx()
    (1,3,5) => ccx()
    (2,3,5) => ccx()
end

@testset "YaoLang/#66" begin
    @code_qasm adder3()
end

end
