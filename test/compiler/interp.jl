using Test
using YaoLocations
using YaoCompiler
using YaoCompiler.Intrinsics
using CompilerPluginTools

@device function circuit(theta, phi)
    1 => X
    @ctrl 1 4 => Rx(theta)
    @ctrl 2 4 => Ry(phi)
    ctrl(X, 1, 4)
    return 2
end

@device function main_circuit()
    1:4 => circuit(1.0, 2.0)
end

@testset "nested routine const prop" begin
    op = main_circuit()
    interp = YaoInterpreter(;group_quantum_stmts=true)
    ci, type = code_typed(Intrinsics.main, (typeof(op), ); interp)[1]

    @test ci.code[1].head === :invoke
    @test ci.code[1].args[2] == GlobalRef(Intrinsics, :gate)
    @test ci.code[1].args[3] == QuoteNode(X)
    @test ci.code[1].args[4] == QuoteNode(Locations(1))

    @test ci.code[2].head === :invoke
    @test ci.code[2].args[2] == GlobalRef(Intrinsics, :ctrl)
    @test ci.code[2].args[3] == QuoteNode(Rx(1.0))
    @test ci.code[2].args[4] == QuoteNode(Locations(4))

    @test ci.code[3].head === :invoke
    @test ci.code[3].args[2] == GlobalRef(Intrinsics, :ctrl)
    @test ci.code[3].args[3] == QuoteNode(Ry(2.0))
    @test ci.code[3].args[4] == QuoteNode(Locations(4))
    @test ci.code[3].args[5] == QuoteNode(CtrlLocations(2))

    @test ci.code[4].head === :invoke
    @test ci.code[4].args[2] == GlobalRef(Intrinsics, :ctrl)
    @test ci.code[4].args[3] == QuoteNode(X)
    @test ci.code[4].args[4] == QuoteNode(Locations(1))
    @test ci.code[4].args[5] == QuoteNode(CtrlLocations(4))
    @test ci.code[5] == ReturnNode(2)
end
