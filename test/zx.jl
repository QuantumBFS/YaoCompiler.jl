using Test
using YaoAPI
using MLStyle
using YaoLocations
using YaoCompiler
using GPUCompiler
using YaoCompiler.Intrinsics
using CompilerPluginTools
using YaoHIR
using ZXCalculus
using YaoArrayRegister

@operation function test_a()
    1 => X
    1 => X
end

interp = YaoInterpreter(;options=HardwareFreeOptions(;
        clifford_simplification=true, phase_teleportation=false
    )
)

ir, = code_typed(Intrinsics.apply, (AnyReg, typeof(test_a())); interp)[1]
@test_codeinfo ir begin
    Expr(:invoke, _, apply, _, &(QuoteNode(H)), &(QuoteNode(Locations(1))))::Nothing
    Expr(:invoke, _, apply, _, &(QuoteNode(H)), &(QuoteNode(Locations(1))))::Nothing
end

@operation function test_b()
    1 => Rx(2.0)
    1 => Rz(3.0)
end

ir, = code_typed(Intrinsics.apply, (AnyReg, typeof(test_b())); interp)[1]

@test_codeinfo ir begin
    Expr(:invoke, _, apply, _, &(QuoteNode(H)), &(QuoteNode(Locations(1))))::Nothing
    Expr(:invoke, _, apply, _, &(QuoteNode(Rz(2.0))), &(QuoteNode(Locations(1))))::Nothing
    Expr(:invoke, _, apply, _, &(QuoteNode(H)), &(QuoteNode(Locations(1))))::Nothing
    Expr(:invoke, _, apply, _, &(QuoteNode(Rz(3.0))), &(QuoteNode(Locations(1))))::Nothing
end

@operation function test_cir()
    5 => H
    @ctrl 4 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 1 5 => X
    5 => T
    @ctrl 4 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 1 5 => X
    4 => T
    5 => T
    @ctrl 1 4 => X
    4 => shift(7 / 4 * π)
    1 => T
    @ctrl 1 4 => X
    @ctrl 4 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 3 5 => X
    5 => T
    @ctrl 4 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 3 5 => X
    4 => T
    5 => T
    @ctrl 3 4 => X
    4 => shift(7 / 4 * π)
    5 => H
    3 => T
    @ctrl 3 4 => X
    @ctrl 4 5 => X
    5 => H
    @ctrl 3 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 2 5 => X
    5 => T
    @ctrl 3 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 2 5 => X
    3 => T
    5 => T
    @ctrl 2 3 => X
    3 => shift(7 / 4 * π)
    5 => H
    2 => T
    @ctrl 2 3 => X
    @ctrl 3 5 => X
    5 => H
    @ctrl 2 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 1 5 => X
    5 => T
    @ctrl 2 5 => X
    5 => shift(7 / 4 * π)
    @ctrl 1 5 => X
    2 => T
    5 => T
    @ctrl 1 2 => X
    2 => shift(7 / 4 * π)
    5 => H
    1 => T
    @ctrl 1 2 => X
    @ctrl 2 5 => X
    @ctrl 1 5 => X
end

ir, = code_typed(Intrinsics.apply, (AnyReg, typeof(test_cir())); interp)[1]

include("test/emulate.jl")

r = rand_state(5)
cir = test_cir()
target_f = YaoCompiler.compile(
    JLEmulationTarget(), Intrinsics.apply, Tuple{typeof(r),typeof(cir)},
    HardwareFreeOptions(
        clifford_simplification=false, phase_teleportation=false,
    )
)

test_f = YaoCompiler.compile(JLEmulationTarget(), Intrinsics.apply, Tuple{typeof(r),typeof(cir)}, HardwareFreeOptions(
    clifford_simplification=true, phase_teleportation=true,
))

r = rand_state(5)
target_r = copy(r)
test_r = copy(r)

target_f(target_r, cir)
test_f(test_r, cir)

@test test_r ≈ target_r
