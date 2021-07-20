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
