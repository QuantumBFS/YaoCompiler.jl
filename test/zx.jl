using Test
using YaoAPI
using MLStyle
using YaoLocations
using YaoCompiler
using GPUCompiler
using YaoCompiler.Intrinsics
using CompilerPluginTools

@device function test_a()
    1 => X
    1 => X
end

interp = YaoInterpreter(;options=HardwareFreeOptions(;clifford_simplification=true, phase_teleportation=false))
code_ircode(Intrinsics.apply, (AnyReg, typeof(test_a())); interp)

using ZXCalculus

zxd = ZXDiagram(1)
push_gate!(zxd, Val(:X), 1)
push_gate!(zxd, Val(:X), 1)
clifford_simplification(zxd)
convert_to_chain(zxd)


function test_b()
    function (r)
        apply!(r, X, 1)
        apply!(r, X, 2)
    end
end
