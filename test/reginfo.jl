module TestRegInfo

using MLStyle
using YaoLocations
using YaoCompiler
using YaoCompiler.Intrinsics
using YaoTargetQASM
using CompilerPluginTools
using Test

@device function test_basic(theta, phi)
    # syntax sugar
    1 => X
    @gate 2 => Z
    @ctrl 1 4 => Rx(theta)
    @ctrl 2 4 => Ry(phi)
    a = @measure 3
    # direct intrinsic
    apply(Y, 3)
    apply(X, 1, 4)
    c = measure((2, 3))
    return (a = a, b = c)
end

@device function test_pure_quantum()
    ret = @gate 1:4 => test_basic(1.0, 2.0)
    @ctrl 2 1 => Rx(2.2)
    return ret
end

@testset "RegInfo" begin
    op = test_pure_quantum()
    interp = YaoInterpreter()
    ci, type = code_typed(Intrinsics.apply, (AnyReg, typeof(op)); interp)[1]
    info = RegInfo(OpenQASMTarget(), ci)

    @test_broken info.creg_ssa_map[:a] == 5
    @test_broken info.creg_ssa_map[:b] == 8
    @test_broken info.ssa_creg_map[5] === :a
    @test_broken info.ssa_creg_map[8] === :b
    @test info.creg_size[:a] == 1
    @test info.creg_size[:b] == 2
    @test info.qreg_to_locs[1] == [1, 4]
    @test info.qreg_to_locs[2] == [2, 3]
    @test info.locs_to_qreg[1] == 1
    @test info.locs_to_qreg[2] == 2
    @test info.locs_to_qreg[3] == 2
    @test info.locs_to_qreg[4] == 1
    @test info.locs_to_addr[1] == 0
    @test info.locs_to_addr[2] == 0
    @test info.locs_to_addr[3] == 1
    @test info.locs_to_addr[4] == 1

    buf = IOBuffer()
    show(buf, MIME"text/plain"(), info)
    s = String(take!(buf))
    @test occursin("RegInfo\n", s)
    @test occursin("creg:\n", s)
    @test occursin("  a[1]\n", s)
    @test occursin("  b[2]\n", s)
    @test occursin("qreg:\n", s)
    @test occursin("  %2[2] => [2, 3]", s)
    @test occursin("  %1[2] => [1, 4]", s)
end

end
