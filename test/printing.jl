using Test
using YaoCompiler
using YaoLocations
using YaoArrayRegister
using YaoCompiler.Intrinsics

@device function test_intrinsic(theta, phi)
    # syntax sugar
    1 => X
    @gate 2 => Z
    @ctrl 1 4 => Rx(theta)
    @ctrl 2 4 => Ry(phi)
    return
end

buf = IOBuffer()
show(buf, MIME"text/plain"(), test_intrinsic)
@test occursin("test_intrinsic (generic routine with 1 methods)", String(take!(buf)))

show(buf, MIME"text/plain"(), X)
@test occursin("XGate (intrinsic operation)", String(take!(buf)))

show(buf, MIME"text/plain"(), Rx(2.0))
@test occursin("Rx(2.0) (intrinsic operation)", String(take!(buf)))

show(buf, MIME"text/plain"(), typeof(test_intrinsic))
@test occursin("typeof(test_intrinsic)", String(take!(buf)))
