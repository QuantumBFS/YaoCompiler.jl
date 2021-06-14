module Intrinsics

using YaoAPI: AbstractRegister
using YaoHIR
using YaoLocations
using CompilerPluginTools

export main, apply, measure, barrier, expect

# true intrinsics
@intrinsic_stub device apply(r::AbstractRegister, gate::Routine)
@intrinsic_stub device apply(r::AbstractRegister, gate::Routine, ::Locations)
@intrinsic_stub device apply(r::AbstractRegister, gate::Routine, ::Locations, ::CtrlLocations)
@intrinsic_stub device measure(r::AbstractRegister, ::Locations)
@intrinsic_stub device barrier(r::AbstractRegister, ::Locations)
@intrinsic_stub device expect(r::AbstractRegister, ::Locations, nshots::Int)

# this is just for doing overlay since we don't
# have explicit register semantic inside @device
@intrinsic_stub device apply(gate::Routine)
@intrinsic_stub device apply(gate::Routine, ::Locations)
@intrinsic_stub device apply(gate::Routine, ::Locations, ::CtrlLocations)
@intrinsic_stub device measure(::Locations)
@intrinsic_stub device barrier(::Locations)
@intrinsic_stub device expect(::Locations, nshots::Int)

using YaoHIR: X, Y, Z, H, S, T, SWAP, shift, Rx, Ry, Rz, UGate
export X, Y, Z, H, S, T, SWAP, shift, Rx, Ry, Rz, UGate

end
