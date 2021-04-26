export @intrinsic

# NOTE:
# we only define basic staff for now
# but in principal we should generate simulation
# instruction and QASM directly from here
# since these are not real "intrinsic" APIs

macro intrinsic(ex)
    return esc(intrinsic_m(ex))
end

function intrinsic_m(name::Symbol)
    tname = Symbol(name, :Gate)
    return quote
        Core.@__doc__ struct $tname <: $IntrinsicRoutine end
        Core.@__doc__ const $name = $tname()
    end
end

is_one_qubit_gate(x) = false

# function intrinsic_m(ex::Expr)
#     ex.head === :call || error("expect a function call or a symbol")
#     name = ex.args[1]::Symbol
#     args = rm_annotations.(ex.args[2:end])

#     body = Expr(:block)
#     def = Expr(:struct, false, name, body)

#     return quote
#         Core.@__doc__ const $name = $IntrinsicRoutine{$(QuoteNode(name))}()

#         function (self::$IntrinsicRoutine{$(QuoteNode(name))})($(ex.args[2:end]...))
#             return $IntrinsicSpec(self, $(rm_annotations.(ex.args[2:end])...))
#         end
#     end
# end

module Intrinsics

using MLStyle
using YaoLocations
using CompilerPluginTools
using ..YaoCompiler: YaoCompiler, @intrinsic, IntrinsicRoutine, Routine

export X, Y, Z, H, S, T, shift, Rx, Ry, Rz, main, apply, measure, barrier, expect

@intrinsic_stub device main(gate::Routine)

function apply(gate::Routine, loc::Locations)
    throw(CompilerPluginTools.IntrinsicError("apply must be executed inside @device"))
end

function apply(gate::Routine, loc::Locations, ctrl::CtrlLocations)
    throw(CompilerPluginTools.IntrinsicError("apply must be executed inside @device"))
end

isintrinsic(::typeof(apply)) = true

# NOTE: other measurement options are just syntax sugars
# TODO: check in typeinf
@intrinsic_stub device measure(locs::Locations)
@intrinsic_stub device barrier(locs::Locations)

# NOTE: this is in principal a for loop + measure
# but for easy manipulation, let's make it a first-class
@intrinsic_stub device expect(locs::Locations, nshots::Int)

@intrinsic X
@intrinsic Y
@intrinsic Z
@intrinsic H
@intrinsic S
@intrinsic T

YaoCompiler.is_one_qubit_gate(::Type{<:XGate}) = true
YaoCompiler.is_one_qubit_gate(::Type{<:YGate}) = true
YaoCompiler.is_one_qubit_gate(::Type{<:ZGate}) = true
YaoCompiler.is_one_qubit_gate(::Type{<:HGate}) = true
YaoCompiler.is_one_qubit_gate(::Type{<:SGate}) = true
YaoCompiler.is_one_qubit_gate(::Type{<:TGate}) = true

# TODO: implement intrinsic macro for function calls
# @intrinsic shift(θ::Real)
# @intrinsic Rx(θ::Real)
# @intrinsic Ry(θ::Real)
# @intrinsic Rz(θ::Real)

for name in [:shift, :Rx, :Ry, :Rz]
    @eval struct $name{T<:Real} <: IntrinsicRoutine
        θ::T

        $name(θ) = new{typeof(θ)}(θ)
    end

    @eval YaoCompiler.is_one_qubit_gate(::Type{<:$name}) = true
end

struct UGate{T} <: IntrinsicRoutine
    α::T
    β::T
    γ::T
end

YaoCompiler.is_one_qubit_gate(::Type{<:UGate}) = true
# this is a workaround since MLStyle doesn't support a.b pattern

@as_record XGate
@as_record YGate
@as_record ZGate
@as_record HGate
@as_record SGate
@as_record TGate

@as_record shift
@as_record Rx
@as_record Ry
@as_record Rz
@as_record UGate

end
