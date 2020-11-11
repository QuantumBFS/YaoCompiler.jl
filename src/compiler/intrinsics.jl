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

using ..YaoCompiler: @intrinsic, IntrinsicRoutine
export X, Y, Z, H, S, T, shift, Rx, Ry, Rz

@intrinsic X
@intrinsic Y
@intrinsic Z
@intrinsic H
@intrinsic S
@intrinsic T

# TODO: implement intrinsic macro for function calls
# @intrinsic shift(θ::Real)
# @intrinsic Rx(θ::Real)
# @intrinsic Ry(θ::Real)
# @intrinsic Rz(θ::Real)

for name in [:shift, :Rx, :Ry, :Rz]
    @eval struct $name{T} <: IntrinsicRoutine
        θ::T
    end
end

end
