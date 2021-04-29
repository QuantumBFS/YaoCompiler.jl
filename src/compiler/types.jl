"""
    GenericRoutine{name} <: Function

A `GenericRoutine` cannot be directly execute on a quantum
device. It is a Julia `Function` that returns `Operation`,
and `Operation` can be executed on quanutm device.

!!! note
    An instance of `GenericRoutine` should be treated like
    `Function`.
"""
abstract type GenericRoutine{name} <: Function end

"""
    abstract type Routine end

This defines operations that one can execute on a quantum device.
"""
abstract type Routine end

"""
    abstract type IntrinsicRoutine <: Routine end

`IntrinsicRoutine` are the routine that can be executed
by the compile target that is pre-defined in the compiler.
"""
abstract type IntrinsicRoutine <: Routine end

"""
    Operation{P, Args} <: Routine

An `Operation` is a user defined composite routine
that can be called in other `Operation` or execute
on target device.
"""
struct Operation{P,Args} <: Routine
    parent::P
    args::Args
end

"""
    AdjointOperation{P} <: Routine

An `AdjointOperation` is the adjoint of another
`Routine`.
"""
struct AdjointOperation{P} <: Routine
    parent::P
end

Base.adjoint(x::Routine) = AdjointOperation(x)
Base.adjoint(x::AdjointOperation) = x.parent

routine_name(::Type) = nothing
routine_name(x) = routine_name(typeof(x))
routine_name(::Type{<:GenericRoutine{name}}) where {name} = name
routine_name(::Type{T}) where {T<:IntrinsicRoutine} = nameof(T)
routine_name(::Type{<:Operation{P}}) where {P} = routine_name(P)
routine_name(::Type{<:Adjoint{P}}) where {P} = Symbol("adjoint_", routine_name(P))

# this is only a place holder
# we are not using struct because
# singleton types will get const prop
struct MeasureResult{T}
    result::T
end

# struct Chain
#     args::Vector{Any}
# end

# struct Gate
#     operation
#     locations # SSAValue/Locations
# end

# struct Ctrl
#     gate::Gate
#     ctrl # SSAValue/CtrlLocation
# end

# @as_record Chain
# @as_record Gate
# @as_record Ctrl
