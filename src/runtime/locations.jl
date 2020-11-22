export Locations, CtrlLocations, AbstractLocations, LocationError, merge_locations

const LocationStorageTypes = Union{Int,NTuple{N,Int} where N,UnitRange{Int},Vector{Int}}

abstract type AbstractLocations end

"""
    merge_locations(locations...)

Construct a new `Locations` by merging two or more existing locations.
"""
merge_locations(x::AbstractLocations, y::AbstractLocations, locations::AbstractLocations...) =
    merge_locations(merge_locations(x, y), locations...)

merge_locations(x::AbstractLocations) = x

"""
    Locations <: AbstractLocations

Type to annotate locations in quantum circuit.

    Locations(x)

Create a `Locations` object from a raw location statement. Valid storage types are:

- `Int`: single position
- `NTuple{N, Int}`: a list of locations
- `UnitRange{Int}`: contiguous locations

Other types will be converted to the storage type via `Tuple`.
"""
struct Locations{T<:LocationStorageTypes} <: AbstractLocations
    storage::T

    Base.@pure function Locations(x::T) where {T<:LocationStorageTypes}
        new{T}(x)
    end
end

# skip it if x is a location
Locations(x::Locations) = x
Locations(xs...) = Locations(xs)
Locations(xs::NTuple{N,<:Locations}) where {N} = merge_locations(xs...)
Locations(x::NTuple{N,T}) where {N,T} = throw(LocationError("expect Int, got $T"))

Base.@propagate_inbounds Base.getindex(l::Locations, idx...) = Locations(getindex(l.storage, idx...))
Base.length(l::Locations) = length(l.storage)
Base.iterate(l::Locations) = iterate(l.storage)
Base.iterate(l::Locations, st) = iterate(l.storage, st)
Base.eltype(::Type{T}) where {T<:Locations} = Int
Base.eltype(x::Locations) = Int
Base.Tuple(x::Locations) = (x.storage...,)

struct LocationError <: Exception
    msg::String
end

function merge_locations(l1::Locations, l2::Locations)
    Locations((l1.storage..., l2.storage...))
end

# location mapping
# TODO: preserve sign when indexing
# TODO: provide a @inlocation macro via Expr(:meta, :inlocation, true) so when we compile to Julia functions
#       we can use unsafe_mapping directly
@inline unsafe_mapping(parent::Locations{Int}, sub::Locations{Int}) = parent
@inline unsafe_mapping(parent::Locations{Int}, sub::Locations{NTuple{N,Int}}) where {N} = parent
@inline unsafe_mapping(parent::Locations{Int}, sub::Locations{UnitRange{Int}}) = parent
@inline unsafe_mapping(parent::Locations{NTuple{N,Int}}, sub::Locations{Int}) where {N} =
    Locations(@inbounds parent[sub.storage])
@inline unsafe_mapping(parent::Locations{NTuple{N,Int}}, sub::Locations{NTuple{M,Int}}) where {N,M} =
    Locations(map(x -> @inbounds(parent[x]), sub.storage))
@inline unsafe_mapping(parent::Locations{NTuple{N,Int}}, sub::Locations{UnitRange{Int}}) where {N} =
    Locations(@inbounds parent[sub.storage])
@inline unsafe_mapping(parent::Locations{UnitRange{Int}}, sub::Locations{Int}) =
    Locations(@inbounds parent[sub.storage])
@inline unsafe_mapping(parent::Locations{UnitRange{Int}}, sub::Locations{NTuple{N,Int}}) where {N} =
    Locations(map(x -> @inbounds(parent[x]), sub.storage))
@inline unsafe_mapping(parent::Locations{UnitRange{Int}}, sub::Locations{UnitRange{Int}}) =
    Locations(@inbounds parent[sub.storage])

@inline map_error(parent, sub) = throw(LocationError("got $sub in parent space $parent"))

@noinline function map_check(parent, sub)
    map_check_nothrow(parent, sub) || map_error(parent, sub)
end

function map_check_nothrow(parent::Locations{Int}, sub::Locations{Int})
    sub.storage == 1
end

function map_check_nothrow(parent::Locations{Int}, sub::Locations{Tuple{Int}})
    sub.storage[1] == 1
end

function map_check_nothrow(parent::Locations{Int}, sub::Locations{NTuple{N,Int}}) where {N}
    false
end

function map_check_nothrow(parent::Locations{Int}, sub::Locations{UnitRange{Int}})
    (length(sub) == 1) && (sub.storage.start == 1)
end

function map_check_nothrow(parent::Locations{NTuple{N,Int}}, sub::Locations{Int}) where {N}
    1 <= sub.storage <= N
end

function map_check_nothrow(
    parent::Locations{NTuple{N,Int}},
    sub::Locations{NTuple{M,Int}},
) where {N,M}
    all(x -> (1 <= x <= N), sub.storage)
end

function map_check_nothrow(parent::Locations{NTuple{N,Int}}, sub::Locations{UnitRange{Int}}) where {N}
    (1 <= sub.storage.start) && (sub.storage.stop <= N)
end

function map_check(parent::Locations{UnitRange{Int}}, sub::Locations{Int})
    1 <= sub.storage <= length(parent)
end

function map_check(parent::Locations{UnitRange{Int}}, sub::Locations{NTuple{N,Int}}) where {N}
    all(x -> (1 <= x <= length(parent)), sub.storage)
end

function map_check(parent::Locations{UnitRange{Int}}, sub::Locations{UnitRange{Int}})
    (1 <= sub.storage.start) && (sub.storage.stop <= length(parent))
end

# comparing
function Base.:(==)(l1::Locations, l2::Locations)
    length(l1) == length(l2) || return false
    return all(l1.storage .== l2.storage)
end

struct CtrlFlags{L,N}
    data::NTuple{N,UInt64}
end

function flags(bits::Vararg{UInt8,L}) where {L}
    N = Base.num_bit_chunks(L)
    data = ntuple(N) do k
        x = UInt64(0)
        p = UInt64(0)
        for i in (64k-63):min(64k, L)
            x += bits[i] << p
            p += UInt64(1)
        end
        return x
    end
    return CtrlFlags{L,N}(data)
end

function _default_flags(L::Int)
    N = Base.num_bit_chunks(L)
    return CtrlFlags{L,N}(ntuple(x -> UInt64(0), N))
end

function Base.all(flags::CtrlFlags)
    return all(iszero, flags.data)
end

function Base.getindex(flags::CtrlFlags, idx::Int)
    i1, i2 = Base.get_chunks_id(idx)
    u = UInt64(1) << i2
    r = (flags.data[i1] & u) == 0
    return r
end

function merge_flags(a::CtrlFlags{LA}, b::CtrlFlags{LB}) where {LA,LB}
    L = LA + LB
    N = Base.num_bit_chunks(L)
    last_chunk_len = rem(LA, 64)

    if iszero(last_chunk_len)
        return CtrlFlags{L,N}((a.data..., b.data...))
    else
        return flags(ntuple(k -> UInt8(!a[k]), LA)..., ntuple(k -> UInt8(!b[k]), LB)...)
    end
end

function Base.show(io::IO, x::CtrlFlags{L,N}) where {L,N}
    print(io, "CtrlFlags(")
    p = L - 1
    for d in x.data
        for k in 0:min(63, p)
            print(io, d >> k & UInt64(1))
        end
        p -= 64
    end
    print(io, ")")
end

# CtrlLocations
struct CtrlLocations{T<:LocationStorageTypes,L,N} <: AbstractLocations
    storage::Locations{T}
    flags::CtrlFlags{L,N}

    Base.@pure CtrlLocations(storage::Locations{T}, flags::CtrlFlags{L,N}) where {T,L,N} =
        new{T,L,N}(storage, flags)
end

# skip itself
CtrlLocations(x::CtrlLocations) = x
CtrlLocations(x::Locations) = CtrlLocations(x, _default_flags(length(x)))
CtrlLocations(x::LocationStorageTypes, configs::NTuple{L,UInt8}) where {L} =
    CtrlLocations(Locations(x), flags(configs...))
CtrlLocations(xs...) = CtrlLocations(Locations(xs...))

Base.length(l::CtrlLocations) = length(l.storage)
Base.iterate(l::CtrlLocations) = iterate(l.storage)
Base.iterate(l::CtrlLocations, st) = iterate(l.storage, st)

function Base.show(io::IO, x::Locations)
    print(io, "Locations(")
    print_locations(io, x)
    print(io, ")")
end

function Base.show(io::IO, x::CtrlLocations)
    print(io, "CtrlLocations(")
    print_locations(io, x)
    print(io, ")")
end

function print_locations(io::IO, x::Locations)
    if x.storage isa AbstractRange
        return printstyled(io, x.storage; color = :light_blue)
    end

    nlocations = length(x)
    for i in 1:nlocations
        printstyled(io, x.storage[i]; color = :light_blue)

        if i != nlocations
            print(io, ", ")
        end
    end
end

function print_locations(io::IO, x::CtrlLocations)
    if all(x.flags)
        print_locations(io, x.storage)
    else
        nlocations = length(x)
        for i in 1:nlocations
            l = x.storage.storage[i]
            if x.flags[i]
                @show "pass"
                printstyled(io, l; color = :light_blue)
            else
                printstyled(io, "!", l; color = :light_blue)
            end

            if i != nlocations
                print(io, ", ")
            end
        end
    end
end

function merge_locations(l1::CtrlLocations, l2::CtrlLocations)
    CtrlLocations(merge_locations(l1.storage, l2.storage), merge_flags(l1.flags, l2.flags))
end

# NOTE: CtrlLocations can not be mapped by Locations
@inline unsafe_mapping(parent::Locations, sub::CtrlLocations) =
    CtrlLocations(unsafe_mapping(parent, sub.storage), sub.flags)
map_check_nothrow(parent::Locations, sub::CtrlLocations) = map_check_nothrow(parent, sub.storage)

Base.:(==)(l1::CtrlLocations, l2::CtrlLocations) =
    (l1.storage == l2.storage) && (l1.flags == l2.flags)

# parent location has to be Locations since CtrlLocations can't be parent
@inline function Base.getindex(parent::Locations, sub::AbstractLocations)
    map_check(parent, sub)
    return unsafe_mapping(parent, sub)
end
