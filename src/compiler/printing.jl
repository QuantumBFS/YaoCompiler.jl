const COLOR_SCHEME = Dict(:intrinsic => :blue, :routine => :blue)

function print_routine(io::IO, x::IntrinsicRoutine)
    print(io, routine_name(x))
    fnames = fieldnames(typeof(x))
    if !isempty(fnames)
        print(io, "(")
        for k in 1:length(fnames)
            print(io, getfield(x, fnames[k]))
            if k != length(fnames)
                print(io, ", ")
            end
        end
        print(io, ")")
    end
end

Base.show(io::IO, x::IntrinsicRoutine) = print_routine(io, x)

function Base.show(io::IO, ::MIME"text/plain", x::IntrinsicRoutine)
    print_routine(io, x)
    printstyled(io, " (intrinsic operation)"; color = COLOR_SCHEME[:intrinsic])
end

function Base.show(io::IO, ::GenericRoutine{name}) where {name}
    print(io, name)
end

function Base.show(io::IO, ::MIME"text/plain", fn::GenericRoutine{name}) where {name}
    print(io, name)
    printstyled(
        io,
        " (generic routine with ",
        length(methods(fn).ms),
        " methods)";
        color = COLOR_SCHEME[:routine],
    )
end

function Base.show(io::IO, ::Type{T}) where {name,T<:GenericRoutine{name}}
    mod = T.name.module
    print(io, "typeof(")
    mod === Main || print(io, mod, ".")
    print(io, name, ")")
end
