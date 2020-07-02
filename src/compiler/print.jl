function Base.show(io::IO, ir::YaoIR)
    indent = get(io, :indent, 0)
    printstyled(io, tab^indent, "circuit", color = :light_blue, bold = true)
    print(io, tab^indent, " ")
    print_head(io, ir.name)
    print_args(io, ir.args)

    if !isempty(ir.whereparams)
        print_where(io, ir.whereparams)
    end
    println(io)
    print(io, ir.body)
end

function print_head(io::IO, name)
    name isa Expr && name.head === :(::) && print(io, "(")
    print_name(io, name)
    name isa Expr && name.head === :(::) && print(io, ")")
end

print_name(io::IO, name::Symbol) = printstyled(io, name)

function print_name(io::IO, name::Expr)
    if name.head === :(::)
        print(io, name.args[1], "::")
        printstyled(io, name.args[2]; color = :green, bold = true)
    elseif name.head === :(kw)
        print(io, name.args[1], "=", name.args[2])
    else # not sure this is the case, but print it anyway
        print(io, name)
    end
end

function print_args(io::IO, args::Vector{Any})
    print(io, "(")
    for (k, each) in enumerate(args)
        print_name(io, each)
        if k != lastindex(args)
            print(io, ", ")
        end
    end
    print(io, ")")
end

function print_where(io::IO, whereparams::Vector{Any})
    printstyled(io, " where ", color = :light_blue)
    print(io, "{")
    for (k, each) in enumerate(whereparams)
        print(io, each)
        if k != lastindex(whereparams)
            print(io, ", ")
        end
    end
    print(io, "}")
end

Inner.print_stmt(io::IO, ::Val{:quantum}, ex) = print_quantum(io, Val(ex.args[1]), ex)

print_quantum(io, ::Val, ex) = print(io, ex)
print_quantum(io, ::Val{:gate}, ex) = print_quantum(io, ex)
print_quantum(io, ::Val{:ctrl}, ex) = print_quantum(io, ex)
print_quantum(io, ::Val{:measure}, ex) = print_quantum(io, ex)

function print_quantum(io, ::Val{:register}, ex)
    if ex.args[2] === :new
        printstyled(io, "%new%"; color = :light_blue, bold = true)
        print(io, "(", ex.args[3], ")")
    elseif ex.args[2] === :prev
        printstyled(io, "%prev%"; color = :light_blue, bold = true)
    else
        print(io, ex)
    end
end

function print_quantum(io, ex)
    printstyled(io, ex.args[1]; color = :light_blue, bold = true)
    print(io, "(")
    #         location          gate
    print(io, ex.args[2])
    for each in ex.args[3:end]
        print(io, ", ")
        print(io, each)
    end
    print(io, ")")
end
