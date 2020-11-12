struct EchoReg{B} <: AbstractRegister{B} end
Base.show(io::IO, x::EchoReg) = print(io, "echo register")
EchoReg() = EchoReg{1}()

_snameof(x::IntrinsicRoutine) = string(routine_name(x))
_snameof(x::AbstractLocations) = string(x)

function execute(::typeof(Semantic.main), ::EchoReg, op::IntrinsicRoutine)
    @info "executing $op"
    return
end

function execute(::typeof(Semantic.gate), ::EchoReg, op::IntrinsicRoutine, loc::Locations)
    loc = sprint(print_locations, loc; context = :color => true)
    @info "executing $loc => $op"
    return
end

function execute(
    ::typeof(Semantic.ctrl),
    ::EchoReg,
    op::IntrinsicRoutine,
    loc::Locations,
    ctrl::CtrlLocations,
)
    loc = sprint(print_locations, loc; context = :color => true)
    ctrl = sprint(print_locations, ctrl; context = :color => true)
    @info "executing @ctrl $(ctrl) $loc => $op"
    return
end

function execute(::typeof(Semantic.measure), ::EchoReg, loc::Locations)
    loc = sprint(print_locations, loc; context = :color => true)
    @info "executing @measure $loc"
    return 0
end

function execute(::typeof(Semantic.barrier), ::EchoReg, loc::Locations)
    loc = sprint(print_locations, loc; context = :color => true)
    @info "executing @barrier $loc"
    return
end

"""
    TraceTape <: AbstractRegister{1}

Tracing all the intrinsic quantum operations.
"""
struct TraceTape <: AbstractRegister{1}
    inst::Vector{Any}
end

TraceTape() = TraceTape([])

function Base.:(==)(a::TraceTape, b::TraceTape)
    return a.inst == b.inst
end

Base.length(x::TraceTape) = length(x.inst)
Base.getindex(x::TraceTape, idx::Int) = x.inst[idx]

function Base.show(io::IO, tape::TraceTape)
    println(io, "YaoCompiler.trace:")
    nstmt = length(tape.inst)
    for i in 1:nstmt
        stmt = tape.inst[i]
        printstyled(io, nameof(stmt.args[1]); color = :light_blue)
        print(io, "\t"^2)

        for (i, each) in enumerate(stmt.args[2:end])
            print(io, each)

            if i != length(stmt.args[2:end])
                print(io, "\t")
            end
        end

        if i != nstmt
            println(io)
        end
    end
    return
end

function execute(stub::typeof(Semantic.main), r::TraceTape, op::IntrinsicRoutine)
    push!(r.inst, Expr(:call, stub, op))
    return
end

function execute(stub::typeof(Semantic.gate), r::TraceTape, op::IntrinsicRoutine, loc::Locations)
    push!(r.inst, Expr(:call, stub, op, loc))
    return
end

function execute(
    stub::typeof(Semantic.ctrl),
    r::TraceTape,
    op::IntrinsicRoutine,
    loc::Locations,
    ctrl::CtrlLocations,
)
    push!(r.inst, Expr(:call, stub, op, loc, ctrl))
    return
end

function execute(stub::typeof(Semantic.barrier), r::TraceTape, loc::Locations)
    push!(r.inst, Expr(:call, stub, loc))
    return
end

function execute(stub::typeof(Semantic.measure), r::TraceTape, locs::Locations)
    error("one should not trace programs contain @measure, since we cannot purify this hybrid program")
end

function trace_m(ex)
    ex isa Expr && ex.head === :call || error("expect a function call")
    tape = gensym(:tape)
    if ex.args[1] in (:gate, :ctrl)
        # execute(gate/ctrl, tape, spec, loc[, ctrl])
        return quote
            $tape = $TraceTape()
            $(Expr(:call, execute, GlobalRef(Semantic, ex.args[1]), tape, ex.args[2:end]...))
            $tape
        end
    else
        # execute(main, tape, spec)
        return quote
            $tape = $TraceTape()
            $(Expr(:call, execute, GlobalRef(Semantic, :main), tape, ex))
            $tape
        end
    end
end

function echo_m(ex)
    ex isa Expr && ex.head === :call || error("expect a function call")
    tape = gensym(:tape)
    if ex.args[1] in (:gate, :ctrl)
        return Expr(:call, execute, GlobalRef(Semantic, ex.args[1]), EchoReg(), ex.args[2:end]...)
    else
        return Expr(:call, execute, GlobalRef(Semantic, :main), EchoReg(), ex)
    end
end

macro trace(ex)
    esc(trace_m(ex))
end

macro echo(ex)
    esc(echo_m(ex))
end

# NOTE: we glue ArrayReg in a seperate package
