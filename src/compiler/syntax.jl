xlocations(ex) = Expr(:call, :($YaoLocations.Locations), ex)
xctrl_locations(ex) = Expr(:call, :($YaoLocations.CtrlLocations), ex)

"""
    @gate <locs> => <gate>

Syntax sugar for `apply(gate, locs)`, must be used inside `@device`.
See also [`@device`](@ref).

!!! tips
    You don't have to write `@gate` in most cases inside `@device`.
    But in case there is ambuigity, you can annotate the expression
    with `@gate` explicitly.
"""
macro gate(ex::Expr)
    @match ex begin
        :($locs => $gate) => esc(xcall(GlobalRef(Intrinsics, :apply), gate, xlocations(locs)))
        _ => error("syntax: invalid syntax, expect @gate <locs> => <gate>")
    end
end

"""
    @ctrl <ctrl_locs> <locs> => <gate>

Syntax sugar for `apply(gate, locs, ctrl_locs)`, must be used inside `@device`.
See also [`@device`](@ref).
"""
macro ctrl(ctrl_locs, ex::Expr)
    @match ex begin
        :($locs => $gate) => esc(
            xcall(GlobalRef(Intrinsics, :apply), gate, xlocations(locs), xctrl_locations(ctrl_locs)),
        )
        _ => error("syntax: invalid syntax, expect @ctrl <ctrl_locs> <locs> => <gate>")
    end
end

"""
    @measure locs

Syntax sugar for `measure(locs)`.
"""
macro measure(locs)
    esc(xcall(GlobalRef(Intrinsics, :measure), xlocations(locs)))
end

"""
    @barrier locs

Syntax sugar for `barrier(locs)`.
"""
macro barrier(locs)
    esc(xcall(GlobalRef(Intrinsics, :barrier), xlocations(locs)))
end

"""
    @device <function def>

Annotate a Julia function as YaoLang device kernel.
"""
macro device(ex)
    esc(device_m(__module__, ex))
end

"""
    routine_stub(routine, args...)

A function to store the raw `CodeInfo` of defined routines
to support multiple dispatch. We don't directly define `main`
`gate`, `ctrl` etc. because we can't support multiple dispatch
on them directly.
"""
function routine_stub end

function device_m(mod::Module, ex)
    is_function(ex) || error("expect a function definition")
    jlfn = JLFunction(ex)
    isnothing(jlfn.kwargs) || error("kwargs is not supported")

    return codegen_routine(jlfn)
end

function codegen_routine(jlfn::JLFunction)
    typename =
        isnothing(jlfn.name) ? gensym(:routine) :
        Meta.isexpr(jlfn.name, :(::)) ? jlfn.name.args[end] : Symbol("#", jlfn.name, "#")

    return quote
        $(codegen_routine_type(jlfn, typename))
        $(codegen_operation(jlfn, typename))
        $(codegen_routine_stub(jlfn, typename))
        $(codegen_binding(jlfn, typename))
    end
end

function codegen_routine_type(def::JLFunction, typename)
    def.name isa Symbol || isnothing(def.name) || return
    name = isnothing(def.name) ? typename : def.name

    jlstruct =
        JLStruct(; name = typename, supertype = :($YaoCompiler.GenericRoutine{$(QuoteNode(name))}))
    return codegen_ast(jlstruct)
end

function codegen_binding(def::JLFunction, typename)
    Meta.isexpr(def.name, :(::)) && return

    if isnothing(def.name)
        return :(Core.@__doc__ $typename())
    else
        return :(Core.@__doc__ const $(def.name) = $typename())
    end
end

function codegen_operation(def::JLFunction, typename)
    self = gensym(:self)
    args = name_only.(def.args)

    jlfn = JLFunction(;
        name = :($self::$typename),
        args = def.args,
        whereparams = def.whereparams,
        rettype = def.rettype,
        line = def.line,
        body = quote
            $YaoCompiler.Operation($self, $(xtuple(args...)))
        end,
    )

    return codegen_ast(jlfn)
end

function codegen_routine_stub(def::JLFunction, typename)
    self = @match def.name begin
        :($name::$type) => name
        :(::$type) => gensym(:self)
        _ => gensym(:self)
    end

    jlfn = JLFunction(;
        name = :($YaoCompiler.routine_stub),
        args = [:($self::$typename), def.args...],
        whereparams = def.whereparams,
        rettype = def.rettype,
        line = def.line,
        body = transpile_gate_syntax(transpile_intrinsic(def.body)),
    )

    return codegen_ast(jlfn)
end

function is_syntax_macro(ex)
    @match ex begin
        Symbol("@gate") => true
        Symbol("@ctrl") => true
        Symbol("@measure") => true
        Symbol("@barrier") => true
        Expr(:., :YaoCompiler, QuoteNode(name)) => is_syntax_macro(name)
        Expr(:., YaoCompiler, QuoteNode(name)) => is_syntax_macro(name)
        GlobalRef(YaoCompiler, name) => is_syntax_macro(name)
        _ => false
    end
end

# NOTE: locs => gate is only a syntax sugar for gate(gate, locs)
function transpile_intrinsic(ex)
    @match ex begin
        Expr(:call, :apply, gate, locs) =>
            xcall(GlobalRef(Intrinsics, :apply), gate, xlocations(locs))
        Expr(:call, :apply, gate, locs, ctrl) =>
            xcall(GlobalRef(Intrinsics, :apply), gate, xlocations(locs), xctrl_locations(ctrl))
        Expr(:call, :measure, locs) => xcall(GlobalRef(Intrinsics, :measure), xlocations(locs))
        Expr(:call, :barrier, locs) => xcall(GlobalRef(Intrinsics, :barrier), xlocations(locs))
        Expr(:call, :expect, locs) => xcall(GlobalRef(Intrinsics, :expect), xlocations(locs))
        # check misused keyword error
        Expr(:call, :apply, _...) => error("syntax: apply is a preserved intrinsic function")
        Expr(:call, :measure, _...) => error("syntax: apply is a preserved intrinsic function")
        Expr(:call, :barrier, _...) => error("syntax: apply is a preserved intrinsic function")
        Expr(:call, :expect, _...) => error("syntax: apply is a preserved intrinsic function")
        Expr(head, args...) => Expr(head, map(transpile_intrinsic, args)...)
        _ => ex
    end
end

function transpile_gate_syntax(ex)
    @match ex begin
        # this only treat => syntax in block/let/if/for etc. as gate stmt
        :($locs => $gate) => xcall(GlobalRef(Intrinsics, :apply), gate, xlocations(locs))
        # this will appear in anonymous function definition
        # TODO: disambuigity this and function contains only single line
        # @device function circuit(theta, phi)
        #     1 => X
        # end
        # Expr(:block, stmt1, line::LineNumberNode, stmt2) => ex
        Expr(:macrocall, Symbol("@device"), _...) => error("syntax: cannot have nested @device")
        Expr(:macrocall, name, args...) => begin
            if is_syntax_macro(name)
                return ex
            else
                # we force top scope locs=>gate to be treated as gate stmt inside
                # all @device region including non-YaoCompiler macros to make things
                # like @inbounds etc. work
                return Expr(:macrocall, name, map(transpile_gate_syntax, args)...)
            end
        end
        # we only white list other syntax here to be safe
        Expr(:block, args...) ||
            Expr(:if, args...) ||
            Expr(:elseif, args...) ||
            Expr(:let, args...) ||
            Expr(:for, args...) ||
            Expr(:try, args...) => Expr(ex.head, map(transpile_gate_syntax, args)...)

        Expr(:function, call, body) => Expr(:function, call, transpile_gate_syntax(body))
        _ => ex
    end
end

@generated function Intrinsics.apply(::AbstractRegister, op::Operation{P, Args}) where {P, Args}
    ci, nargs = obtain_codeinfo(op)
    new = NewCodeInfo(ci)
    register = insert!(new.slots, 2, Symbol("#register#"))
    operation = insert!(new.slots, 3, Symbol("#op#"))
    unpack_operation!(new, operation, nargs)
    for (v, stmt) in new
        @switch stmt begin
            @case Expr(:call, GlobalRef(&Intrinsics, name), args...)
            new[v] = xcall(GlobalRef(Intrinsics, name), register, args...)
            @case Expr(:(=), slot, Expr(:call, GlobalRef(&Intrinsics, name), args...))
            new[v] = Expr(:(=), slot, Expr(:call, GlobalRef(Intrinsics, name), register, args...))
            @case _
            nothing
        end
    end
    code_info = finish(new)
    if code_info.method_for_inference_limit_heuristics === nothing
        method = first(methods(P, Args))
        code_info.method_for_inference_limit_heuristics = method
    end
    return code_info
end

function _update_slot_stmt(f, new, v, stmt)
    @switch stmt begin
        @case Expr(:call, _...)
        new[v] = f(stmt)
        @case Expr(:(=), slot, rhs)
        new[v] = Expr(:(=), slot, f(rhs))
        @case _
        nothing
    end
end

@generated function Intrinsics.apply(::AbstractRegister, op::Operation{P, Args}, ::Locations) where {P, Args}
    ci, nargs = obtain_codeinfo(op)
    new = NewCodeInfo(ci)
    register = insert!(new.slots, 2, Symbol("#register#"))
    operation = insert!(new.slots, 3, Symbol("#op#"))
    glob_locs = insert!(new.slots, 4, Symbol("#locs#"))
    unpack_operation!(new, operation, nargs)

    for (v, stmt) in new
        _update_slot_stmt(new, v, stmt) do stmt
            @switch stmt begin
                @case Expr(:call, GlobalRef(&Intrinsics, :apply), gate, locs)
                new_locs = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                return xcall(GlobalRef(Intrinsics, :apply), register, gate, new_locs)

                @case Expr(:call, GlobalRef(&Intrinsics, :apply), gate, locs, ctrl)
                new_locs = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                new_ctrl = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, ctrl))
                return xcall(GlobalRef(Intrinsics, :apply), register, gate, new_locs, new_ctrl)

                @case Expr(:call, GlobalRef(&Intrinsics, :measure), locs)
                new_locs = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                return xcall(GlobalRef(Intrinsics, :measure), register, new_locs)

                @case Expr(:call, GlobalRef(&Intrinsics, :barrier), locs)
                new_locs = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                return xcall(GlobalRef(Intrinsics, :barrier), register, new_locs)

                @case Expr(:call, GlobalRef(&Intrinsics, :expect), locs)
                new_locs = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                return xcall(GlobalRef(Intrinsics, :expect), register, new_locs)
                @case _
                return stmt
            end
        end
    end
    code_info = finish(new)
    if code_info.method_for_inference_limit_heuristics === nothing
        method = first(methods(P, Args))
        code_info.method_for_inference_limit_heuristics = method
    end
    return code_info
end

@generated function Intrinsics.apply(::AbstractRegister, op::Operation{P, Args}, ::Locations, ::CtrlLocations) where {P, Args}
    ci, nargs = obtain_codeinfo(op)
    new = NewCodeInfo(ci)
    register = insert!(new.slots, 2, Symbol("#register#"))
    operation = insert!(new.slots, 3, Symbol("#op#"))
    glob_locs = insert!(new.slots, 4, Symbol("#locs#"))
    glob_ctrl = insert!(new.slots, 5, Symbol("#ctrl#"))
    unpack_operation!(new, operation, nargs)

    for (v, stmt) in new
        _update_slot_stmt(new, v, stmt) do stmt
            @switch stmt begin
                @case Expr(:call, GlobalRef(&Intrinsics, :apply), gate, locs)
                new_locs = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                return xcall(GlobalRef(Intrinsics, :apply), register, gate, new_locs, glob_ctrl)

                @case Expr(:call, GlobalRef(&Intrinsics, :apply), gate, locs, ctrl)
                new_locs = push!(new, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                new_ctrl = push!(new, xcall(GlobalRef(Base, :getindex), glob_locs, ctrl))
                new_ctrl =
                    push!(new, xcall(GlobalRef(YaoLocations, :merge_locations), new_ctrl, glob_ctrl))
                return xcall(GlobalRef(Intrinsics, :apply), register, gate, new_locs, new_ctrl)

                @case Expr(:call, GlobalRef(&Intrinsics, :measure), locs)
                return :(error("cannot apply quantum control on measurement"))

                @case Expr(:call, GlobalRef(&Intrinsics, :barrier), locs)
                new_locs = insert!(new, v, xcall(GlobalRef(Base, :getindex), glob_locs, locs))
                return xcall(GlobalRef(Intrinsics, :barrier), register, new_locs)

                @case Expr(:call, GlobalRef(&Intrinsics, :expect), locs)
                return :(error("cannot apply quantum control on measurement (expectation)"))
                @case _
                return stmt
            end
        end
    end
    code_info = finish(new)
    if code_info.method_for_inference_limit_heuristics === nothing
        method = first(methods(P, Args))
        code_info.method_for_inference_limit_heuristics = method
    end
    return code_info
end


function obtain_codeinfo(::Type{Operation{P,Args}}) where {P,Args}
    nargs = length(Args.parameters)
    tt = Tuple{P,Args.parameters...}
    ms = methods(routine_stub, tt)
    @assert length(ms) == 1
    method = first(ms)
    method_args = Tuple{typeof(routine_stub),tt.parameters...}
    mi = Core.Compiler.specialize_method(method, method_args, Core.svec())
    ci = Core.Compiler.retrieve_code_info(mi)

    name = routine_name(P)
    linetable = Any[]
    for lineinfo in ci.linetable
        push!(
            linetable,
            Core.LineInfoNode(
                lineinfo.module,
                name,
                lineinfo.file,
                lineinfo.line,
                lineinfo.inlined_at,
            ),
        )
    end
    ci.linetable = linetable
    ci.edges = Core.MethodInstance[mi]
    return ci, nargs
end

function unpack_operation!(new::NewCodeInfo, op::NewSlotNumber, nargs::Int)
    # %parent = op.parent
    parent = push!(new, Expr(:call, GlobalRef(Base, :getfield), op, QuoteNode(:parent)))

    if nargs > 0
        # %args = op.args
        args = push!(new, Expr(:call, GlobalRef(Base, :getfield), op, QuoteNode(:args)))
    end

    # %self = %parent
    push!(new, Expr(:(=), SlotNumber(2), parent))

    for k in 1:nargs
        push!(new, Expr(:(=), SlotNumber(k + 2), xcall(GlobalRef(Base, :getindex), args, k)))
    end
    return new
end
