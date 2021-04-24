abstract type YaoCompileTarget <: AbstractCompilerTarget end

@option struct CompileOptions
    group_quantum_stmts::Bool = true
    phase_teleportation::Bool = false
    clifford_simplification::Bool = false
end

"""
compile to generic Julia function calls.
"""
@option struct JLGenericTarget <: YaoCompileTarget
    options::CompileOptions = CompileOptions()
end

"""
compile to basic quantum circuit simulation routines.
"""
@option struct JLEmulationTarget <: YaoCompileTarget
    options::CompileOptions = CompileOptions()
end

@option struct OpenQASM2Target <: YaoCompileTarget
    options::CompileOptions = CompileOptions()
end

@option struct IBMQobjTarget <: YaoCompileTarget
    options::CompileOptions = CompileOptions()
    inline_qelib::Bool=false
end

struct YaoInterpreter{Target <: YaoCompileTarget} <: JuliaLikeInterpreter
    native_interpreter::NativeInterpreter
    target::Target
end

YaoInterpreter() = YaoInterpreter(JLGenericTarget())
YaoInterpreter(target) = YaoInterpreter(NativeInterpreter(), target)

function Core.Compiler.abstract_call(
        interp::YaoInterpreter,
        fargs::Union{Nothing,Vector{Any}},
        argtypes::Vector{Any},
        sv::InferenceState,
        max_methods::Int = InferenceParams(interp).MAX_METHODS,
    )

    # @show fargs
    ft = argtypes[1]
    if isa(ft, Const)
        f = ft.val
    elseif isconstType(ft)
        f = ft.parameters[1]
    elseif isa(ft, DataType) && isdefined(ft, :instance)
        f = ft.instance
    else
        # non-constant function, but the number of arguments is known
        # and the ft is not a Builtin or IntrinsicFunction
        if Core.Compiler.typeintersect(widenconst(ft), Core.Builtin) != Union{}
            Core.Compiler.add_remark!(interp, sv, "Could not identify method table for call")
            return Core.Compiler.CallMeta(Any, false)
        end
        return Core.Compiler.abstract_call_gf_by_type(
            interp,
            nothing,
            argtypes,
            argtypes_to_type(argtypes),
            sv,
            max_methods,
        )
    end

    allconst = true
    for x in argtypes
        if !(x isa Const)
            allconst = false
        end
    end

    if Intrinsics.isintrinsic(f)
        return abstract_call_quantum(interp, f, fargs, argtypes, sv, max_methods)
    elseif (f isa UnionAll || f isa DataType) && f <: IntrinsicRoutine && allconst
        # force intrinsic gates pure
        callinfo = Core.Compiler.abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
        return CallMeta(callinfo.rt, MethodResultPure())
    elseif f === Locations && allconst
        return CallMeta(Const(f(argtypes[2].val)), MethodResultPure())
    elseif f === CtrlLocations && allconst
        la = length(fargs) - 1
        if la == 1
            loc = f(argtypes[2].val)
        elseif la == 2
            loc = f(argtypes[2].val, argtypes[3].val)
        else
            error("Invalid CtrlLocation statement")
        end
        return CallMeta(Const(loc), MethodResultPure())
    elseif f === getindex && allconst
        # getindex(locs, locs)
        la = length(argtypes)
        a = argtypes[2].val
        b = argtypes[3].val
        if la == 3 && a isa AbstractLocations && b isa AbstractLocations
            return CallMeta(Const(f(argtypes[2].val, argtypes[3].val)), MethodResultPure())
        end
    end

    return Core.Compiler.abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
end

function abstract_call_quantum(
    interp::AbstractInterpreter,
    @nospecialize(f),
    fargs::Union{Nothing,Vector{Any}},
    argtypes::Vector{Any},
    sv::InferenceState,
    max_methods::Int = InferenceParams(interp).MAX_METHODS,
)

    if f === Intrinsics.measure
        return Core.Compiler.CallMeta(Int, MethodResultPure())
    elseif f === Intrinsics.gate || f === Intrinsics.ctrl
        gt = widenconst(argtypes[2])
        if gt <: IntrinsicRoutine
            callinfo = CallMeta(Const(nothing), MethodResultPure())
        else
            callinfo = Core.Compiler.abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
        end
        return CallMeta(callinfo.rt, nothing)
    else # mark other intrinsic pure
        return Core.Compiler.CallMeta(Const(nothing), MethodResultPure())
    end
end

function CompilerPluginTools.optimize(interp::YaoInterpreter, ir::IRCode)
    ir = inline_const!(ir)
    ir = const_invoke!(map_check_nothrow, ir, GlobalRef(YaoLocations, :map_check))
    ir = compact!(ir, true) # Simplify CFG
    # group quantum statements so we can work on
    # larger quantum circuits before we start optimizations
    ir = Core.Compiler.cfg_simplify!(ir)

    if interp.target.options.group_quantum_stmts
        ir = group_quantum_stmts!(ir)
    end

    if interp.target.options.phase_teleportation
    end

    if interp.target.options.clifford_simplification
    end

    ir = target_specific_optimization(interp.target, ir)
    ir = compact!(ir)
    return ir
end

target_specific_optimization(::YaoCompileTarget, ir::IRCode) = ir

function group_quantum_stmts!(ir::IRCode)
    perm = group_quantum_stmts_perm(ir)
    return permute_stmts!(ir, perm)
end

function group_quantum_stmts_perm(ir::IRCode)
    perms = Int[]
    cstmts_tape = Int[]
    qstmts_tape = Int[]

    for b in ir.cfg.blocks
        for v in b.stmts
            e = ir.stmts[v][:inst]
            @switch e begin
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), args...)
                    exit_block!(perms, cstmts_tape, qstmts_tape)
                    push!(perms, v)
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), args...)
                    exit_block!(perms, cstmts_tape, qstmts_tape)
                    push!(perms, v)
                @case Expr(:invoke, _, GlobalRef(Intrinsics, _), args...)
                    push!(qstmts_tape, v)
                @case ::ReturnNode || ::GotoIfNot || ::GotoNode
                    exit_block!(perms, cstmts_tape, qstmts_tape)
                    push!(cstmts_tape, v)
                @case Expr(:enter, _...)
                    exit_block!(perms, cstmts_tape, qstmts_tape)
                    push!(cstmts_tape, v)
                @case _
                    push!(cstmts_tape, v)
            end
        end
        exit_block!(perms, cstmts_tape, qstmts_tape)
    end

    append!(perms, cstmts_tape)
    append!(perms, qstmts_tape)

    return perms # permute_stmts(ci, perms)
end

function exit_block!(perms::Vector, cstmts_tape::Vector, qstmts_tape::Vector)
    append!(perms, cstmts_tape)
    append!(perms, qstmts_tape)
    empty!(cstmts_tape)
    empty!(qstmts_tape)
    return perms
end
