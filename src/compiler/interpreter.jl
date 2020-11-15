struct YaoInterpreter <: AbstractInterpreter
    native_interpreter::Core.Compiler.NativeInterpreter
    passes::Vector{Symbol}
end

struct YaoOptimizationParams
    native::OptimizationParams
    passes::Vector{Symbol}
end

default_passes() = [:zx]

YaoInterpreter(; passes::Vector{Symbol} = default_passes()) =
    YaoInterpreter(Core.Compiler.NativeInterpreter(), passes)

InferenceParams(interp::YaoInterpreter) = InferenceParams(interp.native_interpreter)
OptimizationParams(interp::YaoInterpreter) = OptimizationParams(interp.native_interpreter)
YaoOptimizationParams(interp::YaoInterpreter) =
    YaoOptimizationParams(OptimizationParams(interp), interp.passes)
Core.Compiler.get_world_counter(interp::YaoInterpreter) = get_world_counter(interp.native_interpreter)
Core.Compiler.get_inference_cache(interp::YaoInterpreter) =
    get_inference_cache(interp.native_interpreter)
Core.Compiler.code_cache(interp::YaoInterpreter) = Core.Compiler.code_cache(interp.native_interpreter)
Core.Compiler.may_optimize(interp::YaoInterpreter) =
    Core.Compiler.may_optimize(interp.native_interpreter)
Core.Compiler.may_discard_trees(interp::YaoInterpreter) =
    Core.Compiler.may_discard_trees(interp.native_interpreter)
Core.Compiler.may_compress(interp::YaoInterpreter) =
    Core.Compiler.may_compress(interp.native_interpreter)
Core.Compiler.unlock_mi_inference(interp::YaoInterpreter, mi::Core.MethodInstance) =
    Core.Compiler.unlock_mi_inference(interp.native_interpreter, mi)
Core.Compiler.lock_mi_inference(interp::YaoInterpreter, mi::Core.MethodInstance) =
    Core.Compiler.lock_mi_inference(interp.native_interpreter, mi)
Core.Compiler.add_remark!(interp::YaoInterpreter, st::Core.Compiler.InferenceState, msg::String) =
    nothing # println(msg)

function Core.Compiler.abstract_call(
    interp::YaoInterpreter,
    fargs::Union{Nothing,Vector{Any}},
    argtypes::Vector{Any},
    sv::InferenceState,
    max_methods::Int = InferenceParams(interp).MAX_METHODS,
)

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

    if f isa Function && parentmodule(f) === Semantic
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

    if f === Semantic.measure
        return Core.Compiler.CallMeta(Int, MethodResultPure())
    elseif f === Semantic.gate || f === Semantic.ctrl
        gt = widenconst(argtypes[2])
        if gt <: IntrinsicRoutine
            callinfo = CallMeta(Const(nothing), MethodResultPure())
        else
            callinfo = Core.Compiler.abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
        end
        return CallMeta(callinfo.rt, nothing)
    else
        return Core.Compiler.CallMeta(Const(nothing), MethodResultPure())
    end
end

# NOTE: this is copied from Core.Compiler._typeinf
# to insert our own passes
# Keno says we might not need to do this after 1.7+
# NOTE: most functions are different from Base due to bootstrap
# TODO: only run our passes on quantum routines
function Core.Compiler.typeinf(interp::YaoInterpreter, frame::InferenceState)
    Core.Compiler.typeinf_nocycle(interp, frame) || return false
    # with no active ip's, frame is done
    frames = frame.callers_in_cycle
    isempty(frames) && push!(frames, frame)
    for caller in frames
        @assert !(caller.dont_work_on_me)
        caller.dont_work_on_me = true
    end

    for caller in frames
        Core.Compiler.finish(caller, interp)
    end
    # collect results for the new expanded frame
    results = Tuple{InferenceResult,Bool}[
        (frames[i].result, frames[i].cached || frames[i].parent !== nothing)
        for i in 1:length(frames)
    ]

    valid_worlds = frame.valid_worlds
    cached = frame.cached
    if cached || frame.parent !== nothing
        for (caller, doopt) in results
            opt = caller.src
            if opt isa OptimizationState
                run_optimizer = doopt && Core.Compiler.may_optimize(interp)
                if run_optimizer
                    if !(frame.result.linfo.def.sig isa UnionAll) &&
                       parentmodule(frame.result.linfo.def.sig.parameters[1]) === Semantic
                        optimize(opt, YaoOptimizationParams(interp), caller.result)
                    else
                        Core.Compiler.optimize(opt, OptimizationParams(interp), caller.result)
                    end
                    Core.Compiler.finish(opt.src, interp)
                    # finish updating the result struct
                    Core.Compiler.validate_code_in_debug_mode(opt.linfo, opt.src, "optimized")
                    if opt.const_api
                        if caller.result isa Const
                            caller.src = caller.result
                        else
                            @assert isconstType(caller.result)
                            caller.src = Const(caller.result.parameters[1])
                        end
                    elseif opt.src.inferred
                        caller.src = opt.src::CodeInfo # stash a copy of the code (for inlining)
                    else
                        caller.src = nothing
                    end
                end
                # As a hack the et reuses frame_edges[1] to push any optimization
                # edges into, so we don't need to handle them specially here
                valid_worlds = Core.Compiler.intersect(valid_worlds, opt.inlining.et.valid_worlds.x)
            end
        end
    end
    if Core.Compiler.last(valid_worlds) == Core.Compiler.get_world_counter()
        valid_worlds =
            Core.Compiler.WorldRange(Core.Compiler.first(valid_worlds), Core.Compiler.typemax(UInt))
    end
    for caller in frames
        caller.valid_worlds = valid_worlds
        caller.src.min_world = Core.Compiler.first(valid_worlds)
        caller.src.max_world = Core.Compiler.last(valid_worlds)
        if cached
            Core.Compiler.cache_result!(interp, caller.result, valid_worlds)
        end
        if Core.Compiler.last(valid_worlds) == Core.Compiler.typemax(UInt)
            # if we aren't cached, we don't need this edge
            # but our caller might, so let's just make it anyways
            for caller in frames
                Core.Compiler.store_backedges(caller)
            end
        end
        # finalize and record the linfo result
        caller.inferred = true
    end
    return true
end

function is_semantic_fn_call(e)
    return e isa Expr &&
           e.head === :call &&
           e.args[1] isa GlobalRef &&
           e.args[1].mod === YaoCompiler.Semantic
end

function convert_to_quantum_head!(ci::CodeInfo)
    for (v, e) in enumerate(ci.code)
        ci.code[v] = convert_to_quantum_head(e)
    end
    return ci
end
