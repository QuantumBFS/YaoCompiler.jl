# we cache different compile target results
const GLOBAL_CI_CACHE = Dict{Any,GPUCompiler.CodeCache}()

@option struct HardwareFreeOptions <: AbstractCompilerParams
    group_quantum_stmts::Bool = true
    phase_teleportation::Bool = false
    clifford_simplification::Bool = false
end

abstract type YaoCompileTarget <: AbstractCompilerTarget end

"""
    NativeJuliaTarget <: YaoCompileTarget

This target assumes one compiles to native Julia, thus the measurement
type will be directly inferred from native intrinsics.
"""
abstract type NativeJuliaTarget <: YaoCompileTarget end

function target_measure_result_type(::NativeJuliaTarget, interp, f, fargs, argtypes, sv, max_methods)
    callinfo = Core.Compiler.abstract_call_known(parent(interp), f, fargs, argtypes, sv, max_methods)
    return callinfo.rt
end

"""
    target_measure_result_type(target::YaoCompileTarget[, interp::YaoInterpreter, f, fargs, argtypes, sv, max_methods])

Hook for handling different measurement result type. Default type is `MeasureResult`.
"""
function target_measure_result_type(
    target::YaoCompileTarget,
    interp,
    f,
    fargs,
    argtypes,
    sv,
    max_methods,
)
    target_measure_result_type(target)
end

# this is not always true, we need to replace this with a compiler internal
# type
target_measure_result_type(::YaoCompileTarget) = MeasureResult{Int}

@option struct JLGenericTarget <: YaoCompileTarget end

get_cache(target::YaoCompileTarget) = GLOBAL_CI_CACHE[target]

@option struct YaoInterpreter{Target<:YaoCompileTarget} <: JuliaLikeInterpreter
    native_interpreter::NativeInterpreter = NativeInterpreter()
    target::Target = JLGenericTarget()
    cache::CodeCache = get!(GLOBAL_CI_CACHE, target, GPUCompiler.CodeCache())
    options::HardwareFreeOptions = HardwareFreeOptions()
    max_const_invoke_elim::Int = 10
end

YaoInterpreter(target; kw...) = YaoInterpreter(NativeInterpreter(), target; kw...)

function Core.Compiler.code_cache(interp::YaoInterpreter)
    Core.Compiler.WorldView(get_cache(interp.target), Core.Compiler.get_world_counter(interp))
end

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

    if Intrinsics.isintrinsic(f)
        return abstract_call_quantum(interp, f, fargs, argtypes, sv, max_methods)
    elseif (f isa UnionAll || f isa DataType) && f <: IntrinsicRoutine && allconst
        # force intrinsic gates not pure to prevent inline
        callinfo = Core.Compiler.abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
        return CallMeta(callinfo.rt, nothing)
    elseif f === Locations && allconst # force Locations pure to inline
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
        if la == 3 && argtypes[2].val isa AbstractLocations && argtypes[3].val isa AbstractLocations
            return CallMeta(Const(f(argtypes[2].val, argtypes[3].val)), MethodResultPure())
        end
    elseif f === measure_cmp
        return CallMeta(Bool, nothing)
    end

    return Core.Compiler.abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
end

function abstract_call_quantum(
    interp::YaoInterpreter,
    @nospecialize(f),
    fargs::Union{Nothing,Vector{Any}},
    argtypes::Vector{Any},
    sv::InferenceState,
    max_methods::Int = InferenceParams(interp).MAX_METHODS,
)

    if f === Intrinsics.measure
        result_type =
            target_measure_result_type(interp.target, interp, f, fargs, argtypes, sv, max_methods)
        return Core.Compiler.CallMeta(result_type, nothing)
    elseif f === Intrinsics.apply # || f === Intrinsics.ctrl
        gt = widenconst(argtypes[3])

        if gt <: IntrinsicRoutine
            return CallMeta(Const(nothing), nothing)
        else
            ret = Core.Compiler.abstract_call_known(interp, f, fargs, argtypes, sv, max_methods)
            return ret
        end
    else # mark other intrinsic not pure to prevent inline
        return Core.Compiler.CallMeta(Const(nothing), nothing)
    end
end

function elim_location_mapping!(ir::IRCode)
    ir = const_invoke!(map_check_nothrow, ir, GlobalRef(YaoLocations, :map_check_nothrow))
    ir = const_invoke!(unsafe_mapping, ir, GlobalRef(YaoLocations, :unsafe_mapping))
    ir = const_invoke!(merge_locations, ir, GlobalRef(YaoLocations, :merge_locations))
    ir = compact!(ir, true) # Simplify CFG
    # group quantum statements so we can work on
    # larger quantum circuits before we start optimizations
    ir = Core.Compiler.cfg_simplify!(ir)
    return compact!(ir)
end

function CompilerPluginTools.optimize(interp::YaoInterpreter, ir::IRCode)
    ir = inline_const!(ir)
    ir = elim_location_mapping!(ir)

    # try to eliminate location mapping as much as possible
    count = 0
    while count < interp.max_const_invoke_elim &&
        contains_const_invoke(ir, GlobalRef(YaoLocations, :map_check_nothrow))
        ir = elim_location_mapping!(ir)
        count += 1
    end

    if interp.options.group_quantum_stmts
        ir = group_quantum_stmts!(ir)
    end

    # ir = quote_globalref_gate(ir)
    # target agnoistic
    if interp.options.phase_teleportation
        ir = run_pure_quantum_passes(ir) do block_ir::BlockIR
            phase_teleportation(block_ir)
        end
    end
    
    if interp.options.clifford_simplification
        ir = run_pure_quantum_passes(ir) do block_ir::BlockIR
            clifford_simplification(block_ir)
        end
    end

    ir = target_specific_pipeline(interp.target, ir)
    ir = compact!(ir)
    return ir
end

target_specific_pipeline(::YaoCompileTarget, ir::IRCode) = ir

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
                # terminator
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _...) ||
                      Expr(:invoke, _, GlobalRef(Intrinsics, :expect), _...) ||
                      Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _...)

                exit_block!(perms, cstmts_tape, qstmts_tape)
                push!(perms, v)
                # intrinsic
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _...)
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

function compute_quantum_blocks(ir::IRCode)
    quantum_blocks = UnitRange{Int}[]

    for b in ir.cfg.blocks
        start, stop = 0, 0
        for v in b.stmts
            st = ir.stmts[v][:inst]

            @switch st begin
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _...)
                    if start > 0
                        stop += 1
                    else
                        start = stop = v
                    end
                # quantum terminator
                @case Expr(:invoke, _, GlobalRef(Intrinsic, :measure), _...) ||
                    Expr(:invoke, _, GlobalRef(Intrinsic, :barrier), _...) ||
                    Expr(:invoke, _, GlobalRef(Intrinsic, :expect), _...)

                    if start > 0
                        push!(quantum_blocks, start:stop+1)
                    else
                        push!(quantum_blocks, v:v)
                    end
                    start = stop = 0
                # classical terminator
                @case _
                    if start > 0
                        push!(quantum_blocks, start:stop)
                        start = stop = 0
                    end
            end

        end

        if start > 0
            push!(quantum_blocks, start:stop)
        end
    end
    return quantum_blocks
end

function quote_globalref_gate(ir::IRCode, quantum_blocks=compute_quantum_blocks(ir))
    @show ir
    for b in quantum_blocks, v in b
        @switch ir.stmts[v][:inst] begin
            @case Expr(:invoke, mi, &(GlobalRef(Intrinsics, :apply)), reg, op, args...)
                ir.stmts[v][:inst] = Expr(:invoke, mi, GlobalRef(Intrinsics, :apply), reg, CompilerPluginTools.eval_global(op), args...)
            @case _
                nothing
        end
    end
    return ir
end

function run_pure_quantum_passes(f, ir::IRCode, quantum_blocks=compute_quantum_blocks(ir))
    n = count_qubits(ir, quantum_blocks)
    # NOTE: we can't optimize
    # non-constant location program
    isnothing(n) && return ir
    iszero(n) && return ir

    # NOTE: pure quantum block requires locations to be constant
    compact = IncrementalCompact(ir, true)
    for b in quantum_blocks
        circuit = Chain()
        for v in b
            e = ir.stmts[v][:inst]
            @switch e begin
                # constant case
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, QuoteNode(op), QuoteNode(locs))
                    push!(circuit.args, Gate(op, locs))
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, QuoteNode(op), QuoteNode(locs), QuoteNode(ctrl))
                    push!(circuit.args, Ctrl(Gate(op, locs), ctrl))

                # gate operator is not constant
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, op::SSAValue, QuoteNode(locs))
                    op_type = ir.stmts[op.id][:type]
                    push!(circuit.args, Gate(Typed(op, op_type), locs))
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, op::SSAValue, QuoteNode(locs), QuoteNode(ctrl))
                    op_type = ir.stmts[op.id][:type]
                    push!(circuit.args, Ctrl(Gate(Typed(op, op_type), locs), ctrl))

                # terminators
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, ::QuoteNode) ||
                    Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, ::QuoteNode) ||
                    Expr(:invoke, _, GlobalRef(Intrinsics, :expect), _, ::QuoteNode)
                    break

                # not pure quantum anymore
                @case _
                    return ir
            end
        end

        block_ir = f(BlockIR(ir, n, circuit))::BlockIR

        # delete old circuit
        reg = Argument(2)
        reg_type = widenconst(ir.argtypes[2])
        first_terminator = nothing
        for v in b
            e = ir.stmts[v][:inst]
            @switch e begin
                @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, op, locs) ||
                Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, op, locs, ctrl)
                    compact[v] = nothing
                @case _ # don't process measure etc.
                    first_terminator = v
                    break
            end
        end

        # pure quantum block without measure/barrier
        if isnothing(first_terminator)
            first_terminator = last(b) + 1
        end

        # assert type
        first_terminator = first_terminator::Int

        # NOTE: assume we will only get constant gate
        #       and constant locations
        #       reject non-constant gate/locations for now
        for each in YaoHIR.leaves(block_ir.circuit)
            @switch each begin
                @case Gate(op::IntrinsicRoutine, locs::Locations)
                    mi = specialize_apply(reg_type, typeof(op), typeof(locs))
                    e = Expr(:invoke, mi, Intrinsics.apply, reg, QuoteNode(op), QuoteNode(locs))
                @case Ctrl(Gate(op::IntrinsicRoutine, locs::Locations), ctrl::CtrlLocations)
                    mi = specialize_apply(reg_type, typeof(op), typeof(locs), typeof(ctrl))
                    e = Expr(:invoke, mi, Intrinsics.apply, reg, QuoteNode(op), QuoteNode(locs), QuoteNode(ctrl))
                @case Gate(Typed(op::SSAValue, op_type), locs::Locations)
                    mi = specialize_apply(reg_type, op_type, typeof(locs))
                    e = Expr(:invoke, mi, Intrinsics.apply, reg, op, QuoteNode(locs))
                @case Ctrl(Gate(Typed(op::SSAValue, op_type), locs::Locations), ctrl::CtrlLocations)
                    mi = specialize_apply(reg_type, op_type, typeof(locs), typeof(ctrl))
                    e = Expr(:invoke, mi, Intrinsics.apply, reg, op, QuoteNode(locs), QuoteNode(ctrl))
                @case _
                    error("invalid statement: $each")
            end

            Core.Compiler.insert_node!(compact, SSAValue(first_terminator), Nothing, e)
        end
    end

    for _ in compact; end
    new = Core.Compiler.finish(compact)
    return new
end

function count_qubits(ir::IRCode, quantum_blocks=compute_quantum_blocks(ir))
    locations = Set{Locations{Int}}()

    for b in quantum_blocks, v in b
        e = ir.stmts[v][:inst]
        @switch e begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, locs)
                locs isa QuoteNode || return
                union!(locations, collect(locs.value))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, locs, ctrl)
                locs isa QuoteNode || return
                ctrl isa QuoteNode || return
                union!(locations, collect(locs.value))
                union!(locations, collect(ctrl.value.storage))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, locs)
                locs isa QuoteNode || return
                union!(locations, collect(locs.value))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, locs)
                locs isa QuoteNode || return
                union!(locations, collect(locs.value))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :expect), _, locs, _)
                locs isa QuoteNode || return
                union!(locations, collect(locs.value))
            @case _
                nothing
        end
    end
    return length(locations)
end

function specialize_apply(reg_type, op_type, locs_type)
    method = first(methods(Intrinsics.apply, Tuple{reg_type, op_type, locs_type}))
    atypes = Tuple{typeof(Intrinsics.apply), reg_type, op_type, locs_type}
    return Core.Compiler.specialize_method(method, atypes, Core.svec())
end

function specialize_apply(reg_type, op_type, locs_type, ctrl_type)
    method = first(methods(Intrinsics.apply, Tuple{reg_type, op_type, locs_type, ctrl_type}))
    atypes = Tuple{typeof(Intrinsics.apply), reg_type, op_type, locs_type, ctrl_type}
    return Core.Compiler.specialize_method(method, atypes, Core.svec())
end
