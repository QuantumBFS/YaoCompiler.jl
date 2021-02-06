Base.iterate(ic::Core.Compiler.IncrementalCompact) = Core.Compiler.iterate(ic)
Base.iterate(ic::Core.Compiler.IncrementalCompact, st) = Core.Compiler.iterate(ic, st)
Base.getindex(ic::Core.Compiler.IncrementalCompact, idx) = Core.Compiler.getindex(ic, idx)
Base.setindex!(ic::Core.Compiler.IncrementalCompact, v, idx) = Core.Compiler.setindex!(ic, v, idx)

Base.getindex(ic::Core.Compiler.Instruction, idx) = Core.Compiler.getindex(ic, idx)
Base.setindex!(ic::Core.Compiler.Instruction, v, idx) = Core.Compiler.setindex!(ic, v, idx)

Base.getindex(ir::Core.Compiler.IRCode, idx) = Core.Compiler.getindex(ir, idx)
Base.setindex!(ir::Core.Compiler.IRCode, v, idx) = Core.Compiler.setindex!(ir, v, idx)

Base.getindex(ref::UseRef) = Core.Compiler.getindex(ref)
Base.iterate(uses::UseRefIterator) = Core.Compiler.iterate(uses)
Base.iterate(uses::UseRefIterator, st) = Core.Compiler.iterate(uses, st)

Base.iterate(p::Core.Compiler.Pair) = Core.Compiler.iterate(p)
Base.iterate(p::Core.Compiler.Pair, st) = Core.Compiler.iterate(p, st)

Base.getindex(m::Core.Compiler.MethodLookupResult, idx::Int) = Core.Compiler.getindex(m, idx)

function Core.Compiler.optimize(interp::YaoInterpreter, opt::OptimizationState, params::OptimizationParams, @nospecialize(result))
    nargs = Int(opt.nargs) - 1
    @timeit "optimizer" ir = Core.Compiler.run_passes(opt.src, nargs, opt)
    
    # make sure all const are inlined
    # Julia itself may not inline all
    # the const values we want, e.g gates
    ir = inline_const!(ir)
    ir = elim_map_check!(ir)
    ir = compact!(ir, true) # Simplify CFG
    # group quantum statements so we can work on
    # larger quantum circuits before we start optimizations
    # ir = group_quantum_stmts!(ir)

    # # run quantum passes
    # if !isempty(interp.passes)
    #     ir = convert_to_yaoir(ir)

    #     if :zx in interp.passes
    #         ir = run_zx_passes(ir)::YaoIR
    #     end

    #     ir = ir.ir
    # end

    ir = compact!(ir)
    Core.Compiler.finish(opt, params, ir, result)
end

function group_quantum_stmts_perm(ir::IRCode)
    perms = Int[]
    cstmts_tape = Int[]
    qstmts_tape = Int[]

    for b in ir.cfg.blocks
        for v in b.stmts
            e = ir.stmts[v][:inst]
            if is_quantum_statement(e)
                if quantum_stmt_type(e) in [:measure, :barrier]
                    exit_block!(perms, cstmts_tape, qstmts_tape)
                    push!(perms, v)
                else
                    push!(qstmts_tape, v)
                end
            elseif e isa Core.ReturnNode || e isa Core.GotoIfNot || e isa Core.GotoNode
                exit_block!(perms, cstmts_tape, qstmts_tape)
                push!(cstmts_tape, v)
            elseif e isa Expr && e.head === :enter
                exit_block!(perms, cstmts_tape, qstmts_tape)
                push!(cstmts_tape, v)
            else
                push!(cstmts_tape, v)
            end
        end

        exit_block!(perms, cstmts_tape, qstmts_tape)
    end

    append!(perms, cstmts_tape)
    append!(perms, qstmts_tape)

    return perms # permute_stmts(ci, perms)
end

function permute_stmts!(stmt::InstructionStream, perm::Vector{Int})
    inst = []

    for v in perm
        e = stmt.inst[v]

        if e isa Expr
            ex = replace_from_perm(e, perm)
            push!(inst, ex)
        elseif e isa Core.GotoIfNot
            if e.cond isa Core.SSAValue
                cond = Core.SSAValue(findfirst(isequal(e.cond.id), perm))
            else
                # TODO: figure out which case is this
                # and maybe apply permute to this
                cond = e.cond
            end

            dest = findfirst(isequal(e.dest), perm)
            push!(inst, Core.GotoIfNot(cond, dest))
        elseif e isa Core.GotoNode
            push!(inst, Core.GotoNode(findfirst(isequal(e.label), perm)))
        elseif e isa Core.ReturnNode
            if isdefined(e, :val) && e.val isa Core.SSAValue
                push!(inst, Core.ReturnNode(Core.SSAValue(findfirst(isequal(e.val.id), perm))))
            else
                push!(inst, e)
            end
        else
            # RL: I think
            # other nodes won't contain SSAValue
            # let's just ignore them, but if we
            # find any we can add them here
            push!(inst, e)
            # if e isa Core.SlotNumber
            #     push!(inst, e)
            # elseif e isa Core.NewvarNode
            #     push!(inst, e)
            # else
            # end
            # error("unrecognized statement $e :: ($(typeof(e)))")
        end
    end

    copyto!(stmt.inst, inst)
    permute!(stmt.flag, perm)
    permute!(stmt.line, perm)
    permute!(stmt.type, perm)
    permute!(stmt.flag, perm)
    return stmt
end

function replace_from_perm(stmt, perm)
    stmt isa Core.SSAValue && return Core.SSAValue(findfirst(isequal(stmt.id), perm))

    if stmt isa Expr
        return Expr(stmt.head, map(x -> replace_from_perm(x, perm), stmt.args)...)
    else
        return stmt
    end
end

function exit_block!(perms::Vector, cstmts_tape::Vector, qstmts_tape::Vector)
    append!(perms, cstmts_tape)
    append!(perms, qstmts_tape)
    empty!(cstmts_tape)
    empty!(qstmts_tape)
    return perms
end

function group_quantum_stmts!(ir::IRCode)
    perm = group_quantum_stmts_perm(ir)
    permute_stmts!(ir.stmts, perm)
    return ir
end

# NOTE: this perform simple constant propagation
# inside basic blocks to get better format of
# the quantum statements

# force inline all constants
function is_arg_allconst(ir, arg)
    if arg isa Argument
        return false
    elseif arg isa SSAValue
        return is_arg_allconst(ir, ir.stmts[arg.id][:inst])
    elseif !is_inlineable_constant(arg) && !isa(arg, QuoteNode)
        return false
    end
    return true
end

function unwrap_arg(ir, arg)
    if arg isa QuoteNode
        return arg.value
    elseif arg isa SSAValue
        return unwrap_arg(ir, ir.stmts[arg.id][:inst])
    else
        return arg
    end
end

function inline_const!(ir::IRCode)
    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i][:inst]
        stmt isa Expr || continue

        if stmt.head === :call
            sig = Core.Compiler.call_sig(ir, stmt)
            f, ft, atypes = sig.f, sig.ft, sig.atypes
            allconst = true
            for atype in sig.atypes
                if !isa(atype, Const)
                    allconst = false
                    break
                end
            end

            if allconst &&
               isa(f, Core.IntrinsicFunction) &&
               is_pure_intrinsic_infer(f) &&
               intrinsic_nothrow(f, atypes[2:end])

                fargs = anymap(x::Const -> x.val, atypes[2:end])
                val = f(fargs...)
                ir.stmts[i][:inst] = quoted(val)
                ir.stmts[i][:type] = Const(val)
            elseif allconst && isa(f, Core.Builtin) && 
                   (f === Core.tuple || f === Core.getfield)
                fargs = anymap(x::Const -> x.val, atypes[2:end])
                val = f(fargs...)
                ir.stmts[i][:inst] = quoted(val)
                ir.stmts[i][:type] = Const(val)
            end
        elseif stmt.head === :new
            exargs = stmt.args[2:end]
            allconst = all(arg->is_arg_allconst(ir, arg), exargs)
            t = stmt.args[1]
            if allconst && isconcretetype(t) && !t.mutable
                args = anymap(arg->unwrap_arg(ir, arg), exargs)
                val = ccall(:jl_new_structv, Any, (Any, Ptr{Cvoid}, UInt32), t, args, length(args))

                ir.stmts[i][:inst] = quoted(val)
                ir.stmts[i][:type] = Const(val)
            end
        end
    end
    return ir
end

function elim_map_check!(ir::IRCode)
    for i in 1:length(ir.stmts)
        stmt = ir.stmts[i][:inst]
        stmt isa Expr || continue

        if stmt.head === :invoke && stmt.args[2] === GlobalRef(YaoLocations, :map_check)
            exargs = stmt.args[3:end]
            allconst = all(arg->is_arg_allconst(ir, arg), exargs)


            if allconst
                args = anymap(arg->unwrap_arg(ir, arg), exargs)
                val = map_check_nothrow(args[1], args[2])

                ir.stmts[i][:inst] = quoted(val)
                ir.stmts[i][:type] = Const(val)
            end
        end
    end
    return ir
end

struct YaoIR
    ir::IRCode
    qb::Vector{UnitRange{Int}}
end

function compute_quantum_blocks(ir::IRCode)
    quantum_blocks = UnitRange{Int}[]
    last_stmt_is_measure_or_barrier = false

    for b in ir.cfg.blocks
        start, stop = 0, 0
        for v in b.stmts
            st = ir.stmts[v][:inst]
            if is_quantum_statement(st)
                if start > 0
                    stop += 1
                else
                    start = stop = v
                end
            else
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

function convert_to_yaoir(ir::IRCode)
    quantum_blocks = compute_quantum_blocks(ir)
    return YaoIR(ir, quantum_blocks)
end

function map_virtual_location_expr(@nospecialize(e), regmap)
    qt = quantum_stmt_type(e)
    if qt === :gate
        loc = e.args[2]
    elseif qt === :ctrl
        loc = e.args[2]
        ctrl = e.args[3]
    elseif qt === :barrier
        e.args[2]
    elseif qt === :measure
    else
        error("unknown quantum statement: $tt")
    end
end

function map_virtual_location(@nospecialize(loc), regmap)
    if loc isa AbstractLocations
        return loc
    else
        # TODO: use a custom type
        regmap[length(keys(regmap))+1] = loc
        return Locations
    end
end

function run_zx_passes(ir::YaoIR)
    n = count_qubits(ir)
    # NOTE: we can't optimize
    # non-constant location program
    isnothing(n) && return ir
    iszero(n) && return ir

    compact = Core.Compiler.IncrementalCompact(ir.ir, true)
    for b in ir.qb
        qc = QCircuit(n)
        # if there is no quantum terminator
        # this will be the first classical
        # terminator
        first_terminator = nothing
        for v in b
            e = ir.ir.stmts[v][:inst]
            qt = quantum_stmt_type(e)
            if qt === :gate
                e.args[3] isa IntrinsicRoutine || break
                e.args[4] isa Locations || break
                # if can't convert to ZXDiagram, stop
                zx_push_gate!(qc, e.args[3], e.args[4]) || break
                # set old stmts to nothing
                compact[v] = nothing
            elseif qt === :ctrl
                e.args[3] isa IntrinsicRoutine || break
                e.args[4] isa Locations || break
                e.args[5] isa CtrlLocations || break
                zx_push_gate!(qc, e.args[3], e.args[4], e.args[5]) || break
                compact[v] = nothing
            elseif qt === :measure || qt === :barrier
                # we attach terminator after optimization
                if isnothing(first_terminator)
                    first_terminator = v
                end
            end
        end

        if isnothing(first_terminator)
            first_terminator = last(b)
        end

        zxd = ZXDiagram(qc)
        # ZX passes
        phase_teleportation(zxd)
        clifford_simplification(zxd)

        # TODO: we might want to check if
        # the result circuit is simpler indeed
        qc = QCircuit(zxd)
        for g in ZXCalculus.gates(qc)
            if g.name in (:H, :Z, :X, :S, :T, :Sdag, :Tdag)
                spec = getfield(Intrinsics, g.name)
                mi = specialize_gate(typeof(spec), Locations{Int})
                e = Expr(:invoke, mi, Semantic.gate, spec, Locations(g.loc))
            elseif g.name in (:shift, :Rz, :Rx)
                spec = getfield(Intrinsics, g.name)
                mi = specialize_gate(typeof(spec), Locations{Int})
                e = Expr(:invoke, mi, Semantic.gate, spec, Locations(g.loc))
            elseif g.name === :CNOT
                mi = specialize_ctrl(typeof(YaoCompiler.X), Locations{Int}, CtrlLocations{Int})
                e = Expr(
                    :invoke,
                    mi,
                    Semantic.ctrl,
                    YaoCompiler.X,
                    Locations(g.loc),
                    CtrlLocations(g.ctrl),
                )
            elseif g.name === :CZ
                mi = specialize_ctrl(typeof(YaoCompiler.Z), Locations{Int}, CtrlLocations{Int})
                e = Expr(
                    :invoke,
                    mi,
                    Semantic.ctrl,
                    YaoCompiler.Z,
                    Locations(g.loc),
                    CtrlLocations(g.ctrl),
                )
            else
                error("unknown gate $g")
            end
            Core.Compiler.insert_node!(compact, SSAValue(first_terminator), Nothing, e)
        end
    end
    for _ in compact
    end
    new = Core.Compiler.finish(compact)
    return YaoIR(new, compute_quantum_blocks(new))
end

function specialize_gate(spec, loc)
    atypes = Tuple{typeof(Semantic.gate),spec,loc}
    if spec <: IntrinsicRoutine
        method = first(methods(Semantic.gate, Tuple{IntrinsicRoutine,Locations}))
    elseif spec <: RoutineSpec
        method = first(methods(Semantic.gate, Tuple{RoutineSpec,Locations}))
    end

    return Core.Compiler.specialize_method(method, atypes, Core.svec())
end

function specialize_ctrl(spec, loc, ctrl)
    atypes = Tuple{typeof(Semantic.ctrl),spec,loc,ctrl}
    if spec <: IntrinsicRoutine
        method = first(methods(Semantic.ctrl, Tuple{IntrinsicRoutine,Locations,CtrlLocations}))
    elseif spec <: RoutineSpec
        method = first(methods(Semantic.ctrl, Tuple{RoutineSpec,Locations,CtrlLocations}))
    end

    return Core.Compiler.specialize_method(method, atypes, Core.svec())
end

function zx_push_gate!(qc::QCircuit, gate::IntrinsicRoutine, locs::Locations)
    name = routine_name(gate)

    # NOTE: locs can be UnitRange etc. for instruction
    # remember to expand it
    for loc in locs
        each = plain(loc)
        if name in (:H, :X, :Z, :S, :Sdag, :T, :Tdag)
            push_gate!(qc, QGate(name, each))
        elseif name === :Y
            push_gate!(qc, QGate(:X, each))
            push_gate!(qc, QGate(:Z, each))
        elseif name in (:Rz, :Rx, :shift)
            push_gate!(qc, QGate(name, each; param = gate.variables[1]))
        elseif name === :Ry
            push_gate!(qc, QGate(:Sdag, each))
            push_gate!(qc, QGate(:Rx, each; param = gate.variables[1]))
            push_gate!(qc, QGate(:S, each))
        else
            return false
        end
    end
    return true
end

function zx_push_gate!(qc::QCircuit, gate::IntrinsicRoutine, locs::Locations, ctrl::CtrlLocations)
    name = routine_name(gate)
    ctrl = ctrl.storage.storage

    for loc in locs
        each = plain(loc)
        if name === :Z
            push_gate!(qc, QGate(:CZ, each; ctrl = ctrl))
        elseif name === :X
            if ctrl isa Tuple && length(ctrl) == 2
                a, b = ctrl
                c = each
                push_gate!(qc, Val(:H), c)
                push_gate!(qc, Val(:CNOT), c, b)
                push_gate!(qc, Val(:Tdag), c)
                push_gate!(qc, Val(:CNOT), c, a)
                push_gate!(qc, Val(:T), c)
                push_gate!(qc, Val(:CNOT), c, b)
                push_gate!(qc, Val(:Tdag), c)
                push_gate!(qc, Val(:CNOT), c, a)
                push_gate!(qc, Val(:T), b)
                push_gate!(qc, Val(:T), c)
                push_gate!(qc, Val(:H), c)
                push_gate!(qc, Val(:CNOT), b, a)
                push_gate!(qc, Val(:T), a)
                push_gate!(qc, Val(:Tdag), b)
                push_gate!(qc, Val(:CNOT), b, a)
            elseif ctrl isa Int
                push_gate!(qc, Val(:CNOT), each, ctrl)
            end
        else
            return false
        end
    end

    return true
end
