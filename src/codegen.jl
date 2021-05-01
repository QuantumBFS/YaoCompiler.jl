
mutable struct MainState
    src::CodeInfo
    pc::Int
    stmt::Any
end

mutable struct GateState
    src::CodeInfo
    pc::Int
    stmt::Any
    name::Symbol
    cargs::Dict{Int, Int}
    cargnames::Vector{SubString{String}}
end

function YaoCompiler.compile(target::OpenQASMTarget, f, tt::Type, options::HardwareFreeOptions)
    v"2.0" ≤ target.version < v"3.0" || error("only QASM 2.x is supported")
    interp = YaoInterpreter(;target, options)
    mi = method_instance(f, tt)
    src = Core.Compiler.typeinf_ext_toplevel(interp, mi)
    # we need to remove code coverage effect to pass validate
    src = CompilerPluginTools.rm_code_coverage_effect(src)
    validate(target, src)

    if target.toplevel
        st = MainState(src, 0, nothing)
    else
        op = tt.parameters[2]
        op.parameters[1] <: GenericRoutine || error("OpenQASM gate target does not support callable object")
        name = routine_name(op)
        cargnames = retrieve_cargnames(op)
        cargs = extract_cargs_map(src)
        st = GateState(src, 0, nothing, name, cargs, cargnames)
    end
    return emit_qasm(target, src, RegInfo(target, src), st)
end

function push_stmt!(list::Vector, stmt)
    isnothing(stmt) && return list
    if stmt isa Vector
        append!(list, stmt)
    else
        push!(list, stmt)
    end
end

function emit_qasm(target::OpenQASMTarget, ci::CodeInfo, ri::RegInfo, st::MainState)
    cregs, qregs = emit_registers(ri)
    prog = []
    if target.include_qelib1
        push!(prog, Include(qasm_str("qelib1.inc")))
    end
    append!(prog, cregs)
    append!(prog, qregs)

    st.pc = 1
    while st.pc <= length(ci.code)
        push_stmt!(prog, emit_stmt(target, ci.code[st.pc], ri, st))
        st.pc += 1
    end

    return MainProgram(target.version, prog)
end

function emit_qasm(target::OpenQASMTarget, ci::CodeInfo, ri::RegInfo, st::GateState)
    qargs = emit_qargs(ri)
    cargs = qasm_id.(st.cargnames)
    decl = GateDecl(qasm_id(st.name), cargs, qargs)
    prog = []
    st.pc = 1
    while st.pc <= length(ci.code)
        push_stmt!(prog, emit_stmt(target, ci.code[st.pc], ri, st))
        st.pc += 1
    end
    return Gate(decl, prog)
end

qreg_name(k::Int) = string("qreg_", k)

function emit_registers(ri::RegInfo)
    cregs = []
    for (name, size) in ri.creg_size
        push!(cregs, RegDecl(kw_creg, qasm_id(name), qasm_int(size)))
    end

    qregs = []
    for (qreg, locs) in ri.qreg_to_locs
        qreg_id = qasm_id(qreg_name(qreg))
        push!(qregs, RegDecl(kw_qreg, qreg_id, qasm_int(length(locs))))
    end
    return cregs, qregs
end

function emit_stmt(target::OpenQASMTarget, stmt, ri::RegInfo, st)
    @switch stmt begin
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, gate, QuoteNode(locs))
            emit_gate(target, gate, locs, ri, st)
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, gate, QuoteNode(locs), QuoteNode(ctrl))
            emit_ctrl(target, gate, locs, ctrl, ri, st)
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs))
            emit_measure(target, locs, ri, st)
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, QuoteNode(locs))
            emit_barrier(target, locs, ri, st)
        @case GotoIfNot(SSAValue(id), dest)
            emit_goto_ifnot(target, id, dest, ri, st)
        @case _ # skip other statement
            nothing
    end
end

emit_qargs(ri::RegInfo) = [qasm_id(qreg_name(qreg)) for (qreg, _) in ri.qreg_to_locs]

function emit_exp(target::OpenQASMTarget, exp, ri::RegInfo, st)
    @switch exp begin
        @case ::Int
            qasm_int(exp)
        @case ::Irrational{:π}
            pi_token
        @case ::Real # convert other read number to f64
            qasm_f64(exp)
        @case SSAValue(id)
            emit_exp(target, st.src.code[id], ri, st)

        @case Expr(:invoke, mi, GlobalRef(_, :sin), arg) || Expr(:invoke, mi, &(QASM.sin), arg)
            Call(:sin, emit_exp(target, arg, ri, st))

        @case Expr(:invoke, mi, GlobalRef(_, :cos), arg) || Expr(:invoke, mi, &(QASM.cos), arg)
            Call(:cos, emit_exp(target, arg, ri, st))

        @case Expr(:invoke, mi, GlobalRef(_, :tan), arg) || Expr(:invoke, mi, &(QASM.tan), arg)
            Call(:tan, emit_exp(target, arg, ri, st))

        @case Expr(:invoke, mi, GlobalRef(_, :exp), arg) || Expr(:invoke, mi, &(QASM.exp), arg)
            Call(:exp, emit_exp(target, arg, ri, st))

        @case Expr(:invoke, mi, GlobalRef(_, :log), arg) || Expr(:invoke, mi, &(QASM.log), arg)
            Call(:ln, emit_exp(target, arg, ri, st))

        @case Expr(:invoke, mi, GlobalRef(_, :sqrt), arg) || Expr(:invoke, mi, &(QASM.sqrt), arg)
            Call(:sqrt, emit_exp(target, arg, ri, st))

        # unary
        @case Expr(:call, GlobalRef(mod, name), x)
            f = Core.Compiler.abstract_eval_global(mod, name)::Const
            sym = qasm_compatible_intrinsics[f.val]
            token = Token{:reserved}(sym)
            if sym == "-"
                Neg(emit_exp(target, x, ri, st))
            else
                Call(token, emit_exp(target, x, ri, st))
            end
        # binop
        @case Expr(:call, GlobalRef(mod, name), lhs, rhs)
            f = Core.Compiler.abstract_eval_global(mod, name)::Const
            token = Token{:reserved}(qasm_compatible_intrinsics[f.val])
            (emit_exp(target, lhs, ri, st), token, emit_exp(target, rhs, ri, st))
        # get k-th carg
        @case Expr(:call, GlobalRef(&Base, :getfield), &(SSAValue(1)), k::Int, true)
            Token{:id}(st.cargnames[k])
        @case _
            error("unknown expression: $exp")
    end
end

function emit_gate(target::OpenQASMTarget, @nospecialize(gate), locs::Locations, ri::RegInfo, st)
    qargs = [ri[loc] for loc in locs]
    if target.inline_intrinsics
        emit_gate_inline(target, gate, qargs, ri, st)
    else
        emit_gate_noinline(target, gate, qargs, ri, st)
    end
end

function emit_gate_noinline(target::OpenQASMTarget, @nospecialize(gate), qargs, ri::RegInfo, st)
    @switch gate begin
        @case QuoteNode(shift(θ))
            cargs = Any[emit_exp(target, θ, ri, st)]
            Instruction("shift", cargs, qargs)
        @case QuoteNode(Rx(θ))
            cargs = Any[emit_exp(target, θ, ri, st)]
            Instruction("rx", cargs, qargs)
        @case QuoteNode(Ry(θ))
            cargs = Any[emit_exp(target, θ, ri, st)]
            Instruction("ry", cargs, qargs)
        @case QuoteNode(Rz(θ))
            cargs = Any[emit_exp(target, θ, ri, st)]
            Instruction("rz", cargs, qargs)

        @case QuoteNode(val::IntrinsicRoutine)
            name = qasm_gate_name(typeof(val))
            Instruction(name, [], qargs)
        @case QuoteNode(val::Operation)
            name = string(routine_name(val))
            cargs = [emit_exp(target, x, ri, st) for x in val.args]
            Instruction(name, cargs, qargs)
        @case SSAValue(id) # parametric intrinsic gate
            emit_gate_parametric(target, id, qargs, ri, st)
        @case _
            error("unknown gate value: $gate")
    end
end

function emit_gate_inline(target::OpenQASMTarget, @nospecialize(gate), qargs, ri::RegInfo, st)
    @switch gate begin
        @case QuoteNode(shift(θ))
            intrinsic_qasm(typeof(shift), θ, qargs)
        @case QuoteNode(Rx(θ))
            intrinsic_qasm(typeof(Rx), θ, qargs)
        @case QuoteNode(Ry(θ))
            intrinsic_qasm(typeof(Ry), θ, qargs)
        @case QuoteNode(Rz(θ))
            intrinsic_qasm(typeof(Rz), θ, qargs)
        @case QuoteNode(val) # non parametric intrinsic
            intrinsic_qasm(typeof(val), qargs)
        @case SSAValue(id) # parametric intrinsic gate
            emit_gate_parametric(target, id, qargs, ri, st)
        @case _
            error("unknown gate value: $gate")
    end
end

function emit_gate_parametric(target::OpenQASMTarget, id, qargs, ri::RegInfo, st)
    target.toplevel && error("toplevel program cannot have non-constant gate parameter")
    stmt = st.src.code[id]

    if target.inline_intrinsics
        @switch stmt begin
            @case Expr(:new, gt::Type{<:IntrinsicRoutine}, theta)
                carg = emit_exp(target, theta, ri, st)
                return intrinsic_qasm(gt, carg, qargs)
            @case _
                error("expect gate definition, got $stmt")
        end
    else
        @switch stmt begin
            @case Expr(:new, gt::Type{<:IntrinsicRoutine}, theta)
                name = qasm_gate_name(gt)
            @case _
                error("expect gate definition, got $stmt")
        end
        carg = emit_exp(target, theta, ri, st)
        return Instruction(name, Any[carg], qargs)
    end
end

function emit_ctrl(::OpenQASMTarget, @nospecialize(gate), locs::Locations, ctrl::CtrlLocations, ri::RegInfo, st)
    @switch gate begin
        @case QuoteNode(&X)
            if length(ctrl) == 1 && length(locs) == 1
                return CXGate(ri[ctrl], ri[locs])
            elseif length(ctrl) == 2 && length(locs) == 1
                error(
                    "ccx is not valid intrinsic QASM instruction. posssible fix:\n",
                    "1. include a qasm stdlib e.g qelib<version>.inc and use ccx gate from it\n",
                    "2. define your own ccx gate via @device using CX and single qubit gate",
                )
            end
        @case _
            error("control statement is not compatible with QASM")
    end
end

function emit_measure(::OpenQASMTarget, locs::Locations, ri::RegInfo, st)
    cname = string(ri.ssa_creg_map[st.pc])
    return [Measure(ri[loc], Bit(cname, k-1)) for (k, loc) in enumerate(locs)]
end

function emit_barrier(::OpenQASMTarget, locs::Locations, ri::RegInfo, st)
    qargs = []; args = Dict{Int, Vector{Int}}()

    # scan location addrs to remove extra
    # index on qreg
    for loc in locs
        plain_loc = plain(loc)
        qreg = ri.locs_to_qreg[plain_loc]
        addr = ri.locs_to_addr[plain_loc]
        addrs = get!(args, qreg, Int[])
        push!(addrs, addr)
    end

    for (qreg, addrs) in args
        # do not index qreg explicitly if barrier size
        # is the same with register size
        if length(ri.qreg_to_locs[qreg]) == length(addrs)
            push!(qargs, Bit(qreg_name(qreg)))
        else
            for addr in addrs
                push!(qargs, Bit(qreg_name(qreg), addr))
            end
        end
    end
    return Barrier(qargs)
end

function emit_goto_ifnot(target::OpenQASMTarget, cond::Int, dest::Int, ri::RegInfo, st)
    @switch st.src.code[cond] begin
        @case Expr(:invoke, mi, GlobalRef(YaoCompiler, :measure_cmp), SSAValue(lhs), rhs::Int)
            creg, int = lhs, rhs
        @case Expr(:invoke, mi, GlobalRef(YaoCompiler, :measure_cmp), lhs::Int, SSAValue(rhs))
            int, creg = lhs, rhs
        @case _
            error("invalid statement $(st.src.code[cond])")
    end

    haskey(ri.ssa_creg_map, creg) || error("cannot have classical statement in toplevel QASM")
    cname = ri.ssa_creg_map[creg]
    stmt = move_to_next_quantum_statement(st)
    body = emit_stmt(target, stmt, ri, st)
    return IfStmt(qasm_id(cname), qasm_int(int), body)
end

function move_to_next_quantum_statement(st)
    ci = st.src
    pc′ = st.pc + 1
    local stmt
    while pc′ <= length(ci.code)
        stmt = ci.code[pc′]
        @switch stmt begin
            @case Expr(:invoke, _, GlobalRef(&Intrinsics, _), _...)
                break
            @case ::GotoNode
                pc′ = (stmt::Core.GotoNode).label
            @case _
                # NOTE:
                # we can allow some constant statements
                # but not control flows or other nodes
                error("unexpected statement inside control flow: $stmt")
        end
        pc′ += 1
    end

    st.pc = pc′
    st.stmt = stmt
    return stmt
end

function retrieve_cargnames(op)
    mi = method_instance(op.parameters[1].instance, op.parameters[2])
    return split(chop(mi.def.slot_syms), '\0')[2:end]
end

function extract_cargs_map(ci::CodeInfo)
    vars = findfirst(ci.code) do stmt
        @match stmt begin
            Expr(:call, GlobalRef(&Base, :getfield), Argument(3), QuoteNode(:args)) => true
            _ => false
        end
    end

    ssa_cargs = Dict{Int, Int}()
    isnothing(vars) && return ssa_cargs

    for (v, stmt) in enumerate(ci.code)
        @switch stmt begin
            @case Expr(:call, getfield, SSAValue(&vars), k, true)
                ssa_cargs[v] = k
            @case _
                continue
        end
    end
    return ssa_cargs
end
