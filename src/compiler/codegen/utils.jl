function obtain_const_or_stmt(@nospecialize(x), ci::CodeInfo)
    if x isa SSAValue
        stmt = ci.code[x.id]
        typ = ci.ssavaluetypes[x.id]
        if typ isa Const
            return typ.val, typeof(typ.val)
        else
            return stmt, widenconst(typ)
        end
    elseif x isa QuoteNode
        return x.value, typeof(x.value)
    elseif x isa Const
        return x.val, typeof(x.val)
    elseif x isa GlobalRef
        if isdefined(x.mod, x.name) && isconst(x.mod, x.name)
            val = getfield(x.mod, x.name)
        else
            # TODO: move this to parsing time
            throw(UndefVarError(x.name))
        end
        
        return val, typeof(val)
    else
        # special value
        return x, typeof(x)
    end
end

obtain_const(@nospecialize(x), ci::CodeInfo) = obtain_const_or_stmt(x, ci)[1]

function obtain_qasm_ifnot_cond(cond::SSAValue, ci::CodeInfo)
    cond_stmt = ci.code[cond.id]

    (cond_stmt isa Expr && cond_stmt.head === :call) || error("invalid cond statement: $cond_stmt")

    x = obtain_const(cond_stmt.args[2], ci)
    if x isa Int
        creg = cond_stmt.args[3]
    else
        x= obtain_const(cond_stmt.args[3], ci)
        if x isa Int
            creg = cond_stmt.args[2]
        else
            error("one of the value in if condition must be constant for QASM compatible program")
        end
    end

    creg isa SlotNumber || creg isa SSAValue || error("invalid statement: $creg")
    return creg, x
end

function obtain_const_measure_stmt(stmt, ci::CodeInfo)
    if stmt.head === :(=)
        cvar, body = _extract_measure(stmt)
    else
        cvar = nothing
        body = stmt
    end

    if body.head === :invoke
        locs = obtain_const(body.args[3], ci)::Locations
    elseif body.head === :call
        locs = obtain_const(body.args[2], ci)::Locations
    end
    return cvar, locs
end

function obtain_const_gate_stmt(stmt, ci::CodeInfo)
    if stmt.head === :invoke
        gate, gt = obtain_const_or_stmt(stmt.args[3], ci)
        locs = obtain_const(stmt.args[4], ci)::Locations
    elseif stmt.head === :call
        gate, gt = obtain_const_or_stmt(stmt.args[2], ci)
        locs = obtain_const(stmt.args[3], ci)::Locations
    end
    return gate, gt, locs
end

function obtain_const_ctrl_stmt(stmt, ci::CodeInfo)
    # NOTE: QASM compatible code should have constant location
    # QASM compatible code should only contain control X gate
    # as control gates, for other control gates, it should be
    # either decomposed or error, so we will assume the gate
    # is a constant here.
    if stmt.head === :invoke
        gate, gt = obtain_const_or_stmt(stmt.args[3], ci)
        locs = obtain_const(stmt.args[4], ci)::Locations
        ctrl = obtain_const(stmt.args[5], ci)::CtrlLocations
    elseif stmt.head === :call
        gate, gt = obtain_const_or_stmt(stmt.args[2], ci)
        locs = obtain_const(stmt.args[3], ci)::Locations
        ctrl = obtain_const(stmt.args[4], ci)::CtrlLocations
    end

    return gate, gt, locs, ctrl
end

function move_to_first_quantum_stmt(ci::CodeInfo, st)
    pc′ = st.pc + 1
    local stmt′
    while pc′ <= length(ci.code)
        stmt′ = ci.code[pc′]
        if stmt′ isa Expr
            is_quantum_statement(stmt′)
            break
        else
            # NOTE:
            # we can allow some constant statements
            # but not control flows or other nodes
            error("unexpected statement inside control flow: $stmt′")
        end
        pc′ += 1
    end

    st.pc = pc′
    st.stmt = stmt′
    return stmt′
end
