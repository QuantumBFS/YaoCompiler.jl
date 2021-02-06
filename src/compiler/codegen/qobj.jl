const Qobj = Dict{String,Any}

"""
    TargetQobjQASM

Qobj (of type QASM) target. See https://arxiv.org/pdf/1809.03452.pdf
for specifications.
"""
Base.@kwdef struct TargetQobjQASM
    nshots::Int = 1024
    seed::Int = 1
    max_credits::Int = 3
end

# NOTE: Qobj can only generated after running at least :julia optimization pass
#       this is because we need to inline all quantum routines
#       result program can only have constant intrinsic routine

mutable struct CodeGenQobjState
    nqubits::Int

    pc::Int
    stmt::Any

    # NOTE:
    # we alloate memory_slots for all measurement
    # at level of codegen since measure cannot be
    # easily removed it can affect the whole state
    memory_offset::Int
    memory_slots::Int
    # alloate for used measure result
    register_map::Dict{Int,Vector{Int}}
    # allocate qubits on contiguous address
    qubits_map::Dict{Int,Int}
end

function allocate_qobj_qubits!(qubits, locs::Locations, curr::Ref{Int})
    for k in locs.storage
        if !haskey(qubits, k)
            qubits[k] = curr[]
            curr[] += 1
        end
    end
    return qubits
end

function CodeGenQobjState(ci::CodeInfo)
    memory_slots = 0
    register_head = 0
    register_map = Dict{Int,Vector{Int}}()
    qubits_map = Dict{Int,Int}()
    measure_used = measure_ssa_uses!(Set{Int64}(), ci)
    qmem_ptr = Ref{Int}(0)

    for (v, stmt) in eachstmt(ci)
        is_quantum_statement(stmt) || continue
        qt = quantum_stmt_type(stmt)

        if qt === :measure
            _, locs = obtain_const_measure_stmt(stmt, ci)
            allocate_qobj_qubits!(qubits_map, locs, qmem_ptr)
            memory_slots += length(locs)

            if v in measure_used
                register_map[v] = collect(register_head:register_head+length(locs)-1)
                register_head += length(locs)
            end
        elseif qt === :gate
            _, _, locs = obtain_const_gate_stmt(stmt, ci)
            allocate_qobj_qubits!(qubits_map, locs, qmem_ptr)
        elseif qt === :ctrl
            _, _, locs, ctrl = obtain_const_ctrl_stmt(stmt, ci)
            allocate_qobj_qubits!(qubits_map, locs, qmem_ptr)
            allocate_qobj_qubits!(qubits_map, ctrl.storage, qmem_ptr)
        end
    end

    return CodeGenQobjState(qmem_ptr[], 0, nothing, 0, memory_slots, register_map, qubits_map)
end

function get_qubit_addrs(ctrl::CtrlLocations, st::CodeGenQobjState)
    return get_qubit_addrs(ctrl.storage, st)
end

function get_qubit_addrs(storage, st::CodeGenQobjState)
    qubits = Vector{Int}(undef, length(storage))
    for (k, l) in enumerate(storage)
        qubits[k] = st.qubits_map[l]
    end
    return qubits
end

function get_qubit_addrs(locs::Locations, st::CodeGenQobjState)
    return get_qubit_addrs(locs.storage, st)
end

"scan used measurement result"
function measure_ssa_uses!(used, ci::CodeInfo)
    measure_ssa = Int[]
    for (v, stmt) in eachstmt(ci)
        is_quantum_statement(stmt) || continue
        qt = quantum_stmt_type(stmt)

        if qt === :measure
            push!(measure_ssa, v)
        end
    end

    for (v, stmt) in eachstmt(ci)
        stmt isa GotoIfNot || continue
        cond = stmt.cond
        cond_stmt = ci.code[stmt.cond.id]
        for useref in userefs(cond_stmt)
            val = useref[]
            if isa(val, SSAValue) && val.id in measure_ssa
                push!(used, val.id)
            end
        end
    end
    return used
end

# NOTE: we only generate experiment Qobj
# the complete Qobj should be handled in
# client before sending to IBM

function codegen(target::TargetQobjQASM, ci::CodeInfo)
    st = CodeGenQobjState(ci)
    spec = ci.parent.specTypes.parameters[2]
    prog = Qobj(
        "header" => Qobj(
            "name" => routine_name(spec),
            "n_qubits" => st.nqubits,
            "memory_slots" => st.memory_slots,
        ),
        "config" => codegen_config(target, ci, st),
        "instructions" => codegen_inst(target, ci, st),
    )
end

function codegen_config(target::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    return Qobj(
        "n_qubits" => st.nqubits,
        "shots" => target.nshots,
        "memory_slots" => st.memory_slots, # TODO: generate this from ci
        "seed" => target.seed,
        "max_credits" => target.max_credits,
    )
end

function codegen_inst(target::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    prog = Any[]
    st.pc = 1
    while st.pc <= length(ci.code)
        inst = nothing
        st.stmt = ci.code[st.pc]
        if st.stmt isa Expr && is_quantum_statement(st.stmt)
            inst = codegen_stmt(target, ci, st)
            st.pc += 1
        elseif st.stmt isa GotoIfNot
            inst = codegen_ifnot(target, ci, st)
        else
            st.pc += 1
        end

        if !isnothing(inst)
            if inst isa Vector
                append!(prog, inst)
            else
                push!(prog, inst)
            end
        end
    end
    return prog
end

function codegen_stmt(target::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    qt = quantum_stmt_type(st.stmt)
    if qt === :gate
        inst = codegen_gate(target, ci, st)
    elseif qt === :ctrl
        inst = codegen_ctrl(target, ci, st)
    elseif qt === :measure
        inst = codegen_measure(target, ci, st)
    elseif qt === :barrier
        inst = codegen_barrier(target, ci, st)
    else
        error("incompatible statement for Qobj: $(st.stmt)")
    end
    return inst
end

function codegen_gate(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    gate, gt, locs = obtain_const_gate_stmt(st.stmt, ci)

    gt <: RoutineSpec &&
        error("non-intrinsic routine used: $gt, turn on the optimizer or revise your program")
    gt <: IntrinsicRoutine || error("invalid gate type: $gate::$gt")
    gate isa IntrinsicRoutine || error("gate $gate is not constant")
    # TODO: check if the intrinsic is available in target hardware
    #       might need a new field in the target to indicate hardware
    #       basis gate.

    qobj = @match gate begin
        ::Intrinsics.XGate => Qobj("name" => "x")

        ::Intrinsics.YGate => Qobj("name" => "y")

        ::Intrinsics.ZGate => Qobj("name" => "z")

        ::Intrinsics.HGate => Qobj("name" => "h")

        ::Intrinsics.SGate => Qobj("name" => "s")

        ::Intrinsics.TGate => Qobj("name" => "t")

        # TODO: sdg, tdg

        Intrinsics.shift(θ) => Qobj("name" => "u1", "params" => Any[θ])

        Intrinsics.Rx(θ) => Qobj("name" => "rx", "params" => Any[θ])

        Intrinsics.Ry(θ) => Qobj("name" => "ry", "params" => Any[θ])

        Intrinsics.Rz(θ) => Qobj("name" => "rz", "params" => Any[θ])

        _ => error("invalid gate for Qobj backend: $gate")
    end

    # NOTE: intrinsic instructions only supports single qubit
    # operation, we need to expand the program for our syntax
    # sugar here, given we don't have type information in macros
    if length(locs) == 1
        qobj["qubits"] = get_qubit_addrs(locs, st)
        return Any[qobj]
    else
        qobjs = Any[]
        for l in locs.storage
            obj = copy(qobj)
            obj["qubits"] = get_qubit_addrs(l, st)
            push!(qobjs, obj)
        end
        return qobjs
    end
end

function codegen_ctrl(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    gate, gt, locs, ctrl = obtain_const_ctrl_stmt(st.stmt, ci)
    gt <: RoutineSpec &&
        error("non-intrinsic routine used: $gt, turn on the optimizer or revise your program")
    gt <: IntrinsicRoutine || error("invalid gate type: $gate::$gt")

    all(ctrl.flags) || error("inverse ctrl is not supported in Qobj backend")
    gate isa IntrinsicRoutine || error("gate $gate is not constant")

    # control gates support in qelib1.inc
    qobj = @match (gate, length(ctrl), length(locs)) begin
        (::Intrinsics.XGate, 1, 1) => Qobj("name" => "cx")

        (::Intrinsics.YGate, 1, 1) => Qobj("name" => "cy")

        (::Intrinsics.ZGate, 1, 1) => Qobj("name" => "cz")

        (::Intrinsics.HGate, 1, 1) => Qobj("name" => "ch")

        (::Intrinsics.XGate, 2, 1) => Qobj("name" => "ccx")

        (Intrinsics.Rz(θ), 1, 1) => Qobj("name" => "crz", "params" => Any[θ])

        (Intrinsics.shift(θ), 1, 1) => Qobj("name" => "cu1", "params" => Any[θ])

        (Intrinsics.UGate(α, β, γ), 1, 1) => Qobj("name" => "cu3", "params" => Any[α, β, γ])

        _ => error("invalid control statement for Qobj backend, got: $(st.stmt)")
    end

    qobj["qubits"] = vcat(get_qubit_addrs(ctrl, st), get_qubit_addrs(locs, st))
    return qobj
end

function codegen_measure(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    _, locs = obtain_const_measure_stmt(st.stmt, ci)

    qobj = Qobj(
        "name" => "measure",
        "qubits" => get_qubit_addrs(locs, st),
        "memory" => collect(st.memory_offset:st.memory_offset+length(locs)-1),
    )

    if haskey(st.register_map, st.pc)
        qobj["register"] = st.register_map[st.pc]
    end

    return qobj
end

function codegen_barrier(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    locs = obtain_const(st.stmt.args[2], ci)::Locations

    return Qobj("name" => "barrier", "qubits" => get_qubit_addrs(locs, st))
end

function codegen_ifnot(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    cond = st.stmt.cond::SSAValue
    creg, right = obtain_qasm_ifnot_cond(cond, ci)

    if creg isa SSAValue
        haskey(st.register_map, creg.id) ||
            error("classical post processing measurement result in Qobj is not supported")
        condition = st.register_map[creg.id]
    else
        error(
            "invalid value for measurement result: $creg, possible fix: run :julia optimization pass before codegen",
        )
    end

    length(condition) == 1 || error("Qobj only allows single bit condition for if statement")

    # find the first quantum stmt after goto
    move_to_first_quantum_stmt(ci, st)

    # check if this is actually a reset statement:
    # 
    # c = @measure loc
    # if c == 1
    #   loc => X
    # end

    if right == 1
        qt = quantum_stmt_type(st.stmt)
        if qt === :gate
            gate, gt, locs = obtain_const_gate_stmt(st.stmt, ci)
            if gate === Intrinsics.X
                return Qobj("name" => "reset", "qubits" => get_qubit_addrs(locs, st))
            end
        end
    end

    qobj = codegen_stmt(target, ci, st)
    if qobj isa Vector # return from codegen_gate
        for each in qobj
            each["conditional"] = condition[]
        end
    else
        qobj["conditional"] = condition[]
    end
    return qobj
end
