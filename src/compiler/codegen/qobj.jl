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
    pc::Int
    stmt

    # NOTE:
    # we alloate memory_slots for all measurement
    # at level of codegen since measure cannot be
    # easily removed it can affect the whole state
    memory_offset::Int
    memory_slots::Int
    # alloate for used measure result
    register_map::Dict{Int, Vector{Int}}
end

function CodeGenQobjState(ci::CodeInfo)
    memory_slots = 0
    register_head = 0
    register_map = Dict{Int, Vector{Int}}()
    measure_used = measure_ssa_uses!(Set{Int64}(), ci)

    for (v, stmt) in eachstmt(ci)
        is_quantum_statement(stmt) || continue
        qt = quantum_stmt_type(stmt)

        if qt === :measure
            _, x = _extract_measure(stmt)
            # NOTE: locs must be Locations in this codegen
            # we should check it again during code
            # validation stage
            locs = obtain_const(x, ci)::Locations
            memory_slots += length(locs)

            if v in measure_used
                register_map[v] = collect(register_head:register_head+length(locs)-1)
                register_head += length(locs)
            end
        end
    end

    return CodeGenQobjState(0, nothing, 0, memory_slots, register_map)
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
        for useref in userefs(stmt)
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
    prog = Dict(
        "header" => Dict(),
        "config" => codegen_config(target, ci, st),
        "instructions" => codegen_inst(target, ci, st),
    )
end

function codegen_config(target::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    return Dict(
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

    @show gt
    @show gate
    gt <: RoutineSpec && error("non-intrinsic routine used: $gt, turn on the optimizer or revise your program")
    gt <: IntrinsicRoutine || error("invalid gate type: $gate::$gt")
    gate isa IntrinsicRoutine || error("gate $gate is not constant")
    # TODO: check if the intrinsic is available in target hardware
    #       might need a new field in the target to indicate hardware
    #       basis gate.

    qobj = @match gate begin
        ::Intrinsics.XGate => Dict(
            "name" => "x",
        )

        ::Intrinsics.YGate => Dict(
            "name" => "y",
        )

        ::Intrinsics.ZGate => Dict(
            "name" => "z",
        )

        ::Intrinsics.HGate => Dict(
            "name" => "h",
        )

        ::Intrinsics.SGate => Dict(
            "name" => "s",
        )

        ::Intrinsics.TGate => Dict(
            "name" => "t",
        )

        # TODO: sdg, tdg

        Intrinsics.shift(θ) => Dict(
            "name" => "u1",
            "params" => Any[θ],
        )

        Intrinsics.Rx(θ) => Dict(
            "name" => "rx",
            "params" => Any[θ],
        )

        Intrinsics.Ry(θ) => Dict(
            "name" => "ry",
            "params" => Any[θ],
        )

        Intrinsics.Rz(θ) => Dict(
            "name" => "rz",
            "params" => Any[θ],
        )

        _ => error("invalid gate for Qobj backend: $gate")
    end

    # NOTE: intrinsic instructions only supports single qubit
    # operation, we need to expand the program for our syntax
    # sugar here, given we don't have type information in macros
    if length(locs) == 1
        qobj["qubits"] = Any[locs.storage[1] - 1]
        return Any[qobj]
    else
        qobjs = Any[]
        for l in locs.storage
            obj = copy(qobj)
            obj["qubits"] = Any[l]
            push!(qobjs, obj)
        end
        return qobjs
    end
end

function codegen_ctrl(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    gate, gt, locs, ctrl = obtain_const_ctrl_stmt(st.stmt, ci)
    gt <: RoutineSpec && error("non-intrinsic routine used: $gt, turn on the optimizer or revise your program")
    gt <: IntrinsicRoutine || error("invalid gate type: $gate::$gt")

    all(ctrl.configs) || error("inverse ctrl is not supported in Qobj backend")
    gate isa IntrinsicRoutine || error("gate $gate is not constant")

    # control gates support in qelib1.inc
    qobj = @match (gate, length(ctrl), length(locs)) begin
        (::Intrinsics.XGate, 1, 1) => Dict(
            "name" => "cx",
        )

        (::Intrinsics.YGate, 1, 1) => Dict(
            "name" => "cy",
        )

        (::Intrinsics.ZGate, 1, 1) => Dict(
            "name" => "cz",
        )

        (::Intrinsics.HGate, 1, 1) => Dict(
            "name" => "ch",   
        )

        (::Intrinsics.XGate, 2, 1) => Dict(
            "name" => "ccx",
        )

        (Intrinsics.Rz(θ), 1, 1) => Dict(
            "name" => "crz",
            "params" => Any[θ],
        )

        (Intrinsics.shift(θ), 1, 1) => Dict(
            "name" => "cu1",
            "params" => Any[θ],
        )

        (Intrinsics.UGate(α, β, γ), 1, 1) => Dict(
            "name" => "cu3",
            "params" => Any[α, β, γ],
        )

        _ => error("invalid control statement for Qobj backend, got: $(st.stmt)")
    end

    qobj["qubits"] = Int[collect(ctrl.storage)..., collect(locs.storage)...] .- 1
    return qobj
end

function codegen_measure(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    if st.stmt.head === :(=)
        slot = st.stmt.args[1]::SlotNumber
        measure_ex = st.stmt.args[2]
        locs = obtain_const(measure_ex.args[2], ci)::Locations
    else
        locs = obtain_const(st.stmt.args[2], ci)::Locations
    end

    qobj = Dict(
        "name" => "measure",
        "qubits" => collect(locs) .- 1,
        "memory" => collect(st.memory_offset:st.memory_offset+length(locs)-1),
    )

    if haskey(st.register_map, st.pc)
        qobj["register"] = st.register_map[st.pc]
    end

    return qobj
end

function codegen_barrier(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    locs = obtain_const(st.stmt.args[2], ci)::Locations

    return Dict(
        "name" => "barrier",
        "qubits" => collect(locs) .- 1,
    )
end

function codegen_ifnot(::TargetQobjQASM, ci::CodeInfo, st::CodeGenQobjState)
    cond = st.stmt.cond::SSAValue
    creg, right = obtain_qasm_ifnot_cond(cond, ci)

    if creg isa SSAValue
        haskey(st.register_map, creg.id) || error("classical post processing measurement result in Qobj is not supported")
        condition = st.register_map[creg.id]
    else
        error("invalid value for measurement result: $creg, possible fix: run :julia optimization pass before codegen")
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
                return Dict(
                    "name" => "reset",
                    "qubits" => collect(locs.storage) .- 1,
                )
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
