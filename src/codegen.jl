struct MemoryInfo
    n_qubits::Int
    # NOTE:
    # we alloate memory_slots for all measurement
    # at level of codegen since measure cannot be
    # easily removed it can affect the whole state
    memory_offset::Int # init address of classical memory
    returns::Set{Int}
    # allocate slots for returned measure result
    slots::Dict{Int, Vector{Int}}
    # alloate for used measure result
    registers::Dict{Int,Vector{Int}} # ssa value => list of slots
    # allocate qubits on contiguous address
    qubits::Dict{Int,Int} # location => qobj address
end

function MemoryInfo(ci::CodeInfo; offset::Int=0)
    memory_slots = 0
    qubits = Dict{Int, Int}()
    qreg_head = 0
    slots = Dict{Int, Vector{Int}}()
    registers = Dict{Int, Vector{Int}}()
    measure_ssas = measure_ssa(ci)
    measure_uses = measure_ssa_uses(ci, measure_ssas)
    measure_rets = measure_ssa_returns(ci, measure_ssas)
    qmem_ptr = Ref{Int}(0)

    for (v, stmt) in enumerate(ci.code)
        @switch stmt begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations))
                allocate_qobj_qubits!(qubits, locs, qmem_ptr)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations), QuoteNode(ctrl::CtrlLocations))
                allocate_qobj_qubits!(qubits, locs, qmem_ptr)
                allocate_qobj_qubits!(qubits, ctrl.storage, qmem_ptr)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs::Locations))
                allocate_qobj_qubits!(qubits, locs, qmem_ptr)
                # NOTE: we should always allocate slot for measure
                slots[v] = collect(memory_slots:memory_slots+length(locs)-1)
                memory_slots += length(locs)

                if v in measure_uses
                    registers[v] = collect(qreg_head:qreg_head+length(locs)-1)
                    qreg_head += length(locs)
                end
            # skip other statement
            @case _
                nothing
        end
    end
    MemoryInfo(qmem_ptr[], offset, measure_rets, slots, registers, qubits)
end

function allocate_qobj_qubits!(qubits::Dict{Int, Int}, locs::Locations, ptr::Ref{Int})
    for loc in locs
        plain_loc = plain(loc)
        haskey(qubits, plain_loc) && continue
        qubits[plain_loc] = ptr[]
        ptr[] += 1
    end
    return qubits
end

# pretty print in multiline mode
function Base.show(io::IO, ::MIME"text/plain", mi::MemoryInfo)
    summary(io, mi)
    println(io, "(")
    println(io, "  n_qubits=", mi.n_qubits, ",")
    println(io, "  memory_offset=", mi.memory_offset, ",")
    if !isempty(mi.slots)
        println(io, "  slots=Dict{Int, Vector{Int}}(")
        for (v, slots) in mi.slots
            println(io, "    ", v, "=>", slots, ",")
        end
        println(io, "  ),")
    end

    if !isempty(mi.registers)
        println(io, "  registers=Dict{Int, Vector{Int}}(")
        for (v, slots) in mi.registers
            println(io, "    ", v, "=>", slots, ",")
        end
        println(io, "  ),")
    end

    println(io, "  qubits=Dict{Int,Int}(", )
    for (loc, addr) in mi.qubits
        println(io, "    ", loc, "=>", addr, ",")
    end
    println(io, "  ),")
    println(io, ")")
end

mutable struct CodeGenState
    src::CodeInfo
    pc::Int
    memory_ptr::Int
    stmt::Any
end

CodeGenState(src) = CodeGenState(src, 0, 0, nothing)

function YaoCompiler.compile(target::QobjQASMTarget, f, tt::Type, options::HardwareFreeOptions)
    interp = YaoInterpreter(;target, options)
    mi = method_instance(f, tt)
    src = Core.Compiler.typeinf_ext_toplevel(interp, mi)
    # we need to remove code coverage effect to pass validate
    src = CompilerPluginTools.rm_code_coverage_effect(src)
    validate(target, src)

    st = CodeGenState(src)
    mi = MemoryInfo(src; offset=target.memory_offset)
    return emit_qobj(target, src, mi, st)
end

function emit_qobj(target::QobjQASMTarget, ci::CodeInfo, mi::MemoryInfo, st::CodeGenState)
    instructions = Instruction[]
    cfg = Core.Compiler.compute_basic_blocks(ci.code)
    worklist = Int[1]

    while !isempty(worklist)
        bb = pop!(worklist)
        bb_insts = []
        stmts = cfg.blocks[bb].stmts
        terminator = ci.code[last(stmts)]
        # this flag removes the measure stmt for reset
        contains_reset = false

        if !(terminator isa GotoIfNot)
            # NOTE:
            # terminator can be a gate call in program
            # that contains no control flow so let's try
            # to convert the terminator to Qobj
            st.pc = last(stmts)
            push_stmt!(bb_insts, emit_stmt(target, terminator, mi, st))
        else
            cond = terminator.cond::SSAValue

            @switch ci.code[cond.id] begin
                @case Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), result::SSAValue, val::Int) ||
                    Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), val::Int, result::SSAValue)
                    result = result
                    val = val
                @case _
                    error("if condition must be a measurement result compare with a constant integer")
            end

            condition = mi.registers[result.id]
            length(condition) == 1 || error("Qobj only allows single bit condition for if statement")
            # conditional bb
            non_conditional_branch = Core.Compiler.block_for_inst(cfg.index, terminator.dest)
            conditional_bb = findfirst(!isequal(non_conditional_branch), cfg.blocks[bb].succs)
            push!(worklist, non_conditional_branch)
            conditional_stmts = cfg.blocks[conditional_bb].stmts

            # handles multi-stmt if measure_cmp(a, b) ... end
            for v in conditional_stmts
                st.pc = v
                stmt = emit_stmt(target, ci.code[v], mi, st)
                stmt isa Gate || stmt isa Vector || error("cannot use classical control on $stmt in Qobj(type=QASM)")
                # match reset, if measure value is in the return, we will not use reset
                if !(result.id in mi.returns) && length(conditional_stmts) == 1 && is_reset_stmt(ci.code[result.id], stmt, val, mi)
                    push_stmt!(bb_insts, Reset(;qubits=stmt[1].qubits))
                    contains_reset = true
                    continue
                end
                push_stmt!(bb_insts, condition_stmt(stmt, condition[]))
            end
        end

        for v in stmts[1:end-1]
            contains_reset && v == result.id && continue
            st.pc = v
            push_stmt!(instructions, emit_stmt(target, ci.code[v], mi, st))
        end
        append!(instructions, bb_insts)
    end

    memory_slots = 0
    for inst in instructions
        if inst isa Measure
            memory_slots += length(inst.memory)
        end
    end

    return Experiment(;
        header=Dict{String, Any}(
            "n_qubits"=>mi.n_qubits,
            "description"=>target.description
        ),
        config=ExpConfig(;
            shots=target.nshots,
            seed=target.seed,
            max_credits=target.max_credits,
            memory_slots,
        ),
        instructions,
    )
end

# # return Qobj(;
# # qobj_id=target.qobj_id,
# # type="QASM",
# # schema_version=target.schema_version,
# # header=Dict{String, Any}("description"=>string(src.parent)),
# # config=ExpConfig(;
# #     shots=target.shots,
# #     seed=target.seed,
# #     max_credits=target.max_credits,
# #     memory_slots=st.memory_slots,
# # ),
# # experiments=Experiment[],
# # )

function emit_stmt(target::QobjQASMTarget, stmt, mi::MemoryInfo, st::CodeGenState)
    @switch stmt begin
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, gate, QuoteNode(locs))
            emit_gate(target, gate, locs, mi, st)
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, gate, QuoteNode(locs), QuoteNode(ctrl))
            emit_ctrl(target, gate, locs, ctrl, mi, st)
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs))
            emit_measure(target, locs, mi, st)
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, QuoteNode(locs))
            emit_barrier(target, locs, mi, st)
        @case _ # skip other statement
            nothing
    end
end

function emit_gate(target::QobjQASMTarget, @nospecialize(gate), locs::Locations, mi::MemoryInfo, st::CodeGenState)
    # NOTE: intrinsic instructions only supports single qubit
    # operation, and since we treat single qubit intrinsic
    # on multiple location as an IR semantic we will need to
    # implement it in codegen ourselves
    prog = []
    for loc in locs
        qubits = get_qubit_address(mi, loc)
        inst = @switch gate begin
            # parametric gates
            @case QuoteNode(shift(θ))
                Gate(;name="u1", qubits, params=Any[θ])
            @case QuoteNode(Rx(θ))
                Gate(;name="rx", qubits, params=Any[θ])
            @case QuoteNode(Ry(θ))
                Gate(;name="ry", qubits, params=Any[θ])
            @case QuoteNode(Rz(θ))
                Gate(;name="rz", qubits, params=Any[θ])

            @case QuoteNode(&X)
                Gate(;name="x", qubits)
            @case QuoteNode(&Y)
                Gate(;name="y", qubits)
            @case QuoteNode(&Z)
                Gate(;name="z", qubits)
            @case QuoteNode(&H)
                Gate(;name="h", qubits)
            @case QuoteNode(&T)
                Gate(;name="t", qubits)
            @case QuoteNode(&S)
                Gate(;name="s", qubits)
            @case QuoteNode(val::Operation)
                error("fail to inline operationn $val, please report an issue at YaoCompiler")
            @case SSAValue(id) # parametric intrinsic gate
                error("toplevel Qobj do not support parametric routine")
            @case _
                error("unknown gate value: $gate")
        end
        push!(prog, inst)
    end
    return prog
end

function emit_ctrl(::QobjQASMTarget, gate, locs::Locations, ctrl::CtrlLocations, mi::MemoryInfo, st::CodeGenState)
    qubits = get_qubit_address(mi, locs, ctrl)
    @switch (gate, length(ctrl), length(locs)) begin
        @case (QuoteNode(&X), 1, 1)
            Gate(;name="cx", qubits)
        @case (QuoteNode(&Y), 1, 1)
            Gate(;name="cy", qubits)
        @case (QuoteNode(&Z), 1, 1)
            Gate(;name="cz", qubits)
        @case (QuoteNode(&H), 1, 1)
            Gate(;name="ch", qubits)
        @case (QuoteNode(&X), 2, 1)
            Gate(;name="ccx", qubits)

        @case (QuoteNode(Rx(θ)), 1, 1)
            Gate(;name="crx", qubits, params=Any[θ])
        @case (QuoteNode(Ry(θ)), 1, 1)
            Gate(;name="cry", qubits, params=Any[θ])
        @case (QuoteNode(Rz(θ)), 1, 1)
            Gate(;name="crz", qubits, params=Any[θ])
        @case (QuoteNode(shift(θ)), 1, 1)
            Gate(;name="cu1", qubits, params=Any[θ])
        @case (QuoteNode(Intrinsics.UGate(α, β, γ)), 1, 1)
            Gate(;name="cu3", qubits, params=Any[α, β, γ])
        @case _
            error("control statement is not compatible with Qobj(type=QASM)")
    end
end

function emit_measure(::QobjQASMTarget, locs::Locations, mi::MemoryInfo, st::CodeGenState)
    ret = Measure(;
        qubits=get_qubit_address(mi, locs),
        memory=mi.slots[st.pc],
        # check if the measure result is used
        # if it is used, store it in register
        register=haskey(mi.registers, st.pc) ? mi.registers[st.pc] : nothing
    )
    return ret
end

function emit_barrier(target::QobjQASMTarget, locs::Locations, mi::MemoryInfo, st::CodeGenState)
    return Barrier(;qubits=get_qubit_address(mi, locs))
end

function condition_stmt(stmt::Vector, conditional)
    [condition_stmt(each, conditional) for each in stmt]
end

function condition_stmt(stmt::Gate, conditional)
    return Gate(;
        name=stmt.name,
        qubits=stmt.qubits,
        params=stmt.params,
        texparams=stmt.texparams,
        conditional,
    )
end

function is_reset_stmt(measure, stmt, val, mi::MemoryInfo)
    stmt isa Vector && length(stmt) == 1 || return false
    stmt[1] isa Gate && stmt[1].name == "x" && val == 1 || return false

    # check if measure loc is the same as X gate loc
    @switch measure begin
        @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs::Locations))
            return get_qubit_address(mi, locs) == stmt[1].qubits
        @case _
            return false
    end
end

function get_qubit_address(mi::MemoryInfo, locs::Locations)
    qubits = Vector{Int}(undef, length(locs))
    get_qubit_address!(qubits, mi, locs)
    return qubits
end

function get_qubit_address(mi::MemoryInfo, locs::Locations, ctrl::CtrlLocations)
    qubits = Vector{Int}(undef, length(locs) + length(ctrl))
    get_qubit_address!(qubits, mi, locs)
    get_qubit_address!(qubits, mi, ctrl.storage, length(locs))
    return qubits
end

function get_qubit_address!(qubits::Vector{Int}, mi::MemoryInfo, locs::Locations, offset::Int=0)
    for (k, loc) in enumerate(locs)
        qubits[k + offset] = mi.qubits[plain(loc)]
    end
    return qubits
end

function measure_ssa(ci::CodeInfo)
    measure_ssa = Int[]
    for (v, stmt) in enumerate(ci.code)
        @switch stmt begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs))
                push!(measure_ssa, v)
            @case _
                nothing
        end
    end
    return measure_ssa
end

measure_ssa_uses(ci::CodeInfo, measure_ssa=measure_ssa(ci)) =
    measure_ssa_uses!(Set{Int}(), ci, measure_ssa)

function measure_ssa_uses!(used::Set{Int}, ci::CodeInfo, measure_ssa=measure_ssa(ci))
    for (v, stmt) in enumerate(ci.code)
        @switch stmt begin
            @case GotoIfNot(SSAValue(id), dest)
                cond_stmt = ci.code[id]
                for useref in Core.Compiler.userefs(cond_stmt)
                    val = useref[]
                    if isa(val, SSAValue) && val.id in measure_ssa
                        push!(used, val.id)
                    end
                end
            @case _
                nothing
        end
    end
    return used
end

function measure_ssa_returns(ci::CodeInfo, measure_ssa=measure_ssa(ci))
    measure_ssa_returns!(Set{Int}(), ci, measure_ssa)
end

function measure_ssa_returns!(ret::Set{Int}, ci::CodeInfo, measure_ssa=measure_ssa(ci))
    for (v, stmt) in enumerate(ci.code)
        stmt isa ReturnNode || continue
        find_return_ssa_deps!(ret, ci, stmt, measure_ssa)
    end
    return ret
end

function find_return_ssa_deps!(ret::Set, ci::CodeInfo, stmt, measure_ssa)
    for useref in Core.Compiler.userefs(stmt)
        val = useref[]
        isa(val, SSAValue) || continue
        if val.id in measure_ssa
            push!(ret, val.id)
        else
            find_return_ssa_deps!(ret, ci, ci.code[val.id], measure_ssa)
        end
    end
    return ret
end
