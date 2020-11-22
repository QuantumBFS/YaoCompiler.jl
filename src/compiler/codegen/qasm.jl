using RBNF
using RBNF: Token

const pi_token = Token{:reserved}("pi")
const half_pi_token = (pi_token, Token{:reserved}("/"), Token{:int}("2"))

# Intrinsics
# // 3-parameter 2-pulse single qubit gate
# gate u3(theta,phi,lambda) q { U(theta,phi,lambda) q; }
# // 2-parameter 1-pulse single qubit gate
# gate u2(phi,lambda) q { U(pi/2,phi,lambda) q; }
# // 1-parameter 0-pulse single qubit gate
# gate u1(lambda) q { U(0,0,lambda) q; }
# // Pauli gate: bit-flip
# gate x a { u3(pi,0,pi) a; }
# // Pauli gate: bit and phase flip
# gate y a { u3(pi,pi/2,pi/2) a; }
# // Pauli gate: phase flip
# gate z a { u1(pi) a; }
# // Clifford gate: Hadamard
# gate h a { u2(0,pi) a; }
# // Clifford gate: sqrt(Z) phase gate
# gate s a { u1(pi/2) a; }
# // Clifford gate: conjugate of sqrt(Z)
# gate sdg a { u1(-pi/2) a; }
# // C3 gate: sqrt(S) phase gate
# gate t a { u1(pi/4) a; }
# // C3 gate: conjugate of sqrt(S)
# gate tdg a { u1(-pi/4) a; }
function code_qasm(gate::IntrinsicRoutine)
    gt = typeof(gate)
    args = Any[getfield(gate, x) for x in fieldnames(gt)]
    if isempty(args)
        return code_qasm(gt)
    else
        return code_qasm(gt, args)
    end
end

qasm_gate_name(::Type{Intrinsics.XGate}) = "x"
qasm_gate_name(::Type{Intrinsics.YGate}) = "y"
qasm_gate_name(::Type{Intrinsics.ZGate}) = "z"
qasm_gate_name(::Type{Intrinsics.HGate}) = "h"
qasm_gate_name(::Type{Intrinsics.SGate}) = "s"
qasm_gate_name(::Type{Intrinsics.TGate}) = "t"
qasm_gate_name(::Type{<:Intrinsics.Rx}) = "rx"
qasm_gate_name(::Type{<:Intrinsics.Ry}) = "ry"
qasm_gate_name(::Type{<:Intrinsics.Rz}) = "rz"

code_qasm(::Type{Intrinsics.XGate}) = QASM.UGate(pi_token, Token{:int}("0"), pi_token, [])

function code_qasm(::Type{Intrinsics.YGate})
    QASM.UGate(pi_token, half_pi_token, half_pi_token, nothing)
end

function code_qasm(::Type{Intrinsics.ZGate})
    QASM.UGate(Token{:int}("0"), Token{:int}("0"), pi_token, nothing)
end

function code_qasm(::Type{Intrinsics.HGate})
    QASM.UGate(half_pi_token, Token{:int}("0"), pi_token, nothing)
end

function code_qasm(::Type{Intrinsics.SGate})
    QASM.UGate(Token{:int}("0"), Token{:int}("0"), half_pi_token, nothing)
end

function code_qasm(::Type{Intrinsics.TGate})
    QASM.UGate(
        Token{:int}("0"),
        Token{:int}("0"),
        (pi_token, Token{:reserved}("/"), Token{:int}("4")),
        nothing,
    )
end

# // Rotation around X-axis
# gate rx(theta) a { u3(theta,-pi/2,pi/2) a; }
# // rotation around Y-axis
# gate ry(theta) a { u3(theta,0,0) a; }
# // rotation around Z axis
# gate rz(phi) a { u1(phi) a; }

to_token(x::AbstractFloat) = Token{:float64}(string(Float64(x)))
to_token(x::String) = Token{:id}(x)
to_token(x::Integer) = Token{:int}(string(x))
to_token(x) = x

function code_qasm(::Type{<:Intrinsics.Rx}, vars::Vector{Any})
    QASM.UGate(to_token(vars[1]), QASM.Negative(half_pi_token), half_pi_token, nothing)
end

# TODO: check this, U(theta, 0, 0) is Rz?
function code_qasm(::Type{<:Intrinsics.Ry}, vars::Vector{Any})
    QASM.UGate(Token{:int}("0"), to_token(vars[1]), Token{:int}("0"), nothing)
end

function code_qasm(::Type{<:Intrinsics.Rz}, vars::Vector{Any})
    QASM.UGate(Token{:int}("0"), Token{:int}("0"), to_token(vars[1]), nothing)
end

abstract type TargetQASM end
# toplevel QASM
Base.@kwdef struct TargetQASMTopLevel <: TargetQASM
    inline_intrinsic::Bool = false
end

# QASM gate decl
Base.@kwdef struct TargetQASMGate <: TargetQASM
    inline_intrinsic::Bool = false
end

struct RegMap
    cbits::Dict{Any,Tuple{String,Int}}
    regs_to_locs::Dict{Int,Vector{Int}}
    locs_to_reg_addr::Dict{Int,Tuple{Int,Int}}
end

function RegMap(target, ci::CodeInfo)
    locs_to_regs = Dict{Int,Int}()
    # ssa/slot => (name, size)
    cbits = Dict{Any,Tuple{String,Int}}()

    for (v, stmt) in enumerate(ci.code)
        if stmt isa ReturnNode && isdefined(stmt, :val) && stmt.val isa SSAValue
            ret = ci.code[stmt.val.id]
            if ret isa Expr && ret.head === :new && ret.args[1] <: NamedTuple
                cnames = ret.args[1].parameters[1]
                for i in 1:length(cnames)
                    x = ret.args[i+1]::SSAValue
                    _, size = cbits[x.id]
                    cbits[x.id] = (string(cnames[i]), size)
                end
            end
        end

        is_quantum_statement(stmt) || continue
        qt = quantum_stmt_type(stmt)

        if qt === :measure
            cvar, locs = obtain_const_measure_stmt(stmt, ci)
            if cvar isa SlotNumber
                name = string(ci.slotnames[cvar.id])
                cbits[cvar] = (name, length(locs))
            else
                name = creg_name(v)
                cbits[v] = (name, length(locs))
            end

            # allocate new register for measurements
            allocate_new_qreg!(locs_to_regs, locs)
        elseif qt === :barrier
            locs = obtain_const(stmt.args[2], ci)::Locations
            record_locations!(target, locs_to_regs, locs)
        elseif qt === :gate
            if stmt.head === :invoke
                locs = obtain_const(stmt.args[4], ci)::Locations
            elseif stmt.head === :call
                locs = obtain_const(stmt.args[3], ci)::Locations
            end
            record_locations!(target, locs_to_regs, locs)
        elseif qt === :ctrl
            if stmt.head === :invoke
                locs = obtain_const(stmt.args[4], ci)::Locations
                ctrl = obtain_const(stmt.args[5], ci)::CtrlLocations
            elseif stmt.head === :call
                locs = obtain_const(stmt.args[3], ci)::Locations
                ctrl = obtain_const(stmt.args[4], ci)::CtrlLocations
            end

            record_locations!(target, locs_to_regs, locs)
            record_locations!(target, locs_to_regs, ctrl.storage)
        end
    end

    regs_to_locs = Dict{Int,Vector{Int}}()
    for (k, r) in locs_to_regs
        locs = get!(regs_to_locs, r, Int[])
        push!(locs, k)
    end

    # loc => reg, addr
    locs_to_reg_addr = Dict{Int,Tuple{Int,Int}}()
    for (r, locs) in regs_to_locs
        sort!(locs)
        for (k, loc) in enumerate(locs)
            locs_to_reg_addr[loc] = (r, k - 1)
        end
    end

    return RegMap(cbits, regs_to_locs, locs_to_reg_addr)
end

function Base.getindex(map::RegMap, loc::Locations)
    return ntuple(length(loc)) do k
        map.locs_to_reg_addr[loc.storage[k]]
    end
end

function Base.getindex(map::RegMap, loc::CtrlLocations)
    return map[loc.storage]
end

mutable struct QASMCodeGenState
    pc::Int
    stmt::Any
    regmap::RegMap

    name::Any
    carg_names::Any
    ssa_cname_map::Any
end

function QASMCodeGenState(target::TargetQASMTopLevel, ci::CodeInfo)
    return QASMCodeGenState(0, nothing, RegMap(target, ci), nothing, nothing, nothing)
end

function QASMCodeGenState(target::TargetQASMGate, ci::CodeInfo)
    name, carg_names, ssa_cname_map = scan_cargs(ci)
    return QASMCodeGenState(0, nothing, RegMap(target, ci), name, carg_names, ssa_cname_map)
end

# NOTE:
# we can only transform routines satisfy the following:
# 1. locations are constants, in Julia we can calculate locations dynamically
#    but this is not allowed in QASM
# 2. do not contain classical functions calls except for 
# fn = ("sin" | "cos" | "tan" | "exp" | "ln" | "sqrt")
# binop = ('+' | '-' | '*' | '/')
# since QASM's if is actually a GotoIfNot node
# we don't reloop the SSA here, but assume the CodeInfo should
# not contain any GotoNode, which is incompatible with QASM

function allocate_new_qreg!(locs_to_regs, locs)
    if length(locs) == 1
        raw = locs.storage
        get!(locs_to_regs, raw, 1)
    else
        k_reg = maximum(get(locs_to_regs, each, 1) for each in locs) + 1

        for each in locs
            locs_to_regs[each] = k_reg
        end
    end
end

function record_locations!(::TargetQASMTopLevel, locs_to_regs::Dict{Int,Int}, locs::Locations)
    for each in locs
        get!(locs_to_regs, each, 1)
    end
    return locs_to_regs
end

function record_locations!(::TargetQASMGate, locs_to_regs::Dict{Int,Int}, locs::Locations)
    for each in locs
        if !haskey(locs_to_regs, each)
            locs_to_regs[each] = length(keys(locs_to_regs)) + 1
        end
    end
    return locs_to_regs
end

qreg_name(idx::Int) = string("qreg_", idx)
creg_name(idx::Int) = string("creg_", idx)

function codegen(target::TargetQASMTopLevel, ci::CodeInfo)
    st = QASMCodeGenState(target, ci)
    prog = Any[]

    # allocate registers
    for (k, locs) in st.regmap.regs_to_locs
        push!(
            prog,
            QASM.RegDecl(
                Token{:reserved}("qreg"),
                # we probably want to have a better strategy
                # to avoid potential name conflicts
                Token{:id}(qreg_name(k)),
                Token{:int}(string(length(locs))),
            ),
        )
    end

    for (_, (name, size)) in st.regmap.cbits
        pushfirst!(
            prog,
            QASM.RegDecl(Token{:reserved}("creg"), Token{:id}(name), Token{:int}(string(size))),
        )
    end

    # NOTE: QASM compatible code won't have
    # branches except simple `if x == y` statement
    # as a result, we don't need program counter
    # here, just execute directly
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

        isnothing(inst) && continue
        if inst isa Vector
            append!(prog, inst)
        else
            push!(prog, inst)
        end
    end
    return QASM.MainProgram(v"2.0", prog)
end

function qasm_gate_name(spec)
    name = string(routine_name(spec))
    if '#' in name
        name = "__julia_lambda" * replace(name, "#" => "_")
    end
    return name
end

function scan_cargs(ci::CodeInfo)
    v = findfirst(ci.code) do stmt
        @match stmt begin
            Expr(:call, getfield, Argument(2), :variables) => true
            Expr(:call, GlobalRef(Base, :getfield), Argument(2), QuoteNode(:variables)) => true
            Expr(:call, getfield, SlotNumber(2), QuoteNode(:variables)) => true
            Expr(:call, GlobalRef(Base, :getfield), SlotNumber(2), QuoteNode(:variables)) => true
            _ => false
        end
    end

    @assert !isnothing(ci.parent)
    spec = ci.parent.specTypes.parameters[2]

    # none of the classical parameters are used
    isnothing(v) && return qasm_gate_name(spec), String[], Dict{Int,String}()

    tt = Tuple{spec.parameters[1],spec.parameters[2].parameters...}
    ms = methods(routine_stub, tt)
    length(ms) == 1 || error("ambiguous method call")
    method = first(ms)
    carg_names = split(chop(method.slot_syms), '\0')[3:end]
    cargs = Dict{Int,String}()

    for (v, stmt) in enumerate(ci.code)
        name = @match stmt begin
            Expr(:call, getfield, SSAValue(v), k, true) => carg_names[k]
            Expr(:(=), SlotNumber(slot), Expr(:call, GlobalRef(Base, :getindex), SSAValue(v), i)) => string(ci.slotnames[slot])
            _ => nothing
        end

        isnothing(name) && continue
        cargs[v] = name
    end

    return qasm_gate_name(spec), carg_names, cargs
end

function codegen(target::TargetQASMGate, ci::CodeInfo)
    st = QASMCodeGenState(target, ci)
    qargs = Any[Token{:id}(qreg_name(k)) for (k, _) in st.regmap.regs_to_locs]
    cargs = Any[Token{:id}(cname) for cname in st.carg_names]
    decl = QASM.GateDecl(Token{:id}(st.name), cargs, qargs)

    prog = Any[]

    # NOTE: QASM compatible code won't have
    # branches except simple `if x == y` statement
    # as a result, we don't need program counter
    # here, just execute directly
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

        isnothing(inst) && continue
        if inst isa Vector
            append!(prog, inst)
        else
            push!(prog, inst)
        end
    end
    return QASM.Gate(decl, prog)
end

function _qasm_name(x)
    if x in [:X, :Y, :Z, :H, :T, :Rx, :Ry, :Rz]
        return lowercase(string(x))
    else
        return string(x)
    end
end

function codegen_stmt(target::TargetQASM, ci::CodeInfo, st::QASMCodeGenState)
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
        error("incompatible statement for QASM: $(st.stmt)")
    end
    return inst
end

function index_qreg(r::Int, addr::Int, regmap::RegMap)
    # do not index qreg if it only has one qubit
    if length(regmap.regs_to_locs[r]) == 1 && addr == 0
        return QASM.Bit(qreg_name(r))
    else
        return QASM.Bit(qreg_name(r), addr)
    end
end

function index_qreg(loc::Locations, regmap::RegMap)
    return map(regmap[loc]) do (r, addr)
        index_qreg(r, addr, regmap)
    end
end

index_qreg(ctrl::CtrlLocations, regmap::RegMap) = index_qreg(ctrl.storage, regmap)

function codegen_gate(target::TargetQASM, ci::CodeInfo, st::QASMCodeGenState)
    # NOTE: QASM compatible code should have constant location
    gate, gt, locs = obtain_const_gate_stmt(st.stmt, ci)

    gt <: IntrinsicRoutine || gt <: RoutineSpec || error("invalid gate type: $gate::$gt")


    if gt <: IntrinsicRoutine
        return codegen_intrinsic_gate(target, gate, gt, locs, ci, st)
    else
        name = qasm_gate_name(gt)
        cargs = codegen_cargs(target, ci, gate, st)
        qargs = Any[]

        for k in locs.storage
            r, addr = st.regmap.locs_to_reg_addr[k]
            push!(qargs, index_qreg(r, addr, st.regmap))
        end

        return QASM.Instruction(name, cargs, qargs)
    end
end

# TODO: polish duplicated code
function codegen_intrinsic_gate(
    target::TargetQASM,
    @nospecialize(gate),
    gt,
    locs::Locations,
    ci::CodeInfo,
    st::QASMCodeGenState,
)
    # expand the single qubit gates syntax sugar
    # this might be better to be moved to codeinfo
    # pre-processing stage
    # TODO: we probably should do this right after
    #       type infer
    if target.inline_intrinsic
        if gate isa IntrinsicRoutine
            inst = code_qasm(gate)
        elseif gate isa Expr
            cargs = codegen_cargs(target, ci, gate, st)
            inst = code_qasm(gt, cargs)
        end

        if is_one_qubit_gate(gt)
            inst = inst::QASM.UGate
            insts = []
            for k in locs.storage
                r, addr = st.regmap.locs_to_reg_addr[k]
                push!(insts, QASM.UGate(inst.z1, inst.y, inst.z2, index_qreg(r, addr, st.regmap)))
            end
            return insts
        else
            inst = inst::QASM.Instruction
            for k in locs.storage
                r, addr = st.regmap.locs_to_reg_addr[k]
                push!(qargs, index_qreg(r, addr, st.regmap))
            end

            return QASM.Instruction(inst.name, copy(inst.cargs), qargs)
        end
    else
        cargs = codegen_cargs(target, ci, gate, st)
        name = qasm_gate_name(gt)

        if is_one_qubit_gate(gt)
            insts = []
            for k in locs.storage
                r, addr = st.regmap.locs_to_reg_addr[k]
                push!(insts, QASM.Instruction(name, cargs, Any[index_qreg(r, addr, st.regmap)]))
            end
            return insts
        else
            for k in locs.storage
                r, addr = st.regmap.locs_to_reg_addr[k]
                push!(qargs, index_qreg(r, addr, st.regmap))
            end
            return QASM.Instruction(name, cargs, qargs)
        end
    end
end

function codegen_cargs(::TargetQASMTopLevel, ::CodeInfo, @nospecialize(gate), ::QASMCodeGenState)
    if gate isa RoutineSpec
        # constant parameters
        # non-constant parameters in toplevel is not allowed
        return Any[Token{:unnamed}(string(x)) for x in gate.variables]
    elseif gate isa IntrinsicRoutine
        return Any[Token{:unnamed}(string(getfield(gate, each))) for each in fieldnames(typeof(gate))]
    else
        error("classical variable in toplevel is not allowed in QASM")
    end
end

function codegen_cargs(
    target::TargetQASMGate,
    ci::CodeInfo,
    @nospecialize(gate),
    st::QASMCodeGenState,
)
    if gate isa Expr
        # IntrinsicSpec/RoutineSpec take a tuple
        # so we need to find the actual variables
        if gate.head === :new || gate.head === :call
            vars = gate.args[2:end]
        end

        cargs = Any[]
        for each in vars
            val = codegen_exp(target, ci, each, st)
            if val isa Vector
                append!(cargs, val)
            else
                push!(cargs, val)
            end
        end
        return cargs
    elseif gate isa RoutineSpec
        # constant parameters
        # non-constant parameters in toplevel is not allowed
        return Any[codegen_exp(target, ci, x, st) for x in gate.variables]
    elseif gate isa IntrinsicRoutine
        return Any[
            codegen_exp(target, ci, getfield(gate, each), st) for each in fieldnames(typeof(gate))
        ]
    else
        error("invalid instruction: $gate")
    end
end

# NOTE: toplevel program does not allow non-constant
# classical function calls
function codegen_exp(target::TargetQASM, ci::CodeInfo, @nospecialize(stmt), st::QASMCodeGenState)
    if stmt isa SlotNumber
        return Token{:id}(string(ci.slotnames[stmt.id]))
    end

    if stmt isa SSAValue
        if haskey(st.ssa_cname_map, stmt.id)
            return Token{:id}(st.ssa_cname_map[stmt.id])
        else
            return codegen_exp(target, ci, ci.code[stmt.id], st)
        end
    end

    if stmt isa Int
        return Token{:int}(string(stmt))
    elseif stmt isa AbstractFloat
        return Token{:float64}(string(Float64(stmt)))
    end

    stmt isa Expr || error("classical expression for QASM cannot contain control flow, got $stmt")

    if stmt.head === :call
        f = stmt.args[1]

        if f isa GlobalRef
            mod, fn_name = f.mod, f.name
            fn = Core.Compiler.abstract_eval_global(mod, fn_name).val
            fn === Any && error("cannot determine function call: $stmt")
        else
            fn = f
            fn_name = nameof(fn)
        end
        args = stmt.args[2:end]
    elseif stmt.head === :invoke
        fn = stmt.args[2]
        fn_name = stmt.args[1].def.name
        args = stmt.args[3:end]
    else
        error("incompatible expression for QASM: $stmt")
    end

    if fn === Core.tuple
        return Any[codegen_exp(target, ci, each, st) for each in args]
    end

    if fn === Core.Intrinsics.neg_float
        fn_name = :(-)
    end

    if length(args) == 1
        return QASM.FnExp(fn_name, codegen_exp(target, ci, args[1], st))
    elseif length(args) == 2 # binop
        if (fn === +) || (fn === Core.Intrinsics.add_float) || (fn === Core.Intrinsics.add_int)
            token = Token{:reserved}("+")
        elseif (fn === -) || (fn === Core.Intrinsics.sub_float) || (fn === Core.Intrinsics.sub_int)
            token = Token{:reserved}("-")
        elseif (fn === *) || (fn === Core.Intrinsics.mul_float) || (fn === Core.Intrinsics.mul_int)
            token = Token{:reserved}("*")
        elseif (fn === /) || (fn === Core.Intrinsics.div_float)
            token = Token{:reserved}("/")
        else
            error("incompatible binop for QASM: $fn")
            # token = Token{:reserved}(fn_name)
        end
        return (codegen_exp(target, ci, args[1], st), token, codegen_exp(target, ci, args[2], st))
    else
        error("incompatible function call for QASM: $stmt")
    end
end

function codegen_ctrl(::TargetQASM, ci::CodeInfo, st::QASMCodeGenState)
    gate, gt, locs, ctrl = obtain_const_ctrl_stmt(st.stmt, ci)
    gt <: IntrinsicRoutine || gt <: RoutineSpec || error("invalid gate type: $gate::$gt")

    all(ctrl.flags) || error("inverse ctrl is not supported in QASM backend yet")
    if gate === Intrinsics.X && length(ctrl) == 1 && length(locs) == 1
        qargs = Any[index_qreg(ctrl, st.regmap)..., index_qreg(locs, st.regmap)...]
        return QASM.Instruction(Token{:id}("CX"), Any[], qargs)
    elseif gate === Intrinsics.X && length(ctrl) == 2 && length(locs) == 1
        error(
            "ccx is not valid intrinsic QASM instruction. posssible fix:\n",
            "1. include a qasm stdlib e.g qelib<version>.inc and use ccx gate from it\n",
            "2. define your own ccx gate via @device using CX and single qubit gate",
        )
        # ctrl.flags[1] || error("inverse ctrl is not supported in QASM backend yet")
        # qargs = Any[index_qreg(ctrl, st.regmap)..., index_qreg(locs, st.regmap)...]
        # return QASM.Instruction(
        #     Token{:id}("ccx"),
        #     Any[], qargs
        # )
    else
        error("invalid control statement for QASM backend, got: $(st.stmt)")
    end
end

function codegen_measure(::TargetQASM, ci::CodeInfo, st::QASMCodeGenState)
    if st.stmt.head === :(=)
        slot = st.stmt.args[1]::SlotNumber
        measure_ex = st.stmt.args[2]
        locs = obtain_const(measure_ex.args[2], ci)::Locations
        cname, _ = st.regmap.cbits[slot]
    else
        locs = obtain_const(st.stmt.args[2], ci)::Locations
        cname, _ = st.regmap.cbits[st.pc]
    end

    r, addr = st.regmap.locs_to_reg_addr[first(locs)]

    if length(locs) == 1
        qarg = index_qreg(r, addr, st.regmap)
        return QASM.Measure(qarg, QASM.Bit(cname, 0))
    else
        # by construction the registers are the same
        # and is exactly of size length(locs)
        return QASM.Measure(QASM.Bit(qreg_name(r)), QASM.Bit(cname, length(locs)))
    end
end

function codegen_barrier(::TargetQASM, ci::CodeInfo, st::QASMCodeGenState)
    locs = obtain_const(st.stmt.args[2], ci)::Locations

    qargs = Any[]
    args = Dict{Int,Vector{Int}}()

    for each in locs
        r, addr = st.regmap.locs_to_reg_addr[each]
        addrs = get!(args, r, Int[])
        push!(addrs, addr)
    end

    for (r, addrs) in args
        # do not index qreg explicitly if barrier size
        # is the same with register size
        if length(st.regmap.regs_to_locs[r]) == length(addrs)
            push!(qargs, QASM.Bit(qreg_name(r)))
        else
            for addr in addrs
                push!(qargs, QASM.Bit(qreg_name(r), addr))
            end
        end
    end
    return QASM.Barrier(qargs)
end

function codegen_ifnot(target::TargetQASM, ci::CodeInfo, st::QASMCodeGenState)
    cond = st.stmt.cond::SSAValue
    creg, right = obtain_qasm_ifnot_cond(cond, ci)

    if creg isa SlotNumber
        cname = ci.slotnames[creg.id]
    elseif creg isa SSAValue
        # NOTE:
        # in QASM, this will only be assigned once
        # from measure statement
        # TODO: we might want to make this more general
        # by having an intermediate type MeasureResult
        cname = creg_name(creg.id)
    end
    # find the first quantum stmt after goto
    move_to_first_quantum_stmt(ci, st)
    body = codegen_stmt(target, ci, st)
    return QASM.IfStmt(Token{:id}(cname), Token{:int}(string(right)), body)
end

function validate(target::TargetQASM, ci::CodeInfo)
    for (v, stmt) in enumerate(ci)
        if is_quantum_statement(stmt)
        elseif stmt isa GotoIfNot
        elseif stmt isa Expr
            if stmt.head === :call
                validate_call(target, ci, stmt)
            end
        end
        stmt_type = ci.ssavaluetypes[v]

    end
end

# NOTE:
# valid function in QASM compatible program:
# basic math functions:
# fn = ("sin" | "cos" | "tan" | "exp" | "ln" | "sqrt")
# binop = ('+' | '-' | '*' | '/')
function validate_call(::TargetQASM, ci::CodeInfo, stmt::Expr)
    if stmt.args[1] isa GlobalRef
        stmt.mod === Base
        stmt.name in []
    end
end
