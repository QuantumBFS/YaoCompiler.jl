module QASM

export @qasm_str

using RBNF
using ExprTools
using OpenQASM
using OpenQASM.Types
using OpenQASM.Types: Gate
using ..YaoCompiler

mutable struct VirtualRegister
    type::Symbol
    address::UnitRange{Int}
end

mutable struct RegisterRecord
    map::Dict{String,VirtualRegister}
    nqubits::Int
    ncbits::Int
end

mutable struct GateRegisterRecord
    map::Dict
    total::Int
end

Base.getindex(x::RegisterRecord, key) = x.map[key]
Base.getindex(x::VirtualRegister, xs...) = x.address[xs...]

RegisterRecord() = RegisterRecord(Dict{String,UnitRange{Int}}(), 0, 0)
GateRegisterRecord() = GateRegisterRecord(Dict(), 0)

struct Ctx
    m::Module
    source::LineNumberNode
    record::Any
end

# tokens don't need context
transpile(::Ctx, x::RBNF.Token) = transpile(x)

transpile(x::RBNF.Token{:unnamed}) = Symbol(x.str)
transpile(x::RBNF.Token{:reserved}) = Symbol(x.str)

function transpile(x::RBNF.Token{:id})
    x.str == "pi" && return Base.pi
    return Symbol(x.str)
end

function transpile(x::RBNF.Token{:float64})
    return Base.parse(Float64, x.str)
end

function transpile(x::RBNF.Token{:int})
    return Base.parse(Int, x.str)
end

transpile_list(::Ctx, ::Nothing) = Any[]
transpile_list(ctx::Ctx, x) = Any[transpile(ctx, x)]

function transpile_list(ctx::Ctx, xs::Vector)
    [transpile(ctx, each) for each in xs]
end

transpile(m::Module, ast::MainProgram) = transpile(m, LineNumberNode(0), ast)

function transpile(m::Module, l::LineNumberNode, ast::MainProgram)
    # check sure minimum compatibility
    @assert v"2.0.0" <= ast.version < v"3.0.0"

    code = Expr(:block)
    body = Expr(:block)
    routines = []
    record = scan_registers(ast)
    ctx = Ctx(m, l, record)

    for stmt in ast.prog
        if stmt isa RegDecl
            continue
        elseif stmt isa Types.Gate
            push!(routines, transpile(ctx, stmt))
        elseif stmt isa Include
            file = stmt.file.str[2:end-1]
            # use relative path to current file if not in REPL
            # isinteractive can be true in IDEs
            if !isnothing(l.file) && !(isinteractive() && isempty(PROGRAM_FILE))
                file = joinpath(dirname(string(l.file)), file)
            end
            source = read(file, String)
            ast = OpenQASM.parse(source)
            push!(code.args, transpile(m, l, ast))
        else
            ex = transpile(ctx, stmt)
            if !isnothing(ex)
                push!(body.args, ex)
            end
        end
    end

    # if there are classical registers
    # return them in a NamedTuple
    ret = Expr(:tuple)
    for (k, r) in record.map
        if r.type === :classical
            name = Symbol(k)
            push!(ret.args, Expr(:(=), name, name))
        end
    end

    if !isempty(body.args)
        if isempty(ret.args)
            push!(body.args, :(return))
        else
            push!(body.args, :(return $ret))
        end
    end

    # routines
    for each in routines
        push!(code.args, each)
    end

    # create an anoymous routine
    # if there are global statements
    if !isempty(body.args)
        def = Dict{Symbol,Any}(:name => gensym(:qasm), :body => body)
        push!(code.args, YaoCompiler.device_def(def))
    end
    return code
end

function transpile(ctx::Ctx, stmt::Types.Gate)
    name = transpile(stmt.decl.name)
    args = transpile_list(ctx, stmt.decl.cargs)
    record = transpile_gate_registers(stmt.decl.qargs)
    body = Expr(:block)
    new_ctx = Ctx(ctx.m, ctx.source, record)

    for each in stmt.body
        push!(body.args, transpile(new_ctx, each))
    end

    def = Dict(:name => name, :args => args, :body => body)
    return YaoCompiler.device_def(def)
end

function transpile_gate_registers(stmt::Vector)
    record = GateRegisterRecord()
    for each in stmt
        haskey(record.map, each.str) && throw(Meta.ParseError("duplicated register name $(each.str)"))
        record.total += 1
        record.map[each.str] = record.total
    end
    return record
end

semantic_gate(gate, locs) = Expr(:call, GlobalRef(YaoCompiler.Semantic, :gate), gate, locs)
semantic_ctrl(gate, locs, ctrl) =
    Expr(:call, GlobalRef(YaoCompiler.Semantic, :ctrl), gate, locs, ctrl)

function transpile(ctx::Ctx, stmt::UGate)
    code = Expr(:block)
    locs = transpile(ctx, stmt.qarg)
    push!(
        code.args,
        semantic_gate(Expr(:call, GlobalRef(Intrinsics, :Rz), transpile(ctx, stmt.z1)), locs),
    )
    push!(
        code.args,
        semantic_gate(Expr(:call, GlobalRef(Intrinsics, :Ry), transpile(ctx, stmt.y)), locs),
    )
    push!(
        code.args,
        semantic_gate(Expr(:call, GlobalRef(Intrinsics, :Rz), transpile(ctx, stmt.z2)), locs),
    )
    return code
end

function transpile(ctx::Ctx, stmt::CXGate)
    return semantic_ctrl(
        GlobalRef(Intrinsics, :X),
        transpile(ctx, stmt.qarg),
        CtrlLocations(transpile(ctx, stmt.ctrl)),
    )
end

function transpile(ctx::Ctx, stmt::IfStmt)
    return :(
        if $(transpile(ctx, stmt.left)) == $(transpile(ctx, stmt.right))
            $(transpile(ctx, stmt.body))
        end
    )
end

function transpile(ctx::Ctx, stmt::Measure)
    locs = transpile(ctx, stmt.qarg)
    name = transpile(ctx, stmt.carg)
    return Expr(:(=), name, Expr(:call, GlobalRef(YaoCompiler.Semantic, :measure), locs))
end

function transpile(ctx::Ctx, stmt::Barrier)
    return Expr(
        :call,
        GlobalRef(YaoCompiler.Semantic, :barrier),
        transpile_locations(ctx, stmt.qargs),
    )
end

function transpile(ctx::Ctx, stmt::Instruction)
    op = stmt.name
    # NOTE: these are not intrinsic function in QASM
    # users need qelib1.inc to get the definition
    # but for convenience we treat them as intrinsic
    # function here in YaoCompiler, since they are predefined
    # as stdlib in YaoCompiler.

    # isnothing(stmt.lst1) || throw(Meta.ParseError("$op gate should not have classical parameters"))
    locs = transpile_locations(ctx, stmt.qargs)

    if op == "x"
        semantic_gate(Intrinsics.X, locs)
    elseif op == "y"
        semantic_gate(Intrinsics.Y, locs)
    elseif op == "z"
        semantic_gate(Intrinsics.Z, locs)
    elseif op == "h"
        semantic_gate(Intrinsics.H, locs)
    elseif op == "s"
        semantic_gate(Intrinsics.S, locs)
    elseif op == "ccx"
        semantic_ctrl(Intrinsics.X, locs[3], CtrlLocations(locs[1:2]))
    else # some user defined routine
        gate = Expr(:call, GlobalRef(ctx.m, Symbol(op)), transpile_list(ctx, stmt.cargs)...)
        semantic_gate(gate, locs)
    end
end

function transpile(ctx::Ctx, stmt::FnExp)
    return Expr(:call, stmt.fn, transpile(ctx, stmt.arg))
end

function transpile_locations(ctx, stmts::Vector)
    locs = map(stmts) do stmt
        transpile(ctx, stmt)
    end
    return merge_locations(locs...)
end

function transpile(ctx::Ctx, stmt::Bit)
    record = ctx.record
    if record isa RegisterRecord
        r = record[stmt.name.str]
        r.type === :classical && return Symbol(stmt.name.str)

        if isnothing(stmt.address)
            return Locations(r[:])
        else
            address = transpile(stmt.address)
            return Locations(r[address+1])
        end
    else
        return Locations(record.map[stmt.name.str])
    end
end

transpile(ctx::Ctx, stmt::Negative) = Expr(:call, -, transpile(ctx, stmt.value))

function transpile(ctx::Ctx, stmt::Tuple)
    length(stmt) == 3 || throw(Meta.ParseError("unrecognized expression: $stmt"))
    stmt[2]::RBNF.Token
    if stmt[2].str in ("+", "-", "*", "/")
        return Expr(:call, Symbol(stmt[2].str), transpile(ctx, stmt[1]), transpile(ctx, stmt[3]))
    else
        throw(Meta.ParseError("unrecognized expression: $stmt"))
    end
end

function scan_registers(ast::MainProgram)
    return scan_registers!(RegisterRecord(), ast)
end

function scan_registers!(record::RegisterRecord, ast::MainProgram)
    for stmt in ast.prog
        scan_registers!(record, stmt)
    end
    return record
end

function scan_registers!(record::RegisterRecord, ast::RegDecl)
    nbits = transpile(ast.size)
    nqubits = record.nqubits
    ncbits = record.ncbits

    if ast.type.str == "qreg"
        record.map[ast.name.str] = VirtualRegister(:quantum, (nqubits+1):(nqubits+nbits))
        record.nqubits += nbits
    else # classical
        record.map[ast.name.str] = VirtualRegister(:classical, (ncbits+1):(ncbits+nbits))
        record.ncbits += nbits
    end
    return record
end

scan_registers!(record::RegisterRecord, ast) = record

function qasm_str_m(m::Module, l::LineNumberNode, source::String)
    ast = OpenQASM.parse(source)
    return transpile(m, l, ast)
end

macro qasm_str(source::String)
    return esc(qasm_str_m(__module__, __source__, source))
end

macro qasm_str(source::Expr)
    source.head === :string || error("expect a String")

    args = map(source.args) do x
        x isa String && return x
        return Base.eval(__module__, x)
    end

    return esc(qasm_str_m(__module__, __source__, join(args)))
end

macro include_str(path)
    if path isa Expr
        path.head === :string || error("expect a String")
        file = map(path.args) do x
            x isa String && return x
            return Base.eval(__module__, x)
        end |> join
    elseif path isa String
        file = path
    else
        error("expect a String")
    end

    return esc(qasm_str_m(__module__, __source__, read(file, String)))
end

end # end module
