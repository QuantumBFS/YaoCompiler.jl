macro qasm_str(s::String)
    s = Meta.parse("\"\"\"\n$s\"\"\"")
    s isa String && return esc(qasm_str_m(__module__, __source__, s))
    args = map(s.args) do x
        x isa String && return x
        Base.eval(__module__, x)
    end
    return esc(qasm_str_m(__module__, __source__, join(args)))
end

function qasm_str_m(m::Module, lino::LineNumberNode, src::String)
    ast = OpenQASM.parse(src)
    return transpile(m, lino, ast)
end

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
    nbits = transpile_token(ast.size)
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

struct TranspileCtx
    m::Module
    source::LineNumberNode
    record::Any
end

function transpile(m::Module, lino::LineNumberNode, ast::MainProgram)
    @assert v"2.0.0" ≤ ast.version < v"3.0.0"

    code = Expr(:block)
    body = Expr(:block)
    routines = []
    record = scan_registers(ast)
    ctx = TranspileCtx(m, lino, record)

    for stmt in ast.prog
        if stmt isa RegDecl
            continue
        elseif stmt isa Types.Gate
            push!(routines, transpile(ctx, stmt))
        elseif stmt isa Include
            file = stmt.file.str[2:end-1]
            # use relative path to current file if not in REPL
            # isinteractive can be true in IDEs
            if !isnothing(lino.file) && !(isinteractive() && isempty(PROGRAM_FILE))
                file = joinpath(dirname(string(lino.file)), file)
            end
            source = read(file, String)
            ast = OpenQASM.parse(source)
            push!(code.args, transpile(m, lino, ast))
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

    if !isempty(body.args)
        def = JLFunction(name=gensym(:qasm), body=body)
        push!(code.args, YaoCompiler.codegen_routine(def))
    end
    return code
end

function transpile(ctx::TranspileCtx, stmt::Gate)
    name = transpile(ctx, stmt.decl.name)
    args = transpile_exp(ctx, stmt.decl.cargs)
    record = transpile_gate_registers(stmt.decl.qargs)
    body = Expr(:block)
    new_ctx = TranspileCtx(ctx.m, ctx.source, record)

    for each in stmt.body
        push!(body.args, transpile(new_ctx, each))
    end
    def = JLFunction(;name, args, body)
    return YaoCompiler.codegen_routine(def)
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

function transpile(ctx::TranspileCtx, stmt)
    # NOTE: apply etc. are treated as keyword inside device
    # no need to put Intrinsics infront of them
    @switch stmt begin
        @case UGate(z1, y, z2, qarg)
            locs = transpile(ctx, qarg)
            quote
                apply($Intrinsics.Rz($(transpile_exp(ctx, z1))), $locs)
                apply($Intrinsics.Ry($(transpile_exp(ctx, y))), $locs)
                apply($Intrinsics.Rz($(transpile_exp(ctx, z2))), $locs)
            end

        @case CXGate(ctrl, qarg)
            locs = transpile(ctx, qarg)
            ctrl = transpile(ctx, ctrl)
            :(apply($Intrinsics.X, $locs, $ctrl))

        @case IfStmt(left, right, body)
            quote
                if $(transpile(ctx, left)) == $(transpile(ctx, right))
                    $(transpile(ctx, body))
                end
            end

        @case Measure(qarg, carg)
            :($(transpile(ctx, carg)) = measure($(transpile(ctx, qarg))))

        @case Barrier(qargs)
            :(barrier($(transpile_qargs(ctx, qargs))))

        @case Instruction(op, cargs, qargs)
            args = transpile_exp(ctx, cargs)
            locs = transpile_qargs(ctx, qargs)
            @match op begin
                "x" => :(apply($Intrinsics.X, $locs))
                "y" => :(apply($Intrinsics.Y, $locs))
                "z" => :(apply($Intrinsics.Z, $locs))
                "h" => :(apply($Intrinsics.H, $locs))
                "s" => :(apply($Intrinsics.S, $locs))
                _ => begin
                    gate = Expr(:call, GlobalRef(ctx.m, Symbol(op)), transpile_exp(ctx, cargs)...)
                    :(apply($gate, $locs))
                end
            end

        # tokens
        @case Bit(name, address)
            ctx.record isa RegisterRecord || return Locations(ctx.record.map[name.str])
            r = ctx.record[name.str]
            r.type === :classical && return Symbol(name.str)
            isnothing(address) && return Locations(r[:])
            address = transpile(ctx, address)
            Locations(r[address+1])

        @case ::Token
            transpile_token(stmt)

        @case _
            error("unknown statement: $stmt")
    end
end

function transpile_qargs(ctx::TranspileCtx, stmts::Vector)
    locs = map(stmts) do stmt
        transpile(ctx, stmt)
    end
    return merge_locations(locs...)
end

function transpile_exp(ctx::TranspileCtx, stmt)
    @switch stmt begin
        @case Call(name, args)
            Expr(:call, name, transpile_exp(ctx, args))
        @case Neg(val)
            Expr(:call, :-, transpile_exp(ctx, val))
        @case Token{:reserved}(_, _, _, "pi", _)
            return π
        @case (Token{:reserved}(_, _, _, "pi", _), Token{:reserved}(_, _, _, "/", _), Token{:int}(_, _, _, "2", _))
            Expr(:call, GlobalRef(Base, :(/)), π, 2)
        @case (lhs, op::Token, rhs)
            Expr(:call, Symbol(op.str), transpile_exp(ctx, lhs), transpile_exp(ctx, rhs))
        @case ::Vector
            map(x->transpile_exp(ctx, x), stmt)
        @case ::Token
            transpile_token(stmt)
        @case _
            error("unknown expression: $stmt")
    end
end

function transpile_token(stmt)
    @switch stmt begin
        @case ::Token{:unnamed}
            Symbol(stmt.str)
        @case ::Token{:reserved}
            Symbol(stmt.str)
        @case ::Token{:id}
            stmt.str == "pi" && return Base.pi
            return Symbol(stmt.str)
        @case ::Token{:float64}
            return Base.parse(Float64, stmt.str)
        @case ::Token{:int}
            return Base.parse(Int, stmt.str)
        @case ::Token
            error("unknown token: $stmt")
    end
end
