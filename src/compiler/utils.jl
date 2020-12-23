"""
    rm_annotations(x)

Remove type annotation of given expression.
"""
function rm_annotations(x)
    x isa Expr || return x
    if x.head == :(::)
        if length(x.args) == 1 # anonymous
            return
        else
            return x.args[1]
        end
    elseif x.head in [:(=), :kw] # default values
        return rm_annotations(x.args[1])
    else
        return x
    end
end

function annotations(x)
    x isa Expr || return x
    if x.head == :(::)
        x.args[end]
    elseif x.head in [:(=), :kw]
        return annotations(x.args[1])
    else
        return x
    end
end

function splatting_variables(variables, free)
    Expr(:(=), Expr(:tuple, variables...), free)
end

export gate_count

function gate_count(spec)
    ci, _ = code_yao(Semantic.main, spec; optimize = true)
    return gate_count(ci)
end

function gate_count(ci::CodeInfo)
    count = Dict{Symbol,Any}()
    for stmt in ci.code
        is_quantum_statement(stmt) || continue
        qt = quantum_stmt_type(stmt)

        if qt === :gate || qt === :ctrl
            gc = get!(count, qt, IdDict{Any,Int}())
            if stmt.head === :invoke
                gate, gt = obtain_gate_stmt(stmt.args[3], ci)
            elseif stmt.head === :call
                gate, gt = obtain_gate_stmt(stmt.args[2], ci)
            end

            if gate isa IntrinsicRoutine && isempty(gate.variables)
                gc[gate] = get(gc, gate, 0) + 1
            else
                gc[parent(gate)] = get(gc, parent(gate), 0) + 1
            end
        else
            count[qt] = get(count, qt, 0) + 1
        end
    end
    return count
end

get_type(x::SSAValue, ci::CodeInfo) = widenconst(ci.ssavaluetypes[x.id])
get_type(x::SSAValue, ir::IRCode) = widenconst(ir.stmts.type[x.id])
get_type(x::SSAValue, ir::YaoIR) = get_type(x, ir.ir)

eachstmt(ci::CodeInfo) = enumerate(ci.code)
eachstmt(ir::IRCode) = enumerate(ir.stmts.inst)
eachstmt(ir::YaoIR) = eachstmt(ir.ir)

function count_qubits(ir)
    min_loc, max_loc = 0, 0
    for (v, e) in eachstmt(ir)
        minmax = find_minmax_locations(e, ir)

        # non-constant location
        if minmax === false
            return
        end

        if !isnothing(minmax)
            min_loc = min(minmax[1], min_loc)
            max_loc = max(minmax[2], max_loc)
        end
    end
    return max_loc - min_loc
end

function find_minmax_locations(@nospecialize(e), ir)
    if e isa Locations
        return minimum(e.storage), maximum(e.storage)
    elseif e isa Expr
        min_loc, max_loc = 0, 0
        for each in e.args
            minmax = find_minmax_locations(each, ir)
            if isnothing(minmax)
                continue
            elseif minmax === false
                return false
            else
                min_loc = min(minmax[1], min_loc)
                max_loc = max(minmax[2], max_loc)
            end
        end
        return min_loc, max_loc
        @show println(ir)
    elseif e isa SSAValue && get_type(e, ir) <: AbstractLocations
        return false
    else
        return
    end
end
