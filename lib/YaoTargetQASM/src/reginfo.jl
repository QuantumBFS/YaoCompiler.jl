struct RegInfo
    creg_ssa_map::Dict{Symbol, Int}
    ssa_creg_map::Dict{Int, Symbol}
    creg_size::Dict{Symbol, Int}
    qreg_to_locs::Dict{Int, Vector{Int}}
    locs_to_qreg::Dict{Int, Int}
    locs_to_addr::Dict{Int, Int}
end

RegInfo(ci::CodeInfo) = RegInfo(OpenQASMTarget(), ci)

function RegInfo(target::OpenQASMTarget, ci::CodeInfo)
    creg_ssa_map, ssa_creg_map = extract_creg_map(ci)
    creg_size = extract_creg_size(ci, creg_ssa_map)
    locs_to_qreg = extract_qreg(target, ci)
    qreg_to_locs, locs_to_qreg, locs_to_addr = extract_locs_address(locs_to_qreg)
    RegInfo(creg_ssa_map, ssa_creg_map, creg_size, qreg_to_locs, locs_to_qreg, locs_to_addr)
end

function Base.getindex(ri::RegInfo, loc::Locations{Int})
    plain_loc = plain(loc)
    qreg = ri.locs_to_qreg[plain_loc]
    addr = ri.locs_to_addr[plain_loc]
    if length(ri.qreg_to_locs[qreg]) == 1 && addr == 0
        return Bit(qreg_name(qreg))
    else
        return Bit(qreg_name(qreg), addr)
    end
end

function Base.getindex(ri::RegInfo, loc::CtrlLocations)
    length(loc) == 1 || error("multiple control is not supported by QASM")
    loc.flags[1] || error("inverse control is not supported by QASM")
    return ri[loc.storage]
end

function Base.show(io::IO, ::MIME"text/plain", info::RegInfo)
    println(io, "RegInfo")
    tab = "  "
    isempty(info.creg_size) || printstyled(io, "creg:\n"; color=:light_cyan)
    for (name, size) in info.creg_size
        printstyled(io, tab, name; color=:green)
        printstyled(io, "[", size, "]"; color=:light_blue)
        println(io)
    end

    isempty(info.qreg_to_locs) || printstyled(io, "qreg:\n"; color=:light_cyan)
    for (k, (qreg, locs)) in enumerate(info.qreg_to_locs)
        printstyled(io, tab, "%", qreg; color=:light_magenta)
        printstyled(io, "[", length(locs), "]"; color=:light_blue)
        print(io, " => ", locs)
        if k != length(info.qreg_to_locs)
            println(io)
        end
    end
end

function extract_creg_map(ci::CodeInfo)
    creg_ssa_map = Dict{Symbol, Int}()
    ssa_creg_map = Dict{Int, Symbol}()

    # find alias name for return NamedTuple
    for (v, stmt) in enumerate(ci.code)
        ret = @match stmt begin
            ReturnNode(SSAValue(id)) => ci.code[id]
            _ => nothing
        end

        isnothing(ret) && continue

        @switch ret begin
            @case Expr(:new, type::Type{<:NamedTuple}, args...)
                cregs = type.parameters[1]
                ctypes = type.parameters[2]
                ctypes <: NTuple{N, <:MeasureResult} where N || continue
                for (name, val) in zip(cregs, args)
                    val::SSAValue
                    creg_ssa_map[name] = val.id
                    ssa_creg_map[val.id] = name
                end
            @case _
                nothing
        end
    end
    return creg_ssa_map, ssa_creg_map
end

function extract_creg_size(ci::CodeInfo, creg_ssa_map::Dict{Symbol, Int})
    creg_size = Dict{Symbol, Int}()
    for (key, id) in creg_ssa_map
        stmt = ci.code[id]
        @switch stmt begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs::Locations))
                creg_size[key] = length(locs)
            @case _
                error("expect measurement statement, got $stmt")
        end
    end
    return creg_size
end

function extract_locs_address(locs_to_qreg::Dict{Int, Int})
    qreg_to_locs = Dict{Int, Vector{Int}}()
    for (k, r) in locs_to_qreg
        locs = get!(qreg_to_locs, r, Int[])
        push!(locs, k)
    end
    
    # loc => qreg, addr
    locs_to_qreg = Dict{Int, Int}()
    locs_to_addr = Dict{Int, Int}()

    for (r, locs) in qreg_to_locs
        sort!(locs)
        for (k, loc) in enumerate(locs)
            locs_to_qreg[loc] = r
            # QASM starts from 0
            locs_to_addr[loc] = k - 1
        end
    end
    return qreg_to_locs, locs_to_qreg, locs_to_addr
end

function extract_qreg(target::OpenQASMTarget, ci::CodeInfo)
    locs_to_qreg = Dict{Int, Int}()

    for (v, stmt) in enumerate(ci.code)
        @switch stmt begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations))
                record_locations!(locs_to_qreg, target, locs)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations), QuoteNode(ctrl::CtrlLocations))
                record_locations!(locs_to_qreg, target, locs)
                record_locations!(locs_to_qreg, target, ctrl.storage)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs::Locations))
                allocate_new_qreg!(locs_to_qreg, locs)
                record_locations!(locs_to_qreg, target, locs)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, QuoteNode(locs::Locations))
                record_locations!(locs_to_qreg, target, locs)
            # skip other statement
            @case _
                nothing
        end
    end
    return locs_to_qreg
end

function record_locations!(locs_to_qreg::Dict{Int, Int}, target::OpenQASMTarget, locs::Locations)
    for loc in locs
        plain_loc = plain(loc)
        if target.toplevel
            get!(locs_to_qreg, plain_loc, 1)
        else
            # force allocate new register for qasm gate
            if !haskey(locs_to_qreg, plain_loc)
                locs_to_qreg[plain_loc] = length(keys(locs_to_qreg)) + 1
            end
        end
    end
    return locs_to_qreg
end

function allocate_new_qreg!(locs_to_qreg::Dict{Int, Int}, locs::Locations)
    max_qreg_addr = maximum(locs) do loc
        # we will add 1 later
        get(locs_to_qreg, plain(loc), 0)
    end

    for loc in locs
        locs_to_qreg[plain(loc)] = max_qreg_addr + 1
    end
    return locs_to_qreg
end
