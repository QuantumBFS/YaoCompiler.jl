@option struct JLDummyTarget <: YaoCompileTarget
end

function target_specific_pipeline(::JLDummyTarget, ir::IRCode)
    mi = method_instances(println, (widenconst(ir.argtypes[2]), ))[1]
    for i in 1:length(ir.stmts)
        e = ir.stmts[i][:inst]
        @switch e begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), locs)
                ir.stmts[i][:inst] = QuoteNode(5)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), locs)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), Argument(2))
                ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), gate, locs)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), Argument(2))
                ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), gate, locs, ctrl)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), Argument(2))
                ir.stmts[i][:type] = Nothing
            @case _
                nothing
        end
    end
    return ir
end
