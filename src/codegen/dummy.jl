@option struct JLDummyTarget <: YaoCompileTarget
end

function target_specific_optimization(::JLDummyTarget, ir::IRCode)
    mi = method_instances(println, (String, ))[1]
    for i in 1:length(ir.stmts)
        e = ir.stmts[i][:inst]
        @switch e begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), args...)
                ir.stmts[i][:inst] = QuoteNode(5)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), args...)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), "Intrinsics.barrier")
                ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :gate), args...)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), "Intrinsics.gate")
                ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :ctrl), args...)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), "Intrinsics.ctrl")
                ir.stmts[i][:type] = Nothing
            @case _
                nothing
        end
    end
    return ir
end
