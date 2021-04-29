const qasm_compatible_intrinsics = Dict{Any, String}(
    Core.Intrinsics.add_float => "+",
    Core.Intrinsics.add_float_fast => "+",
    Core.Intrinsics.add_int => "+",
    Core.Intrinsics.sub_float => "-",
    Core.Intrinsics.sub_float_fast => "-",
    Core.Intrinsics.sub_int => "-",
    Core.Intrinsics.div_float => "/",
    Core.Intrinsics.div_float_fast => "/",
    Core.Intrinsics.mul_float => "*",
    Core.Intrinsics.mul_float_fast => "*",
    Core.Intrinsics.mul_int => "*",
    Core.Intrinsics.neg_float => "-",
    Core.Intrinsics.neg_float_fast => "-",
    Core.Intrinsics.neg_int => "-",
)

function validate(target::OpenQASMTarget, ci::CodeInfo)
    for (v, stmt) in enumerate(ci.code)
        @switch stmt begin
            # valid operations
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations), QuoteNode(ctrl::CtrlLocations))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs::Locations))
                target.toplevel || error("cannot have measure inside QASM gate statement")
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, QuoteNode(locs::Locations))
            @case Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), ::SSAValue, ::Int)
                target.toplevel || error("cannot have measure_cmp inside QASM gate statement")
            @case Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), ::Int, ::SSAValue)
                target.toplevel || error("cannot have measure_cmp inside QASM gate statement")
            @case Expr(:new, ::Type{<:NamedTuple}, _...)
            @case Expr(:new, ::Type{<:IntrinsicRoutine}, _...)
            @case Expr(:invoke, mi, &(QASM.sin), _)
            @case Expr(:invoke, mi, &(QASM.cos), _)
            @case Expr(:invoke, mi, &(QASM.tan), _)
            @case Expr(:invoke, mi, &(QASM.exp), _)
            @case Expr(:invoke, mi, &(QASM.log), _)
            @case Expr(:invoke, mi, &(QASM.sqrt), _)

            # if we are lucky they are not inlined, we can still convert them without overlay
            # TODO: remove this in 1.7+ using external method table
            @case Expr(:invoke, mi, GlobalRef(_, :sin), _)
                mi.def.module === Base.Math || error("invalid function: $(mi.def.module).sin")
            @case Expr(:invoke, mi, GlobalRef(_, :cos), _)
                mi.def.module === Base.Math || error("invalid function: $(mi.def.module).cos")
            @case Expr(:invoke, mi, GlobalRef(_, :tan), _)
                mi.def.module === Base.Math || error("invalid function: $(mi.def.module).tan")
            @case Expr(:invoke, mi, GlobalRef(_, :exp), _)
                mi.def.module === Base.Math || error("invalid function: $(mi.def.module).exp")
            @case Expr(:invoke, mi, GlobalRef(_, :log), _)
                mi.def.module === Base.Math || error("invalid function: $(mi.def.module).log")
            @case Expr(:invoke, mi, GlobalRef(_, :sqrt), _)
                mi.def.module === Base.Math || error("invalid function: $(mi.def.module).sqrt")
            @case ::ReturnNode
            @case ::GotoIfNot

            # gate statements
            @case Expr(:call, GlobalRef(&Base, :getfield), Argument(3), QuoteNode(:args))
                target.toplevel && error("cannot have parameters in toplevel QASM program")
            @case Expr(:call, GlobalRef(&Base, :getfield), ::SSAValue, ::Int, true)
                target.toplevel && error("cannot have parameters in toplevel QASM program")

            # intrinsic math function
            @case Expr(:call, GlobalRef(mod, name), _...)
                f = Core.Compiler.abstract_eval_global(mod, name)
                f === Any && error("cannot determine function: $mod.$name")
                f = f.val
                f isa Core.IntrinsicFunction && f in keys(qasm_compatible_intrinsics) ||
                    error("function $f is not compatible with QASM $(target.version)")

            # errors
            @case Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), ::SSAValue, ::SSAValue)
                error("can only compare measurement result with a constant integer in QASM $(target.version)")
            @case Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), _, _)
                error("if statement condition must be comparision between measurement results in QASM $(target.version)")
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :expect), _, QuoteNode(locs::Locations))
                error("intrinsic expect is not supported for QASM")
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, locs)
                error("location is not constant, got apply($locs)")
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, locs, ctrl)
                error("location is not constant, got apply($locs, $ctrl)")
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, locs)
                error("location is not constant, got measure($locs)")
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, locs)
                error("location is not constant, got barrier($locs)")

            @case _
                error("unsupported operation: $stmt")
        end
    end
end
