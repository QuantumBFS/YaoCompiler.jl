# NOTE:
# this is almost just the simple version as QASM 2.0 toplevel
# we should think about how to merge these two

# TODO: improve this to actually validate:
# 1. reset
# 2. intrinsic gate set from given target
# 3. validate the code has no loop
function validate(::QobjQASMTarget, ci::CodeInfo)
    for (v, stmt) in enumerate(ci.code)
        @switch stmt begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), _, _, QuoteNode(locs::Locations), QuoteNode(ctrl::CtrlLocations))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), _, QuoteNode(locs::Locations))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), _, QuoteNode(locs::Locations))
            @case Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), ::SSAValue, ::Int)
            @case Expr(:invoke, _, GlobalRef(YaoCompiler, :measure_cmp), ::Int, ::SSAValue)
            @case Expr(:new, ::Type{<:NamedTuple}, _...)
            @case Expr(:new, ::Type{<:IntrinsicRoutine}, _...)
            @case ::ReturnNode
            @case ::GotoIfNot
            # return args
            @case Expr(:call, GlobalRef(&Core, :tuple), _...)
            @case _
                error("unsupported operation: $stmt")
        end
    end
end
