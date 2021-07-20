using Test
using YaoAPI
using MLStyle
using YaoLocations
using YaoCompiler
using GPUCompiler
using YaoCompiler.Intrinsics
using CompilerPluginTools
using YaoCompiler.Intrinsics: measure

@operation function test_basic(theta, phi)
    # syntax sugar
    1 => X
    @gate 2 => Z
    @ctrl 1 4 => Rx(theta)
    @ctrl 2 4 => Ry(phi)
    a = @measure 3
    # direct intrinsic
    apply(Y, 3)
    apply(X, 1, 4)
    c = measure(2)
    return (a = a, b = c)
end

@operation function test_pure_quantum()
    ret = @gate 1:4 => test_basic(1.0, 2.0)
    @ctrl 2 1 => Rx(2.2)
    return ret
end

ci, type = @yao_code_typed(test_pure_quantum())[1]

@operation function routine2(theta)
    2 => Rx(theta)
end

ci, type = @yao_code_typed(routine2(2.0))[1]

op = test_pure_quantum()
interp = YaoInterpreter()
ci, type = @yao_code_typed(op)[1]

@test_codeinfo ci begin
    Expr(
        :invoke,
        _,
        GlobalRef(Intrinsics, :apply),
        Argument(2),
        QuoteNode(X),
        QuoteNode(Locations(1)),
    )::Nothing
    Expr(
        :invoke,
        _,
        GlobalRef(Intrinsics, :apply),
        Argument(2),
        QuoteNode(Z),
        QuoteNode(Locations(2)),
    )::Nothing
    Expr(
        :invoke,
        _,
        GlobalRef(Intrinsics, :apply),
        Argument(2),
        QuoteNode(Rx(1.0)),
        QuoteNode(Locations(4)),
        QuoteNode(&(CtrlLocations(1))),
    )::Nothing
    Expr(
        :invoke,
        _,
        GlobalRef(Intrinsics, :apply),
        Argument(2),
        QuoteNode(Ry(2.0)),
        QuoteNode(Locations(4)),
        QuoteNode(&(CtrlLocations(2))),
    )::Nothing
    Expr(
        :invoke,
        _,
        GlobalRef(Intrinsics, :measure),
        Argument(2),
        QuoteNode(Locations(3)),
    )::MeasureResult{Int}
    Expr(
        :invoke,
        _,
        GlobalRef(Intrinsics, :apply),
        Argument(2),
        QuoteNode(Y),
        QuoteNode(Locations(3)),
    )::Nothing
end

struct JLDummyTarget <: YaoCompileTarget end

function test_pass()
    @test true
    return
end

function YaoCompiler.target_specific_pipeline(::JLDummyTarget, ir::IRCode)
    mi = method_instances(test_pass, ())[1]
    for i in 1:length(ir.stmts)
        e = ir.stmts[i][:inst]
        @switch e begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), reg, locs)
            ir.stmts[i][:inst] = QuoteNode(MeasureResult(5))
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), reg, locs)
            ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Main, :test_pass))
            ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, gate, locs)
            ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Main, :test_pass))
            ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, gate, locs, ctrl)
            ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Main, :test_pass))
            ir.stmts[i][:type] = Nothing
            @case _
            nothing
        end
    end
    return ir
end

f = YaoCompiler.compile(JLDummyTarget(), Intrinsics.apply, Tuple{AnyReg,typeof(op)})
test_ret = f(AnyReg(), op)
@test test_ret.a.result == 5
@test test_ret.b.result == 5
