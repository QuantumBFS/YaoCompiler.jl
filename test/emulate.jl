using Test
using YaoCompiler
using YaoLocations
using YaoArrayRegister
using YaoCompiler.Intrinsics

struct JLEmulationTarget <: YaoCompileTarget end

for G in [:X, :Y, :Z, :H, :S, :T]
    @eval function Intrinsics.apply(r::ArrayReg, ::Intrinsics.$(Symbol(G, :Gate)), locs::Locations)
        println(locs)
        # instruct!(r, Val($(QuoteNode(G))), Tuple(locs))
        return
    end

    @eval function Intrinsics.apply(r::ArrayReg, ::Intrinsics.$(Symbol(G, :Gate)), locs::Locations, ctrl::CtrlLocations)
        ctrl_locs = Tuple(ctrl.storage)
        ctrl_configs = ntuple(length(ctrl)) do k
            Int(ctrl.flags[k])
        end
        println(ctrl_locs, ctrl_configs)
        # instruct!(r, Val($(QuoteNode(G))), Tuple(locs), ctrl_locs, ctrl_configs)
        return
    end
end

for axis in [:x, :y, :z]
    @eval function Intrinsics.apply(r::ArrayReg, op::Intrinsics.$(Symbol(:R, axis)), locs::Locations)
        println(locs, op.θ)
        # instruct!(r, Val($(QuoteNode(Symbol(:R, axis)))), Tuple(locs), op.θ)
        return
    end

    @eval function Intrinsics.apply(r::ArrayReg, op::Intrinsics.$(Symbol(:R, axis)), locs::Locations, ctrl::CtrlLocations)
        ctrl_locs = Tuple(ctrl.storage)
        ctrl_configs = ntuple(length(ctrl)) do k
            Int(ctrl.flags[k])
        end
        println(ctrl, op.θ)
        # instruct!(r, Val($(QuoteNode(Symbol(:R, axis)))), Tuple(locs), ctrl_locs, ctrl_configs, op.θ)
        return
    end
end

using MLStyle
using CompilerPluginTools
struct JLDummyTarget <: YaoCompileTarget end

function YaoCompiler.target_specific_pipeline(::JLDummyTarget, ir::IRCode)
    mi = method_instances(println, (widenconst(ir.argtypes[2]), ))[1]
    for i in 1:length(ir.stmts)
        e = ir.stmts[i][:inst]
        @switch e begin
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :measure), reg, locs)
                ir.stmts[i][:inst] = QuoteNode(5)
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :barrier), reg, locs)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), Argument(2))
                ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, gate, locs)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), Argument(2))
                ir.stmts[i][:type] = Nothing
            @case Expr(:invoke, _, GlobalRef(Intrinsics, :apply), reg, gate, locs, ctrl)
                ir.stmts[i][:inst] = Expr(:invoke, mi, GlobalRef(Base, :println), Argument(2))
                ir.stmts[i][:type] = Nothing
            @case _
                nothing
        end
    end
    return ir
end

@device function test_intrinsic(theta, phi)
    # syntax sugar
    1 => X
    @gate 2 => Z
    @ctrl 1 4 => Rx(theta)
    @ctrl 2 4 => Ry(phi)
    return
end

@device function test_location_map(theta)
    # 3:6 => test_intrinsic(theta, 2.5)
    @ctrl 1 3:6 => test_intrinsic(theta, 2.1)
    return
end

r = rand_state(10)
op = test_location_map(1.0)
ci, _ = code_typed(Intrinsics.apply, (typeof(r), typeof(op)); interp=YaoInterpreter(;target=JLEmulationTarget()))[1]

f = YaoCompiler.compile(JLEmulationTarget(), Intrinsics.apply, Tuple{typeof(r), typeof(op)})
f(r)

ci, _ = code_typed(Intrinsics.apply, (typeof(r), typeof(op)); interp=YaoInterpreter())[1]
ci = code_lowered(Intrinsics.apply, (typeof(r), typeof(op)))[1]

using CompilerPluginTools
ir, _ = code_ircode(Intrinsics.apply, (typeof(r), typeof(op)); interp=YaoInterpreter(;target=JLEmulationTarget()))[1]
ir = inline_const!(ir)
ir = const_invoke!(YaoLocations.map_check_nothrow, ir, GlobalRef(YaoLocations, :map_check))
ir = compact!(ir, true) # Simplify CFG
    # group quantum statements so we can work on
    # larger quantum circuits before we start optimizations
ir = Core.Compiler.cfg_simplify!(ir)
ir = YaoCompiler.group_quantum_stmts!(ir)
ir = compact!(ir)


f = YaoCompiler.compile(JLEmulationTarget(), Intrinsics.apply, Tuple{typeof(r), typeof(op)})

function location_map(r, theta)
    instruct!(r, Val(:X), (3, ))
    instruct!(r, Val(:Z), (4, ))
    instruct!(r, Val(:Rx), (6, ), (3, ), (1, ), theta)
    instruct!(r, Val(:Ry), (6, ), (4, ), (1, ), 2.5)

    instruct!(r, Val(:X), (3, ), (1, ), (1, ))
    instruct!(r, Val(:Z), (4, ), (1, ), (1, ))
    instruct!(r, Val(:Rx), (6, ), (1, 3), (1, 1), theta)
    instruct!(r, Val(:Ry), (6, ), (1, 4), (1, 1), 2.1)
    return
end

r1, r2 = copy(r), copy(r)
f(r1)
location_map(r2, 1.0)
