using Test
using YaoCompiler
using YaoLocations
using YaoArrayRegister
using YaoCompiler.Intrinsics

struct JLEmulationTarget <: NativeJuliaTarget end

for G in [:X, :Y, :Z, :H, :S, :T]
    @eval function Intrinsics.apply(r::ArrayReg, ::Intrinsics.$(Symbol(G, :Gate)), locs::Locations)
        instruct!(r, Val($(QuoteNode(G))), Tuple(locs))
        return
    end

    @eval function Intrinsics.apply(r::ArrayReg, ::Intrinsics.$(Symbol(G, :Gate)), locs::Locations, ctrl::CtrlLocations)
        ctrl_locs = Tuple(ctrl.storage)
        ctrl_configs = ntuple(length(ctrl)) do k
            Int(ctrl.flags[k])
        end
        instruct!(r, Val($(QuoteNode(G))), Tuple(locs), ctrl_locs, ctrl_configs)
        return
    end
end

for axis in [:x, :y, :z]
    @eval function Intrinsics.apply(r::ArrayReg, op::Intrinsics.$(Symbol(:R, axis)), locs::Locations)
        instruct!(r, Val($(QuoteNode(Symbol(:R, axis)))), Tuple(locs), op.θ)
        return
    end

    @eval function Intrinsics.apply(r::ArrayReg, op::Intrinsics.$(Symbol(:R, axis)), locs::Locations, ctrl::CtrlLocations)
        ctrl_locs = Tuple(ctrl.storage)
        ctrl_configs = ntuple(length(ctrl)) do k
            Int(ctrl.flags[k])
        end
        instruct!(r, Val($(QuoteNode(Symbol(:R, axis)))), Tuple(locs), ctrl_locs, ctrl_configs, op.θ)
        return
    end
end

@noinline function Intrinsics.measure(r::ArrayReg, locs::Locations)
    result = YaoArrayRegister.measure!(r, Tuple(locs))
    return MeasureResult{typeof(result)}(result)
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
    3:6 => test_intrinsic(theta, 2.5)
    @ctrl 1 3:6 => test_intrinsic(theta, 2.1)
    return
end

r = rand_state(10)
op = test_location_map(1.0)
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
f(r1, op)
location_map(r2, 1.0)
@test r1 ≈ r2
