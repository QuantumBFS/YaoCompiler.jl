@option struct JLEmulationTarget <: YaoCompileTarget
end

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
