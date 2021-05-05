macro yao_code_lowered(ex)
    esc(yao_code_lowered_m(ex))
end

macro yao_code_lowered(ctrl, ex)
    esc(yao_code_lowered_m(ctrl, ex))
end

macro yao_code_typed(ex)
    esc(yao_code_typed_m(ex))
end

macro yao_code_typed(ctrl, ex)
    esc(yao_code_typed_m(ctrl, ex))
end

function yao_code_lowered_m(ex)
    @match ex begin
        # apply(reg, gate, locs)
        :($locs => $gate) => quote
            Base.code_lowered(
                $Intrinsics.apply,
                ($YaoCompiler.AnyReg, typeof($gate), typeof($Locations($(locs))))
            )
        end
        # apply(reg, gate)
        _ => quote
            Base.code_lowered($Intrinsics.apply, ($YaoCompiler.AnyReg, typeof($ex)))
        end
    end
end

function yao_code_lowered_m(ctrl, ex)
    @match ex begin
        # apply(reg, gate, locs, ctrl)
        :($locs => $gate) => quote
            Base.code_lowered(
                $Intrinsics.apply,
                ($YaoCompiler.AnyReg, typeof($gate), typeof($Locations($(locs))), typeof($CtrlLocations($ctrl)))
            )
        end
        _ => error("expect a locs => gate expression")
    end
end

function yao_code_typed_m(ex)
    @match ex begin
        # apply(reg, gate, locs)
        :($locs => $gate) => quote
            $Base.code_typed(
                $Intrinsics.apply,
                ($YaoCompiler.AnyReg, typeof($gate), typeof($Locations($(locs))));
                interp=$(YaoInterpreter())
            )
        end
        # apply(reg, gate)
        _ => quote
            $Base.code_typed(
                $Intrinsics.apply, ($YaoCompiler.AnyReg, typeof($ex));
                interp=$(YaoInterpreter()),
            )
        end
    end
end

function yao_code_typed_m(ctrl, ex)
    @match ex begin
        # apply(reg, gate, locs, ctrl)
        :($locs => $gate) => quote
            Base.code_typed(
                $Intrinsics.apply,
                ($YaoCompiler.AnyReg, typeof($gate), typeof($Locations($(locs))), typeof($CtrlLocations($ctrl)));
                interp=$(YaoInterpreter()),
            )
        end
        _ => error("expect a locs => gate expression")
    end
end
