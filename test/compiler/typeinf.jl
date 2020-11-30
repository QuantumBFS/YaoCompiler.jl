using YaoCompiler
using YaoCompiler.Intrinsics

qasm"""OPENQASM 2.0; 
include "qelib1.inc";

gate post q {x q;}
"""

@device function circuit()
    1:3 => ccx()
    # (2, 3) => cx()
    # 2 => h()
    # c = @measure 1
    # if c == 1
    #     3 => post()
    # end
    # 3 => rx(1.0)
    # return (c=c, )
end

ci, = @code_yao optimize = true circuit()
# ast = @code_qasm optimize = true passes = :julia gate = true cu3(0.1, 0.2, 0.3)
# ast = @code_qasm optimize = true circuit()

spec = circuit()
args = ()
f = YaoCompiler.Semantic.main
args_t = Base.typesof(spec, args...)
atypes = Base.typesof(f, spec, args...)
matches = methods(f, args_t)
length(matches) == 1 || error("call is ambiguous")
method = first(matches)
mi = Core.Compiler.specialize_method(method, atypes, Core.svec())


result = Core.Compiler.InferenceResult(mi, Any[Core.Const(f), Core.Const(spec), Core.Const.(args)...])
world = Core.Compiler.get_world_counter()
# interp = Core.Compiler.NativeInterpreter(inf_params=Core.Compiler.InferenceParams(aggressive_constant_propagation=true))

interp = YaoCompiler.YaoInterpreter(
    Core.Compiler.NativeInterpreter(
        inf_params = Core.Compiler.InferenceParams(aggressive_constant_propagation = true),
    ),
    Symbol[],
)
# interp = YaoCompiler.YaoInterpreter(;passes=Symbol[])
frame = Core.Compiler.InferenceState(result, false, interp) #=cached=#
Core.Compiler.typeinf(interp, frame)
frame.src

opt = Core.Compiler.OptimizationState(frame, Core.Compiler.OptimizationParams(interp), interp)
def = opt.linfo.def
nargs = Int(opt.nargs) - 1
ci = opt.src
sv = opt
preserve_coverage = Core.Compiler.coverage_enabled(sv.mod)
ir = Core.Compiler.convert_to_ircode(
    ci,
    Core.Compiler.copy_exprargs(ci.code),
    preserve_coverage,
    nargs,
    sv,
)
ir = Core.Compiler.slot2reg(ir, ci, nargs, sv)
ir = Core.Compiler.compact!(ir)
ir = Core.Compiler.compact!(ir)

ir = Core.Compiler.ssa_inlining_pass!(ir, ir.linetable, sv.inlining, ci.propagate_inbounds)

ir = Core.Compiler.compact!(ir)
ir = Core.Compiler.getfield_elim_pass!(ir)
ir = Core.Compiler.adce_pass!(ir)
ir = Core.Compiler.type_lift_pass!(ir)
ir = Core.Compiler.compact!(ir)

YaoCompiler.inline_const!(ir)
YaoCompiler.elim_map_check!(ir)
YaoCompiler.compact!(ir)
