using YaoCompiler
using YaoCompiler.Intrinsics
target = YaoTargetQASM.OpenQASMTarget()
interp = YaoInterpreter(;target)
code_typed(Intrinsics.apply, Tuple{AnyReg, typeof(Adder3.adder3())}; interp)
ast = code_qasm(typeof(Adder3.adder3()))

Adder3.t()

qelib = joinpath(pkgdir(YaoTargetQASM), "test", "qelib1.inc")
@macroexpand qasm"""
OPENQASM 2.0;
include "$qelib";
gate majority a,b,c 
{ 
  cx c,b; 
  cx c,a; 
  ccx a,b,c; 
}
"""

using OpenQASM
using YaoTargetQASM
using YaoCompiler
using YaoCompiler.Intrinsics
using CompilerPluginTools

qasm"""
OPENQASM 2.0;
gate cx c,t { CX c,t; }
gate u1(lambda) q { U(0,0,lambda) q; }
gate t a { u1(pi/4) a; }
gate h a { u2(0,pi) a; }
gate tdg a { u1(-pi/4) a; }

gate ccx a,b,c
{
  h c;
  cx b,c; tdg c;
  cx a,c; t c;
  cx b,c; tdg c;
  cx a,c; t b; t c; h c;
  cx a,b; t a; tdg b;
  cx a,b;
}

gate majority a,b,c 
{ 
  cx c,b; 
  cx c,a; 
  ccx a,b,c; 
}
"""



ast = code_qasm(typeof(majority()))

@operation function test_inline()
    1:3 => ccx()
end

ast = code_qasm(typeof(test_inline()))
code_qasm(typeof(ccx()))

code_typed(Intrinsics.apply, (AnyReg, typeof(majority())); interp=YaoInterpreter())

interp = YaoInterpreter()
mi = method_instance(Intrinsics.apply, (AnyReg, typeof(test_inline())))
result = Core.Compiler.InferenceResult(mi)
frame = Core.Compiler.InferenceState(result, false, interp)
Core.Compiler.typeinf(interp, frame)

opt_params = Core.Compiler.OptimizationParams(interp)
opt = Core.Compiler.OptimizationState(frame, opt_params, interp)
preserve_coverage = Core.Compiler.coverage_enabled(opt.mod)
ci = opt.src; nargs = opt.nargs - 1;
ir = Core.Compiler.convert_to_ircode(ci, Core.Compiler.copy_exprargs(ci.code), preserve_coverage, nargs, opt)
ir = Core.Compiler.slot2reg(ir, ci, nargs, opt)
ir = compact!(ir)
sv = opt
todo = Core.Compiler.assemble_inline_todo!(ir, sv.inlining)
state = sv.inlining
todo = Core.Compiler.Pair{Int, Any}[]
et = state.et
method_table = state.method_table
ir
r = Core.Compiler.process_simple!(ir, todo, 6, state)
ir = Core.Compiler.ssa_inlining_pass!(ir, ir.linetable, sv.inlining, sv.src.propagate_inbounds)

# ir = compact!(ir)
# ir = Core.Compiler.getfield_elim_pass!(ir)
# ir = Core.Compiler.adce_pass!(ir)
# ir = Core.Compiler.type_lift_pass!(ir)
# ir = compact!(ir)
# ir = inline_const!(ir)
# ir = YaoCompiler.elim_location_mapping!(ir)

# code_typed(Intrinsics.apply, (AnyReg, typeof(ccx()), typeof(Locations(1:3))); interp)
# op = tdg()
# todo = assemble_inline_todo!(ir, state)
# ir