module TestParse

using Test
using OpenQASM.Types
using YaoCompiler
using YaoCompiler.QASM
using RBNF: Token

qasm"""OPENQASM 2.0; 
include "qelib1.inc";
"""

@testset "qasm gate codegen" begin
    ast = @code_qasm gate = true cu3(0.1, 0.2, 0.3)
    @test ast isa Gate
    @test length(ast.decl.cargs) == 3
    @test length(ast.decl.qargs) == 2
    @test length(ast.body) == 5
    @test ast.decl.cargs[1].str == "theta"
    @test ast.decl.cargs[2].str == "phi"
    @test ast.decl.cargs[3].str == "lambda"

    inst = ast.body[1]
    @test inst isa Instruction
    @test inst.name == "u1"
    @test length(inst.cargs) == 1
    @test length(inst.qargs) == 1
    @test length(inst.cargs[1]) == 3
    @test length(inst.cargs[1][1]) == 3
    carg = inst.cargs[1][1]
    @test carg[1].str == "lambda"
    @test carg[2].str == "-"
    @test carg[3].str == "phi"
    @test inst.cargs[1][2].str == "/"
    @test inst.cargs[1][3].str == "2"
end

@testset "@code_qasm optimize=true passes=:julia gate=true" begin
    ast = @code_qasm optimize = true passes = :julia gate = true cu3(0.1, 0.2, 0.3)
    @test ast.body[1] isa Instruction
    @test ast.body[1].name == "rz"
    @test ast.body[1].cargs[1] isa Token{:int}
    @test ast.body[1].cargs[1].str == "0"

    @test ast.body[2] isa Instruction
    @test ast.body[2].name == "ry"
    @test ast.body[2].cargs[1] isa Token{:int}
    @test ast.body[2].cargs[1].str == "0"

    @test ast.body[3] isa Instruction
    @test ast.body[3].name == "rz"
    @test ast.body[3].cargs[1] isa Tuple
    @test ast.body[3].cargs[1][1][1].str == "lambda"
    @test ast.body[3].cargs[1][1][2].str == "-"
    @test ast.body[3].cargs[1][1][3].str == "phi"
    @test ast.body[3].cargs[1][2].str == "/"
    @test ast.body[3].cargs[1][3].str == "2.0"

    @test ast.body[4] isa Instruction
    @test ast.body[4].name == "CX"
    @test isempty(ast.body[4].cargs)
    @test length(ast.body[4].qargs) == 2

    @test ast.body[5] isa Instruction
    @test ast.body[5].name == "rz"
    @test ast.body[5].cargs[1].fn == :(-)
    @test ast.body[5].cargs[1].arg isa Tuple
    @test ast.body[5].cargs[1].arg[1].str == "theta"
    @test ast.body[5].cargs[1].arg[2].str == "/"
    @test ast.body[5].cargs[1].arg[3].str == "2.0"

    @test ast.body[6] isa Instruction
    @test ast.body[6].name == "ry"
    @test ast.body[6].cargs[1].str == "0"
end

end # TestParse


using YaoCompiler
using YaoCompiler.Intrinsics

@device function qobj_test()
    1 => X
    2 => X
    @ctrl (1, 2) 3 => X
    @ctrl 1 2 => X
    c = @measure 1:3
    return c
end


ci, = @code_yao optimize = true qobj_test()
target = YaoCompiler.TargetQobjQASM()
qobj = YaoCompiler.codegen(target, ci)

YaoCompiler.measure_ssa_uses!(Set{Int64}(), ci)
using InteractiveUtils
clipboard(string(qobj))

qobj["header"]
qobj["config"]
qobj["instructions"]

qasm"""OPENQASM 2.0; 
include "qelib1.inc";
"""

ci, = @code_yao optimize = true circuit()
ast = @code_qasm optimize = true passes = :julia gate = true cu3(0.1, 0.2, 0.3)
ast = @code_qasm optimize = true circuit()

@device function u1(theta)
    1 => Rz(theta)
end

@device function cu1(lambda)
    2 => u1(lambda / 2)
    @ctrl 1 2 => X
    1 => u1(-lambda / 2)
    @ctrl 1 2 => X
    1 => u1(lambda / 2)
    return
end

# this is broken
@device function circuit()
    1 => X
    (3, 5) => cu1(0.1)
end

target = YaoCompiler.TargetQobjQASM()
ci, = @code_yao optimize = true circuit()

YaoCompiler.codegen(target, ci)

@code_qasm optimize = true circuit()

ci, = @code_yao optimize = true circuit()

function foo()
    x = Base.div_float(1.0, 2.0)
    y = Base.div_float(x, 3.2)
    return y
end


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
YaoCompiler.elim_mapcheck!(ir)
YaoCompiler.compact!(ir)
idx = 1
todo = Pair{Int,Any}[]
stmt = ir.stmts[1][:inst]
sig = Core.Compiler.call_sig(ir, stmt)
calltype = ir.stmts[idx][:type]


ir = YaoCompiler.group_quantum_stmts!(ir)
ir = YaoCompiler.propagate_consts_bb!(ir)
ir = YaoCompiler.compact!(ir)

ir = YaoCompiler.elim_location_mapping!(ir)


ir = run_passes(opt.src, nargs, opt, params.passes)

YaoCompiler.optimize(opt, YaoOptimizationParams(interp), result.result)
opt.src.inferred = true


ast = @code_qasm gate = true cu3(0.1, 0.2, 0.3)
ci, = @code_yao optimize = true cu3(0.1, 0.2, 0.3)

spec = cu3(0.1, 0.2, 0.3)

@code_yao gate(spec, Locations(1:4))
@code_yao optimize = true circuit()

target = YaoCompiler.TargetQobjQASM()
YaoCompiler.codegen(target, ci)
