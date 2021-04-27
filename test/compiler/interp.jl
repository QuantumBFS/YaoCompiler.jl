using Test
using YaoLocations
using YaoCompiler
using GPUCompiler
using YaoCompiler.Intrinsics
using CompilerPluginTools
using YaoCompiler.Intrinsics: measure

@device function circuit(theta, phi)
    1 => X
    1 => Y # equivalent to @apply 1 => Y
    @apply 1 4 => Rx(theta)
    @apply 2 4 => Ry(phi)
    apply(X, 1, 4)
    c = measure(2)
    return (a = 1, b = c)
end

@device function main_circuit()
    ret = @apply 1:4 => circuit(1.0, 2.0)
    apply(Rx(2.0), 1, 2)
    return ret
end

@testset "nested routine const prop" begin
    op = main_circuit()
    interp = YaoInterpreter()
    ci, type = code_typed(Intrinsics.main, (typeof(op), ); interp)[1]
    println(ci)
    @test ci.code[1].head === :invoke
    @test ci.code[1].args[2] == GlobalRef(Intrinsics, :gate)
    @test ci.code[1].args[3] == QuoteNode(X)
    @test ci.code[1].args[4] == QuoteNode(Locations(1))

    @test ci.code[2].head === :invoke
    @test ci.code[2].args[2] == GlobalRef(Intrinsics, :ctrl)
    @test ci.code[2].args[3] == QuoteNode(Rx(1.0))
    @test ci.code[2].args[4] == QuoteNode(Locations(4))

    @test ci.code[3].head === :invoke
    @test ci.code[3].args[2] == GlobalRef(Intrinsics, :ctrl)
    @test ci.code[3].args[3] == QuoteNode(Ry(2.0))
    @test ci.code[3].args[4] == QuoteNode(Locations(4))
    @test ci.code[3].args[5] == QuoteNode(CtrlLocations(2))

    @test ci.code[4].head === :invoke
    @test ci.code[4].args[2] == GlobalRef(Intrinsics, :ctrl)
    @test ci.code[4].args[3] == QuoteNode(X)
    @test ci.code[4].args[4] == QuoteNode(Locations(1))
    @test ci.code[4].args[5] == QuoteNode(CtrlLocations(4))
    @test ci.code[5] == ReturnNode(2)
end

# function code_cache(mxi::MixtapeInterpreter)
#     return WorldView(get_cache(typeof(mxi.inner)), get_world_counter(mxi))
# end

# Core.Compiler.InferenceState

YaoCompiler.GLOBAL_CI_CACHE[YaoCompiler.JLDummyTarget()] = YaoCompiler.GPUCompiler.CodeCache()
interp = YaoInterpreter()
op = main_circuit()

op = circuit(1.0, 2.0)

ci = @code_lowered Intrinsics.main(op)
ci, type = code_typed(Intrinsics.apply, (DummyReg, typeof(op)); interp)[1]
code_ircode(Intrinsics.main, (typeof(op), ); interp)[1]
mi = method_instances(Intrinsics.main, (typeof(op), ))[1]

fspec = FunctionSpec(Intrinsics.main, Tuple{typeof(op)}, false, nothing) #=name=#
job = CompilerJob(YaoCompiler.JLDummyTarget(), fspec, YaoCompiler.YaoCompileParams())
llvm_specfunc, llvm_func, llvm_mod = YaoCompiler.compile_method_instance(job, mi)

using YaoAPI
struct DummyReg <: AbstractRegister{1}
end

using YaoArrayRegister
r = rand_state(5)
op = main_circuit()
f = YaoCompiler.compile(YaoCompiler.JLEmulationTarget(), Intrinsics.apply, Tuple{typeof(r), typeof(op)})
f(r, op)

@device function circuit(theta, phi)
    1 => X
    1 => Y # equivalent to @apply 1 => Y
    @apply 1 4 => Rx(theta)
    @apply 2 4 => Ry(phi)
    apply(X, 1, 4)
    return 2 + im
end

@device function main_circuit()
    ret = @apply 1:4 => circuit(1.0, 2.0)
    apply(Rx(2.0), 1, 2)
    return ret
end

function test_circuit(r, theta, phi)
    instruct!(r, Val(:X), (1, ))
    instruct!(r, Val(:Y), (1, ))
    instruct!(r, Val(:Rx), (4, ), (1, ), (1, ), theta)
    instruct!(r, Val(:Ry), (4, ), (2, ), (1, ), phi)
    instruct!(r, Val(:X), (1, ), (4, ), (1, ))
    instruct!(r, Val(:Rx), (1, ), (2, ), (1, ), 2.0)
end

r = rand_state(5)
r1 = copy(r)
r2 = copy(r)

test_circuit(r1, 1.0, 2.0)

f(r2)
r1 ≈ r2


r = rand_state(5)
op = main_circuit()
f = YaoCompiler.compile(YaoCompiler.OpenQASM2Target(), Intrinsics.apply, Tuple{typeof(r), typeof(op)})
