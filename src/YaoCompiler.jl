module YaoCompiler

export @device,
    @gate,
    @ctrl,
    @measure,
    @barrier,
    compile,
    YaoInterpreter,
    YaoCompileTarget,
    JLGenericTarget,
    NativeJuliaTarget,
    MeasureResult,
    TargetHostKernel,
    HardwareFreeOptions,
    Routine,
    GenericRoutine,
    IntrinsicRoutine,
    Operation,
    AdjointOperation,
    routine_name,
    IntrinsicError,
    AnyReg,
    # reexport YaoLocations
    Locations,
    CtrlLocations,
    # reflection
    @yao_code_lowered,
    @yao_code_typed

using MLStyle
using YaoAPI
using LLVM
using Expronicon
using YaoHIR
using YaoLocations
using TimerOutputs
using LinearAlgebra
using GPUCompiler
using ZXCalculus
using Configurations
using CompilerPluginTools
using LLVM.Interop
using YaoHIR: routine_name, measure_cmp, MeasureResult
using GPUCompiler: CodeCache, CompilerJob, AbstractCompilerTarget, AbstractCompilerParams, WorldView
using YaoLocations: map_check, map_check_nothrow, map_error, plain, unsafe_mapping
using CompilerPluginTools: Argument
using Base.Meta: ParseError

const to = TimerOutput()
timings() = (TimerOutputs.print_timer(to); println())
enable_timings() = (TimerOutputs.enable_debug_timings(Compiler); return)

@as_record Locations
@as_record CtrlLocations

"""
    AnyReg <: AbstractRegister{1}

A place holder for registers when compilation is not register specific.
"""
struct AnyReg <: AbstractRegister{1} end
Base.show(io::IO, ::AnyReg) = print(io, "AnyReg()")

include("compiler/intrinsics.jl")
include("compiler/syntax.jl")
include("compiler/interp.jl")
include("compiler/reflection.jl")

include("codegen/llvmopt.jl")
include("codegen/native.jl")

# We have one global JIT and TM
const orc = Ref{LLVM.OrcJIT}()
const tm = Ref{LLVM.TargetMachine}()

function __init__()
    TimerOutputs.reset_timer!(to)
    opt_level = Base.JLOptions().opt_level
    if opt_level < 2
        optlevel = LLVM.API.LLVMCodeGenLevelNone
    elseif opt_level == 2
        optlevel = LLVM.API.LLVMCodeGenLevelDefault
    else
        optlevel = LLVM.API.LLVMCodeGenLevelAggressive
    end

    tm[] = LLVM.JITTargetMachine(; optlevel = optlevel)
    LLVM.asm_verbosity!(tm[], true)

    if LLVM.has_orc_v1()
        orc[] = LLVM.OrcJIT(tm[]) # takes ownership of tm
    end

    atexit() do
        return LLVM.dispose(orc[])
    end
end

end # module
