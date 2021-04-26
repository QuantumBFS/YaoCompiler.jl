module YaoCompiler

# export Routine,
#     GenericRoutine,
#     IntrinsicRoutine,
#     RoutineSpec,
#     IntrinsicSpec,
#     @ctrl,
#     @measure,
#     @gate,
#     @barrier,
#     @device
# export routine_name


export @device, @apply, @gate, @ctrl, @measure, @barrier

export YaoInterpreter
# # reflections
# export @code_yao, @code_qasm
# export gate_count
# export Intrinsics

using MLStyle
using YaoAPI
using LLVM
using BitBasis
using Expronicon
using ZXCalculus
using YaoLocations
using TimerOutputs
using LinearAlgebra
using GPUCompiler
using Configurations
using CompilerPluginTools
using LLVM.Interop
using GPUCompiler: CodeCache, CompilerJob, AbstractCompilerTarget, AbstractCompilerParams, WorldView
using YaoLocations: map_check, map_check_nothrow, map_error, plain
using CompilerPluginTools: Argument
using Base.Meta: ParseError

const to = TimerOutput()
timings() = (TimerOutputs.print_timer(to); println())
enable_timings() = (TimerOutputs.enable_debug_timings(Compiler); return)

include("compiler/types.jl")
include("compiler/printing.jl")
include("compiler/intrinsics.jl")
include("compiler/syntax.jl")
include("compiler/interp.jl")

include("codegen/llvmopt.jl")
include("codegen/native.jl")
include("codegen/dummy.jl")
include("codegen/emulation.jl")
include("codegen/qasm2.jl")
include("codegen/qobj2.jl")

# We have one global JIT and TM
const orc = Ref{LLVM.OrcJIT}()
const tm = Ref{LLVM.TargetMachine}()

function __init__()
    opt_level = Base.JLOptions().opt_level
    if opt_level < 2
        optlevel = LLVM.API.LLVMCodeGenLevelNone
    elseif opt_level == 2
        optlevel = LLVM.API.LLVMCodeGenLevelDefault
    else
        optlevel = LLVM.API.LLVMCodeGenLevelAggressive
    end

    tm[] = LLVM.JITTargetMachine(; optlevel=optlevel)
    LLVM.asm_verbosity!(tm[], true)

    orc[] = LLVM.OrcJIT(tm[]) # takes ownership of tm
    atexit() do
        return LLVM.dispose(orc[])
    end
end


# include("compiler/qasm.jl")

# using .QASM: @qasm_str
# export @qasm_str

# # compiler internal extensions
# include("compiler/interpreter.jl")
# include("compiler/codeinfo.jl")
# include("compiler/optimize.jl")

# # code generators
# include("compiler/codegen/codegen.jl")

# include("compiler/reflection.jl")
# include("compiler/utils.jl")
# # include("compiler/validation.jl")
# # include("compiler/trace.jl")

# function __init__()
#     TimerOutputs.reset_timer!(to)
# end

# include("runtime/intrinsics.jl")

end # module
