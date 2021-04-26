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


export @device, @gate, @ctrl, @measure, @barrier

export YaoInterpreter
# # reflections
# export @code_yao, @code_qasm
# export gate_count
# export Intrinsics

using MLStyle
using YaoAPI
using BitBasis
using Expronicon
using ZXCalculus
using YaoLocations
using TimerOutputs
using LinearAlgebra
using GPUCompiler
using Configurations
using CompilerPluginTools
using GPUCompiler: CodeCache, CompilerJob, AbstractCompilerTarget, AbstractCompilerParams
using YaoLocations: map_check, map_check_nothrow, map_error, plain
using Base.Meta: ParseError

const to = TimerOutput()
timings() = (TimerOutputs.print_timer(to); println())
enable_timings() = (TimerOutputs.enable_debug_timings(Compiler); return)

include("compiler/types.jl")
include("compiler/printing.jl")
include("compiler/intrinsics.jl")
include("compiler/syntax.jl")
include("compiler/interp.jl")

include("codegen/native.jl")

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
