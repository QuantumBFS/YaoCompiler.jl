module YaoTargetQobj

export MemoryInfo, QobjQASMTarget, code_qobj, measure_ssa_uses, measure_ssa_returns

using UUIDs
using MLStyle
using IBMQClient
using YaoCompiler
using Configurations
using IBMQClient.Schema
using YaoCompiler.Intrinsics
using CompilerPluginTools
using YaoCompiler: YaoCompileTarget
using YaoLocations: plain
# let's just reuse TargetQASM since QobjQASM is QASM-like
using YaoTargetQASM
using YaoTargetQASM: push_stmt!, move_to_next_quantum_statement

@option struct QobjQASMTarget <: YaoCompileTarget
    nshots::Int = 1024
    seed::Int = 1
    max_credits::Int = 3
    qobj_id::String = string(uuid1())
    schema_version::VersionNumber = v"1.3"
    memory_offset::Int = 0
    description::Maybe{String} = nothing
end

function code_qobj(op::Type{<:Operation}; kw...)
    description = get(kw, :description, string(op))
    target = QobjQASMTarget(;kw..., description)
    return YaoCompiler.compile(target, Intrinsics.apply, Tuple{AnyReg, op})
end

include("validate.jl")
include("codegen.jl")

end
