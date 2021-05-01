module YaoTargetQobj

using MLStyle
using Configurations
using YaoCompiler
using YaoCompiler.Intrinsics
using YaoCompiler: YaoCompileTarget
using Core: CodeInfo
using YaoLocations: plain

@option struct QobjTarget <: YaoCompileTarget
    nshots::Int = 1024
    seed::Int = 1
    max_credits::Int = 3
end

include("schema.jl")
include("codegen.jl")
include("intrinsics.jl")

end
