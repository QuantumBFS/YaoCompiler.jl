using MLStyle
using YaoLocations
using YaoCompiler
using YaoCompiler.Intrinsics
using OpenQASM
using YaoTargetQASM
using YaoTargetQASM.QASM
using Test

@testset "reginfo" begin
    include("reginfo.jl")
end

@testset "codegen" begin
    include("codegen.jl")
end

@testset "frontend" begin
    include("frontend.jl")
end