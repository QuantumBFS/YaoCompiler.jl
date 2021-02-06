using YaoCompiler
using Test

@testset "compiler" begin
    include("compiler/parse.jl")
    include("compiler/utils.jl")
    include("compiler/circuit.jl")
end
