using YaoCompiler
using Test

@testset "interpreter" begin
    include("compiler/interp.jl")    
end


# @testset "compiler" begin
#     include("compiler/parse.jl")
#     include("compiler/utils.jl")
#     include("compiler/circuit.jl")
# end
