using YaoCompiler
using Test

@testset "interpreter" begin
    include("interp.jl")
end

@testset "emulate" begin
    include("emulate.jl")
end

@testset "printing" begin
    include("printing.jl")
end

@testset "zx optimization" begin
    include("zx.jl")
end