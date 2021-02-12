module TestParse

using Test
using OpenQASM.Types
using YaoCompiler
using YaoCompiler.Intrinsics
using YaoCompiler.QASM
using RBNF: Token

qasm"""OPENQASM 2.0; 
include "qelib1.inc";

gate post q {x q;}
"""

@testset "qasm gate codegen" begin
    ast = @code_qasm gate = true cu3(0.1, 0.2, 0.3)
    @test ast isa Gate
    @test length(ast.decl.cargs) == 3
    @test length(ast.decl.qargs) == 2
    @test length(ast.body) == 5
    @test ast.decl.cargs[1].str == "theta"
    @test ast.decl.cargs[2].str == "phi"
    @test ast.decl.cargs[3].str == "lambda"

    inst = ast.body[1]
    @test inst isa Instruction
    @test inst.name == "u1"
    @test length(inst.cargs) == 1
    @test length(inst.qargs) == 1
    @test length(inst.cargs[1]) == 3
    @test length(inst.cargs[1][1]) == 3
    carg = inst.cargs[1][1]
    @test carg[1].str == "lambda"
    @test carg[2].str == "-"
    @test carg[3].str == "phi"
    @test inst.cargs[1][2].str == "/"
    @test inst.cargs[1][3].str == "2"
end

@testset "@code_qasm optimize=true passes=:julia gate=true" begin
    ast = @code_qasm optimize = true passes = :julia gate = true cu3(0.1, 0.2, 0.3)
    @test ast.body[1] isa Instruction
    @test ast.body[1].name == "rz"
    @test ast.body[1].cargs[1] isa Token{:int}
    @test ast.body[1].cargs[1].str == "0"

    @test ast.body[2] isa Instruction
    @test ast.body[2].name == "ry"
    @test ast.body[2].cargs[1] isa Token{:int}
    @test ast.body[2].cargs[1].str == "0"

    @test ast.body[3] isa Instruction
    @test ast.body[3].name == "rz"
    @test ast.body[3].cargs[1] isa Tuple
    @test ast.body[3].cargs[1][1][1].str == "lambda"
    @test ast.body[3].cargs[1][1][2].str == "-"
    @test ast.body[3].cargs[1][1][3].str == "phi"
    @test ast.body[3].cargs[1][2].str == "/"
    @test ast.body[3].cargs[1][3].str == "2.0"

    @test ast.body[4] isa Instruction
    @test ast.body[4].name == "CX"
    @test isempty(ast.body[4].cargs)
    @test length(ast.body[4].qargs) == 2

    @test ast.body[5] isa Instruction
    @test ast.body[5].name == "rz"
    @test ast.body[5].cargs[1].fn == :(-)
    @test ast.body[5].cargs[1].arg isa Tuple
    @test ast.body[5].cargs[1].arg[1].str == "theta"
    @test ast.body[5].cargs[1].arg[2].str == "/"
    @test ast.body[5].cargs[1].arg[3].str == "2.0"

    @test ast.body[6] isa Instruction
    @test ast.body[6].name == "ry"
    @test ast.body[6].cargs[1].str == "0"
end

@device function circuit()
    1:3 => ccx()
    (2, 3) => cx()
    2 => h()
    c = @measure 1
    if c == 1
        3 => post()
    end
    3 => rx(1.0)
    return (c = c,)
end

@testset "branches & const inline" begin
    ast = @code_qasm optimize = true circuit()
    @test ast isa MainProgram
    # @test ast.prog[3] isa Instruction
    # @test ast.prog[3].name == "h"
end

end # TestParse
