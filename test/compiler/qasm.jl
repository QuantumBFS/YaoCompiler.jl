using YaoLang, YaoArrayRegister

qasm_0 = """
OPENQASM 2.0;
qreg qubits[5];
x qubits[4];
h qubits[4];
h qubits[4];
ccx qubits[0],qubits[3],qubits[4];
h qubits[4];
h qubits[4];
ccx qubits[2],qubits[3],qubits[4];
h qubits[4];
h qubits[4];
cx qubits[3],qubits[4];
h qubits[4];
h qubits[4];
ccx qubits[1],qubits[2],qubits[4];
h qubits[4];
h qubits[4];
cx qubits[2],qubits[4];
h qubits[4];
h qubits[4];
ccx qubits[0],qubits[1],qubits[4];
h qubits[4];
h qubits[4];
cx qubits[1],qubits[4];
cx qubits[0],qubits[4];
"""

qasm_1 = """OPENQASM 2.0;
qreg q[3];
x q[2];
ccx q[0], q[2], q[1];
x q[2];
ccx q[2], q[1], q[0];
cx q[2], q[0];
x q[2];
ccx q[0], q[2], q[1];
ccx q[0], q[2], q[1];
"""

qasm_2 = """OPENQASM 2.0;
qreg q[3];
ccx q[0], q[1], q[2];
ccx q[0], q[1], q[2];
ccx q[1], q[0], q[2];
x q[0];
ccx q[1], q[0], q[2];
ccx q[2], q[1], q[0];
ccx q[2], q[1], q[0];
ccx q[1], q[0], q[2];
"""

qasm_3 = """OPENQASM 2.0;
qreg q[3];
ccx q[1], q[2], q[0];
x q[0];
x q[1];
ccx q[1], q[0], q[2];
ccx q[1], q[0], q[2];
ccx q[1], q[0], q[2];
ccx q[0], q[1], q[2];
ccx q[0], q[2], q[1];
"""

qasm_4 = """OPENQASM 2.0;
qreg q[3];
ccx q[1], q[2], q[0];
x q[1];
ccx q[2], q[1], q[0];
ccx q[2], q[1], q[0];
ccx q[0], q[1], q[2];
ccx q[0], q[2], q[1];
ccx q[2], q[0], q[1];
cx q[1], q[2];
"""

srcs = [qasm_0, qasm_1, qasm_2, qasm_3, qasm_4]
for i in 0:4
    src = srcs[i+1]
    ir_original = YaoLang.Compiler.YaoIR(@__MODULE__, src, :circ_original)
    ir_original.pure_quantum = YaoLang.Compiler.is_pure_quantum(ir_original)
    ir_optimized = YaoLang.Compiler.YaoIR(@__MODULE__, src, :circ_optimized)
    ir_optimized.pure_quantum = YaoLang.Compiler.is_pure_quantum(ir_optimized)
    ir_optimized = YaoLang.Compiler.optimize(ir_optimized, [:zx_teleport])
    code_original =
        YaoLang.Compiler.codegen(YaoLang.Compiler.JuliaASTCodegenCtx(ir_original), ir_original)
    code_optimized =
        YaoLang.Compiler.codegen(YaoLang.Compiler.JuliaASTCodegenCtx(ir_optimized), ir_optimized)

    eval(code_original)
    eval(code_optimized)

    nbits = YaoLang.Compiler.count_nqubits(ir_original)

    circ_or = circ_original()
    circ_op = circ_optimized()

    mat_original = zeros(ComplexF64, 2^nbits, 2^nbits)
    for i in 1:2^nbits
        st = zeros(ComplexF64, 2^nbits)
        st[i] = 1
        r0 = ArrayReg(st)
        r0 |> circ_or
        mat_original[:, i] = r0.state
    end

    mat_optimized = zeros(ComplexF64, 2^nbits, 2^nbits)
    for i in 1:2^nbits
        st = zeros(ComplexF64, 2^nbits)
        st[i] = 1
        r0 = ArrayReg(st)
        r0 |> circ_op
        mat_optimized[:, i] = r0.state
    end

    ind_or = findfirst(abs.(mat_original) .> 1e-10)
    ind_op = findfirst(abs.(mat_optimized) .> 1e-10)
    @test ind_or == ind_op

    mat_optimized = mat_optimized .* (mat_original[ind_or] / mat_optimized[ind_op])
    @test sum(abs.(mat_original - mat_optimized) .> 1e-10) == 0
end
