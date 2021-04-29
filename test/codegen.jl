using MLStyle
using YaoLocations
using YaoCompiler
using YaoCompiler.Intrinsics
using OpenQASM
using YaoTargetQASM
using YaoTargetQASM.QASM
using Test

@device function test_basic(theta, phi)
    # syntax sugar
    1 => X
    2 => Z
    4 => Rx(sin(theta)+2)
    4 => Ry(phi)
    @ctrl 1 4 => X
    a = @measure 3
    # direct intrinsic
    apply(Y, 3)
    apply(X, 1, 4)
    c = measure((2, 3))
    if c == 1
        1 => X
    end
    return (a = a, b = c)
end

@device function test_pure_quantum()
    ret = @gate 1:4 => test_basic(1.0, π)
    @barrier 1:4
    @ctrl 2 1 => X
    return ret
end

test_qasm = code_qasm(Operation{typeof(test_pure_quantum), Tuple{}}; include_qelib1=true)

s = """
OPENQASM 2.0;
include "qelib1.inc";
creg a[1];
creg b[2];
qreg qreg_2[2];
qreg qreg_1[2];
x qreg_1[0];
z qreg_2[0];
rx(2.8414709848078967) qreg_1[1];
ry(pi) qreg_1[1];
CX qreg_1[0], qreg_1[1];
measure qreg_2[1] -> a[0];
y qreg_2[1];
CX qreg_1[1], qreg_1[0];
measure qreg_2[0] -> b[0];
measure qreg_2[1] -> b[1];
if (b == 1) x qreg_1[0];
barrier qreg_2, qreg_1;
CX qreg_2[0], qreg_1[0];
"""

target_qasm = OpenQASM.parse(s)
@test test_qasm ≈ target_qasm

@device function test_gate(theta, phi)
    1 => X
    2 => Z
    4 => Rx(-sin(theta)+2)
    4 => Rx(sin(theta)+tan(phi))
    4 => Ry(cos(phi)-log(phi))
    4 => Ry(QASM.cos(phi)*QASM.sqrt(phi))
    @ctrl 1 4 => X
    return
end

ft = Operation{typeof(test_gate), Tuple{Float64, Float64}}
test_qasm = code_qasm(ft; toplevel=false)

s = """
gate test_gate(theta, phi) qreg_2, qreg_3, qreg_1 {
  x qreg_1;
  z qreg_2;
  rx(-(sin(theta))+2.0) qreg_3;
  rx(sin(theta)+tan(phi)) qreg_3;
  ry(cos(phi)-ln(phi)) qreg_3;
  ry(cos(phi)*sqrt(phi)) qreg_3;
  CX qreg_1, qreg_3;
}
"""
target_qasm = OpenQASM.parse_gate(s)
@test test_qasm ≈ target_qasm

test_qasm = code_qasm(ft; toplevel=false, inline_intrinsics=true)

s = """
gate test_gate(theta, phi) qreg_2, qreg_3, qreg_1 {
  U(pi, 0, pi) qreg_1;
  U(0, 0, pi) qreg_2;
  U(-(sin(theta))+2.0, -(pi/2), pi/2) qreg_3;
  U(sin(theta)+tan(phi), -(pi/2), pi/2) qreg_3;
  U(0, cos(phi)-ln(phi), 0) qreg_3;
  U(0, cos(phi)*sqrt(phi), 0) qreg_3;
  CX qreg_1, qreg_3;
}
"""
target_qasm = OpenQASM.parse_gate(s)
@test test_qasm ≈ target_qasm
