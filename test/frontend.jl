using Test
using OpenQASM
using YaoTargetQASM

module QeLib

using YaoTargetQASM
qelib = joinpath(pkgdir(YaoTargetQASM), "test", "qelib1.inc")

qasm"""OPENQASM 2.0;
include "$qelib";

gate post q {x q;}
"""

end

ast = code_qasm(typeof(QeLib.s()))
@test ast ≈ OpenQASM.parse("""
OPENQASM 2.0;
qreg qreg_1[1];
rz(0) qreg_1;
ry(0) qreg_1;
rz(1.5707963267948966) qreg_1;
""")

ast = code_qasm(typeof(QeLib.x()))
@test ast ≈ OpenQASM.parse("""
OPENQASM 2.0;
qreg qreg_1[1];
rz(pi) qreg_1;
ry(0) qreg_1;
rz(pi) qreg_1;
""")

ast = code_qasm(typeof(QeLib.y()))
@test ast ≈ OpenQASM.parse("""
OPENQASM 2.0;
qreg qreg_1[1];
rz(pi) qreg_1;
ry(1.5707963267948966) qreg_1;
rz(1.5707963267948966) qreg_1;
""")

ast = code_qasm(typeof(QeLib.z()))
@test ast ≈ OpenQASM.parse("""
OPENQASM 2.0;
qreg qreg_1[1];
rz(0) qreg_1;
ry(0) qreg_1;
rz(pi) qreg_1;
""")

ast = code_qasm(typeof(QeLib.h()))
@test ast ≈ OpenQASM.parse("""
OPENQASM 2.0;
qreg qreg_1[1];
rz(1.5707963267948966) qreg_1;
ry(0) qreg_1;
rz(pi) qreg_1;
""")

ast = code_qasm(typeof(QeLib.rx(2.0)); toplevel=false)
@test ast ≈ OpenQASM.parse_gate("""
gate rx(theta) qreg_1 {
  rz(theta) qreg_1;
  ry(-1.5707963267948966) qreg_1;
  rz(1.5707963267948966) qreg_1;
}
""")

ast = code_qasm(typeof(QeLib.ccx()))
@test ast ≈ OpenQASM.parse("""
OPENQASM 2.0;
qreg qreg_1[3];
h qreg_1[2];
CX qreg_1[1], qreg_1[2];
rz(0) qreg_1[2];
ry(0) qreg_1[2];
rz(-0.7853981633974483) qreg_1[2];
CX qreg_1[0], qreg_1[2];
rz(0) qreg_1[2];
ry(0) qreg_1[2];
rz(0.7853981633974483) qreg_1[2];
CX qreg_1[1], qreg_1[2];
rz(0) qreg_1[2];
ry(0) qreg_1[2];
rz(-0.7853981633974483) qreg_1[2];
CX qreg_1[0], qreg_1[2];
rz(0) qreg_1[1];
ry(0) qreg_1[1];
rz(0.7853981633974483) qreg_1[1];
rz(0) qreg_1[2];
ry(0) qreg_1[2];
rz(0.7853981633974483) qreg_1[2];
h qreg_1[2];
CX qreg_1[0], qreg_1[1];
rz(0) qreg_1[0];
ry(0) qreg_1[0];
rz(0.7853981633974483) qreg_1[0];
rz(0) qreg_1[1];
ry(0) qreg_1[1];
rz(-0.7853981633974483) qreg_1[1];
CX qreg_1[0], qreg_1[1];
""")

ast = code_qasm(typeof(QeLib.crz(2.0)); toplevel=false)
@test ast ≈ OpenQASM.parse_gate("""
gate crz(lambda) qreg_2, qreg_1 {
  rz(0) qreg_1;
  ry(0) qreg_1;
  rz(lambda/2.0) qreg_1;
  CX qreg_2, qreg_1;
  rz(0) qreg_1;
  ry(0) qreg_1;
  rz(-(lambda)/2.0) qreg_1;
  CX qreg_2, qreg_1;
}
""")
