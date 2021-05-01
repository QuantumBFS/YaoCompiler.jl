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

module Adder3

using YaoTargetQASM
qelib = joinpath(pkgdir(YaoTargetQASM), "test", "qelib1.inc")

const adder3 = qasm"""
OPENQASM 2.0;
include "$qelib";
gate majority a,b,c 
{ 
  cx c,b; 
  cx c,a; 
  ccx a,b,c; 
}
gate unmaj a,b,c 
{ 
  ccx a,b,c; 
  cx c,a; 
  cx a,b; 
}
qreg cin[1];
qreg a[4];
qreg b[4];
qreg cout[1];
creg ans[5];
// set input states
x a[0]; // a = 0001
x b;    // b = 1111
// add a to b, storing result in b
majority cin[0],b[0],a[0];
majority a[0],b[1],a[1];
majority a[1],b[2],a[2];
majority a[2],b[3],a[3];
cx a[3],cout[0];
unmaj a[2],b[3],a[3];
unmaj a[1],b[2],a[2];
unmaj a[0],b[1],a[1];
unmaj cin[0],b[0],a[0];
measure b[0] -> ans[0];
measure b[1] -> ans[1];
measure b[2] -> ans[2];
measure b[3] -> ans[3];
measure cout[0] -> ans[4];
"""
end

