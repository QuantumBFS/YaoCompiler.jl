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

test_qasm = code_qasm(Operation{typeof(test_pure_quantum), Tuple{}})

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

s = """OPENQASM 2.0;
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
target_qasm = OpenQASM.parse(s).prog[1]
@test OpenQASM.issimilar(test_qasm.decl, target_qasm.decl)
@test_broken OpenQASM.issimilar(test_qasm.body, target_qasm.body)
@test_broken OpenQASM.issimilar(test_qasm.body[3].cargs, target_qasm.body[3].cargs)
@test OpenQASM.issimilar(test_qasm.body[3].qargs, target_qasm.body[3].qargs)

# typeof(test_qasm.body[end-1].cargs[1])
# typeof(target_qasm.body[end-1].cargs[1])

# OpenQASM.issimilar.(test_qasm.body, target_qasm.body)
test_qasm = code_qasm(ft; toplevel=false, inline_intrinsics=true)
