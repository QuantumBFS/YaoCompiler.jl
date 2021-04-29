const pi_token = Token{:reserved}("pi")

divpi(n::Int) = (pi_token, Token{:reserved}("/"), qasm_int(n))

# Intrinsics
# // 3-parameter 2-pulse single qubit gate
# gate u3(theta,phi,lambda) q { U(theta,phi,lambda) q; }
# // 2-parameter 1-pulse single qubit gate
# gate u2(phi,lambda) q { U(pi/2,phi,lambda) q; }
# // 1-parameter 0-pulse single qubit gate
# gate u1(lambda) q { U(0,0,lambda) q; }
# // Pauli gate: bit-flip
# gate x a { u3(pi,0,pi) a; }
# // Pauli gate: bit and phase flip
# gate y a { u3(pi,pi/2,pi/2) a; }
# // Pauli gate: phase flip
# gate z a { u1(pi) a; }
# // Clifford gate: Hadamard
# gate h a { u2(0,pi) a; }
# // Clifford gate: sqrt(Z) phase gate
# gate s a { u1(pi/2) a; }
# // Clifford gate: conjugate of sqrt(Z)
# gate sdg a { u1(-pi/2) a; }
# // C3 gate: sqrt(S) phase gate
# gate t a { u1(pi/4) a; }
# // C3 gate: conjugate of sqrt(S)
# gate tdg a { u1(-pi/4) a; }

qasm_gate_name(::Type{Intrinsics.XGate}) = "x"
qasm_gate_name(::Type{Intrinsics.YGate}) = "y"
qasm_gate_name(::Type{Intrinsics.ZGate}) = "z"
qasm_gate_name(::Type{Intrinsics.HGate}) = "h"
qasm_gate_name(::Type{Intrinsics.SGate}) = "s"
qasm_gate_name(::Type{Intrinsics.TGate}) = "t"
qasm_gate_name(::Type{<:Intrinsics.Rx}) = "rx"
qasm_gate_name(::Type{<:Intrinsics.Ry}) = "ry"
qasm_gate_name(::Type{<:Intrinsics.Rz}) = "rz"

function intrinsic_qasm(::Type{Intrinsics.XGate})
    UGate(pi_token, qasm_int(0), pi_token, [])
end

function intrinsic_qasm(::Type{Intrinsics.YGate})
    UGate(pi_token, divpi(2), divpi(2), nothing)
end

function intrinsic_qasm(::Type{Intrinsics.ZGate})
    UGate(qasm_int(0), qasm_int(0), pi_token, nothing)
end

function intrinsic_qasm(::Type{Intrinsics.HGate})
    UGate(divpi(2), qasm_int(0), pi_token, nothing)
end

function intrinsic_qasm(::Type{Intrinsics.SGate})
    UGate(qasm_int(0), qasm_int(0), divpi(2), nothing)
end

function intrinsic_qasm(::Type{Intrinsics.TGate})
    UGate(qasm_int(0),qasm_int(0),divpi(4), nothing)
end

# // Rotation around X-axis
# gate rx(theta) a { u3(theta,-pi/2,pi/2) a; }
# // rotation around Y-axis
# gate ry(theta) a { u3(theta,0,0) a; }
# // rotation around Z axis
# gate rz(phi) a { u1(phi) a; }
# NOTE: theta can be a variable or exp
function intrinsic_qasm(::Type{<:Intrinsics.Rx}, theta)
    UGate(theta, Negative(divpi(2)), divpi(2), nothing)
end

function intrinsic_qasm(::Type{<:Intrinsics.Ry}, theta)
    UGate(qasm_int(0), theta, qasm_int(0), nothing)
end

function intrinsic_qasm(::Type{<:Intrinsics.Rz}, theta)
    UGate(qasm_int(0), qasm_int(0), theta, nothing)
end
