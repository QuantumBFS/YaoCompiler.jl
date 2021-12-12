module YaoTargetQASM

using MLStyle
using OpenQASM
using OpenQASM.Types
using OpenQASM.Tools
using YaoCompiler
using YaoLocations
using Expronicon
using Configurations
using CompilerPluginTools
using RBNF: Token
using YaoCompiler.Intrinsics
using YaoCompiler: YaoCompileTarget
using Core: CodeInfo
using YaoLocations: plain
using OpenQASM.Types: UGate

export OpenQASMTarget, RegInfo, code_qasm, QASM, @qasm_str

@option struct OpenQASMTarget <: YaoCompileTarget
    version::VersionNumber = v"2.0"
    toplevel::Bool = true
    inline_intrinsics::Bool = false
    include_qelib1::Bool = false
end

module QASM

Base.@pure @noinline sin(x) = Base.Math.sin(x)
Base.@pure @noinline cos(x) = Base.Math.cos(x)
Base.@pure @noinline tan(x) = Base.Math.tan(x)
Base.@pure @noinline exp(x) = Base.Math.exp(x)
Base.@pure @noinline log(x) = Base.Math.log(x)
Base.@pure @noinline sqrt(x) = Base.Math.sqrt(x)

end

function code_qasm(op::Type{<:Operation}; kw...)
    target = OpenQASMTarget(;kw...)
    return YaoCompiler.compile(target, Intrinsics.apply, Tuple{AnyReg, op})
end

include("validate.jl")
include("intrinsic.jl")
include("reginfo.jl")
include("codegen.jl")
include("frontend.jl")

end
