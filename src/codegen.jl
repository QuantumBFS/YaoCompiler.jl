const Qobj = Dict{String, Any}()

mutable struct CodeGenState
    src::CodeInfo
    pc::Int
    stmt::Any
end

function YaoCompiler.compile(target::QobjTarget, f, tt::Type, options::HardwareFreeOptions)
    interp = YaoInterpreter(;target, options)
    mi = method_instance(f, tt)
    src = Core.Compiler.typeinf_ext_toplevel(interp, mi)
    # we need to remove code coverage effect to pass validate
    src = CompilerPluginTools.rm_code_coverage_effect(src)
    validate(target, src)

    if target.toplevel
        st = MainState(src, 0, nothing)
    else
        op = tt.parameters[2]
        op.parameters[1] <: GenericRoutine || error("OpenQASM gate target does not support callable object")
        name = routine_name(op)
        cargnames = retrieve_cargnames(op)
        cargs = extract_cargs_map(src)
    end
    return emit_qasm(target, src, RegInfo(target, src), st)
end
