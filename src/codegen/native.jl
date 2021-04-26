struct YaoCompileParams <: AbstractCompilerParams end

function GPUCompiler.ci_cache_populate(cache, target::YaoCompileTarget, mi, min_world, max_world)
    interp = YaoInterpreter(target)
    src = Core.Compiler.typeinf_ext_toplevel(interp, mi)
    wvc = WorldView(cache, min_world, max_world)
    @assert Core.Compiler.haskey(wvc, mi)

    # if src is rettyp_const, the codeinfo won't cache ci.inferred
    # (because it is normally not supposed to be used ever again).
    # to avoid the need to re-infer, set that field here.
    ci = Core.Compiler.getindex(wvc, mi)
    if ci !== nothing && ci.inferred === nothing
        ci.inferred = src
    end
    return ci
end

function compile_method_instance(@nospecialize(job::CompilerJob), method_instance::MethodInstance)
end

# function jit(ctx::CompilationContext, f::F, tt::TT=Tuple{}) where {F,TT<:Type}
#     fspec = FunctionSpec(f, tt, false, nothing) #=name=#
#     job = CompilerJob(MixtapeCompilerTarget(), fspec, MixtapeCompilerParams(ctx))
#     return GPUCompiler.cached_compilation(jit_compiled_cache, job, _jit, _jitlink)
# end
