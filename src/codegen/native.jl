function execute(op::Operation{P, Args}) where {P, Args}
    
end

function jit(ctx::CompilationContext, f::F, tt::TT=Tuple{}) where {F,TT<:Type}
    fspec = FunctionSpec(f, tt, false, nothing) #=name=#
    job = CompilerJob(MixtapeCompilerTarget(), fspec, MixtapeCompilerParams(ctx))
    return GPUCompiler.cached_compilation(jit_compiled_cache, job, _jit, _jitlink)
end