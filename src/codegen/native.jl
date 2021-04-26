const YaoCompilerJob = CompilerJob{T, P} where {T <: YaoCompileTarget, P <: YaoCompileParams}

function ci_cache_populate(cache::CodeCache, target::YaoCompileTarget, mi::MethodInstance,
        min_world, max_world, options::HardwareFreeOptions)
    interp = YaoInterpreter(;target, options, cache)
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

function compile_method_instance(cache, target::YaoCompileTarget, method_instance::MethodInstance, options::HardwareFreeOptions, world)
    if GPUCompiler.ci_cache_lookup(cache, method_instance, world, typemax(Cint)) === nothing
        ci_cache_populate(cache, target, method_instance, world, typemax(Cint), options)
    end

    # set-up the compiler interface
    jl_options = Base.JLOptions()
    debug_info_kind = if jl_options.debug_level == 0
        LLVM.API.LLVMDebugEmissionKindNoDebug
    elseif jl_options.debug_level == 1
        LLVM.API.LLVMDebugEmissionKindLineTablesOnly
    elseif jl_options.debug_level >= 2
        LLVM.API.LLVMDebugEmissionKindFullDebug
    end

    lookup_fun = (mi, min_world, max_world) -> GPUCompiler.ci_cache_lookup(cache, mi, min_world, max_world)
    lookup_cb = @cfunction($lookup_fun, Any, (Any, UInt, UInt))
    params = Base.CodegenParams(;
        track_allocations  = false,
        code_coverage      = false,
        prefer_specsig     = true,
        gnu_pubnames       = false,
        debug_info_kind    = Cint(debug_info_kind),
        lookup             = Base.unsafe_convert(Ptr{Nothing}, lookup_cb)
    )

    # generate IR
    GC.@preserve lookup_cb begin
        native_code = ccall(:jl_create_native, Ptr{Cvoid},
                            (Vector{MethodInstance}, Base.CodegenParams, Cint),
                            [method_instance], params, #=extern policy=# 1)
        @assert native_code != C_NULL
        llvm_mod_ref = ccall(:jl_get_llvm_module, LLVM.API.LLVMModuleRef,
                            (Ptr{Cvoid},), native_code)
        @assert llvm_mod_ref != C_NULL
        llvm_mod = LLVM.Module(llvm_mod_ref)
    end

    # get the top-level code
    code = GPUCompiler.ci_cache_lookup(cache, method_instance, world, typemax(Cint))

    # get the top-level function index
    llvm_func_idx = Ref{Int32}(-1)
    llvm_specfunc_idx = Ref{Int32}(-1)
    ccall(:jl_get_function_id, Nothing,
          (Ptr{Cvoid}, Any, Ptr{Int32}, Ptr{Int32}),
          native_code, code, llvm_func_idx, llvm_specfunc_idx)
    @assert llvm_func_idx[] != -1
    @assert llvm_specfunc_idx[] != -1

    # get the top-level function)
    llvm_func_ref = ccall(:jl_get_llvm_function, LLVM.API.LLVMValueRef,
                          (Ptr{Cvoid}, UInt32), native_code, llvm_func_idx[]-1)
    @assert llvm_func_ref != C_NULL
    llvm_func = LLVM.Function(llvm_func_ref)
    llvm_specfunc_ref = ccall(:jl_get_llvm_function, LLVM.API.LLVMValueRef,
                              (Ptr{Cvoid}, UInt32), native_code, llvm_specfunc_idx[]-1)
    @assert llvm_specfunc_ref != C_NULL
    llvm_specfunc = LLVM.Function(llvm_specfunc_ref)

    # # configure the module
    # triple!(llvm_mod, llvm_triple(job.target))
    # if julia_datalayout(job.target) !== nothing
    #     datalayout!(llvm_mod, julia_datalayout(job.target))
    # end
    return llvm_specfunc, llvm_func, llvm_mod
end

function jit_compile(job::YaoCompilerJob)
    world = Base.get_world_counter()
    mi = method_instance(job.source.f, job.source.tt, world)
    cache = get!(GLOBAL_CI_CACHE, job.target, GPUCompiler.CodeCache())
    llvm_specfunc, llvm_func, llvm_mod =
        compile_method_instance(cache, job.target, mi, job.params.options, world)

    specfunc_name = LLVM.name(llvm_specfunc)
    func_name = LLVM.name(llvm_func)
    linkage!(llvm_func, LLVM.API.LLVMExternalLinkage)
    linkage!(llvm_specfunc, LLVM.API.LLVMExternalLinkage)
    run_pipeline!(llvm_mod)
    return llvm_mod, func_name, specfunc_name
end

struct TargetHostKernel{F, TT, Target <: YaoCompileTarget}
    f::F
    target::Target
    specfunc::Ptr{Cvoid}
    func::Ptr{Cvoid}
end

function jit_link(job::CompilerJob, (llvm_mod, func_name, specfunc_name))
    fspec = job.source
    jitted_mod = compile!(orc[], llvm_mod)
    specfunc_addr = addressin(orc[], jitted_mod, specfunc_name)
    specfunc_ptr = pointer(specfunc_addr)
    func_addr = addressin(orc[], jitted_mod, func_name)
    func_ptr = pointer(func_addr)
    if specfunc_ptr === C_NULL || func_ptr === C_NULL
        @error "Compilation error" fspec specfunc_ptr func_ptr
    end
    TargetHostKernel{typeof(fspec.f), fspec.tt, typeof(job.target)}(fspec.f, job.target, specfunc_ptr, func_ptr)
end

const jit_compiled_cache = Dict{UInt,Any}()

function compile(target::YaoCompileTarget, f::F, tt::TT=Tuple{}, options::HardwareFreeOptions=HardwareFreeOptions()) where {F, TT <: Type}
    fspec = FunctionSpec(f, tt, false, nothing)
    job = CompilerJob(target, fspec, YaoCompileParams(options))
    return GPUCompiler.cached_compilation(jit_compiled_cache, job, jit_compile, jit_link)
end

@generated function (kernel::TargetHostKernel{F, TT})(args...) where {F, TT}
    expr = quote
        args = Any[args...]
        ccall(kernel.func, Any, (Any, Ptr{Any}, Int32), kernel.f, args, length(args))
    end
    return expr
end

# function jit(ctx::CompilationContext, f::F, tt::TT=Tuple{}) where {F,TT<:Type}
#     fspec = FunctionSpec(f, tt, false, nothing) #=name=#
#     job = CompilerJob(MixtapeCompilerTarget(), fspec, MixtapeCompilerParams(ctx))
#     return GPUCompiler.cached_compilation(jit_compiled_cache, job, _jit, _jitlink)
# end
