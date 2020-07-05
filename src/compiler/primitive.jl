export @primitive

const PRIMITIVES = Any[]

"""
    generate_forward_stub(name::Symbol, op)

Generate forward stub which forward primitive circuit to instruction interfaces.
"""
function generate_forward_stub(name::Symbol, op)
    quoted_name = QuoteNode(name)
    stub = gensym(name)

    return quote
        function $stub(::$(Circuit){$quoted_name}, r::$(AbstractRegister), locs::$(Locations))
            $(YaoAPI).instruct!(r, $op, Tuple(locs))
            return
        end

        function $stub(::$(Circuit){$quoted_name}, r::$(TraceTape), locs::$(Locations))
            $(GlobalRef(Compiler, :trace!))(
                r,
                Expr(:quantum, :gate, GlobalRef(YaoLang, $quoted_name), locs),
            )
            return
        end

        function $stub(
            ::$(Circuit){$quoted_name},
            r::$(AbstractRegister),
            locs::$(Locations),
            ctrl_locs::$(CtrlLocations),
        )
            raw_ctrl_locs = Tuple(ctrl_locs.storage)
            ctrl_cfg = map(Int, (ctrl_locs.configs...,))
            $(YaoAPI).instruct!(r, $op, Tuple(locs), raw_ctrl_locs, ctrl_cfg)
            return
        end

        function $stub(
            ::$(Circuit){$quoted_name},
            r::$(TraceTape),
            locs::$(Locations),
            ctrl_locs::$(CtrlLocations),
        )
            $(GlobalRef(Compiler, :trace!))(
                r,
                Expr(:quantum, :ctrl, GlobalRef(YaoLang, $quoted_name), locs, ctrl_locs),
            )
            return
        end

        (::$PrimitiveCircuit{$quoted_name})() = $Circuit{$quoted_name}($stub)
        Core.@__doc__ const $name = $Circuit{$quoted_name}($stub)
        push!($(GlobalRef(Compiler, :PRIMITIVES)), $name)
        $name
    end
end

function primitive_m(x::Symbol)
    generate_forward_stub(x, :(Val($(QuoteNode(x)))))
end

function primitive_m(ex::Expr)
    def = splitdef(ex; throw = false)
    def === nothing && return assign_statement(ex)

    haskey(def, :name) || throw(Meta.ParseError("Invalid Syntax: expect a function name"))
    name = def[:name]
    quoted_name = QuoteNode(name)
    stub = gensym(name)
    if haskey(def, :args)
        args = map(rm_annotations, def[:args])
    else
        args = ()
    end

    mat_stub_def = deepcopy(def)
    mat_stub = gensym(:mat)
    mat_stub_def[:name] = mat_stub

    primitive_def = deepcopy(def)
    primitive_def[:name] = :(::$(PrimitiveCircuit{name}))
    primitive_def[:body] = quote
        m = $(Expr(:call, mat_stub, args...))
        return Circuit{$quoted_name}($stub, (m, $(args...)))
    end

    circ = gensym(:circ)
    register = gensym(:register)
    locs = gensym(:locs)
    ctrl_locs = gensym(:ctrl_locs)
    matrix = gensym(:m)

    stub_def = Dict{Symbol,Any}()
    stub_def[:name] = stub
    stub_def[:args] =
        Any[:($circ::Circuit{$quoted_name}), :($register::$AbstractRegister), :($locs::$Locations)]
    stub_def[:body] = quote
        $matrix = $circ.free[1]
        YaoAPI.instruct!($register, $matrix, Tuple($locs))
        return
    end

    ctrl_stub_def = Dict{Symbol,Any}()
    ctrl_stub_def[:name] = stub
    ctrl_stub_def[:args] = Any[
        :($circ::Circuit{$quoted_name}),
        :($register::$AbstractRegister),
        :($locs::$Locations),
        :($ctrl_locs::$CtrlLocations),
    ]
    ctrl_stub_def[:body] = quote
        $matrix = $circ.free[1]
        # issue #10
        raw_ctrl_locs = ($(ctrl_locs).storage...,)
        ctrl_cfg = map(Int, ($(ctrl_locs).configs...,))
        YaoAPI.instruct!($register, $matrix, Tuple($locs), raw_ctrl_locs, ctrl_cfg)
        return
    end

    trace_stub_def = Dict{Symbol,Any}()
    trace_stub_def[:name] = stub
    trace_stub_def[:args] =
        Any[:($circ::Circuit{$quoted_name}), :($register::$TraceTape), :($locs::$Locations)]
    trace_stub_def[:body] = Expr(
        :call,
        GlobalRef(Compiler, :trace!),
        register,
        :(Expr(
            :quantum,
            :gate,
            Expr(:call, GlobalRef(YaoLang, $quoted_name), $circ.free[2:end]...),
            $locs,
        )),
    )

    trace_ctrl_stub_def = Dict{Symbol,Any}()
    trace_ctrl_stub_def[:name] = stub
    trace_ctrl_stub_def[:args] = Any[
        :($circ::Circuit{$quoted_name}),
        :($register::$TraceTape),
        :($locs::$Locations),
        :($ctrl_locs::$CtrlLocations),
    ]
    trace_ctrl_stub_def[:body] = Expr(
        :call,
        GlobalRef(Compiler, :trace!),
        register,
        :(Expr(
            :quantum,
            :ctrl,
            Expr(:call, GlobalRef(YaoLang, $quoted_name), $circ.free[2:end]...),
            $locs,
            $ctrl_locs,
        )),
    )

    quote
        $(combinedef(mat_stub_def))
        $(combinedef(stub_def))
        $(combinedef(ctrl_stub_def))
        $(combinedef(primitive_def))
        $(combinedef(trace_stub_def))
        $(combinedef(trace_ctrl_stub_def))
        Core.@__doc__ const $name = $(PrimitiveCircuit{name})()
        push!($(GlobalRef(Compiler, :PRIMITIVES)), $name)
        $name
    end
end

function assign_statement(ex::Expr)
    ex.head === :(=) ||
        throw(Meta.ParseError("Invalid Syntax, expect <primitive gate name> = <matrix expr>, got $ex"))
    ex.args[1] isa Symbol || throw(Meta.ParseError("Invalid Syntax, expect Symbol got $(ex.args[1])"))
    name = ex.args[1]
    matrix_const = gensym(:matrix_const)

    return quote
        const $matrix_const = $(esc(ex.args[2]))
        $(generate_forward_stub(name, matrix_const))
    end
end

"""
    @primitive ex

Define a primitive quantum instruction. `ex` can be a Symbol, if the corresponding instruction
interface of `YaoAPI.instruct!` is implemented. Or `ex` can be an assignment statement for constant
instructions. Or `ex` can be a function that returns corresponding matrix given a set of classical
parameters.

# Example

Since the instructions interface `YaoAPI.instruct!` of Pauli operators are defined, we can use

```julia
@primitive X
```

to declare a Pauli X primitive instruction.

Or we can also define a Hadamard primitive instruction via its matrix form

```julia
@primitive H = [1 1;1 -1]/sqrt(2)
```

For parameterized gates, such as phase shift gate, we can define it as

```julia
@primitive shift(θ::Real) = Diagonal([1.0, exp(im * θ)])
```
"""
macro primitive(ex)
    return esc(primitive_m(ex))
end
