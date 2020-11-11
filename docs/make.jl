using Documenter, YaoCompiler

makedocs(;
    modules = [YaoCompiler],
    format = Documenter.HTML(prettyurls = !("local" in ARGS)),
    pages = [
        "Home" => "index.md",
        "Semantics" => "semantics.md",
        "Compilation" => "compilation.md",
        "References" => "references.md",
    ],
    repo = "https://github.com/QuantumBFS/YaoCompiler.jl",
    sitename = "YaoCompiler.jl",
)

deploydocs(; repo = "github.com/QuantumBFS/YaoCompiler.jl")
