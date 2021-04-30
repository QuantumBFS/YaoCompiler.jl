using YaoTargetQobj
using Documenter

DocMeta.setdocmeta!(YaoTargetQobj, :DocTestSetup, :(using YaoTargetQobj); recursive=true)

makedocs(;
    modules=[YaoTargetQobj],
    authors="Roger-Luo <rogerluo.rl18@gmail.com> and contributors",
    repo="https://github.com/QuantumBFS/YaoTargetQobj.jl/blob/{commit}{path}#{line}",
    sitename="YaoTargetQobj.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://QuantumBFS.github.io/YaoTargetQobj.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/QuantumBFS/YaoTargetQobj.jl",
)
