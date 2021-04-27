using YaoTargetQASM
using Documenter

DocMeta.setdocmeta!(YaoTargetQASM, :DocTestSetup, :(using YaoTargetQASM); recursive=true)

makedocs(;
    modules=[YaoTargetQASM],
    authors="Roger-Luo <rogerluo.rl18@gmail.com> and contributors",
    repo="https://github.com/Roger-luo/YaoTargetQASM.jl/blob/{commit}{path}#{line}",
    sitename="YaoTargetQASM.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://Roger-luo.github.io/YaoTargetQASM.jl",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/Roger-luo/YaoTargetQASM.jl",
)
