using Documenter
using SphericalHarmonicModes

DocMeta.setdocmeta!(SphericalHarmonicModes, :DocTestSetup, :(using SphericalHarmonicModes); recursive=true)

makedocs(;
    modules=[SphericalHarmonicModes],
    authors="Jishnu Bhattacharya",
    repo="https://github.com/jishnub/SphericalHarmonicModes.jl/blob/{commit}{path}#L{line}",
    sitename="SphericalHarmonicModes.jl",
    format=Documenter.HTML(;
        prettyurls=get(ENV, "CI", "false") == "true",
        canonical="https://jishnub.github.io/SphericalHarmonicModes.jl",
        assets=String[],
    ),
    pages=[
        "Reference" => "index.md",
    ],
)

deploydocs(;
    repo="github.com/jishnub/SphericalHarmonicModes.jl",
)
