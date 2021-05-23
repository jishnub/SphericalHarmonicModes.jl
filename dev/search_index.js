var documenterSearchIndex = {"docs":
[{"location":"","page":"Reference","title":"Reference","text":"CurrentModule = SphericalHarmonicModes","category":"page"},{"location":"#SphericalHarmonicModes.jl","page":"Reference","title":"SphericalHarmonicModes.jl","text":"","category":"section"},{"location":"","page":"Reference","title":"Reference","text":"Modules = [SphericalHarmonicModes]","category":"page"},{"location":"#SphericalHarmonicModes.FullRange","page":"Reference","title":"SphericalHarmonicModes.FullRange","text":"FullRange(l::Int)\n\nThe range -l:l for an l ≥ 0.\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.L2L1Triangle","page":"Reference","title":"SphericalHarmonicModes.L2L1Triangle","text":"L2L1Triangle(l1_min::Int, l1_max::Int, Δl_max::Int, l2_min::Int = max(0, l1_min - Δl_max), l2_max = l1_max + Δl_max)\nL2L1Triangle(l1_range::AbstractUnitRange{Int}, Δl_max::Int, l2_range::AbstractUnitRange{Int})\n\nReturn an iterator that loops over pairs of (l2,l1) where l1 lies in l1_range, l2 lies in l2_range, and l2 and l1 obey the triangle condition max(0, l1 - Δl_max) ⩽ l2 ⩽ l1 + Δl_max. If l2_range is not specified, it defaults to the maximal range permissible.\n\nwarning: Warning\nThe ranges l1_range and l2_range will be curtailed to the minimal permissible subsets.\n\nExamples\n\njulia> L2L1Triangle(1:2, 2) |> collect\n9-element Vector{Tuple{Int64, Int64}}:\n (0, 1)\n (1, 1)\n (2, 1)\n (3, 1)\n (0, 2)\n (1, 2)\n (2, 2)\n (3, 2)\n (4, 2)\n\njulia> L2L1Triangle(2:3, 1) |> collect\n6-element Vector{Tuple{Int64, Int64}}:\n (1, 2)\n (2, 2)\n (3, 2)\n (2, 3)\n (3, 3)\n (4, 3)\n\njulia> L2L1Triangle(2:3, 1, 2:3) |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (2, 2)\n (3, 2)\n (2, 3)\n (3, 3)\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.LM","page":"Reference","title":"SphericalHarmonicModes.LM","text":"LM(l_range::Union{Integer, AbstractUnitRange{<:Integer}}, m_range::Union{Integer, AbstractUnitRange{<:Integer}})\nLM(l_range::Union{Integer, AbstractUnitRange{<:Integer}}, [T = FullRange]) where T<:Union{FullRange, ZeroTo, ToZero}\n\nReturn an iterator that loops over pairs of spherical harmonic modes (l,m), with l increasing faster than m. The loop runs over all the valid modes that may be obtained from the ranges provided. If m_range is not specified, the loop runs over all valid values of m for each l. Neither l_range nor m_range may be empty.\n\nOptionally m_range may be provided implicitly using the range specifiers FullRange, ZeroTo and ToZero, or as a SingleValuedRange type. Additionally, l_range may be of type ZeroTo or SingleValuedRange. Iterators constructed using these special types would often permit optimizations.\n\nwarning: Warning\nAn overlarge l_range will be curtailed to match the valid range compatible with m_range. A smaller l_range than that compatible with m_range will raise an error.\n\nExamples\n\njulia> LM(0:1) |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (1, -1)\n (0, 0)\n (1, 0)\n (1, 1)\n\njulia> LM(0:1, 1:1) |> collect\n1-element Vector{Tuple{Int64, Int64}}:\n (1, 1)\n\njulia> r = LM(ZeroTo(1), FullRange);\n\njulia> r |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (1, -1)\n (0, 0)\n (1, 0)\n (1, 1)\n\nSee also: ML\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.ML","page":"Reference","title":"SphericalHarmonicModes.ML","text":"ML(l_range::Union{Integer, AbstractUnitRange{<:Integer}}, m_range::Union{Integer, AbstractUnitRange{<:Integer}})\nML(l_range::Union{Integer, AbstractUnitRange{<:Integer}}, [T = FullRange]) where T<:Union{FullRange, ZeroTo, ToZero}\n\nReturn an iterator that loops over pairs of spherical harmonic modes (l,m), with m increasing faster than l. The loop runs over all the valid modes that may be obtained from the ranges provided.  If m_range is not specified, the loop runs over all valid values of m for each l. Neither l_range nor m_range may be empty.\n\nOptionally m_range may be provided implicitly using the range specifiers FullRange, ZeroTo and ToZero, or as a SingleValuedRange type. Additionally l_range may be of type ZeroTo or SingleValuedRange. Iterators constructed using these special types would often permit optimizations.\n\nwarning: Warning\nAn overlarge l_range will be curtailed to match the valid range compatible with m_range. A smaller l_range than that compatible with m_range will raise an error.\n\nExamples\n\njulia> ML(0:1) |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (0, 0)\n (1, -1)\n (1, 0)\n (1, 1)\n\njulia> ML(0:1, 1:1) |> collect\n1-element Vector{Tuple{Int64, Int64}}:\n (1, 1)\n\njulia> r = ML(ZeroTo(1), FullRange);\n\njulia> r |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (0, 0)\n (1, -1)\n (1, 0)\n (1, 1)\n\nSee also: LM\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.ModeRange","page":"Reference","title":"SphericalHarmonicModes.ModeRange","text":"SphericalHarmonicModes.ModeRange\n\nAbstract type whose subtypes are iterators over combinations of spherical harmonic modes. This is the topmost node in the type hierarchy defined in this package.\n\nDirect subtypes of ModeRange are SHModeRange and L2L1Triangle.\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.SHModeRange","page":"Reference","title":"SphericalHarmonicModes.SHModeRange","text":"SphericalHarmonicModes.SHModeRange <: SphericalHarmonicModes.ModeRange\n\nAbstract supertype of iterators that loop over (l,m) pairs. The types LM and ML are subtypes of this.\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.SingleValuedRange","page":"Reference","title":"SphericalHarmonicModes.SingleValuedRange","text":"SingleValuedRange(n::Int)\n\nThe range n:n.\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.ToZero","page":"Reference","title":"SphericalHarmonicModes.ToZero","text":"ToZero(l::Int)\n\nThe range -l:0 for an l ≥ 0.\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.ZeroTo","page":"Reference","title":"SphericalHarmonicModes.ZeroTo","text":"ZeroTo(l::Int)\n\nThe range 0:l for an l ≥ 0.\n\n\n\n\n\n","category":"type"},{"location":"#SphericalHarmonicModes.flip-Tuple{SphericalHarmonicModes.SHModeRange}","page":"Reference","title":"SphericalHarmonicModes.flip","text":"SphericalHarmonicModes.flip(mr::SphericalHarmonicModes.SHModeRange)\n\nReturn an iterator that flips the order in which the modes (l,m) are iterated over. flip(::LM) will return an ML iterator and vice versa.\n\nExamples\n\njulia> LM(0:1) |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (1, -1)\n (0, 0)\n (1, 0)\n (1, 1)\n\njulia> SphericalHarmonicModes.flip(LM(0:1)) |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (0, 0)\n (1, -1)\n (1, 0)\n (1, 1)\n\njulia> SphericalHarmonicModes.flip(LM(0:1)) == ML(0:1)\ntrue\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.l1_range-Tuple{L2L1Triangle}","page":"Reference","title":"SphericalHarmonicModes.l1_range","text":"l1_range(mr::L2L1Triangle)\n\nReturn the range of l1 spanned by the iterator.\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.l2_range-Tuple{L2L1Triangle, Integer}","page":"Reference","title":"SphericalHarmonicModes.l2_range","text":"l2_range(mr::L2L1Triangle, l1::Integer)\n\nReturn a subsection of the range of l2 spanned by the iterator for which (l1,l2) satisfy l1 - mr.Δl_max ⩽ l2 ⩽ l1 + mr.Δl_max.\n\nExamples\n\njulia> r = L2L1Triangle(1:2, 1);\n\njulia> collect(r)\n6-element Vector{Tuple{Int64, Int64}}:\n (0, 1)\n (1, 1)\n (2, 1)\n (1, 2)\n (2, 2)\n (3, 2)\n\njulia> l2_range(r, 1)\n0:2\n\njulia> l2_range(r, 2)\n1:3\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.l2_range-Tuple{L2L1Triangle}","page":"Reference","title":"SphericalHarmonicModes.l2_range","text":"l2_range(mr::L2L1Triangle)\n\nReturn the range of l2 spanned by the iterator.\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.l_range-Tuple{SphericalHarmonicModes.SHModeRange, Integer}","page":"Reference","title":"SphericalHarmonicModes.l_range","text":"l_range(mr::SphericalHarmonicModes.SHModeRange, m::Integer)\n\nReturn the subsection of the range of l spanned by the iterator for which (l,m) is a valid spherical harmonic mode.\n\nExamples\n\njulia> r = LM(1:2, 1:2);\n\njulia> collect(r)\n3-element Vector{Tuple{Int64, Int64}}:\n (1, 1)\n (2, 1)\n (2, 2)\n\njulia> l_range(r, 1)\n1:2\n\njulia> l_range(r, 2)\n2:2\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.l_range-Tuple{SphericalHarmonicModes.SHModeRange}","page":"Reference","title":"SphericalHarmonicModes.l_range","text":"l_range(mr::SphericalHarmonicModes.SHModeRange)\n\nReturn the range of l spanned by the iterator.\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.m_range-Tuple{SphericalHarmonicModes.SHModeRange, Integer}","page":"Reference","title":"SphericalHarmonicModes.m_range","text":"m_range(mr::SphericalHarmonicModes.SHModeRange, l::Integer)\n\nReturn the subsection of the range of m spanned by the iterator for which (l,m) is a valid spherical harmonic mode.\n\nExamples\n\njulia> r = LM(1:2, 1:2);\n\njulia> collect(r)\n3-element Vector{Tuple{Int64, Int64}}:\n (1, 1)\n (2, 1)\n (2, 2)\n\njulia> m_range(r, 1)\n1:1\n\njulia> m_range(r, 2)\n1:2\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.m_range-Tuple{SphericalHarmonicModes.SHModeRange}","page":"Reference","title":"SphericalHarmonicModes.m_range","text":"m_range(mr::SphericalHarmonicModes.SHModeRange)\n\nReturn the range of m spanned by the iterator.\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.modeindex-Tuple{SphericalHarmonicModes.ModeRange, Tuple{Vararg{Integer, N} where N}}","page":"Reference","title":"SphericalHarmonicModes.modeindex","text":"modeindex(mr::SphericalHarmonicModes.ModeRange, mode::Tuple)\n\nReturn the index of mode in the iterator mr. Raise an error if mode is not present in mr.\n\nExamples\n\njulia> r = LM(1:2, 1:2);\n\njulia> collect(r)\n3-element Vector{Tuple{Int64, Int64}}:\n (1, 1)\n (2, 1)\n (2, 2)\n\njulia> modeindex(r, (2,1))\n2\n\njulia> modeindex(r, (3,2))\nERROR: Mode with (l=3,m=2) is not included in the range given by LM(1:2, 1:2)\n\n\n\n\n\n","category":"method"},{"location":"#SphericalHarmonicModes.ofordering-Tuple{SphericalHarmonicModes.SHModeRange, SphericalHarmonicModes.SHModeRange}","page":"Reference","title":"SphericalHarmonicModes.ofordering","text":"SphericalHarmonicModes.ofordering(S, m)\n\nConvert m to the ordering of S.\n\nExamples\n\njulia> SphericalHarmonicModes.ofordering(LM(0:1), ML(0:1)) |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (1, -1)\n (0, 0)\n (1, 0)\n (1, 1)\n\njulia> SphericalHarmonicModes.ofordering(ML(0:1), ML(0:1)) |> collect\n4-element Vector{Tuple{Int64, Int64}}:\n (0, 0)\n (1, -1)\n (1, 0)\n (1, 1)\n\n\n\n\n\n","category":"method"}]
}
