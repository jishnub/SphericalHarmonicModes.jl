module SphericalHarmonicModes

import Base: @propagate_inbounds

export LM, ML, L2L1Triangle
export FullRange, ZeroTo, ToZero
export modeindex, l_range, m_range, l2_range, l1_range

firstlast(r::AbstractRange) = (first(r), last(r))

"""
    SphericalHarmonicModes.ModeRange

Abstract type whose subtypes are iterators over combinations of spherical harmonic modes.
This is the topmost node in the type hierarchy defined in this package.
"""
abstract type ModeRange end
"""
    SphericalHarmonicModes.SHModeRange <: SphericalHarmonicModes.ModeRange

Abstract supertype of iterators that loop over `(l,m)` pairs. 
The types [`LM`](@ref) and [`ML`](@ref) are subtypes of this.
"""
abstract type SHModeRange <: ModeRange end

Base.eltype(::SHModeRange) = Tuple{Int,Int}

"""
    LM(l_range::AbstractUnitRange{Int}, m_range::AbstractUnitRange{Int})
    LM(l_range::AbstractUnitRange{Int}, [T = FullRange]) where T<:Union{FullRange, ZeroTo, ToZero}

Return an iterator that loops over pairs of spherical harmonic modes `(l,m)`, 
with `l` increasing faster than `m`. The loop runs over all the valid modes that 
may be obtained from the ranges provided. If `m_range` is not specified, the loop 
runs over all valid values of `m` for each `l`. 
Neither `l_range` nor `m_range` may be empty.

Optionally `m_range` may be provided implicitly using the range specifiers 
[`FullRange`](@ref), [`ZeroTo`](@ref) and [`ToZero`](@ref).
Additionally, `l_range` may be of type [`ZeroTo`](@ref).
Iterators constructed using these special types would often permit optimizations.

!!! warning 
    An overlarge `l_range` will be curtailed to match the valid range compatible with 
    `m_range`. A smaller `l_range` than that compatible with `m_range` will raise an error.

# Examples
```jldoctest
julia> LM(0:1) |> collect
4-element Array{Tuple{Int64,Int64},1}:
 (1, -1)
 (0, 0)
 (1, 0)
 (1, 1)

julia> LM(0:1, 1:1) |> collect
1-element Array{Tuple{Int64,Int64},1}:
 (1, 1)

julia> r = LM(ZeroTo(1), FullRange);

julia> r |> collect
4-element Array{Tuple{Int64,Int64},1}:
 (1, -1)
 (0, 0)
 (1, 0)
 (1, 1)
```

See also: [`ML`](@ref)
"""
struct LM{LT,MT} <: SHModeRange
    l_range :: LT
    m_range :: MT

    function LM(l_range::AbstractUnitRange{Int}, m_range::AbstractUnitRange{Int})
        ensure_nonempty(l_range)
        ensure_nonempty(m_range)
        
        l_min,l_max = firstlast(l_range)
        m_min,m_max = firstlast(m_range)

        check_if_lm_range_is_valid(l_min,l_max,m_min,m_max)
        
        if max(m_min,-m_max) > l_min
            l_min = max(m_min,-m_max)
            l_range = l_min:l_max
        end

        new{typeof(l_range),typeof(m_range)}(l_range, m_range)
    end
end

"""
    ML(l_range::AbstractUnitRange{Int}, m_range::AbstractUnitRange{Int})
    ML(l_range::AbstractUnitRange{Int}, [T = FullRange]) where T<:Union{FullRange, ZeroTo, ToZero}

Return an iterator that loops over pairs of spherical harmonic modes `(l,m)`, 
with `m` increasing faster than `l`. The loop runs over all the valid modes that 
may be obtained from the ranges provided.  If `m_range` is not specified, the loop 
runs over all valid values of `m` for each `l`.
Neither `l_range` nor `m_range` may be empty.

Optionally `m_range` may be provided implicitly using the range specifiers 
[`FullRange`](@ref), [`ZeroTo`](@ref) and [`ToZero`](@ref).
Additionally `l_range` may be of type [`ZeroTo`](@ref).
Iterators constructed using these special types would often permit optimizations.

!!! warning 
    An overlarge `l_range` will be curtailed to match the valid range compatible with 
    `m_range`. A smaller `l_range` than that compatible with `m_range` will raise an error.

# Examples
```jldoctest
julia> ML(0:1) |> collect
4-element Array{Tuple{Int64,Int64},1}:
 (0, 0)
 (1, -1)
 (1, 0)
 (1, 1)

julia> ML(0:1, 1:1) |> collect
1-element Array{Tuple{Int64,Int64},1}:
 (1, 1)

julia> r = ML(ZeroTo(1), FullRange);

julia> r |> collect
4-element Array{Tuple{Int64,Int64},1}:
 (0, 0)
 (1, -1)
 (1, 0)
 (1, 1)
```

See also: [`LM`](@ref)
"""
struct ML{LT,MT} <: SHModeRange
    l_range :: LT
    m_range :: MT

    function ML(l_range::AbstractUnitRange{Int}, m_range::AbstractUnitRange{Int})
        ensure_nonempty(l_range)
        ensure_nonempty(m_range)

        l_min,l_max = firstlast(l_range)
        m_min,m_max = firstlast(m_range)
        
        check_if_lm_range_is_valid(l_min,l_max,m_min,m_max)
        
        if max(m_min,-m_max) > l_min
            l_min = max(m_min,-m_max)
            l_range = l_min:l_max
        end

        new{typeof(l_range),typeof(m_range)}(l_range, m_range)
    end
end

"""
    L2L1Triangle(l1_min::Int, l1_max::Int, Δl_max::Int, l2_min::Int = max(0, l1_min - Δl_max), l2_max = l1_max + Δl_max)
    L2L1Triangle(l1_range::AbstractUnitRange{Int}, Δl_max::Int, l2_range::AbstractUnitRange{Int})

Return an iterator that loops over pairs of `(l2,l1)` where `l1` lies in `l1_range`, 
`l2` lies in `l2_range`, and `l2` and `l1` obey the triangle condition 
`max(0, l1 - Δl_max) ⩽ l2 ⩽ l1 + Δl_max`.
If `l2_range` is not specified, it defaults to the maximal range permissible.

!!! warning 
    The ranges `l1_range` and `l2_range` will be curtailed to the minimal permissible subsets.

# Examples
```jldoctest
julia> L2L1Triangle(1:2, 2) |> collect
9-element Array{Tuple{Int64,Int64},1}:
 (0, 1)
 (1, 1)
 (2, 1)
 (3, 1)
 (0, 2)
 (1, 2)
 (2, 2)
 (3, 2)
 (4, 2)

julia> L2L1Triangle(2:3, 1) |> collect
6-element Array{Tuple{Int64,Int64},1}:
 (1, 2)
 (2, 2)
 (3, 2)
 (2, 3)
 (3, 3)
 (4, 3)

julia> L2L1Triangle(2:3, 1, 2:3) |> collect
4-element Array{Tuple{Int64,Int64},1}:
 (2, 2)
 (3, 2)
 (2, 3)
 (3, 3)
```
"""
struct L2L1Triangle <: ModeRange
    l1_min :: Int
    l1_max :: Int
    Δl_max :: Int
    l2_min :: Int
    l2_max :: Int

    function L2L1Triangle(l1_min::Integer,l1_max::Integer,
        Δl_max::Integer,l2_min::Integer = max(0, l1_min - Δl_max),
        l2_max::Integer = l1_max + Δl_max)

        map(ensure_nonnegative,(l1_min,l1_max,Δl_max,l2_min,l2_max))

        (l1_min > l1_max) && throw_ordererror("l1",l1_min,l1_max)
        (l2_min > l2_max) && throw_ordererror("l2",l2_min,l2_max)
        
        (l2_min > l1_max + Δl_max) && 
        throw(ArgumentError(
            "l2_min = $l2_min is greater than l1_max + Δl_max = $(l1_max + Δl_max)"*
            " for l1_max = $l1_max and Δl_max = $Δl_max"))
        (l2_max < l1_min - Δl_max) && 
        throw(ArgumentError(
            "l2_max = $l2_max is less than l1_min - Δl_max = $(l1_min - Δl_max)"*
            " for l1_max = $l1_max and Δl_max = $Δl_max"))

        # Trim the range if necessary
        if l2_min < l1_min - Δl_max
            l2_min = l1_min - Δl_max
        end
        if l2_max > l1_max + Δl_max
            l2_max = l1_max + Δl_max
        end
        
        (l2_min > l2_max) && throw_ordererror("l2",l2_min,l2_max)

        if l1_min < l2_min - Δl_max
            l1_min = l2_min - Δl_max
        end
        if l1_max > l2_max + Δl_max
            l1_max = l2_max + Δl_max
        end

        (l1_min > l1_max) && throw_ordererror("l1",l1_min,l1_max)
        
        new(l1_min,l1_max,Δl_max,l2_min,l2_max)
    end
end
Base.eltype(::L2L1Triangle) = Tuple{Int,Int}

throw_mboundserror(l_max, m) = 
    throw(ArgumentError(" m = $m does not satisfy -$l_max ⩽ m ⩽ $l_max"))

ensure_nonempty(r) = isempty(r) && throw(ArgumentError("range of modes must not be empty, received $r"))

struct ModeMissingError{M,T} <: Exception
    firstmode :: T
    secondmode :: T
    itr :: M
end

Base.showerror(io::IO, e::ModeMissingError{<:SHModeRange}) = 
    print(io,"Mode with (l=",e.firstmode, ",m=",e.secondmode,")",
    " is not included in the range given by ",e.itr)

Base.showerror(io::IO, e::ModeMissingError{L2L1Triangle}) = 
    print(io,"Mode with (l2=",e.firstmode,",l1=",e.secondmode,")",
    " is not included in the range given by ",e.itr)

function ensure_nonnegative(l::Integer)
    l >= zero(l) || throw(
        ArgumentError("l = $l does not correspond to a valid mode"))
end

function check_if_lm_range_is_valid(l_min, l_max, m_min, m_max)
    map(ensure_nonnegative, l_min)
    
    if abs(m_max) > l_max
        throw_mboundserror(l_max, m_max)
    end
    
    if abs(m_min) > l_max
        throw_mboundserror(l_max, m_min)
    end
end

@inline function check_if_mode_present(mr, l, m)
    (l,m) in mr || throw(ModeMissingError(l,m,mr))
end

abstract type PartiallySpecifiedRange <: AbstractUnitRange{Int} end
abstract type ZeroClampedRange <: PartiallySpecifiedRange end
Base.isempty(::PartiallySpecifiedRange) = false

"""
    ZeroTo(l::Int)

The range `0:l` for an `l ≥ 0`.
"""
struct ZeroTo <: ZeroClampedRange
    l :: Int

    function ZeroTo(l::Int)
        ensure_nonnegative(l)
        new(l)
    end
end
Base.isempty(::ZeroTo) = false
Base.first(::ZeroTo) = 0
Base.last(r::ZeroTo) = r.l
Base.show(io::IO, r::ZeroTo) = print(io, "ZeroTo(", repr(r.l), ")")


"""
    ToZero(l::Int)

The range `-l:0` for an `l ≥ 0`.
"""
struct ToZero <: ZeroClampedRange
    l :: Int

    function ToZero(l)
        ensure_nonnegative(l)
        new(l)
    end
end
Base.first(r::ToZero) = -r.l
Base.last(::ToZero) = 0
Base.show(io::IO, r::ToZero) = print(io,"-",repr(r.l),":0")

"""
    FullRange(l::Int)

The range `-l:l` for an `l ≥ 0`.
"""
struct FullRange <: PartiallySpecifiedRange
    l :: Int

    function FullRange(l)
        ensure_nonnegative(l)
        new(l)
    end
end
Base.first(r::FullRange) = -r.l
Base.last(r::FullRange) = r.l
Base.show(io::IO, r::FullRange) = print(io, "-",repr(r.l),":",repr(r.l))

Base.intersect(a::T, b::FullRange) where {T<:ZeroClampedRange} = T(min(a.l, b.l))
Base.intersect(a::FullRange, b::T) where {T<:ZeroClampedRange} = T(min(a.l, b.l))

Base.intersect(a::T, b::T) where {T<:ZeroClampedRange} = T(min(a.l, b.l))
Base.intersect(a::ZeroClampedRange, b::ZeroClampedRange) = ZeroTo(0)

for DT in [:LM, :ML]
    @eval function $DT(l_range, ::Type{MT}) where {MT<:PartiallySpecifiedRange}
        ensure_nonempty(l_range)
        m_range = MT(last(l_range))
        $DT(l_range, m_range)
    end
    @eval $DT(l_range) = $DT(l_range, FullRange)
    
    @eval $DT(m::$DT) = m
    @eval $DT(m::SHModeRange) = $DT(l_range(m), m_range(m))
    
    @eval Base.:(==)(a::$DT, b::$DT) = l_range(a) == l_range(b) && m_range(a) == m_range(b)
end

"""
    SphericalHarmonicModes.flip(mr::SphericalHarmonicModes.SHModeRange)

Return an iterator that flips the order in which the modes `(l,m)` are iterated over.
`flip(::LM)` will return an `ML` iterator and vice versa.

# Examples
```jldoctest
julia> LM(0:1) |> collect
4-element Array{Tuple{Int64,Int64},1}:
 (1, -1)
 (0, 0)
 (1, 0)
 (1, 1)

julia> SphericalHarmonicModes.flip(LM(0:1)) |> collect
4-element Array{Tuple{Int64,Int64},1}:
 (0, 0)
 (1, -1)
 (1, 0)
 (1, 1)

julia> SphericalHarmonicModes.flip(LM(0:1)) == ML(0:1)
true
```
"""
flip(m::LM) = ML(m)
flip(m::ML) = LM(m)

function L2L1Triangle(l_min::Integer, l_max::Integer, mr::SHModeRange, args...)
    Δ = last(l_range(mr))
    L2L1Triangle(l_min, l_max, Δ, args...)
end

function L2L1Triangle(l1_min::Integer, l1_max::Integer, Δl_max::Integer,
    l2_range::AbstractUnitRange{<:Integer})

    l2_min, l2_max = firstlast(l2_range)
    L2L1Triangle(l1_min, l1_max, Δl_max, l2_min, l2_max)
end

function L2L1Triangle(l1_range::AbstractUnitRange{<:Integer}, args...)
    
    l1_min, l1_max = firstlast(l1_range)
    L2L1Triangle(l1_min, l1_max, args...)
end

# Get the ranges of the modes

"""
    l_range(mr::SphericalHarmonicModes.SHModeRange)

Return the range of `l` spanned by the iterator.
"""
l_range(mr::SHModeRange) = mr.l_range

"""
    m_range(mr::SphericalHarmonicModes.SHModeRange)

Return the range of `m` spanned by the iterator.
"""
m_range(mr::SHModeRange) = mr.m_range

"""
    l1_range(mr::L2L1Triangle)

Return the range of `l1` spanned by the iterator.
"""
l1_range(mr::L2L1Triangle) = mr.l1_min:mr.l1_max

"""
    l2_range(mr::L2L1Triangle)

Return the range of `l2` spanned by the iterator.
"""
l2_range(mr::L2L1Triangle) = mr.l2_min:mr.l2_max

"""
    l_range(mr::SphericalHarmonicModes.SHModeRange, m::Integer)

Return the subsection of the range of `l` spanned by the iterator for which
`(l,m)` is a valid spherical harmonic mode.

# Examples
```jldoctest
julia> r = LM(1:2, 1:2);

julia> collect(r)
3-element Array{Tuple{Int64,Int64},1}:
 (1, 1)
 (2, 1)
 (2, 2)

julia> l_range(r, 1)
1:2

julia> l_range(r, 2)
2:2
```
"""
@inline function l_range(mr::SHModeRange, m::Integer)
    l_min, l_max = firstlast(l_range(mr))
    max(abs(m), l_min):l_max
end

"""
    m_range(mr::SphericalHarmonicModes.SHModeRange, l::Integer)

Return the subsection of the range of `m` spanned by the iterator for which
`(l,m)` is a valid spherical harmonic mode.

# Examples
```jldoctest
julia> r = LM(1:2, 1:2);

julia> collect(r)
3-element Array{Tuple{Int64,Int64},1}:
 (1, 1)
 (2, 1)
 (2, 2)

julia> m_range(r, 1)
1:1

julia> m_range(r, 2)
1:2
```
"""
@inline function m_range(mr::SHModeRange, l::Integer)
    m_min, m_max = firstlast(m_range(mr))
    max(-l, m_min):min(l, m_max)
end
# Optimized methods for special m-ranges
@inline function m_range(mr::Union{LM{<:Any,T},ML{<:Any,T}}, l::Integer) where {T<:PartiallySpecifiedRange}
    T(l)
end

"""
    l2_range(mr::L2L1Triangle, l1::Integer)

Return a subsection of the range of `l2` spanned by the iterator for which 
`(l1,l2)` satisfy `l1 - mr.Δl_max ⩽ l2 ⩽ l1 + mr.Δl_max`.

# Examples
```jldoctest
julia> r = L2L1Triangle(1:2, 1);

julia> collect(r)
6-element Array{Tuple{Int64,Int64},1}:
 (0, 1)
 (1, 1)
 (2, 1)
 (1, 2)
 (2, 2)
 (3, 2)

julia> l2_range(r, 1)
0:2

julia> l2_range(r, 2)
1:3
```
"""
@inline function l2_range(mr::L2L1Triangle, l1::Integer)
    max(l1 - mr.Δl_max, mr.l2_min):min(l1 + mr.Δl_max, mr.l2_max)
end

# Convenience function to generate the first step in the iteration
first_m(mr::LM) = first(m_range(mr))
first_l(mr::LM) = first(l_range(mr, first_m(mr)))
first_l(mr::ML) = first(l_range(mr))
first_m(mr::ML) = first(m_range(mr, first_l(mr)))

first_l1(mr::L2L1Triangle) = max(mr.l1_min, mr.l2_min - mr.Δl_max)
first_l2(mr::L2L1Triangle) = first(l2_range(mr,first_l1(mr)))

function Base.iterate(mr::LM, state=((first_l(mr),first_m(mr)), 1))

    (l,m), count = state

    if count > length(mr)
        return nothing
    end

    l_min, l_max = firstlast(l_range(mr))

    if l == l_max
        next_m = m + 1
        next_l = max(l_min, abs(next_m))
    else
        next_m = m
        next_l = l + 1
    end

    return (l,m), ((next_l,next_m), count + 1)
end

function Base.iterate(mr::ML, state=((first_l(mr),first_m(mr)), 1))
    
    (l,m), count = state

    if count > length(mr)
        return nothing
    end

    m_min, m_max = firstlast(m_range(mr))

    if m == min(m_max,l)
        next_l = l + 1
        next_m = max(m_min,-next_l)
    else
        next_l = l
        next_m = m + 1
    end

    return (l,m), ((next_l,next_m), count + 1)
end

function Base.iterate(mr::L2L1Triangle, state=((first_l2(mr),first_l1(mr)), 1))

    (l2,l1),count = state
    if count > length(mr)
        return nothing
    end

    l2_range_l1 = l2_range(mr,l1)

    if l2 == last(l2_range_l1)
        next_l1 = l1 + 1
        next_l2 = first(l2_range(mr,next_l1))
    else
        next_l1 = l1
        next_l2 = l2 + 1
    end

    return (l2,l1),((next_l2,next_l1),count+1)
end

function Base.in((l,m)::NTuple{2,Integer}, mr::SHModeRange)
    l in l_range(mr) && m in m_range(mr,l)
end

function Base.in((l2,l1)::NTuple{2,Integer}, mr::L2L1Triangle)
    (mr.l1_min <= l1 <= mr.l1_max) && 
    (mr.l2_min <= l2 <= mr.l2_max) && 
    (l2 in l2_range(mr,l1))
end

function Base.last(mr::LM)
    m_max = last(m_range(mr))
    (last(l_range(mr, m_max)), m_max)
end
function Base.last(mr::ML)
    l_max = last(l_range(mr))
    (l_max, last(m_range(mr, l_max)) )
end
Base.last(mr::L2L1Triangle) = (last(l2_range(mr,mr.l1_max)), mr.l1_max)

"""
    modeindex(mr::SphericalHarmonicModes.ModeRange, mode::Tuple)

Return the index of `mode` in the iterator `mr`. Raise an error if `mode` 
is not present in `mr`.

# Examples
```jldoctest
julia> r = LM(1:2, 1:2);

julia> collect(r)
3-element Array{Tuple{Int64,Int64},1}:
 (1, 1)
 (2, 1)
 (2, 2)

julia> modeindex(r, (2,1))
2

julia> modeindex(r, (3,2))
ERROR: Mode with (l=3,m=2) is not included in the range given by LM(1:2, 1:2)
```
"""
@propagate_inbounds modeindex(mr::ModeRange, T::Tuple{Vararg{Integer}}) = modeindex(mr, T...)

# Nskip = sum(length(l_range(mr,m_i)) fot m_i=mr.m_min:m-1)
@inline function nskip(mr::LM, m)
    Nskip = 0
    
    l_min, l_max = firstlast(l_range(mr))
    m_min, m_max = firstlast(m_range(mr))

    up = min(m - 1, -1)
    lo = max(m_min, -l_min)
    if up >= lo
        Nskip += (1 + l_max - l_min)*(1 - lo + up)
    end

    up = min(m - 1, l_min)
    lo = max(m_min, 0)
    if up >= lo
        Nskip += (1 + l_max - l_min)*(1 - lo + up)
    end

    up = min(m - 1, m_max, l_max)
    lo = max(m_min, l_min + 1)
    if up >= lo
        Nskip += div((-1 + lo - up)*(-2*(1 + l_max) + lo + up),2)
    end

    up = min(m - 1, -1, -l_min - 1)
    lo = max(m_min, -l_max)
    if up >= lo
        Nskip += div(-((-1 + lo - up)*(2 + 2l_max + lo + up)),2)
    end
    Nskip
end

@inline function nskip(mr::LM{ZeroTo, ZeroTo}, m)
    l_max = last(l_range(mr))
    div( (3 + 2l_max -m)*m , 2)
end
@inline function nskip(mr::LM{ZeroTo, ToZero}, m)
    l_max = last(l_range(mr))
    div( (l_max + m)*(l_max + m + 1) , 2)
end
@inline function nskip(mr::LM{ZeroTo, FullRange}, m)
    l_max = last(l_range(mr))
    if m <= 0
        Nskip = div( (l_max + m)*(l_max + m + 1) , 2)
    else
        Nskip = div(l_max*(l_max + 1) + (3 + 2l_max -m)*m , 2)
    end
    return Nskip
end
@inline function nskip(mr::LM{<:AbstractUnitRange{Int}, ZeroTo}, m)
    l_min, l_max = firstlast(l_range(mr))
    Nl = l_max - l_min + 1
    
    if iszero(m)
        Nskip = 0
    elseif m - 1 <= l_min
        Nskip = Nl * m
    else
        Nskip = Nl * (l_min + 1) + div( (m - l_min - 1)*(2 + 2l_max - l_min - m) , 2)
    end

    return Nskip
end
@inline function nskip(mr::LM{<:AbstractUnitRange{Int}, ToZero}, m)
    l_min = first(l_range(mr))
    l_max = last(l_range(mr))
    Nl = l_max - l_min + 1
    
    if m == -l_max
        Nskip = 0
    elseif m <= -l_min
        Nskip = div((l_max + m)*(l_max + m + 1), 2)
    else
        Nskip = div((Nl - 1) * Nl, 2) + Nl * (m + l_min)
    end

    return Nskip
end
@inline function nskip(mr::LM{<:AbstractUnitRange{Int}, FullRange}, m)
    l_min, l_max = firstlast(l_range(mr))
    Nl = l_max - l_min + 1

    if m == -l_max
        Nskip = 0
    elseif m <= -l_min
        Nskip = div((l_max + m)*(l_max + m + 1), 2)
    elseif m <= l_min + 1
        Nskip = div((Nl - 1) * Nl, 2) + Nl * (m + l_min)
    else
        Nskip = Nl * (2l_min + 1) + 
            div( (Nl - 1) * Nl + (m - l_min - 1)*(2 + 2l_max - l_min - m) , 2)
    end

    return Nskip
end

@inline function modeindex(mr::LM, l::Integer, m::Integer)
    @boundscheck check_if_mode_present(mr, l, m)

    nskip(mr, m) + l - first(l_range(mr,m)) + 1
end

# Nskip = sum(length(m_range(mr,l_i)) for l_i=mr.l_min:l-1)
@inline function nskip(mr::ML, l)

    Nskip = 0
    
    l_min, l_max = firstlast(l_range(mr))
    m_min, m_max = firstlast(m_range(mr))

    up = min(-m_min, m_max, l - 1)
    lo = l_min
    if up >= lo
        Nskip += -(-1 + lo - up)*(1 + lo + up)
    end

    up = min(-m_min, l - 1)
    lo = max(l_min, m_max + 1, -m_max)
    if up >= lo
        Nskip += div(-((-1 + lo - up)*(2 + 2m_max + lo + up)),2)
    end

    up = min(m_max, l - 1)
    lo = max(l_min, -m_min + 1, m_min)
    if up >= lo
        Nskip += div(-((-1 + lo - up)*(2 - 2m_min + lo + up)),2)
    end

    up = l - 1
    lo = max(-m_min + 1, m_max + 1, l_min)
    if up >= lo
        Nskip += (1 + m_max - m_min)*(1 - lo + up)
    end

    Nskip
end
@inline function nskip(mr::ML{<:AbstractUnitRange{Int},FullRange}, l)
    l_min = first(l_range(mr))
    (l - l_min)*(l + l_min)
end

@inline function nskip(mr::ML{<:AbstractUnitRange{Int},<:ZeroClampedRange}, l)
    l_min = first(l_range(mr))
    div((l - l_min)*(l + l_min + 1), 2)
end

@inline function modeindex(mr::ML, l::Integer, m::Integer)
    @boundscheck check_if_mode_present(mr, l, m)

    nskip(mr, l) + m - first(m_range(mr,l)) + 1
end

@inline function nskip(mr::L2L1Triangle, l1)

    Nskip = 0

    l_min,l_max,l2_min,l2_max = mr.l1_min,mr.l1_max,mr.l2_min,mr.l2_max
    Δl_max = mr.Δl_max

    up = min(l1 - 1, l2_max - Δl_max)
    lo = max(l_min, l2_min + Δl_max)
    if up >= lo
        Nskip += (1 + 2Δl_max)*(1 - lo + up)
    end

    up = min(l1 - 1, l2_max + Δl_max)
    lo = max(l_min, l2_min + Δl_max, l2_max - Δl_max + 1)
    if up >= lo
        Nskip += div((-1 + lo - up)*(-2*(1 + Δl_max + l2_max) + lo + up),2)
    end

    up = min(l1 - 1, l2_min + Δl_max - 1, l2_max - Δl_max)
    lo = max(l_min, l2_min - Δl_max)
    if up >= lo
        Nskip += div(-((-1 + lo - up)*(2 + 2Δl_max - 2l2_min + lo + up)),2)
    end

    up = min(l1 - 1, l2_min + Δl_max - 1)
    lo = max(l_min, l2_max - Δl_max + 1)
    if up >= lo
        Nskip += (1 + l2_max - l2_min)*(1 - lo + up)
    end

    Nskip
end

@inline function modeindex(mr::L2L1Triangle, l2::Integer, l1::Integer)
    # Check if l1 and l2 supplied correspond to valid modes
    @boundscheck map(ensure_nonnegative,(l1,l2))
    @boundscheck check_if_mode_present(mr,l2,l1)

    nskip(mr, l1) + l2 - first(l2_range(mr,l1)) + 1
end

Base.length(mr::ModeRange) = @inbounds modeindex(mr, last(mr))

# Optimized definition
function Base.length(mr::Union{LM{<:AbstractUnitRange{Int}, FullRange}, ML{<:AbstractUnitRange{Int}, FullRange}})
    # 2l + 1 m's for each l
    l_min, l_max = firstlast(l_range(mr))
    (l_max + 1)^2 - l_min^2
end
function Base.length(mr::Union{LM{<:AbstractUnitRange{Int}, <:ZeroClampedRange}, ML{<:AbstractUnitRange{Int}, <:ZeroClampedRange}})
    # l + 1 m's for each l
    l_min, l_max = firstlast(l_range(mr))
    div((1 + l_max - l_min)*(2 + l_max + l_min),2)
end

Base.firstindex(mr::ModeRange) = 1
Base.lastindex(mr::ModeRange) = length(mr)

# Size and axes behave similar to vectors
@inline Base.size(mr::ModeRange) = (length(mr),)
@inline Base.size(mr::ModeRange, d::Integer) = d == 1 ? length(mr) : d > 1 ? 1 : throw(BoundsError(size(mr),d))

@inline Base.axes(mr::ModeRange) = (Base.OneTo(length(mr)),)
@inline Base.axes(mr::ModeRange, d::Integer) = d == 1 ? Base.OneTo(length(mr)) : d > 1 ? Base.OneTo(1) : throw(BoundsError(axes(mr),d))

Base.keys(mr::ModeRange) = Base.OneTo(length(mr))

# Intersect ranges

for DT in [:LM, :ML]
    @eval function Base.intersect(mr1::$DT, mr2::$DT)
        lr = intersect(l_range(mr1), l_range(mr2))
        mr = intersect(m_range(mr1), m_range(mr2))
        (isempty(lr) || isempty(mr)) && return nothing
        $DT(lr, mr)
    end
end

# Display the iterators

function Base.show(io::IO, mr::LM)
    print(io,"LM(",l_range(mr),", ",m_range(mr),")")
end

function Base.show(io::IO, mr::ML)
    print(io,"ML(",l_range(mr),", ",m_range(mr),")")
end

function Base.show(io::IO, mr::L2L1Triangle)
    print(io,"L2L1Triangle(",l1_range(mr),", ",mr.Δl_max,
        ", ",l2_range(mr),")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::LM)
    println(io, "Spherical harmonic modes with l increasing faster than m")
    print(io,"(l_min = ",first(l_range(mr)),", l_max = ",last(l_range(mr)),
        ", m_min = ",first(m_range(mr)),", m_max = ",last(m_range(mr)),")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::ML)
    println(io, "Spherical harmonic modes with m increasing faster than l")
    print(io,"(l_min = ",first(l_range(mr)),", l_max = ",last(l_range(mr)),
        ", m_min = ",first(m_range(mr)),", m_max = ",last(m_range(mr)),")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::L2L1Triangle)
    print(io, "Spherical harmonic modes (l2,l1) that satisfy ",
        "l1 - ",mr.Δl_max," ⩽ l2 ⩽ l1 + ",mr.Δl_max, ", with ",
        first(l2_range(mr))," ⩽ l2 ⩽ ",last(l2_range(mr))," and ",
        first(l1_range(mr))," ⩽ l1 ⩽ ",last(l1_range(mr))       
    )
end

end # module
