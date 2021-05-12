
abstract type PartiallySpecifiedRange{T} <: AbstractUnitRange{Int} end
abstract type ZeroClampedRange{T} <: PartiallySpecifiedRange{T} end
Base.isempty(::PartiallySpecifiedRange) = false


"""
    SingleValuedRange(n::Int)

The range `n:n`.
"""
struct SingleValuedRange <: AbstractUnitRange{Int}
    n :: Int
end
Base.isempty(::SingleValuedRange) = false
Base.first(x::SingleValuedRange) = x.n
Base.last(x::SingleValuedRange) = x.n
Base.length(x::SingleValuedRange) = 1
Base.show(io::IO, x::SingleValuedRange) = print(io, repr(x.n), ":", repr(x.n))

function SingleValuedRange(r::AbstractUnitRange{<:Integer})
    length(r) == 1 || throw(ArgumentError("range must contain only one value"))
    SingleValuedRange(first(r))
end

"""
    ZeroTo(l::Int)

The range `0:l` for an `l ≥ 0`.
"""
struct ZeroTo{T} <: ZeroClampedRange{T}
    l :: Int

    function ZeroTo{T}(l) where {T}
        ensure_nonnegative(l)
        new{T}(l)
    end
end
ZeroTo(l) = ZeroTo{false}(l)
Base.first(::ZeroTo) = 0
Base.last(r::ZeroTo) = r.l
Base.show(io::IO, r::ZeroTo) = print(io, "0:", repr(r.l))

# conversion to false is always possible
ZeroTo{false}(z::ZeroTo) = ZeroTo{false}(z.l)
function ZeroTo{false}(z::AbstractUnitRange{<:Integer})
    first(z) == 0 || throw(ArgumentError("range does not begin at zero"))
    ZeroTo{false}(last(z))
end

function ZeroTo(r::AbstractUnitRange{<:Integer})
    first(r) == 0 || throw(ArgumentError("range must start at zero"))
    ZeroTo(last(r))
end

"""
    ToZero(l::Int)

The range `-l:0` for an `l ≥ 0`.
"""
struct ToZero{T} <: ZeroClampedRange{T}
    l :: Int

    function ToZero{T}(l) where {T}
        ensure_nonnegative(l)
        new{T}(l)
    end
end
ToZero(l) = ToZero{false}(l)
Base.first(r::ToZero) = -r.l
Base.last(::ToZero) = 0
Base.show(io::IO, r::ToZero) = print(io,"-",repr(r.l),":0")

# conversion to false is always possible
ToZero{false}(z::ToZero) = ToZero{false}(z.l)
function ToZero{false}(z::AbstractUnitRange{<:Integer})
    last(z) == 0 || throw(ArgumentError("range does not end at zero"))
    ToZero{false}(-first(z))
end

function ToZero(r::AbstractUnitRange{<:Integer})
    last(r) == 0 || throw(ArgumentError("range must end at zero"))
    ToZero(-first(r))
end

"""
    FullRange(l::Int)

The range `-l:l` for an `l ≥ 0`.
"""
struct FullRange{T} <: PartiallySpecifiedRange{T}
    l :: Int

    function FullRange{T}(l) where {T}
        ensure_nonnegative(l)
        new{T}(l)
    end
end
FullRange(l) = FullRange{false}(l)
Base.first(r::FullRange) = -r.l
Base.last(r::FullRange) = r.l
Base.show(io::IO, r::FullRange) = print(io, repr(-r.l),":",repr(r.l))

# conversion to false is always possible
FullRange{false}(z::FullRange) = FullRange{false}(z.l)
function FullRange{false}(z::AbstractUnitRange{<:Integer})
    last(z) == -first(z) || throw(ArgumentError("first element of range must be negative the last element"))
    FullRange{false}(last(z))
end

function FullRange(r::AbstractUnitRange{<:Integer})
    first(r) == -last(r) || throw(ArgumentError("first element of range must be negative the last element"))
    FullRange(last(r))
end

Base.intersect(a::FullRange, b::FullRange) = FullRange(min(maximum(a), maximum(b)))
Base.intersect(a::T, b::FullRange) where {T<:ZeroClampedRange} = T(min(a.l, b.l))
Base.intersect(a::FullRange, b::T) where {T<:ZeroClampedRange} = T(min(a.l, b.l))

Base.intersect(a::T, b::T) where {T<:ZeroClampedRange} = T(min(a.l, b.l))
Base.intersect(a::ZeroClampedRange, b::ZeroClampedRange) = ZeroTo(0)

function Base.intersect(a::SingleValuedRange, b::AbstractUnitRange{<:Integer})
    first(a) in b || return nothing
    return a
end
Base.intersect(b::AbstractUnitRange{<:Integer}, a::SingleValuedRange) = intersect(a, b)
function Base.intersect(a::SingleValuedRange, b::SingleValuedRange)
    first(a) == first(b) || return nothing
    return a
end

Base.intersect(a::SingleValuedRange, b::PartiallySpecifiedRange) = intersect(a, UnitRange(b))
Base.intersect(a::PartiallySpecifiedRange, b::SingleValuedRange) = intersect(UnitRange(a), b)

Base.promote_rule(::Type{<:PartiallySpecifiedRange}, ::Type{<:PartiallySpecifiedRange}) = UnitRange{Int}
Base.promote_rule(::Type{<:PartiallySpecifiedRange}, ::Type{SingleValuedRange}) = UnitRange{Int}
