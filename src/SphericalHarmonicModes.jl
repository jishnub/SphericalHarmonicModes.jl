module SphericalHarmonicModes

export LM,ML,L′L
export modeindex,l_range,m_range,l′_range

abstract type ModeRange end
abstract type SHModeRange <: ModeRange end
abstract type ModeProduct <: ModeRange end

Base.eltype(::SHModeRange) = Tuple{Int,Int}

struct LM <: SHModeRange
	l_min :: Int
	l_max :: Int
	m_min :: Int
	m_max :: Int

	function LM(l_min,l_max,m_min,m_max)
		check_if_st_range_is_valid(l_min,l_max,m_min,m_max)
		l_min = max(l_min,max(m_min,-m_max))
		new(l_min,l_max,m_min,m_max)
	end
end

struct ML <: SHModeRange
	l_min :: Int
	l_max :: Int
	m_min :: Int
	m_max :: Int

	function ML(l_min,l_max,m_min,m_max)
		check_if_st_range_is_valid(l_min,l_max,m_min,m_max)
		l_min = max(l_min,max(m_min,-m_max))
		new(l_min,l_max,m_min,m_max)
	end
end

struct L′L <: ModeProduct
	l_min :: Int
	l_max :: Int
	Δl_max :: Int
	l′_min :: Int
	l′_max :: Int

	function L′L(l_min::Integer,l_max::Integer,
		Δl_max::Integer,l′_min::Integer,l′_max::Integer)

		map(check_if_non_negative,(l_min,l_max,Δl_max,l′_min,l′_max))

		# Alter the ranges if necessary
		l_min = max(l_min,l′_min-Δl_max,0)
		l′_min = max(l_min-Δl_max,0,l′_min)
		l′_max = max(min(max(l_max+Δl_max,0),l′_max),l′_min)
		l_max = min(l_max,l′_max+Δl_max)

		new(l_min,l_max,Δl_max,l′_min,l′_max)
	end
end

Base.eltype(::L′L) = Tuple{Int,Int}

# Throw errors if values are invalid
struct OrderError{T} <: Exception
	var :: String
	low :: T
	high :: T
end

struct tRangeError <: Exception 
	l_max :: Integer
	t :: Integer
end

struct ModeMissingError{M,T} <: Exception
	s :: T
	t :: T
	m :: M
end

struct NegativeDegreeError{T<:Integer} <: Exception
	s :: T
end

struct InvalidModeError{ML<:Integer,Tt<:Integer} <: Exception
	s :: ML
	t :: Tt
end

struct NonContiguousError <: Exception
end

Base.showerror(io::IO, e::tRangeError) = print(io," t = ", e.t,
		" does not satisfy ",-e.l_max," ⩽ m ⩽ ",e.l_max)

Base.showerror(io::IO, e::OrderError) = print(io,e.var,"min = ",e.low,
	" is not consistent with ",e.var,"max = ",e.high)

Base.showerror(io::IO, e::ModeMissingError{<:SHModeRange}) = 
	print(io,"Mode with (l=",e.s,",m=",e.t,")",
	" is not included in the range given by ",e.m)

Base.showerror(io::IO, e::ModeMissingError{L′L}) = 
	print(io,"Mode with (l′=",e.s,",l=",e.t,")",
	" is not included in the range given by ",e.m)

Base.showerror(io::IO, e::NegativeDegreeError) = print(io,"l = ",e.s,
	" does not correspond to a valid mode")

Base.showerror(io::IO, e::InvalidModeError) = print(io,"(l=",e.s,",m=",e.t,")",
	" is not a valid mode. |m| <= l is not satisfied")

Base.showerror(io::IO, e::NonContiguousError) = print(io,
	"ModeRanges only support contiguous indexing")

function check_if_non_negative(l::Integer)
	l >= 0 || throw(NegativeDegreeError(l))
end

function check_if_st_range_is_valid(l_min,l_max,m_min,m_max)
	check_if_non_negative.((l_min,l_max))
	if m_min > m_max 
		throw(OrderError("t",m_min,m_max))
	end
	if l_min > l_max
		throw(OrderError("s",l_min,l_max))
	end
	if abs(m_max) > l_max
		throw(tRangeError(l_max,m_max))
	end
	if abs(m_min) > l_max
		throw(tRangeError(l_max,m_min))
	end
end

@inline function check_if_valid_mode(l::Integer,m::Integer)
	abs(m) <= l || throw(InvalidModeError(l,m))
end

@inline function check_if_mode_present(mr,l,m)
	(l,m) in mr || throw(ModeMissingError(l,m,mr))
end



# Constructors. Both ML and LM are constructed identically, 
# so we can dispatch on the supertype
(::Type{T})(l_min::Integer,l_max::Integer) where {T<:SHModeRange} = T(l_min,l_max,-l_max,l_max)
(::Type{T})(l::Integer) where {T<:SHModeRange} = T(l,l)

(::Type{T})(l_range::AbstractUnitRange{<:Integer},
	m_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(extrema(l_range)...,extrema(m_range)...)

(::Type{T})(l_range::AbstractUnitRange{<:Integer},m::Integer) where {T<:SHModeRange} = 
	T(extrema(l_range)...,m,m)

(::Type{T})(l::Integer,m_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(l,l,extrema(m_range)...)

(::Type{T})(l_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(extrema(l_range)...)

function L′L(l_min::Integer,l_max::Integer,Δl_max::Integer,
	l′_min::Integer=max(l_min-Δl_max,0))

	L′L(l_min,l_max,Δl_max,l′_min,l_max+Δl_max)
end

L′L(l_min::Integer,l_max::Integer,mr::SHModeRange,args...) = L′L(l_min,l_max,mr.l_max,args...)

L′L( l_range::AbstractUnitRange{<:Integer},Δl_max::Integer,
	l′_range::AbstractUnitRange{<:Integer}) = 
	L′L(extrema(l_range)...,Δl_max,extrema(l′_range)...)

L′L(l_min::Integer,l_max::Integer,Δl_max::Integer,
	l′_range::AbstractUnitRange{<:Integer}) = 
	L′L(l_min,l_max,Δl_max,extrema(l′_range)...)

L′L( l_range::AbstractUnitRange{<:Integer},Δl_max::Union{Integer,<:SHModeRange},
	args...) = L′L(extrema(l_range)...,Δl_max,args...)

# Get the ranges of the modes

@inline l_range(mr::SHModeRange) = mr.l_min:mr.l_max
@inline m_range(mr::SHModeRange) = mr.m_min:mr.m_max

@inline l_range(mr::L′L) = mr.l_min:mr.l_max
@inline l′_range(mr::L′L) = mr.l′_min:mr.l′_max

@inline l_range(mr::SHModeRange,m::Integer) = max(abs(m),mr.l_min):mr.l_max

@inline m_range(mr::SHModeRange,l::Integer) = max(-l,mr.m_min):min(l,mr.m_max)

@inline function l′_range(mr::L′L,l::Integer)
	max(l - mr.Δl_max,mr.l′_min):min(l + mr.Δl_max,mr.l′_max)
end

function _length(mr::LM)

	# We evaluate Sum[l_max - Max[Abs[t], l_min] + 1] piecewise in 
	# Mathematica. There are four cases to consider
	# depending on which of |t| and l_min is larger

	l_min,l_max,m_min,m_max = mr.l_min,mr.l_max,mr.m_min,mr.m_max

	N = 0

	up = min(-1, m_max)
	lo = max(m_min,-l_min)
	if up >= lo
		N += (1 + l_max - l_min)*(1 - lo + up)
	end
	
	up = min(m_max, l_min)
	lo = max(m_min, 0)
	if up >= lo
		N += (1 + l_max - l_min)*(1 - lo + up)
	end

	up = m_max
	lo = max(m_min, l_min + 1)
	if up >= lo
		N += div(-((1 + up - lo)*(-2 - 2l_max + up + lo)),2)
	end

	up = min(-1, -1 - l_min, m_max)
	lo = max(-l_max, m_min)
	if up >= lo
		N += div(-((-1 + lo - up)*(2 + 2l_max + lo + up)),2)
	end

	return N
end

function _length(mr::ML)

	# We evaluate Sum[min(s,m_max)-max(-s,m_min)+1] piecewise in 
	# Mathematica. There are four cases to consider
	# depending on s, m_max and m_min

	l_min,l_max,m_min,m_max = mr.l_min,mr.l_max,mr.m_min,mr.m_max

	N = 0

	up = min(-m_min, m_max,l_max)
	lo = l_min
	if up >= lo
		N += -(-1 + lo - up)*(1 + lo + up)
	end
	
	up = min(-m_min, l_max)
	lo = max(l_min, m_max + 1, -m_max)
	if up >= lo
		N += div(-((-1 + lo - up)*(2 + 2m_max + lo + up)),2)
	end

	up = min(m_max, l_max)
	lo = max(l_min, -m_min + 1, m_min)
	if up >= lo
		N += div(-((-1 + lo - up)*(2 - 2m_min + lo + up)),2)
	end

	up = l_max
	lo = max(-m_min + 1, m_max + 1,l_min)
	if up >= lo
		N += (1 + m_max - m_min)*(1 + up - lo)
	end

	return N
end

function _length(mr::L′L)

	# Number of modes is given by count(l′_range(mr,s) for s in l_range(mr))
	l_min,l_max,l′_min,l′_max = mr.l_min,mr.l_max,mr.l′_min,mr.l′_max
	Δl_max = mr.Δl_max
    
    N = 0

    up = min(l_max, l′_max - Δl_max)
    lo = max(l_min, l′_min + Δl_max)
    if up >= lo
    	N += (1 + 2*Δl_max)*(1 - lo + up)
    end

    up = min(l_max,l′_max + Δl_max)
    lo = max(l_min, l′_min + Δl_max, l′_max - Δl_max + 1)
    if up >= lo
    	N += div((-1 + lo - up)*(-2*(1 + Δl_max + l′_max) + lo + up),2)
    end

    up = min(l_max, l′_min + Δl_max - 1, l′_max - Δl_max)
    lo = max(l_min,l′_min - Δl_max)
    if up >= lo
    	N += div(-((-1 + lo - up)*(2 + 2Δl_max - 2l′_min + lo + up)),2)
    end

    up = min(l_max, l′_min + Δl_max - 1)
    lo = max(l_min,l′_max - Δl_max + 1)
    if up >= lo
    	N += (1 + l′_max - l′_min)*(1 - lo + up)
    end

    return N
end

Base.length(mr::ModeRange) = _length(mr)

Base.firstindex(mr::ModeRange) = 1
Base.lastindex(mr::ModeRange) = length(mr)

# Size and axes behave similar to vectors
@inline Base.size(mr::ModeRange) = (length(mr),)
@inline Base.size(mr::ModeRange,d::Integer) = d == 1 ? length(mr) : 1

@inline Base.axes(mr::ModeRange) = (Base.OneTo(length(mr)),)
@inline Base.axes(mr::ModeRange,d::Integer) = d == 1 ? Base.OneTo(length(mr)) : Base.OneTo(1)

# Convenience function to generate the first step in the iteration
@inline first_m(mr::LM) = mr.m_min
@inline first_l(mr::LM) = first(l_range(mr,first_m(mr)))
@inline first_l(mr::ML) = mr.l_min
@inline first_m(mr::ML) = first(m_range(mr,first_l(mr)))

@inline first_l(mr::L′L) = max(mr.l_min,mr.l′_min-mr.Δl_max)
@inline first_l′(mr::L′L) = first(l′_range(mr,first_l(mr)))

function Base.iterate(mr::LM, state=((first_l(mr),first_m(mr)), 1))

	(l,m), count = state

	if count > _length(mr)
		return nothing
	end

	if l == mr.l_max
		next_m = m + 1
		next_l = max(mr.l_min,abs(next_m))
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

	if m == min(mr.m_max,l)
		next_l = l + 1
		next_m = max(mr.m_min,-next_l)
	else
		next_l = l
		next_m = m + 1
	end

	return (l,m), ((next_l,next_m), count + 1)
end

function Base.iterate(mr::L′L,state=((first_l′(mr),first_l(mr)), 1))

	(l′,l),count = state
	if count > length(mr)
		return nothing
	end

	l′_range_l = l′_range(mr,l)

	if l′ == last(l′_range_l)
		next_l = l + 1
		next_l′ = first(l′_range(mr,next_l))
	else
		next_l = l
		next_l′ = l′ + 1
	end

	return (l′,l),((next_l′,next_l),count+1)
end

function _in((l,m)::Tuple{<:Integer,<:Integer},mr::ML)
	(abs(m)<=l) && (mr.l_min <= l <= mr.l_max) && (mr.m_min <= m <= mr.m_max) &&
	(m in m_range(mr,l))
end

function _in((l,m)::Tuple{<:Integer,<:Integer},mr::LM)
	(abs(m)<=l) && (mr.l_min <= l <= mr.l_max) && (mr.m_min <= m <= mr.m_max) &&
	(l in l_range(mr,m))
end

function _in((l′,l)::Tuple{<:Integer,<:Integer},mr::L′L)
	(mr.l_min <= l <= mr.l_max) && 
	(mr.l′_min <= l′ <= mr.l′_max) && 
	(l′ in l′_range(mr,l))
end

Base.in(T::Tuple,mr::ModeRange) = _in(T,mr)

Base.last(mr::LM) = (last(l_range(mr,mr.m_max)),mr.m_max)
Base.last(mr::ML) = (mr.l_max,last(m_range(mr,mr.m_max)))
Base.last(mr::L′L) = (last(l′_range(mr,mr.l_max)), mr.l_max)

function modeindex(mr::LM,l::Integer,m::Integer)
	# Check if the s and t supplied correspond to valid modes
	check_if_non_negative(l)
	check_if_valid_mode(l,m)
	check_if_mode_present(mr,l,m)

	Nskip = 0
	
	l_min,l_max = mr.l_min,mr.l_max
	m_min,m_max = mr.m_min,mr.m_max

	# Nskip = sum(length(l_range(mr,ti)) fot ti=mr.m_min:t-1)

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

	ind_l = l - first(l_range(mr,m)) + 1
	Nskip + ind_l
end

function modeindex(mr::LM,l::AbstractUnitRange{<:Integer},m::Integer)
	last(l) in l_range(mr,m) || throw(ModeMissingError(last(l),m,mr))
	start = modeindex(mr,first(l),m)
	start:start + length(l) - 1
end

modeindex(mr::LM,::Colon,m::Integer) = modeindex(mr,l_range(mr,m),m)

function modeindex(mr::ML,l::Integer,m::Integer)
	# Check if the s and t supplied correspond to valid modes
	check_if_non_negative(l)
	check_if_valid_mode(l,m)
	check_if_mode_present(mr,l,m)

	Nskip = 0
	
	l_min,l_max = mr.l_min,mr.l_max
	m_min,m_max = mr.m_min,mr.m_max

	# Nskip = sum(length(m_range(mr,si)) for si=mr.l_min:s-1)

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

	ind_m = m - first(m_range(mr,l)) + 1
	Nskip + ind_m
end

function modeindex(mr::ML,l::Integer,m::AbstractUnitRange{<:Integer})
	last(m) in m_range(mr,l) || throw(ModeMissingError(l,last(m),mr))
	start = modeindex(mr,l,first(m))
	start:start + length(m) - 1
end

modeindex(mr::ML,l::Integer,::Colon) = modeindex(mr,l,m_range(mr,l))

function modeindex(mr::L′L,l′::Integer,l::Integer)
	# Check if s and s′ supplied correspond to valid modes
	check_if_non_negative(l)
	check_if_non_negative(l′)
	check_if_mode_present(mr,l′,l)

	Nskip = 0

	l_min,l_max,l′_min,l′_max = mr.l_min,mr.l_max,mr.l′_min,mr.l′_max
	Δl_max = mr.Δl_max

	up = min(l - 1, l′_max - Δl_max)
	lo = max(l_min, l′_min + Δl_max)
	if up >= lo
		Nskip += (1 + 2Δl_max)*(1 - lo + up)
	end

	up = min(l - 1, l′_max + Δl_max)
	lo = max(l_min, l′_min + Δl_max, l′_max - Δl_max + 1)
	if up >= lo
		Nskip += div((-1 + lo - up)*(-2*(1 + Δl_max + l′_max) + lo + up),2)
	end

	up = min(l - 1, l′_min + Δl_max - 1, l′_max - Δl_max)
	lo = max(l_min, l′_min - Δl_max)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 + 2Δl_max - 2l′_min + lo + up)),2)
	end

	up = min(l - 1, l′_min + Δl_max - 1)
	lo = max(l_min, l′_max - Δl_max + 1)
	if up >= lo
		Nskip += (1 + l′_max - l′_min)*(1 - lo + up)
	end

	ind_l′ = l′ - first(l′_range(mr,l)) + 1
	Nskip + ind_l′
end

function modeindex(mr::L′L,l′::AbstractUnitRange{<:Integer},l::Integer)
	last(l′) in l′_range(mr,l) || throw(ModeMissingError(last(l′),l,mr))
	start = modeindex(mr,first(l′),l)
	start:start + length(l′) - 1
end

modeindex(mr::L′L,::Colon,l::Integer) = modeindex(mr,l′_range(mr,l),l)

modeindex(mr::ModeRange,T::Tuple{<:Any,<:Any}) = modeindex(mr,T...)
modeindex(mr::ModeRange,T::Vararg{<:Any,2}) = modeindex(mr,T...)
modeindex(mr::ModeRange,::Colon,::Colon) = Base.OneTo(length(mr))

# modeindex with two ranges works only if the values are contiguously stored in the iterator
# If the values are contiguous then it returns modeindex(l_min,m_min):modeindex(l_max,m_max)
# All (l,m) values need to be present in the iterator
# Some invalid values might be caught by internal modeindex calls
function modeindex(mr::ModeRange,l_range::AbstractUnitRange,m_range::AbstractUnitRange)
	m_min,m_max = extrema(m_range)
	l_min,l_max = extrema(l_range)
	firstind = modeindex(mr,l_min,m_min)
	lastind = modeindex(mr,l_max,m_max)

	if (lastind - firstind + 1) != length(l_range)*length(m_range)
		throw(NonContiguousError())
	end
	firstind:lastind
end

# Return the range of indices of m containing the entirety of mpart
# If mpart is contiguously stored in m then collect(m)[modeindex(mr,mpart)] == collect(mpart)
function modeindex(mr::T,mpart::T) where {T<:ModeRange}
	mode_start = first(mpart)
	mode_end = last(mpart)
	modeindex(mr,mode_start):modeindex(mr,mode_end)
end

# Display the iterators

function Base.show(io::IO, mr::SHModeRange)
	print(io,"(l=",l_range(mr),",m=",m_range(mr),")")
end

function Base.show(io::IO, mr::L′L)
	print(io,"(l=",l_range(mr),",Δl_max=",mr.Δl_max,
		",l′=",l′_range(mr),")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::LM)
	println("Spherical harmonic modes with l increasing faster than m")
	print(io,"(l_min = ",mr.l_min,", l_max = ",mr.l_max,
		", m_min = ",mr.m_min,", m_max = ",mr.m_max,")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::ML)
	println("Spherical harmonic modes with m increasing faster than l")
	print(io,"(l_min = ",mr.l_min,", l_max = ",mr.l_max,
		", m_min = ",mr.m_min,", m_max = ",mr.m_max,")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::L′L)
	println("Spherical harmonic modes (l′,l) where |l-Δl| ⩽ l′ ⩽ l+Δl "*
		"for 0 ⩽ Δl ⩽ Δl_max, l_min ⩽ l ⩽ l_max, and l′_min ⩽ l′ ⩽ l′_max")
	print(io,"(",mr.l′_min," ⩽ l′ ⩽ ",mr.l′_max," and ",
		mr.l_min," ⩽ l ⩽ ",mr.l_max,", with Δl_max = ",mr.Δl_max,")")
end

end # module
