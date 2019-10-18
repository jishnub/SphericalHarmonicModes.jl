module SphericalHarmonicModes

export SHModeRange,st,ts,modeindex,s_valid_range,t_valid_range,s_range,t_range,
s′s,s′_range,s′_valid_range,_length

abstract type ModeRange end
abstract type SHModeRange <: ModeRange end
abstract type ModeProduct <: ModeRange end

Base.eltype(::ModeRange) = Tuple{Int,Int}

# Throw errors if values are invalid
struct OrderError{T} <: Exception
	var :: String
	low :: T
	high :: T
end

struct tRangeError <: Exception 
	smax :: Integer
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

struct InvalidModeError{Ts<:Integer,Tt<:Integer} <: Exception
	s :: Ts
	t :: Tt
end

Base.showerror(io::IO, e::tRangeError) = print(io," t = ", e.t,
		" does not satisfy ",-e.smax," ⩽ t ⩽ ",e.smax)

Base.showerror(io::IO, e::OrderError) = print(io,e.var,"min = ",e.low,
	" is not consistent with ",e.var,"max = ",e.high)

Base.showerror(io::IO, e::ModeMissingError{<:SHModeRange}) = 
	print(io,"Mode with (s=",e.s,",t=",e.t,")",
	" is not included in the range given by ",e.m)

Base.showerror(io::IO, e::ModeMissingError{<:ModeProduct}) = 
	print(io,"Mode with (s′=",e.s,",s=",e.t,")",
	" is not included in the range given by ",e.m)

Base.showerror(io::IO, e::NegativeDegreeError) = print(io,"s = ",e.s,
	" does not correspond to a valid mode")

Base.showerror(io::IO, e::InvalidModeError) = print(io,"(s=",e.s,",t=",e.t,")",
	" is not a valid mode. |t| <= s is not satisfied")

function check_if_non_negative(s::Integer)
	s >= 0 || throw(NegativeDegreeError(s))
end

function check_if_st_range_is_valid(smin,smax,tmin,tmax)
	check_if_non_negative.((smin,smax))
	if tmin > tmax 
		throw(OrderError("t",tmin,tmax))
	end
	if smin > smax
		throw(OrderError("s",smin,smax))
	end
	if abs(tmax) > smax
		throw(tRangeError(smax,tmax))
	end
	if abs(tmin) > smax
		throw(tRangeError(smax,tmin))
	end
end

@inline function check_if_valid_mode(s::Integer,t::Integer)
	abs(t) <= s || throw(InvalidModeError(s,t))
end

@inline function check_if_mode_present(m,s,t)
	(s,t) in m || throw(ModeMissingError(s,t,m))
end

struct st <: SHModeRange
	smin :: Int
	smax :: Int
	tmin :: Int
	tmax :: Int

	function st(smin,smax,tmin,tmax)
		check_if_st_range_is_valid(smin,smax,tmin,tmax)
		smin = max(smin,max(tmin,-tmax))
		new(smin,smax,tmin,tmax)
	end
end

struct ts <: SHModeRange
	smin :: Int
	smax :: Int
	tmin :: Int
	tmax :: Int

	function ts(smin,smax,tmin,tmax)
		check_if_st_range_is_valid(smin,smax,tmin,tmax)
		smin = max(smin,max(tmin,-tmax))
		new(smin,smax,tmin,tmax)
	end
end

struct s′s <: ModeProduct
	smin :: Int
	smax :: Int
	Δs_max :: Int
	s′min :: Int
	s′max :: Int

	function s′s(smin::Integer,smax::Integer,
		Δs_max::Integer,s′min::Integer,s′max::Integer)

		check_if_non_negative.((smin,smax,Δs_max,s′min,s′max))

		# Alter the ranges if necessary
		smin = max(smin,s′min-Δs_max,0)
		s′min = max(smin-Δs_max,0,s′min)
		s′max = max(min(max(smax+Δs_max,0),s′max),s′min)
		smax = min(smax,s′max+Δs_max)

		new(smin,smax,Δs_max,s′min,s′max)
	end
end

# Constructors. Both ts and st are constructed identically, 
# so we can dispatch on the supertype
(::Type{T})(smin::Integer,smax::Integer) where {T<:SHModeRange} = T(smin,smax,-smax,smax)
(::Type{T})(s::Integer) where {T<:SHModeRange} = T(s,s)

(::Type{T})(s_range::AbstractUnitRange{<:Integer},
	t_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(extrema(s_range)...,extrema(t_range)...)

(::Type{T})(s_range::AbstractUnitRange{<:Integer},t::Integer) where {T<:SHModeRange} = 
	T(extrema(s_range)...,t,t)

(::Type{T})(s::Integer,t_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(s,s,extrema(t_range)...)

(::Type{T})(s_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(extrema(s_range)...)

function s′s(smin::Integer,smax::Integer,Δs_max::Integer,
	s′min::Integer=max(smin-Δs_max,0))

	s′s(smin,smax,Δs_max,s′min,smax+Δs_max)
end

s′s(smin::Integer,smax::Integer,m::SHModeRange,args...) = s′s(smin,smax,m.smax,args...)

s′s( s_range::AbstractUnitRange{<:Integer},Δs_max::Integer,
	s′_range::AbstractUnitRange{<:Integer}) = 
	s′s(extrema(s_range)...,Δs_max,extrema(s′_range)...)

s′s(smin::Integer,smax::Integer,Δs_max::Integer,
	s′_range::AbstractUnitRange{<:Integer}) = 
	s′s(smin,smax,Δs_max,extrema(s′_range)...)

s′s( s_range::AbstractUnitRange{<:Integer},Δs_max::Union{Integer,<:SHModeRange},
	args...) = 
	s′s(extrema(s_range)...,Δs_max,args...)

# Get the ranges of the modes

@inline s_range(m::SHModeRange) = m.smin:m.smax
@inline t_range(m::SHModeRange) = m.tmin:m.tmax

@inline s_range(m::s′s) = m.smin:m.smax
@inline s′_range(m::s′s) = m.s′min:m.s′max

@inline function s_valid_range(m::SHModeRange,t::Integer)
	max(abs(t),m.smin):m.smax
end

@inline function t_valid_range(m::SHModeRange,s::Integer)
	max(-s,m.tmin):min(s,m.tmax)
end

@inline function s′_valid_range(m::s′s,s::Integer)
	max(s - m.Δs_max,m.s′min):min(s + m.Δs_max,m.s′max)
end

function _length(m::st)

	# We evaluate Sum[smax - Max[Abs[t], smin] + 1] piecewise in 
	# Mathematica. There are four cases to consider
	# depending on which of |t| and smin is larger

	smin,smax,tmin,tmax = m.smin,m.smax,m.tmin,m.tmax

	N = 0

	up = min(-1, tmax)
	lo = max(tmin,-smin)
	if up >= lo
		N += (1 + smax - smin)*(1 - lo + up)
	end
	
	up = min(tmax, smin)
	lo = max(tmin, 0)
	if up >= lo
		N += (1 + smax - smin)*(1 - lo + up)
	end

	up = tmax
	lo = max(tmin, smin + 1)
	if up >= lo
		N += div(-((1 + up - lo)*(-2 - 2smax + up + lo)),2)
	end

	up = min(-1, -1 - smin, tmax)
	lo = max(-smax, tmin)
	if up >= lo
		N += div(-((-1 + lo - up)*(2 + 2smax + lo + up)),2)
	end

	return N
end

function _length(m::ts)

	# We evaluate Sum[min(s,tmax)-max(-s,tmin)+1] piecewise in 
	# Mathematica. There are four cases to consider
	# depending on s, tmax and tmin

	smin,smax,tmin,tmax = m.smin,m.smax,m.tmin,m.tmax

	N = 0

	up = min(-tmin, tmax,smax)
	lo = smin
	if up >= lo
		N += -(-1 + lo - up)*(1 + lo + up)
	end
	
	up = min(-tmin, smax)
	lo = max(smin, tmax + 1, -tmax)
	if up >= lo
		N += div(-((-1 + lo - up)*(2 + 2tmax + lo + up)),2)
	end

	up = min(tmax, smax)
	lo = max(smin, -tmin + 1, tmin)
	if up >= lo
		N += div(-((-1 + lo - up)*(2 - 2tmin + lo + up)),2)
	end

	up = smax
	lo = max(-tmin + 1, tmax + 1,smin)
	if up >= lo
		N += (1 + tmax - tmin)*(1 + up - lo)
	end

	return N
end

function _length(m::s′s)

	# Number of modes is given by count(s′_valid_range(m,s) for s in s_range(m))
	smin,smax,spmin,spmax = m.smin,m.smax,m.s′min,m.s′max
	dsmax = m.Δs_max
    
    N = 0

    up = min(smax, spmax - dsmax)
    lo = max(smin, spmin + dsmax)
    if up >= lo
    	N += (1 + 2*dsmax)*(1 - lo + up)
    end

    up = min(smax,spmax + dsmax)
    lo = max(smin, spmin + dsmax, spmax - dsmax + 1)
    if up >= lo
    	N += div((-1 + lo - up)*(-2*(1 + dsmax + spmax) + lo + up),2)
    end

    up = min(smax, spmin + dsmax - 1, spmax - dsmax)
    lo = max(smin,spmin - dsmax)
    if up >= lo
    	N += div(-((-1 + lo - up)*(2 + 2dsmax - 2spmin + lo + up)),2)
    end

    up = min(smax, spmin + dsmax - 1)
    lo = max(smin,spmax - dsmax + 1)
    if up >= lo
    	N += (1 + spmax - spmin)*(1 - lo + up)
    end

    return N
end

Base.length(m::ModeRange) = _length(m)

Base.firstindex(m::ModeRange) = 1
Base.lastindex(m::ModeRange) = length(m)

# Size and axes behave similar to vectors
@inline Base.size(m::ModeRange) = (length(m),)
@inline Base.size(m::ModeRange,d::Integer) = d == 1 ? length(m) : 1

@inline Base.axes(m::ModeRange) = (Base.OneTo(length(m)),)
@inline Base.axes(m::ModeRange,d::Integer) = d == 1 ? Base.OneTo(length(m)) : Base.OneTo(1)

# Convenience function to generate the first step in the iteration
@inline first_t(m::st) = m.tmin
@inline first_s(m::st) = first(s_valid_range(m,first_t(m)))
@inline first_s(m::ts) = m.smin
@inline first_t(m::ts) = first(t_valid_range(m,first_s(m)))

@inline first_s(m::s′s) = max(m.smin,m.s′min-m.Δs_max)
@inline first_s′(m::s′s) = first(s′_valid_range(m,first_s(m)))

function Base.iterate(m::st, state=((first_s(m),first_t(m)), 1))

	(s,t), count = state

	if count > _length(m)
		return nothing
	end

	if s == m.smax
		next_t = t + 1
		next_s = max(m.smin,abs(next_t))
	else
		next_t = t
		next_s = s + 1
	end

	return (s,t), ((next_s,next_t), count + 1)
end

function Base.iterate(m::ts, state=((first_s(m),first_t(m)), 1))
	
	(s,t), count = state

	if count > length(m)
		return nothing
	end

	if t == min(m.tmax,s)
		next_s = s + 1
		next_t = max(m.tmin,-next_s)
	else
		next_s = s
		next_t = t + 1
	end

	return (s,t), ((next_s,next_t), count + 1)
end

function Base.iterate(m::s′s,state=((first_s′(m),first_s(m)), 1))

	(s′,s),count = state
	if count > length(m)
		return nothing
	end

	s′_range_s = s′_valid_range(m,s)

	if s′ == last(s′_range_s)
		next_s = s + 1
		next_s′ = first(s′_valid_range(m,next_s))
	else
		next_s = s
		next_s′ = s′ + 1
	end

	return (s′,s),((next_s′,next_s),count+1)
end

function _in((s,t)::Tuple{<:Integer,<:Integer},m::ts)
	(abs(t)<=s) && (m.smin <= s <= m.smax) && (m.tmin <= t <= m.tmax) &&
	(t in t_valid_range(m,s))
end

function _in((s,t)::Tuple{<:Integer,<:Integer},m::st)
	(abs(t)<=s) && (m.smin <= s <= m.smax) && (m.tmin <= t <= m.tmax) &&
	(s in s_valid_range(m,t))
end

function _in((s′,s)::Tuple{<:Integer,<:Integer},m::s′s)
	(m.smin <= s <= m.smax) && 
	(m.s′min <= s′ <= m.s′max) && 
	(s′ in s′_valid_range(m,s))
end

Base.in(T::Tuple{<:Integer,<:Integer},m::ModeRange) = _in(T,m)

Base.last(m::st) = (last(s_valid_range(m,m.tmax)),m.tmax)
Base.last(m::ts) = (m.smax,last(t_valid_range(m,m.tmax)))
Base.last(m::s′s) = (last(s′_valid_range(m,m.smax)), m.smax)

function modeindex(m::st,s::Integer,t::Integer)
	# Check if the s and t supplied correspond to valid modes
	check_if_non_negative(s)
	check_if_valid_mode(s,t)
	check_if_mode_present(m,s,t)

	Nskip = 0
	
	smin,smax = m.smin,m.smax
	tmin,tmax = m.tmin,m.tmax

	# Nskip = sum(length(s_valid_range(m,ti)) fot ti=m.tmin:t-1)

	up = min(t - 1, -1)
	lo = max(tmin, -smin)
	if up >= lo
		Nskip += (1 + smax - smin)*(1 - lo + up)
	end

	up = min(t - 1, smin)
	lo = max(tmin, 0)
	if up >= lo
		Nskip += (1 + smax - smin)*(1 - lo + up)
	end

	up = min(t - 1, tmax, smax)
	lo = max(tmin, smin + 1)
	if up >= lo
		Nskip += div((-1 + lo - up)*(-2*(1 + smax) + lo + up),2)
	end

	up = min(t - 1, -1, -smin - 1)
	lo = max(tmin, -smax)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 + 2smax + lo + up)),2)
	end

	ind_s = s - first(s_valid_range(m,t)) + 1
	Nskip + ind_s
end

function modeindex(m::st,s::AbstractUnitRange{<:Integer},t::Integer)
	last(s) in s_valid_range(m,t) || throw(ModeMissingError(last(s),t,m))
	start = modeindex(m,first(s),t)
	start:start + length(s) - 1
end

modeindex(m::st,::Colon,t::Integer) = modeindex(m,s_valid_range(m,t),t)

function modeindex(m::ts,s::Integer,t::Integer)
	# Check if the s and t supplied correspond to valid modes
	check_if_non_negative(s)
	check_if_valid_mode(s,t)
	check_if_mode_present(m,s,t)

	Nskip = 0
	
	smin,smax = m.smin,m.smax
	tmin,tmax = m.tmin,m.tmax

	# Nskip = sum(length(t_valid_range(m,si)) for si=m.smin:s-1)

	up = min(-tmin, tmax, s - 1)
	lo = smin
	if up >= lo
		Nskip += -(-1 + lo - up)*(1 + lo + up)
	end

	up = min(-tmin, s - 1)
	lo = max(smin, tmax + 1, -tmax)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 + 2tmax + lo + up)),2)
	end

	up = min(tmax, s - 1)
	lo = max(smin, -tmin + 1, tmin)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 - 2tmin + lo + up)),2)
	end

	up = s - 1
	lo = max(-tmin + 1, tmax + 1, smin)
	if up >= lo
		Nskip += (1 + tmax - tmin)*(1 - lo + up)
	end

	ind_t = t - first(t_valid_range(m,s)) + 1
	Nskip + ind_t
end

function modeindex(m::ts,s::Integer,t::AbstractUnitRange{<:Integer})
	last(t) in t_valid_range(m,s) || throw(ModeMissingError(s,last(t),m))
	start = modeindex(m,s,first(t))
	start:start + length(t) - 1
end

modeindex(m::ts,s::Integer,::Colon) = modeindex(m,s,t_valid_range(m,s))

function modeindex(m::s′s,s′::Integer,s::Integer)
	# Check if s and s′ supplied correspond to valid modes
	check_if_non_negative(s)
	check_if_non_negative(s′)
	check_if_mode_present(m,s′,s)

	Nskip = 0

	smin,smax,spmin,spmax = m.smin,m.smax,m.s′min,m.s′max
	dsmax = m.Δs_max

	up = min(s - 1, spmax - dsmax)
	lo = max(smin, spmin + dsmax)
	if up >= lo
		Nskip += (1 + 2dsmax)*(1 - lo + up)
	end

	up = min(s - 1, spmax + dsmax)
	lo = max(smin, spmin + dsmax, spmax - dsmax + 1)
	if up >= lo
		Nskip += div((-1 + lo - up)*(-2*(1 + dsmax + spmax) + lo + up),2)
	end

	up = min(s - 1, spmin + dsmax - 1, spmax - dsmax)
	lo = max(smin, spmin - dsmax)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 + 2dsmax - 2spmin + lo + up)),2)
	end

	up = min(s - 1, spmin + dsmax - 1)
	lo = max(smin, spmax - dsmax + 1)
	if up >= lo
		Nskip += (1 + spmax - spmin)*(1 - lo + up)
	end

	ind_s′ = s′ - first(s′_valid_range(m,s)) + 1
	Nskip + ind_s′
end

function modeindex(m::s′s,s′::AbstractUnitRange{<:Integer},s::Integer)
	last(s′) in s′_valid_range(m,s) || throw(ModeMissingError(last(s′),s,m))
	start = modeindex(m,first(s′),s)
	start:start + length(s′) - 1
end

modeindex(m::s′s,::Colon,s::Integer) = modeindex(m,s′_valid_range(m,s),s)

modeindex(m::ModeRange,T::Tuple{<:Any,<:Any}) = modeindex(m,T...)

# Display the iterators

function Base.show(io::IO, m::SHModeRange)
	print(io,"(s=",s_range(m),",t=",t_range(m),")")
end

function Base.show(io::IO, m::s′s)
	print(io,"(s=",s_range(m),",Δs_max=",m.Δs_max,
		",s′=",s′_range(m),")")
end

function Base.show(io::IO, ::MIME"text/plain", m::st)
	println("Spherical harmonic modes with s increasing faster than t")
	print(io,"smin = ",m.smin,", smax = ",m.smax,
		", tmin = ",m.tmin,", tmax = ",m.tmax)
end

function Base.show(io::IO, ::MIME"text/plain", m::ts)
	println("Spherical harmonic modes with t increasing faster than s")
	print(io,"smin = ",m.smin,", smax = ",m.smax,
		", tmin = ",m.tmin,", tmax = ",m.tmax)
end

function Base.show(io::IO, ::MIME"text/plain", m::s′s)
	println("Spherical harmonic modes (s′,s) where |s-Δs| ⩽ s′ ⩽ s+Δs "*
		"for 0 ⩽ Δs ⩽ Δs_max, and s′min ⩽ s′ ⩽ s′max")
	print(io,m.smin," ⩽ s ⩽ ",m.smax,", Δs_max = ",m.Δs_max,
		", and ",m.s′min," ⩽ s′ ⩽ ",m.s′max)
end

end # module
