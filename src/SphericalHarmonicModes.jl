module SphericalHarmonicModes

export SHModeRange,st,ts,modeindex,s_valid_range,t_valid_range,s_range,t_range,
s′s,s′_range

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

struct ModeMissingError{T} <: Exception
	s :: T
	t :: T
	m :: SHModeRange
end

Base.showerror(io::IO, e::tRangeError) = print(io," t = ", e.t,
		" does not satisfy ",-e.smax," ⩽ t ⩽ ",e.smax)

Base.showerror(io::IO, e::OrderError) = print(io,e.var,"min = ",e.low,
	" is not consistent with ",e.var,"max = ",e.high)

Base.showerror(io::IO, e::ModeMissingError) = print(io,"Mode with (s=",e.s,",t=",e.t,")",
			" is not included in the range given by s=",e.m.smin:e.m.smax,", t=",e.m.tmin:e.m.tmax)

function check_if_st_valid(smin,smax,tmin,tmax)
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

function check_if_st_valid(s_range::AbstractUnitRange{Int})
	if minimum(s_range) < 0
		throw(NegativeDegreeError())
	end
end

struct st <: SHModeRange
	smin :: Int
	smax :: Int
	tmin :: Int
	tmax :: Int

	function st(smin,smax,tmin,tmax)
		check_if_st_valid(smin,smax,tmin,tmax)
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
		check_if_st_valid(smin,smax,tmin,tmax)
		smin = max(smin,max(tmin,-tmax))
		new(smin,smax,tmin,tmax)
	end
end

struct s′s <: ModeProduct
	s_range :: UnitRange{Int}
	Δs_max :: Int
end

# Constructors. Both ts and st are constructed identically, 
# so we can dispatch on the supertype
(::Type{T})(smin::Integer,smax::Integer) where {T<:SHModeRange} = T(smin,smax,-smax,smax)
(::Type{T})(s::Integer) where {T<:SHModeRange} = T(s,s)

(::Type{T})(s_range::AbstractUnitRange,t_range::AbstractUnitRange) where {T<:SHModeRange} = 
	T(minimum(s_range),maximum(s_range),minimum(t_range),maximum(t_range))

(::Type{T})(s_range::AbstractUnitRange,t::Integer) where {T<:SHModeRange} = T(minimum(s_range),maximum(s_range),t,t)
(::Type{T})(s::Integer,t_range::AbstractUnitRange) where {T<:SHModeRange} = T(s,s,minimum(t_range),maximum(t_range))

(::Type{T})(s_range::AbstractUnitRange) where {T<:SHModeRange} = T(minimum(s_range),maximum(s_range))

# s′s(s_range::UnitRange,m::SHModeRange) = s′s(s_range,m.smax)

function neg_skip(smin,smax,tmin,tmax)
	# This is count(t<tmin for s=smin:smax for t=-s:s), evaluated analytically
	smin_part = max(smin,abs(tmin))
	div((1 + smax - smin_part)*(smax + 2tmin + smin_part),2) + 
	(tmin>smin ? tmin^2-smin^2 : 0)
end

function pos_skip(smin,smax,tmin,tmax)
	# This is count(t>tmax for s=smin:smax for t=-s:s), evaluated analytically
	smin_part = max(smin,abs(tmax))
	div((1 + smax - smin_part)*(smax - 2tmax + smin_part),2) + 
	(tmax < -smin ? tmax^2-smin^2 : 0 )
end

# Number of modes does not depend on ordering
function num_modes(smin,smax,tmin,tmax)
	(smax+1)^2-smin^2 - neg_skip(smin,smax,tmin,tmax) - pos_skip(smin,smax,tmin,tmax)
end

num_modes(m::SHModeRange) = num_modes(m.smin,m.smax,m.tmin,m.tmax)
Base.length(m::SHModeRange) = num_modes(m)

function Base.length(modes::s′s)
	
	smin,smax = extrema(modes.s_range)
	Δs_max = modes.Δs_max
	
	N = 0
	
	if smin < Δs_max
		N += -div((-1 + smin - min(smax, Δs_max -1 ))*
			(2 + smin + 2Δs_max + min(smax, Δs_max -1 )),2)
	end

	if smax >= Δs_max
		N += (1 + smax - max(Δs_max,smin)) * (1 + 2Δs_max)
	end

	return N
end

Base.lastindex(m::ModeRange) = length(m)

# Size and axes behave similar to vectors
Base.size(m::ModeRange) = (length(m),)
Base.size(m::ModeRange,d::Integer) = d == 1 ? length(m) : 1

Base.axes(m::ModeRange) = (Base.OneTo(length(m)),)
Base.axes(m::ModeRange,d::Integer) = d == 1 ? Base.OneTo(length(m)) : Base.OneTo(1)

first_t(m::st) = m.tmin
first_s(m::st) = first(s_valid_range(m,first_t(m)))
first_s(m::ts) = m.smin
first_t(m::ts) = first(t_valid_range(m,first_s(m)))

function Base.iterate(m::st, state=((first_s(m),first_t(m)), 1))

	(s,t), count = state

	if count > length(m)
		return nothing
	end

	next_t = (s == m.smax) ? t+1 : t
	next_s = (s == m.smax) ? max(m.smin,abs(next_t)) : s + 1

	return (s,t), ((next_s,next_t), count + 1)
end

function Base.iterate(m::ts, state=((first_s(m),first_t(m)), 1))
	
	(s,t), count = state

	if count > length(m)
		return nothing
	end

	next_s = (t == min(m.tmax,s)) ? s+1 : s
	next_t = (t == min(m.tmax,s)) ? max(m.tmin,-next_s) : t + 1

	return (s,t), ((next_s,next_t), count + 1)
end

function Base.iterate(m::s′s,state=((first(s′_range(m,first(m.s_range))),first(m.s_range)), 1))

	(s′,s),count = state
	if count > length(m)
		return nothing
	end

	s′_range_s = s′_range(m,s)

	s′ind = searchsortedfirst(s′_range_s,s′)
	if s′ind==length(s′_range_s)
		next_s = s + 1
		next_s′ = first(s′_range(m,next_s))
	else
		next_s = s
		next_s′ = s′ + 1
	end

	return (s′,s),((next_s′,next_s),count+1)
end

function Base.in((s,t)::Tuple{<:Integer,<:Integer},m::SHModeRange)
	(abs(t)<=s) && (m.smin <= s <= m.smax) && (m.tmin <= t <= m.tmax)
end

function Base.in((s′,s)::Tuple{<:Integer,<:Integer},m::s′s)
	(s ∉ m.s_range) && return false
	(s′ ∉ s′_range(m,s)) && return false
	return true
end

s′_range(Δs_max::Integer,s::Integer) = max(s-Δs_max,0):s+Δs_max
s′_range(modes::s′s,s::Integer) = s′_range(modes.Δs_max,s)

Base.last(m::st) = (maximum(s_valid_range(m,m.tmax)),m.tmax)
Base.last(m::ts) = (m.smax,maximum(t_valid_range(m,m.tmax)))

Base.last(m::s′s) = (maximum(s′_range(m,maximum(m.s_range))), maximum(m.s_range))

function modeindex(m::st,s::Integer,t::Integer)
	((s,t) ∉ m) && throw(ModeMissingError(s,t,m))
	Nskip = 0
	
	smin,smax = extrema(s_range(m))
	tmin,tmax = extrema(t_range(m))

	# Nskip = sum(length(s_valid_range(m,ti)) fot ti=m.tmin:t-1)

	if smin == 0 && t > 1 && tmin <= 0
		Nskip += -div((-1 + t)*(-2 - 2smax + t),2)
	elseif smin > 0 && t > 1 + smin && tmin == smin
		Nskip += div(-2 - 2smax + 3t + 2smax*t - t^2 - tmin - 2smax*tmin + tmin^2,2)
	elseif smin > 0 && t > 1 + smin && tmin < smin
		Nskip += div((1 + smin - t)*(-2 - 2smax + smin + t),2)
	elseif (smin == 0 && t > 1 && tmin == -1 + t) || (smin > 0 && t > 1 + smin && tmin == -1 + t)
		Nskip += 1 + smax - tmin
	elseif (smin == 0 && t > 1 && 0 < tmin < -1 + t) || (smin > 0 && t > 1 + smin && smin < tmin < -1 + t)
		Nskip += -div((t - tmin)*(-3 - 2smax + t + tmin),2)	
	end

	if smin == 0 && t >= 1 && tmin < 0
		Nskip += -div(tmin*(1 + 2smax + tmin),2)
	elseif smin > 0 && t == 1 - smin && tmin < -1 + t
		Nskip += div((-1 + t - tmin)*(2smax + t + tmin),2)
	elseif smin > 0 && t > 1 - smin && tmin < -smin
		Nskip += div((-1 - 2smax + smin - tmin)*(smin + tmin),2)
	elseif (smin == 0 && t < 1 && tmin == -1 + t) || (smin > 0 && t < 1 - smin && tmin == -1 + t)
		Nskip += 1 + smax + tmin
	elseif (smin == 0 && t < 1 && tmin < -1 + t) || (smin > 0 && t < 1 - smin && tmin < -1 + t)
		Nskip += div((t - tmin)*(1 + 2smax + t + tmin),2)
	end

	if smin > 0 && t == 1 - smin && tmin < -1 + t
		Nskip += 1 + smax - smin
	elseif smin > 0 && t > 1 + smin && -smin <= tmin < smin
		Nskip += (1 + smax - smin)*(1 + smin - tmin)
	elseif smin > 0 && t > 1 + smin && tmin < -smin
		Nskip += (1 + smax - smin)*(1 + 2smin)
	elseif smin > 0 && 1 - smin < t <= 1 + smin && -smin <= tmin < -1 + t
		Nskip += (1 + smax - smin)*(t - tmin)
	elseif smin > 0 && 1 - smin < t <= 1 + smin && tmin < -smin
		Nskip += (1 + smax - smin)*(smin + t)
	elseif (smin == 0 && t == 1 && tmin <= 0) || 
			(smin == 0 && t > 1 && tmin < 0) || 
			(smin == 0 && t > 1 && tmin == 0) || 
			(smin > 0 && t == 1 - smin && tmin == -1 + t) || 
			(smin > 0 && t > 1 + smin && tmin == smin) || 
			(smin > 0 && 1 - smin < t <= 1 + smin && tmin == -1 + t)

		Nskip += 1 + smax - smin
	end

	Nskip + searchsortedfirst(s_valid_range(m,t),s)
end

function modeindex(m::ts,s::Integer,t::Integer)
	((s,t) ∉ m) && throw(ModeMissingError(s,t,m))
	Nskip = 0
	
	smin,smax = extrema(s_range(m))
	tmin,tmax = extrema(t_range(m))

	# Nskip = sum(length(t_valid_range(m,si)) for si=m.smin:s-1)

	if (s > 1 && smin == 0 && 0 < tmax < -1 + s && tmin > -tmax) || 
		(s > 1 && 0 < smin < -1 + s && smin < tmax < -1 + s && tmin > -tmax)

		Nskip += (-1 + s - tmax)*(1 + tmax - tmin)

	elseif s > 1 && 0 < smin < -1 + s && tmax < smin && tmin > -smin
		Nskip +=  (s - smin)*(1 + tmax - tmin)
	elseif (s == 1 && smin == 0 && tmax < 0 && tmin > 0) || (s > 1 && 
			smin == -1 + s && tmax < smin && tmin > -smin)

		Nskip += 1 + tmax - tmin
	elseif (s > 1 && smin == 0 && tmax == 0 && tmin >= 0) || (s > 1 && 
			smin == 0 && tmax < 0 && tmin == 0)

		Nskip += (-1 + s)*(1 + tmax - tmin)

	elseif (s > 1 && 0 < smin < -1 + s && tmax == smin && 
			tmin >= -tmax) || (s > 1 && 0 < smin < -1 + s && tmax < smin && 
			tmin == -smin)

		Nskip += (-1 + s - smin)*(1 + tmax - tmin)
	elseif (s > 1 && smin == 0 && tmax == 0 && 1 - s < tmin < 0) || (s > 1 && 
			smin == 0 && 0 < tmax < -1 + s && 1 - s < tmin <= -tmax) || 
			(s > 1 && smin == 0 && tmax < 0 && 1 - s < tmin < 0) || 
			(s > 1 && 0 < smin < -1 + s && tmax == smin && 1 - s < tmin < -tmax) || 
			(s > 1 && 0 < smin < -1 + s && smin < tmax < -1 + s && 1 - s < tmin <= -tmax) || 
			(s > 1 && 0 < smin < -1 + s && tmax < smin && 1 - s < tmin < -smin)

		Nskip += (1 + tmax - tmin)*(-1 + s + tmin)
	end

	if s > 1 && smin == 0 && tmax > -1 + s && tmin < 1 - s
		Nskip += s^2
	elseif s > 1 && 0 < smin < -1 + s && tmax > -1 + s && tmin < 1 - s
		Nskip += (s - smin)*(s + smin)
	elseif s > 1 && 0 < smin < -1 + s && smin < tmax <= -1 + s && tmin < -tmax
		Nskip += -(-1 + smin - tmax)*(1 + smin + tmax)
	elseif (s > 1 && smin == 0 && tmax == -1 + s && tmin < -tmax) || 
			(s > 1 && smin == 0 && 0 < tmax < -1 + s && tmin <= -tmax)
		Nskip += (1 + tmax)*(1 + 2smin + tmax)
	elseif (s > 1 && 0 < smin < -1 + s && tmax > -1 + s && 1 - s <= tmin < -smin) || 
			(s > 1 && 0 < smin < -1 + s && smin < tmax <= -1 + s && -tmax <= tmin < -smin)

		Nskip += -(1 + smin - tmin)*(-1 + smin + tmin)
	elseif (s > 1 && smin == 0 && tmax == -1 + s && -tmax <= tmin < 0) || 
			(s > 1 && smin == 0 && tmax > -1 + s && 1 - s <= tmin < 0) || 
			(s > 1 && smin == 0 && 0 < tmax < -1 + s && -tmax < tmin < 0)

		Nskip += (-1 + tmin)*(-1 - 2smin + tmin)

	elseif (s == 1 && smin == 0 && tmax >= 0 && tmin <= 0) || 
			(s > 1 && smin == 0 && tmax == 0 && tmin <= 0) || 
			(s > 1 && smin == 0 && tmax == -1 + s && tmin == 0) || 
			(s > 1 && smin == 0 && tmax > -1 + s && tmin == 0) || 
			(s > 1 && smin == 0 && 0 < tmax < -1 + s && tmin == 0) || 
			(s > 1 && smin == -1 + s && tmax == smin && tmin <= -tmax) || 
			(s > 1 && smin == -1 + s && tmax > smin && tmin <= -smin) || 
			(s > 1 && 0 < smin < -1 + s && tmax == smin && tmin <= -tmax) || 
			(s > 1 && 0 < smin < -1 + s && tmax > -1 + s && tmin == -smin) || 
			(s > 1 && 0 < smin < -1 + s && smin < tmax <= -1 + s && tmin == -smin)

		Nskip += 1 + 2smin
	end

	if s >= 1 && 0 <= smin < -1 + s && tmax > -1 + s && tmin == -smin
		Nskip += div(-2 + s + s^2 - smin - 2s*smin + smin^2 + 4tmin - 4s*tmin + 4smin*tmin,2)
	elseif s >= 1 && 0 <= smin < -1 + s && tmax > -1 + s && tmin > -smin
		Nskip += div((s - smin)*(1 + s + smin - 2tmin),2)
	elseif s >= 1 && 0 <= smin < -1 + s && tmax > -1 + s && 1 - s < tmin < -smin
		Nskip += div((2 + s - 3tmin)*(-1 + s + tmin),2)
	elseif s >= 1 && 0 <= smin < -1 + s && smin < tmax <= -1 + s && tmin == -smin
		Nskip += div(-3smin + smin^2 + 3tmax - 2smin*tmax + tmax^2 + 4smin*tmin - 4tmax*tmin,2)
	elseif s >= 1 && 0 <= smin < -1 + s && smin < tmax <= -1 + s && tmin > -smin
		Nskip += -div((-1 + smin - tmax)*(2 + smin + tmax - 2tmin),2)
	elseif s >= 1 && 0 <= smin < -1 + s && 
		smin < tmax <= -1 + s && -tmax < tmin < -smin
		
		Nskip += div((3 + tmax - 3tmin)*(tmax + tmin),2)
	elseif (s >= 1 && smin == -1 + s && tmax == smin && tmin > -tmax) || 
			(s >= 1 && smin == -1 + s && tmax > smin && tmin > -smin) || 
			(s >= 1 && 0 <= smin < -1 + s && tmax == smin && tmin > -tmax)

		Nskip += 1 + smin - tmin
	end

	if s > 1 && smin == 0 && tmax == 0 && 1 - s <= tmin < 0
		Nskip += div(-3tmin + tmin^2,2)
	elseif s > 1 && smin == 0 && tmax == 0 && tmin < 1 - s
		Nskip += div(-2 + s + s^2,2)
	elseif s > 1 && smin == 0 && 0 < tmax < -1 + s && 1 - s <= tmin < -tmax
		Nskip += -div((3 + 3tmax - tmin)*(tmax + tmin),2)
	elseif s > 1 && smin == 0 && 0 < tmax < -1 + s && tmin < 1 - s
		Nskip += div((-1 + s - tmax)*(2 + s + 3tmax),2)
	elseif s > 1 && smin == 0 && tmax < 0 && 1 - s < tmin < 0
		Nskip += div((-1 + tmin)*(-2 - 2tmax + tmin),2)
	elseif s > 1 && smin == 0 && tmax < 0 && tmin <= 1 - s
		Nskip += div(s*(1 + s + 2tmax),2)
	elseif s > 1 && 0 < smin < -1 + s && tmax == smin && 1 - s <= tmin < -tmax
		Nskip += div(-3smin - smin^2 - 2smin*tmax - 3tmin - 2tmax*tmin + tmin^2,2)
	elseif s > 1 && 0 < smin < -1 + s && tmax == smin && tmin < 1 - s
		Nskip += div(-2 + s + s^2 - 3smin - smin^2 - 2tmax + 2s*tmax - 2smin*tmax,2)
	elseif s > 1 && 0 < smin < -1 + s && smin < tmax < -1 + s && 1 - s <= tmin < -tmax
		Nskip += -div((3 + 3tmax - tmin)*(tmax + tmin),2)
	elseif s > 1 && 0 < smin < -1 + s && smin < tmax < -1 + s && tmin < 1 - s
		Nskip += div((-1 + s - tmax)*(2 + s + 3tmax),2)
	elseif s > 1 && 0 < smin < -1 + s && tmax < smin && 1 - s <= tmin < -smin
		Nskip += -div((2 + smin + 2tmax - tmin)*(-1 + smin + tmin),2)
	elseif s > 1 && 0 < smin < -1 + s && tmax < smin && tmin < 1 - s
		Nskip += div((s - smin)*(1 + s + smin + 2tmax),2)
	elseif (s == 1 && smin == 0 && tmax < 0 && tmin <= 0) || 
			(s > 1 && smin == 0 && tmax < 0 && tmin == 0) || 
			(s > 1 && smin == -1 + s && tmax < smin && tmin <= -smin) || 
			(s > 1 && 0 < smin < -1 + s && tmax < smin && tmin == -smin)

		Nskip += 1 + smin + tmax
	end

	Nskip + searchsortedfirst(t_valid_range(m,s),t)
end

function modeindex(m::s′s,s′::Integer,s::Integer)
	
	@assert((s′,s) in m,"Mode $((s′,s)) is not present in $m")

	smin,smax = extrema(m.s_range)
	Δs_max = m.Δs_max

	# Nskip = sum(length(s′range(s,m)) for s in smin:s-1)
	# Exact expressions evaluated in Mathematica
	if s==smin
		Nskip = 0
	elseif s==smin+1 && smin<=Δs_max
		Nskip = 1 + smin + Δs_max
	elseif s==smin+1 && smin>Δs_max
		Nskip = 1 + 2Δs_max
	elseif s - smin > 1 && smin - Δs_max > 0 && s - Δs_max >= 1
		Nskip = (s - smin)*(1 + 2Δs_max)
	elseif s - smin > 1 && s - Δs_max < 1 && smin - Δs_max < 0
		Nskip = div( (s - smin)*(1 + s + smin + 2Δs_max),2)
	elseif s - smin > 1 && s - Δs_max == 1 && smin - Δs_max < 0
		Nskip = -div((-1 + smin - Δs_max)*(2 + smin + 3Δs_max),2)
	elseif smin - Δs_max == 0 && s - smin > 1
		Nskip = s - Δs_max + 2*(s - smin)*Δs_max
	else
		Nskip = div(2s - smin - smin^2 - Δs_max + 4s*Δs_max - 2smin*Δs_max - Δs_max^2,2)
	end

	Nskip + searchsortedfirst(s′_range(m,s),s′)
end

modeindex(m::ModeRange,T::Tuple{<:Integer,<:Integer}) = modeindex(m,T...)

function s_valid_range(m::SHModeRange,t::Integer)
	max(abs(t),m.smin):m.smax
end

function t_valid_range(m::SHModeRange,s::Integer)
	max(-s,m.tmin):min(s,m.tmax)
end

s_range(m::SHModeRange) = m.smin:m.smax
t_range(m::SHModeRange) = m.tmin:m.tmax

s_range(m::s′s) = m.s_range

function Base.show(io::IO, m::SHModeRange)
	print(io,"(s=",m.smin:m.smax,",t=",m.tmin:m.tmax,")")
end

function Base.show(io::IO, m::s′s)
	print(io,"(s=",m.s_range,",Δs_max=",m.Δs_max,")")
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
		"and 0 ⩽ Δs ⩽ Δs_max")
	print(io,minimum(m.s_range)," ⩽ s ⩽ ",maximum(m.s_range),", Δs_max = ",m.Δs_max)
end

end # module
