module SphericalHarmonicModes

export SHModeRange,st,ts,modeindex,s_valid_range,t_valid_range

abstract type SHModeRange end

struct st <: SHModeRange
	smin :: Int64
	smax :: Int64
	tmin :: Int64
	tmax :: Int64

	function st(smin,smax,tmin,tmax)
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
		smin = max(smin,max(tmin,-tmax))
		new(smin,smax,tmin,tmax)
	end
end

struct ts <: SHModeRange
	smin :: Int64
	smax :: Int64
	tmin :: Int64
	tmax :: Int64

	function ts(smin,smax,tmin,tmax)
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
		smin = max(smin,tmin)
		new(smin,smax,tmin,tmax)
	end
end

struct OrderError{T} <: Exception
	var :: String
	low :: T
	high :: T
end

struct tRangeError <: Exception 
	smax :: Integer
	t :: Integer
end

Base.showerror(io::IO, e::tRangeError) = print(io," t = ", e.t,
		" does not satisfy ",-e.smax," ⩽ t ⩽ ",e.smax)

Base.showerror(io::IO, e::OrderError) = print(io,e.var,"min = ",e.low,
	" is not consistent with ",e.var,"max = ",e.high)

Base.eltype(m::SHModeRange) = Tuple{Int64,Int64}

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
Base.lastindex(m::SHModeRange) = length(m)

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

function Base.in((s,t)::Tuple{<:Integer,<:Integer},m::SHModeRange)
	(m.smin <= s <= m.smax) && (m.tmin <= t <= m.tmax)
end

function modeindex(m::st,s::Integer,t::Integer)
	N_skip = 0
	for ti in m.tmin:t-1
		N_skip += length(s_valid_range(m,ti))
	end

	smin_t = max(abs(t),m.smin)
	N_skip + s - smin_t + 1
end

function modeindex(m::ts,s::Integer,t::Integer)
	N_skip = 0
	for si in m.smin:s-1
		N_skip += length(t_valid_range(m,si))
	end

	tmin_s = max(-s,m.tmin)
	N_skip + t - tmin_s + 1
end

modeindex(m::SHModeRange,(s,t)::Tuple{<:Integer,<:Integer}) = modeindex(m,s,t)

function s_valid_range(m::SHModeRange,t::Integer)
	max(abs(t),m.smin):m.smax
end

function t_valid_range(m::SHModeRange,s::Integer)
	max(-s,m.tmin):min(s,m.tmax)
end

function Base.show(io::IO, m::SHModeRange)
	print(io,"(s=",m.smin:m.smax,",t=",m.tmin:m.tmax,")")
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

end # module
