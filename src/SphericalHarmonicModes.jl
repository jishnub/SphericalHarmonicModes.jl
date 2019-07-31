module SphericalHarmonicModes

export moderange,st,ts,st_index,s_valid,t_valid

abstract type moderange end

struct st <: moderange
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

struct ts <: moderange
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

Base.eltype(m::moderange) = Tuple{Int64,Int64}

# Number of modes does not depend on ordering
function num_modes(smin,smax,tmin,tmax)
	neg_skip = count(t<tmin for s=smin:smax for t=-s:s)
	pos_skip = count(t>tmax for s=smin:smax for t=-s:s)
	(smax+1)^2-smin^2 - neg_skip - pos_skip
end

num_modes(m::moderange) = num_modes(m.smin,m.smax,m.tmin,m.tmax)
Base.length(m::moderange) = num_modes(m)
Base.lastindex(m::moderange) = length(m)

first_t(m::st) = m.tmin
first_s(m::st) = first(s_valid(m,first_t(m)))
first_s(m::ts) = m.smin
first_t(m::ts) = first(t_valid(m,first_s(m)))

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

function st_index(m::st,s,t)
	N_skip = 0
	for ti in m.tmin:t-1
		N_skip += length(s_valid(m,ti))
	end

	smin_t = max(abs(t),m.smin)
	N_skip + s - smin_t + 1
end

function st_index(m::ts,s,t)
	N_skip = 0
	for si in m.smin:s-1
		N_skip += length(t_valid(m,si))
	end

	tmin_s = max(-s,m.tmin)
	N_skip + t - tmin_s + 1
end

function s_valid(m::moderange,t::Integer)
	max(abs(t),m.smin):m.smax
end

function t_valid(m::moderange,s::Integer)
	max(-s,m.tmin):min(s,m.tmax)
end

function Base.show(io::IO, m::moderange)
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
