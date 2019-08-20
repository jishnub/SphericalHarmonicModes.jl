using Test,SphericalHarmonicModes

@testset "modes skipped" begin
	for smax=0:10,smin=0:smax,tmin=-smax:smax,tmax=tmin:smax
		
		@test SphericalHarmonicModes.neg_skip(smin,smax,tmin,tmax) >= 0
		@test SphericalHarmonicModes.pos_skip(smin,smax,tmin,tmax) >= 0

		@test SphericalHarmonicModes.neg_skip(smin,smax,tmin,tmax) == 
					count(t<tmin for s=smin:smax for t=-s:s)

		@test SphericalHarmonicModes.pos_skip(smin,smax,tmin,tmax) == 
					count(t>tmax for s=smin:smax for t=-s:s)
	end
end

@testset "modeindex" begin

	function modeindex2(m::ts,s::Integer,t::Integer)
		((s,t) ∉ m) && throw(ModeMissingError(s,t,m))

		N_skip = 0
		for si in m.smin:s-1
			N_skip += length(t_valid_range(m,si))
		end

		tmin_s = max(-s,m.tmin)
		N_skip + t - tmin_s + 1
	end

	function modeindex2(m::st,s::Integer,t::Integer)
		((s,t) ∉ m) && throw(ModeMissingError(s,t,m))

		N_skip = 0
		for ti in m.tmin:t-1
			N_skip += length(s_valid_range(m,ti))
		end

		smin_t = max(abs(t),m.smin)
		N_skip + s - smin_t + 1
	end

	modeindex2(m::SHModeRange,(s,t)::Tuple{<:Integer,<:Integer}) = modeindex(m,s,t)

	m1 = st(0,10)
	m2 = ts(0,10)

	for (s,t) in m1
		@test modeindex(m1,s,t) == modeindex2(m1,s,t)
	end

	for (s,t) in m2
		@test modeindex(m2,s,t) == modeindex2(m2,s,t)
	end
end

@testset "last" begin
	for i=1:100
		m1 = ts(rand(1:5),rand(6:10))
		m2 = st(rand(1:5),rand(6:10))

		@test last(collect(m1)) == last(m1)
		@test last(collect(m2)) == last(m2)
	end
end