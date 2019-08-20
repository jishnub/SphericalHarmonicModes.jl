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

	function modeindex2(m::ts,s,t)
		N_skip = 0
		for si in m.smin:s-1
			N_skip += length(t_valid_range(m,si))
		end

		N_skip + searchsortedfirst(t_valid_range(m,s),t)
	end

	function modeindex2(m::st,s,t)
		N_skip = 0
		for ti in m.tmin:t-1
			N_skip += length(s_valid_range(m,ti))
		end

		N_skip + searchsortedfirst(s_valid_range(m,t),s)
	end

	function modeindex2(m::s′s,s′,s)
		N_skip = 0
		for si in minimum(m.s_range):s-1
			N_skip += length(s′_range(m,si))
		end

		N_skip + searchsortedfirst(s′_range(m,s),s′)
	end

	modeindex2(m::SHModeRange,(s,t)::Tuple{<:Integer,<:Integer}) = modeindex(m,s,t)

	m1 = st(rand(0:3),rand(6:10))
	m2 = ts(rand(0:3),rand(6:10))
	m3 = s′s(rand(1:3):rand(3:5),rand(1:4))

	for (s,t) in m1
		@test modeindex(m1,s,t) == modeindex2(m1,s,t)
	end

	for (s,t) in m2
		@test modeindex(m2,s,t) == modeindex2(m2,s,t)
	end

	for (s′,s) in m3
		@test modeindex(m3,s′,s) == modeindex2(m3,s′,s)
	end
end

@testset "last" begin
	for i=1:100
		m1 = ts(rand(1:5),rand(6:10))
		m2 = st(rand(1:5),rand(6:10))
		m3 = s′s(rand(1:3):rand(4:10),rand(1:5))

		@test last(collect(m1)) == last(m1)
		@test last(collect(m2)) == last(m2)
		@test last(collect(m3)) == last(m3)
	end
end