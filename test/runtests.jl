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

@testset "constructors" begin

	@testset "st" begin
		@test st(0:3,-1:1) == st(0,3,-1,1)
		@test st(0:3,1) == st(0,3,1,1)
		@test st(2,1:2) == st(2,2,1,2)
		@test st(0:3) == st(0,3,-3,3)
		@test st(2) == st(2,2,-2,2)
	end

	@testset "ts" begin
		@test ts(0:3,-1:1) == ts(0,3,-1,1)
		@test ts(0:3,1) == ts(0,3,1,1)
		@test ts(2,1:2) == ts(2,2,1,2)
		@test ts(0:3) == ts(0,3,-3,3)
		@test ts(2) == ts(2,2,-2,2)
	end

	@testset "st and ts produce the same modes" begin
		s_range = rand(0:3):rand(4:6)
		t_range = rand(-maximum(s_range):0):rand(1:maximum(s_range))
		@test sort(collect(ts(s_range,t_range))) == sort(collect(st(s_range,t_range)))
	end

	@testset "s′s " begin
		Δs_max = rand(1:3)
		s_range = rand(1:3):rand(4:10)
		@test s′s(s_range,Δs_max) == s′s(s_range,st(0:Δs_max,0))
		@test s′s(s_range,Δs_max) == s′s(s_range,ts(0:Δs_max,0))
	end
end

@testset "modeindex" begin

	function modeindex2(m::ts,s::Integer,t::Integer)
		N_skip = 0
		for si in m.smin:s-1
			N_skip += length(t_valid_range(m,si))
		end

		N_skip + searchsortedfirst(t_valid_range(m,s),t)
	end

	function modeindex2(m::st,s::Integer,t::Integer)
		N_skip = 0
		for ti in m.tmin:t-1
			N_skip += length(s_valid_range(m,ti))
		end

		N_skip + searchsortedfirst(s_valid_range(m,t),s)
	end

	function modeindex2(m::s′s,s′::Integer,s::Integer)
		N_skip = 0
		for si in minimum(m.s_range):s-1
			N_skip += length(s′_range(m,si))
		end

		N_skip + searchsortedfirst(s′_range(m,s),s′)
	end

	modeindex2(m::SHModeRange,(s,t)::Tuple) = modeindex(m,s,t)

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

	m1c = collect(m1)
	m2c = collect(m2)
	m3c = collect(m3)

	t = rand(t_range(m1)); s1,s2 = rand(s_valid_range(m1,t),2); s1,s2=minmax(s1,s2)
	@test modeindex(m1,s1:s2,t) == findfirst(isequal((s1,t)),m1c):findfirst(isequal((s2,t)),m1c)

	s = rand(s_range(m2)); t1,t2 = rand(t_valid_range(m2,s),2); t1,t2 = minmax(t1,t2)
	@test modeindex(m2,s,t1:t2) == findfirst(isequal((s,t1)),m2c):findfirst(isequal((s,t2)),m2c)

	s = rand(s_range(m3)); s′1,s′2 = rand(s′_range(m3,s),2); s′1,s′2 = minmax(s′1,s′2)
	@test modeindex(m3,s′1:s′2,s) == findfirst(isequal((s′1,s)),m3c):findfirst(isequal((s′2,s)),m3c)
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

