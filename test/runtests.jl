using Test,SphericalHarmonicModes
import SphericalHarmonicModes: NonContiguousError, ModeMissingError, InvalidModeError,
NegativeDegreeError, tRangeError, SHModeRange

@testset "constructors" begin

	@testset "LM" begin
		@test LM(0:3,-1:1) == LM(0,3,-1,1)
		@test LM(0:3,1) == LM(0,3,1,1)
		@test LM(2,1:2) == LM(2,2,1,2)
		@test LM(0:3) == LM(0,3,-3,3)
		@test LM(2) == LM(2,2,-2,2)
	end

	@testset "ML" begin
		@test ML(0:3,-1:1) == ML(0,3,-1,1)
		@test ML(0:3,1) == ML(0,3,1,1)
		@test ML(2,1:2) == ML(2,2,1,2)
		@test ML(0:3) == ML(0,3,-3,3)
		@test ML(2) == ML(2,2,-2,2)
	end

	@testset "L′L " begin
		Δs_max = rand(1:3)
		l_range = rand(1:3):rand(4:10)
		@test L′L(l_range,Δs_max) == L′L(l_range,LM(0:Δs_max,0))
		@test L′L(l_range,Δs_max) == L′L(l_range,ML(0:Δs_max,0))
	end
end

@testset "length" begin
	s_cutoff=5
	@testset "ML" begin
		for smin=0:s_cutoff,smax=smin:s_cutoff,tmin=-smax:smax,tmax=tmin:smax
			mr = ML(smin,smax,tmin,tmax)
			@test begin
				res = length(mr) == sum(length(m_range(mr,l)) 
								for l in l_range(mr))
				if !res
					println(mr)
				end
				res
			end
		end
	end

	@testset "LM" begin
		for smin=0:s_cutoff,smax=smin:s_cutoff,tmin=-smax:smax,tmax=tmin:smax
			mr = LM(smin,smax,tmin,tmax)
			@test begin
				res = length(mr) == sum(length(l_range(mr,m)) 
								for m in m_range(mr))
				if !res
					println(mr)
				end
				res
			end
		end
	end

	@testset "LM==ML" begin
	    for smin=0:s_cutoff,smax=smin:s_cutoff,tmin=-smax:smax,tmax=tmin:smax
			m1 = LM(smin,smax,tmin,tmax)
			m2 = ML(smin,smax,tmin,tmax)
			@test length(m1) == length(m2)
		end
	end

	@testset "L′L default s′minmax" begin
		for smin=0:s_cutoff,smax=smin:s_cutoff,Δs_max=0:s_cutoff
			mr = L′L(smin,smax,Δs_max)
			@test length(mr) == sum(length(l′_range(mr,l)) for l in l_range(mr))			
		end
	end

	@testset "L′L all" begin
		for smin=0:s_cutoff,smax=smin:s_cutoff,Δs_max=0:s_cutoff
			mr = L′L(smin,smax,Δs_max)
			for spmin=0:smax+Δs_max,spmax=spmin:smax+Δs_max
				m = L′L(smin,smax,Δs_max,spmin,spmax)
				@test begin
					res = length(mr) == sum(length(l′_range(mr,l)) for l in l_range(mr))
					if !res
						println(mr)
					end
					res
				end
			end
		end
	end
end

@testset "LM ML modes" begin
	s_cutoff = 1
	for smin in 0:s_cutoff,smax=smin:s_cutoff,tmin=-smax:smax,tmax=tmin:smax
		l_range = smin:smax
		m_range = tmin:tmax
		m1 = LM(l_range,m_range)
		m2 = ML(l_range,m_range)
		@test sort(collect(m1)) == sort(collect(m2))
	end
end

@testset "modeindex" begin

	function modeindex2(m::ML,s::Integer,t::Integer)
		N_skip = 0
		for si in m.l_min:s-1
			N_skip += length(m_range(m,si))
		end

		N_skip + searchsortedfirst(m_range(m,s),t)
	end

	function modeindex2(m::LM,s::Integer,t::Integer)
		N_skip = 0
		for ti in m.m_min:t-1
			N_skip += length(l_range(m,ti))
		end

		N_skip + searchsortedfirst(l_range(m,t),s)
	end

	function modeindex2(m::L′L,s′::Integer,s::Integer)
		N_skip = 0
		for si in m.l_min:s-1
			N_skip += length(l′_range(m,si))
		end

		N_skip + searchsortedfirst(l′_range(m,s),s′)
	end

	modeindex2(m::SHModeRange,(s,t)::Tuple) = modeindex(m,s,t)

	s_cutoff = 5
	@testset "LM" begin
		for smin=0:s_cutoff,smax=smin:s_cutoff,tmin=-smax:smax,tmax=tmin:smax
			m1 = LM(smin,smax,tmin,tmax)
			for (s,t) in m1
				@test modeindex(m1,s,t) == modeindex2(m1,s,t)
			end
			m1c = collect(m1)
			for t in m_range(m1), s1 in l_range(m1,t), 
					s2 in l_range(m1,t)
				
				smin,smax=minmax(s1,s2)
				@test modeindex(m1,smin:smax,t) == 
				findfirst(isequal((smin,t)),m1c):findfirst(isequal((smax,t)),m1c)
			end
		end
	end

	@testset "ML" begin
		for smin=0:s_cutoff,smax=smin:s_cutoff,tmin=-smax:smax,tmax=tmin:smax
			m2 = ML(smin,smax,tmin,tmax)
			for (s,t) in m2
				@test modeindex(m2,s,t) == modeindex2(m2,s,t)
			end
			m2c = collect(m2)
			for s in l_range(m2), t1 in  m_range(m2,s), 
					t2 in m_range(m2,s)

				tmin,tmax = minmax(t1,t2)
				@test modeindex(m2,s,tmin:tmax) == 
				findfirst(isequal((s,tmin)),m2c):findfirst(isequal((s,tmax)),m2c)
			end
		end
	end

	@testset "L′L" begin
		for smin=0:s_cutoff,smax=smin:s_cutoff,
			Δs_max=0:s_cutoff,s′min=0:s_cutoff,s′max=s′min:s_cutoff

			m3 = L′L(smin,smax,Δs_max,s′min,s′max)
			for (s′,s) in m3
				@test begin 
					res = modeindex(m3,s′,s) == modeindex2(m3,s′,s)
					if !res
						println(m3)
					end
					res
				end
			end
			m3c = collect(m3)
			for s in l_range(m3), s′1 in l′_range(m3,s), 
				s′2 in l′_range(m3,s)

				s′min,s′max = minmax(s′1,s′2)
				@test begin 
					res = modeindex(m3,s′min:s′max,s) == 
					findfirst(isequal((s′min,s)),m3c):findfirst(isequal((s′max,s)),m3c)
					if !res
						println(m3)
					end
					res
				end
			end
		end
	end

	@testset "l, l′ and m ranges" begin
	    @testset "LM" begin
	        m=LM(0:3,-2:1)
	        sr = 1:3; tr = 0:1
	        ind1 = modeindex(m,minimum(sr),minimum(tr))
	        ind2 = modeindex(m,maximum(sr),maximum(tr))
	        @test modeindex(m,sr,tr) == ind1:ind2
	        @test_throws ModeMissingError modeindex(m,0:5,0:2)
	        @test_throws NonContiguousError modeindex(m,1:3,-1:0)
	        @test_throws InvalidModeError modeindex(m,l_range(m),m_range(m))
	    end
	    @testset "ML" begin
	        m=ML(0:5,-2:1)
	        sr = 2:5; tr = -2:1
	        ind1 = modeindex(m,minimum(sr),minimum(tr))
	        ind2 = modeindex(m,maximum(sr),maximum(tr))
	        @test modeindex(m,sr,tr) == ind1:ind2
	        @test_throws ModeMissingError modeindex(m,0:5,0:2)
	        @test_throws NonContiguousError modeindex(m,4:5,0:1)
	        @test_throws InvalidModeError modeindex(m,l_range(m),m_range(m))
	    end
	    @testset "L′L" begin
	        m=L′L(0:3,2,0:3);
	        s′r = 0:2; sr = 0:1;
	        ind1 = modeindex(m,minimum(s′r),minimum(sr))
	        ind2 = modeindex(m,maximum(s′r),maximum(sr))
	        @test modeindex(m,s′r,sr) == ind1:ind2
	        @test_throws ModeMissingError modeindex(m,0:4,0:4)
	        @test_throws NonContiguousError modeindex(m,1:3,1:2)
	        @test_throws NonContiguousError modeindex(m,l′_range(m),l_range(m))
	    end
	end

	@testset "ModeRange" begin
	    @testset "LM" begin
	        m=LM(0:2)
	        # Non-Continguous
	        mpart=LM(0:1,0:1)
	        @test modeindex(m,mpart) == 4:7
	        # Contiguous
	        mpart=LM(0:2,0:0)
	        @test modeindex(m,mpart) == 4:6
	        @test collect(m)[modeindex(m,mpart)] == collect(mpart)
	    end
	    @testset "ML" begin
	        m=ML(0:2,-1:1)
	        # Non-Continguous
	        mpart=ML(1:2,0:1)
	        @test modeindex(m,mpart) == 3:7
	        # Contiguous
	        mpart=ML(1:2,-1:1)
	        @test modeindex(m,mpart) == 2:7
	        @test collect(m)[modeindex(m,mpart)] == collect(mpart)
	    end
	    @testset "L′L" begin
	        m=L′L(0:2,2);
	        # Non-Continguous
	        mpart=L′L(1:2,1,1:2)
	        @test modeindex(m,mpart) == 5:10
	        # Contiguous
	        mpart=L′L(1:2,2)
	        @test modeindex(m,mpart) == 4:12
	        @test collect(m)[modeindex(m,mpart)] == collect(mpart)
	    end
	end
end

@testset "last" begin
	@testset "ML" begin
		m1 = ML(rand(1:5),rand(6:10))
		@test last(collect(m1)) == last(m1)    
	end

	@testset "LM" begin
		m2 = LM(rand(1:5),rand(6:10))
		@test last(collect(m2)) == last(m2)
	end
	
	@testset "L′L" begin
		m3 = L′L(rand(1:3):rand(4:10),rand(1:5))
		@test last(collect(m3)) == last(m3)
	end
end

