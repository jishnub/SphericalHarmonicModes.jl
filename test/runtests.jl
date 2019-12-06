using Test,SphericalHarmonicModes
import SphericalHarmonicModes: NonContiguousError, ModeMissingError, InvalidModeError,
NegativeDegreeError, mOutOfBoundsError, SHModeRange

@testset "constructors" begin

	@testset "LM" begin
		@test LM(1:3,-1:1) == LM(1,3,-1,1)
		@test LM(1:3,1) == LM(1,3,1,1)
		@test LM(2,1:2) == LM(2,2,1,2)
		@test LM(0:3) == LM(0,3,-3,3)
		@test LM(2) == LM(2,2,-2,2)
	end

	@testset "ML" begin
		@test ML(1:3,-1:1) == ML(1,3,-1,1)
		@test ML(1:3,1) == ML(1,3,1,1)
		@test ML(2,1:2) == ML(2,2,1,2)
		@test ML(0:3) == ML(0,3,-3,3)
		@test ML(2) == ML(2,2,-2,2)
	end

	@testset "L₂L₁Δ " begin
		Δl_max = rand(1:3)
		l_range = rand(1:3):rand(4:10)
		@test L₂L₁Δ(l_range,Δl_max) == L₂L₁Δ(l_range,LM(0:Δl_max,0))
		@test L₂L₁Δ(l_range,Δl_max) == L₂L₁Δ(l_range,ML(0:Δl_max,0))
	end
end

@testset "length" begin
	l_cutoff=5
	
	function iterated_length_template(mr,f)
		try
			return f(mr)
		catch e
			if e isa ArgumentError
				show(mr)
				println()
				return -1
			else
				throw(e)
			end
		end
	end

	@testset "ML" begin
		iterated_length(mr) = iterated_length_template(mr,
								x->sum(length(m_range(x,l)) for l in l_range(x)))
		for l_min=0:l_cutoff,l_max=l_min:l_cutoff,m_min=-l_max:l_max,m_max=m_min:l_max
			l_min_trimmed = max(l_min,max(m_min,-m_max))
			mr = ML(l_min_trimmed,l_max,m_min,m_max)
			@test begin
				res = length(mr) == iterated_length(mr)
				if !res
					println(mr)
				end
				res
			end
		end
	end

	@testset "LM" begin
		iterated_length(mr) = iterated_length_template(mr,
								x->sum(length(l_range(x,m)) for m in m_range(x)))
		for l_min=0:l_cutoff,l_max=l_min:l_cutoff,m_min=-l_max:l_max,m_max=m_min:l_max
			l_min_trimmed = max(l_min,max(m_min,-m_max))
			mr = LM(l_min_trimmed,l_max,m_min,m_max)
			@test begin
				res = length(mr) == iterated_length(mr)
				if !res
					println(mr)
				end
				res
			end
		end
	end

	@testset "LM==ML" begin
	    for l_min=0:l_cutoff,l_max=l_min:l_cutoff,m_min=-l_max:l_max,m_max=m_min:l_max
	    	l_min_trimmed = max(l_min,max(m_min,-m_max))
			m1 = LM(l_min_trimmed,l_max,m_min,m_max)
			m2 = ML(l_min_trimmed,l_max,m_min,m_max)
			@test length(m1) == length(m2)
		end
	end

	@testset "L₂L₁Δ" begin
		iterated_length(mr) = iterated_length_template(mr,
								x->sum(length(l₂_range(x,l₁)) for l₁ in l₁_range(x)))
		@testset "default s′minmax" begin
			for l_min=0:l_cutoff,l_max=l_min:l_cutoff,Δl_max=0:l_cutoff
				mr = L₂L₁Δ(l_min,l_max,Δl_max)
				@test length(mr) == iterated_length(mr)
			end
		end

		@testset "all" begin
			for l_min=0:l_cutoff,l_max=l_min:l_cutoff,Δl_max=0:l_cutoff,
				l₂_min=max(l_min-Δl_max,0):l_max+Δl_max,l₂_max=l₂_min:l_max+Δl_max

					l_min_trimmed = max(l_min,l₂_min - Δl_max)
					l_max_trimmed = min(l_max,l₂_max + Δl_max)
					mr = L₂L₁Δ(l_min_trimmed,l_max_trimmed,Δl_max,l₂_min,l₂_max)
					@test begin
						res = length(mr) == iterated_length(mr)
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
	l_cutoff = 1
	for l_min in 0:l_cutoff,l_max=l_min:l_cutoff,m_min=-l_max:l_max,m_max=m_min:l_max
		l_min_trimmed = max(l_min,max(m_min,-m_max))
		l_range = l_min_trimmed:l_max
		m_range = m_min:m_max
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

	function modeindex2(m::L₂L₁Δ,s′::Integer,s::Integer)
		N_skip = 0
		for si in m.l₁_min:s-1
			N_skip += length(l₂_range(m,si))
		end

		N_skip + searchsortedfirst(l₂_range(m,s),s′)
	end

	modeindex2(m::SHModeRange,(s,t)::Tuple) = modeindex(m,s,t)

	l_cutoff = 5
	@testset "LM" begin
		for l_min=0:l_cutoff,l_max=l_min:l_cutoff,m_min=-l_max:l_max,m_max=m_min:l_max
			l_min_trimmed = max(l_min,max(m_min,-m_max))
			m1 = LM(l_min_trimmed,l_max,m_min,m_max)
			for (s,t) in m1
				@test modeindex(m1,s,t) == modeindex2(m1,s,t)
			end
			m1c = collect(m1)
			for t in m_range(m1), s1 in l_range(m1,t), 
					s2 in l_range(m1,t)
				
				l_min,l_max=minmax(s1,s2)
				@test modeindex(m1,l_min:l_max,t) == 
				findfirst(isequal((l_min,t)),m1c):findfirst(isequal((l_max,t)),m1c)
			end
		end
	end

	@testset "ML" begin
		for l_min=0:l_cutoff,l_max=l_min:l_cutoff,m_min=-l_max:l_max,m_max=m_min:l_max
			l_min_trimmed = max(l_min,max(m_min,-m_max))
			m2 = ML(l_min_trimmed,l_max,m_min,m_max)
			for (s,t) in m2
				@test modeindex(m2,s,t) == modeindex2(m2,s,t)
			end
			m2c = collect(m2)
			for s in l_range(m2), t1 in  m_range(m2,s), 
					t2 in m_range(m2,s)

				m_min,m_max = minmax(t1,t2)
				@test modeindex(m2,s,m_min:m_max) == 
				findfirst(isequal((s,m_min)),m2c):findfirst(isequal((s,m_max)),m2c)
			end
		end
	end

	@testset "L₂L₁Δ" begin
		for l_min=0:l_cutoff,l_max=l_min:l_cutoff,
			Δl_max=0:l_cutoff,
			s′min=max(0,l_min-Δl_max):l_max+Δl_max,s′max=s′min:l_max+Δl_max

			l_max_trimmed = min(l_max,s′max + Δl_max)
			l_min_trimmed = max(l_min,s′min - Δl_max)

			m3 = L₂L₁Δ(l_min_trimmed,l_max_trimmed,Δl_max,s′min,s′max)
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
			for s in l₁_range(m3), s′1 in l₂_range(m3,s), 
				s′2 in l₂_range(m3,s)

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

	@testset "ranges" begin
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
	    @testset "L₂L₁Δ" begin
	        m=L₂L₁Δ(0:3,2,0:3);
	        s′r = 0:2; sr = 0:1;
	        ind1 = modeindex(m,minimum(s′r),minimum(sr))
	        ind2 = modeindex(m,maximum(s′r),maximum(sr))
	        @test modeindex(m,s′r,sr) == ind1:ind2
	        @test_throws ModeMissingError modeindex(m,0:4,0:4)
	        @test_throws NonContiguousError modeindex(m,1:3,1:2)
	        @test_throws NonContiguousError modeindex(m,l₂_range(m),l₁_range(m))
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
	    @testset "L₂L₁Δ" begin
	        m=L₂L₁Δ(0:2,2);
	        # Non-Continguous
	        mpart=L₂L₁Δ(1:2,1,1:2)
	        @test modeindex(m,mpart) == 5:10
	        # Contiguous
	        mpart=L₂L₁Δ(1:2,2)
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
	
	@testset "L₂L₁Δ" begin
		m3 = L₂L₁Δ(rand(1:3):rand(4:10),rand(1:5))
		@test last(collect(m3)) == last(m3)
	end
end

