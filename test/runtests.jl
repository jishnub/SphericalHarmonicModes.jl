using Test,SphericalHarmonicModes
import SphericalHarmonicModes: ModeMissingError, SHModeRange, 
check_if_lm_range_is_valid

import SphericalHarmonicModes: flip

@test isempty(Test.detect_ambiguities(Base, Core, SphericalHarmonicModes))

@testset "constructors" begin

	@testset "Special Ranges" begin
	    @testset "ZeroTo" begin
	        r = ZeroTo(3)
	        @test first(r) == 0
	        @test last(r) == 3
	        @test !isempty(r)
	    end
	    @testset "FullRange" begin
	        r = FullRange(3)
	        @test first(r) == -3
	        @test last(r) == 3
	        @test !isempty(r)
	    end
	    @testset "ZeroTo" begin
	        r = ZeroTo(3)
	        @test first(r) == 0
	        @test last(r) == 3
	        @test !isempty(r)
	    end
	    @testset "ToZero" begin
	        r = ToZero(3)
	        @test first(r) == -3
	        @test last(r) == 0
	        @test !isempty(r)
	    end
	    @testset "SingleValuedRange" begin
	    	n = 3
	        r = SingleValuedRange(n)
	        @test r == n:n
	        @test first(r) == n
	        @test last(r) == n
	        @test length(r) == 1
	        @test !isempty(r)
	    end
	end

	@testset "LM" begin
		@test eltype(LM(1:2)) == Tuple{Int,Int}
		@test LM{UnitRange{Int},UnitRange{Int}}(2:3,2:2) == LM(2:3,2:2)
		@test LM(1:3,2:2) == LM(2:3,2:2)
		@test LM(1:3) == LM(1:3, -3:3)
		@test LM(1:3, FullRange) == LM(1:3, -3:3)
		@test LM(1:3, ZeroTo) == LM(1:3, 0:3)
		@test LM(1:3, ToZero) == LM(1:3, -3:0)
		@test LM(ZeroTo(3)) == LM(0:3, -3:3)
		@test LM(ZeroTo(3), 1:2) == LM(0:3, 1:2)
		@test LM(ZeroTo(3), FullRange) == LM(0:3, -3:3)
		@test LM(ZeroTo(3), ZeroTo) == LM(0:3, 0:3)
		@test LM(ZeroTo(3), ToZero) == LM(0:3, -3:0)

		@test_throws ArgumentError LM(1:0)
		@test_throws ArgumentError LM(1:1, 1:0)
		@test_throws ArgumentError LM(-1:1)
		@test_throws ArgumentError LM(0:1, 1:3)
		@test_throws ArgumentError LM(0:1, 3:3)
		@test_throws ArgumentError LM(0:1, -3:0)
	end

	@testset "ML" begin
		@test eltype(ML(1:2)) == Tuple{Int,Int}
		@test ML{UnitRange{Int},UnitRange{Int}}(2:3,2:2) == ML(2:3,2:2)
		@test ML(1:3,2:2) == ML(2:3,2:2)
		@test ML(1:3) == ML(1:3, -3:3)
		@test ML(1:3, FullRange) == ML(1:3, -3:3)
		@test ML(1:3, ZeroTo) == ML(1:3, 0:3)
		@test ML(1:3, ToZero) == ML(1:3, -3:0)
		@test ML(ZeroTo(3)) == ML(0:3, -3:3)
		@test ML(ZeroTo(3), 1:2) == ML(0:3, 1:2)
		@test ML(ZeroTo(3), FullRange) == ML(0:3, -3:3)
		@test ML(ZeroTo(3), ZeroTo) == ML(0:3, 0:3)
		@test ML(ZeroTo(3), ToZero) == ML(0:3, -3:0)

		@test_throws ArgumentError ML(1:0)
		@test_throws ArgumentError ML(1:1, 1:0)
		@test_throws ArgumentError ML(-1:1)
		@test_throws ArgumentError ML(0:1, 1:3)
		@test_throws ArgumentError ML(0:1, 3:3)
		@test_throws ArgumentError ML(0:1, -3:0)
	end

	@testset "L2L1Triangle " begin
		Δl_max = 3
		l_range = 1:10
		m = L2L1Triangle(l_range, Δl_max)
		@test m == L2L1Triangle(l_range,LM(0:Δl_max,0:0))
		@test m == L2L1Triangle(l_range,ML(0:Δl_max,0:0))

		l2_range = 5:8
		mr = L2L1Triangle(l_range, Δl_max, l2_range)
		@test mr == L2L1Triangle(2:10,3,5:8)
		mr = L2L1Triangle(extrema(l_range)..., Δl_max, l2_range)
		@test mr == L2L1Triangle(2:10,3,5:8)
		
		l_range = 10:10
		l2_range = 1:8
		mr = L2L1Triangle(l_range, Δl_max, l2_range)
		@test mr == L2L1Triangle(10:10,3,7:8)

		l2_range = 1:20
		mr = L2L1Triangle(l_range, Δl_max, l2_range)
		@test mr == L2L1Triangle(10:10,3,7:13)

		l_range = 1:10
		l2_range = 1:3
		mr = L2L1Triangle(l_range, Δl_max, l2_range)
		@test mr == L2L1Triangle(1:6,3,1:3)

		@test eltype(m) == Tuple{Int,Int}
	end

	@testset "keys" begin
		m = LM(1:2)
		@test keys(m) == Base.OneTo(8)
		@test eachindex(m) == Base.OneTo(8)

		m = ML(1:2)
		@test keys(m) == Base.OneTo(8)
		@test eachindex(m) == Base.OneTo(8)

		m = L2L1Triangle(1:2,1)
		@test keys(m) == Base.OneTo(6)
		@test eachindex(m) == Base.OneTo(6)
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

	function testlength(iterated_length, mr)
		@test begin
			res = length(mr) == iterated_length(mr) == length(collect(mr))
			if !res
				@show mr
			end
			res
		end
	end

	iterated_length(mr::ML) = iterated_length_template(mr,
								x->sum(length(m_range(x,l)) for l in l_range(x)))

	iterated_length(mr::LM) = iterated_length_template(mr,
								x->sum(length(l_range(x,m)) for m in m_range(x)))

	@testset "ML" begin
		for l_min=0:l_cutoff, l_max=l_min:l_cutoff

			for T in [FullRange, ZeroTo, ToZero]
				mr = ML(l_min:l_max, T)
				testlength(iterated_length, mr)

				mr = ML(ZeroTo(l_max), T)
				testlength(iterated_length, mr)

				mr = ML(SingleValuedRange(l_min), T)
				testlength(iterated_length, mr)
			end

			mr = ML(l_min:l_min, SingleValuedRange(l_min))
			testlength(iterated_length, mr)

			mr = ML(ZeroTo(l_min), SingleValuedRange(l_min))
			testlength(iterated_length, mr)

			mr = ML(SingleValuedRange(l_min), SingleValuedRange(l_min))
			testlength(iterated_length, mr)

			for m_min=-l_max:l_max, m_max=m_min:l_max
		
				l_min_trimmed = max(l_min,max(m_min,-m_max))
				mr = ML(l_min_trimmed:l_max, m_min:m_max)
			
				testlength(iterated_length, mr)
			end
		end
	end

	@testset "LM" begin
		for l_min=0:l_cutoff, l_max=l_min:l_cutoff
			
			for T in [FullRange, ZeroTo, ToZero]
				mr = LM(l_min:l_max, T)
				testlength(iterated_length, mr)

				mr = LM(ZeroTo(l_max), T)
				testlength(iterated_length, mr)

				mr = LM(SingleValuedRange(l_min), T)
				testlength(iterated_length, mr)
			end

			mr = LM(l_min:l_min, SingleValuedRange(l_min))
			testlength(iterated_length, mr)

			mr = LM(ZeroTo(l_min), SingleValuedRange(l_min))
			testlength(iterated_length, mr)

			mr = LM(SingleValuedRange(l_min), SingleValuedRange(l_min))
			testlength(iterated_length, mr)

			for m_min=-l_max:l_max, m_max=m_min:l_max
			
				l_min_trimmed = max(l_min,max(m_min,-m_max))
				mr = LM(l_min_trimmed:l_max, m_min:m_max)
				
				testlength(iterated_length, mr)
			end
		end
	end

	@testset "LM == ML" begin
	    for l_min=0:l_cutoff, l_max=l_min:l_cutoff

	    	for T in [FullRange, ZeroTo, ToZero]
		    	m1 = LM(l_min:l_max, T)
				m2 = ML(l_min:l_max, T)
				@test length(m1) == length(m2)

		    	m1 = LM(ZeroTo(l_max), T)
				m2 = ML(ZeroTo(l_max), T)
				@test length(m1) == length(m2)

				m1 = LM(SingleValuedRange(l_min), T)
				m2 = ML(SingleValuedRange(l_min), T)
				@test length(m1) == length(m2)
			end

			m1 = LM(l_min:l_min, SingleValuedRange(l_min))
			m2 = ML(l_min:l_min, SingleValuedRange(l_min))
			@test length(m1) == length(m2)

			m1 = LM(ZeroTo(l_min), SingleValuedRange(l_min))
			m1 = ML(ZeroTo(l_min), SingleValuedRange(l_min))
			@test length(m1) == length(m2)

			m1 = LM(SingleValuedRange(l_min), SingleValuedRange(l_min))
			m2 = ML(SingleValuedRange(l_min), SingleValuedRange(l_min))
			@test length(m1) == length(m2)

	    	for m_min=-l_max:l_max,m_max=m_min:l_max
		    	
		    	l_min_trimmed = max(l_min,max(m_min,-m_max))
				
				m1 = LM(l_min_trimmed:l_max, m_min:m_max)
				m2 = ML(l_min_trimmed:l_max, m_min:m_max)
				
				@test length(m1) == length(m2)
			end
		end
	end

	@testset "L2L1Triangle" begin
		iterated_length(mr) = iterated_length_template(mr,
								x->sum(length(l2_range(x,l1)) for l1 in l1_range(x)))
		@testset "default s′minmax" begin
			for l_min=0:l_cutoff,l_max=l_min:l_cutoff,Δl_max=0:l_cutoff
				mr = L2L1Triangle(l_min,l_max,Δl_max)
				@test length(mr) == iterated_length(mr)
			end
		end

		@testset "all" begin
			for l_min=0:l_cutoff,l_max=l_min:l_cutoff,Δl_max=0:l_cutoff,
				l2_min=max(l_min-Δl_max,0):l_max+Δl_max,l2_max=l2_min:l_max+Δl_max

					l_min_trimmed = max(l_min,l2_min - Δl_max)
					l_max_trimmed = min(l_max,l2_max + Δl_max)
					mr = L2L1Triangle(l_min_trimmed,l_max_trimmed,Δl_max,l2_min,l2_max)
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

@testset "iterate" begin
    @testset "LM" begin
        m = LM(1:2)
        mode,state = iterate(m)
        @test mode == (2,-2)
        mode,state = iterate(m, state)
        @test mode == (1,-1)

        @test iterate(m, (last(m),length(m) + 1)) === nothing
    end
    @testset "ML" begin
        m = ML(1:2)
        mode,state = iterate(m)
        @test mode == (1,-1)
        mode,state = iterate(m, state)
        mode,state = iterate(m, state)
        mode,state = iterate(m, state)
        @test mode == (2,-2)

        @test iterate(m, (last(m),length(m) + 1)) === nothing
    end
    @testset "L2L1Triangle" begin
        m = L2L1Triangle(1:2, 1)
        mode,state = iterate(m)
        @test mode == (0,1)
        mode,state = iterate(m, state)
        mode,state = iterate(m, state)
        mode,state = iterate(m, state)
        @test mode == (1,2)

        @test iterate(m, (last(m),length(m) + 1)) === nothing
    end
end

@testset "LM ML modes" begin
	l_cutoff = 5
	for l_min in 0:l_cutoff, l_max=l_min:l_cutoff

		for T in [FullRange, ZeroTo, ToZero]
			m1 = LM(l_min:l_max, T)
			m2 = ML(l_min:l_max, T)

			@test sort(collect(m1)) == sort(collect(m2))
			
			m1 = LM(ZeroTo(l_max), T)
			m2 = ML(ZeroTo(l_max), T)

			@test sort(collect(m1)) == sort(collect(m2))
		end

		for m_min=-l_max:l_max, m_max=m_min:l_max
			
			l_min_trimmed = max(l_min,max(m_min,-m_max))
			l_range = l_min_trimmed:l_max
			m_range = m_min:m_max

			m1 = LM(l_range,m_range)
			m2 = ML(l_range,m_range)

			@test sort(collect(m1)) == sort(collect(m2))
		end
	end
end

@testset "modeindex" begin

	function modeindex2(m::ML,s::Integer,t::Integer)
		N_skip = 0
		l_min = first(l_range(m))
		for si in l_min:s-1
			N_skip += length(m_range(m,si))
		end

		N_skip + searchsortedfirst(m_range(m,s),t)
	end

	function modeindex2(m::LM,s::Integer,t::Integer)
		N_skip = 0
		m_min = first(m_range(m))
		for ti in m_min:t-1
			N_skip += length(l_range(m,ti))
		end

		N_skip + searchsortedfirst(l_range(m,t),s)
	end

	function modeindex2(m::L2L1Triangle,s′::Integer,s::Integer)
		N_skip = 0
		for si in m.l1_min:s-1
			N_skip += length(l2_range(m,si))
		end

		N_skip + searchsortedfirst(l2_range(m,s),s′)
	end

	modeindex2(m::SHModeRange,(s,t)::Tuple) = modeindex(m,s,t)

	function testmodeindex(mr)
		for (ind,(s,t)) in enumerate(mr)
			@test modeindex(mr,(s,t)) == modeindex(mr,s,t) == modeindex2(mr,s,t) == ind
		end
	end

	l_cutoff = 5
	@testset "LM" begin
		for l_min=0:l_cutoff, l_max=l_min:l_cutoff

			for MT in [ZeroTo, ToZero, FullRange]
				mr = LM(l_min:l_max, MT)
				testmodeindex(mr)

				mr = LM(ZeroTo(l_max), MT)
				testmodeindex(mr)
				
				mr = LM(SingleValuedRange(l_min), MT)
				testmodeindex(mr)
			end

			mr = LM(l_min:l_max, SingleValuedRange(l_min))
			testmodeindex(mr)

			mr = LM(ZeroTo(l_max), SingleValuedRange(l_min))
			testmodeindex(mr)
			
			mr = LM(SingleValuedRange(l_min), SingleValuedRange(l_min))
			testmodeindex(mr)
			
			for m_min=-l_max:l_max, m_max=m_min:l_max
				
				l_min_trimmed = max(l_min,max(m_min,-m_max))
			
				mr = LM(l_min_trimmed:l_max, m_min:m_max)
				testmodeindex(mr)
			end
		end
	end

	@testset "ML" begin
		for l_min=0:l_cutoff, l_max=l_min:l_cutoff

			for MT in [ZeroTo, ToZero, FullRange]
				mr = ML(l_min:l_max, MT)
				testmodeindex(mr)

				mr = ML(ZeroTo(l_max), MT)
				testmodeindex(mr)

				mr = ML(SingleValuedRange(l_min), MT)
				testmodeindex(mr)
			end

			mr = ML(l_min:l_max, SingleValuedRange(l_min))
			testmodeindex(mr)

			mr = ML(ZeroTo(l_max), SingleValuedRange(l_min))
			testmodeindex(mr)
			
			mr = ML(SingleValuedRange(l_min), SingleValuedRange(l_min))
			testmodeindex(mr)

			for m_min=-l_max:l_max, m_max=m_min:l_max
			
				l_min_trimmed = max(l_min,max(m_min,-m_max))

				mr = ML(l_min_trimmed:l_max, m_min:m_max)
				testmodeindex(mr)
			end
		end
	end

	@testset "L2L1Triangle" begin
		for l_min=0:l_cutoff,l_max=l_min:l_cutoff,
			Δl_max=0:l_cutoff,
			s′min=max(0,l_min-Δl_max):l_max+Δl_max,s′max=s′min:l_max+Δl_max

			l_max_trimmed = min(l_max,s′max + Δl_max)
			l_min_trimmed = max(l_min,s′min - Δl_max)

			m3 = L2L1Triangle(l_min_trimmed,l_max_trimmed,Δl_max,s′min,s′max)
			for (ind,(s′,s)) in enumerate(m3)
				@test begin 
					res = modeindex(m3,(s′,s)) == modeindex(m3,s′,s) == modeindex2(m3,s′,s) == ind
					if !res
						println(m3)
					end
					res
				end
			end
		end
	end
end

@testset "last" begin
	@testset "ML" begin
		m1 = ML(rand(1:5):rand(6:10))
		@test last(collect(m1)) == last(m1)    
	end

	@testset "LM" begin
		m2 = LM(rand(1:5):rand(6:10))
		@test last(collect(m2)) == last(m2)
	end
	
	@testset "L2L1Triangle" begin
		m3 = L2L1Triangle(rand(1:3):rand(4:10),rand(1:5))
		@test last(collect(m3)) == last(m3)
	end
end

@testset "firstindex & lastindex" begin
    @testset "firstindex" begin
        mr = LM(1:2)
        @test firstindex(mr) == 1
        mr = ML(1:2)
        @test firstindex(mr) == 1
        mr = L2L1Triangle(1:3,1)
        @test firstindex(mr) == 1
    end
    @testset "lastindex" begin
        mr = LM(1:2)
        @test lastindex(mr) == 8
        mr = ML(1:2)
        @test lastindex(mr) == 8
        mr = L2L1Triangle(1:3,1)
        @test lastindex(mr) == 9
    end
end

@testset "size & axes" begin
    @testset "size" begin
        mr = LM(1:2)
        @test size(mr) == (8,)
        @test size(mr,1) == 8
        @test size(mr,2) == 1
        mr = ML(1:2)
        @test size(mr) == (8,)
        @test size(mr,1) == 8
        @test size(mr,2) == 1
        mr = L2L1Triangle(1:3,1)
        @test size(mr) == (9,)
        @test size(mr,1) == 9
        @test size(mr,2) == 1
    end
    @testset "axes" begin
        mr = LM(1:2)
        @test axes(mr) == (Base.OneTo(8),)
        @test axes(mr,1) == Base.OneTo(8)
        @test axes(mr,2) == Base.OneTo(1)
        mr = ML(1:2)
        @test axes(mr) == (Base.OneTo(8),)
        @test axes(mr,1) == Base.OneTo(8)
        @test axes(mr,2) == Base.OneTo(1)
        mr = L2L1Triangle(1:3,1)
        @test axes(mr) == (Base.OneTo(9),)
        @test axes(mr,1) == Base.OneTo(9)
        @test axes(mr,2) == Base.OneTo(1)
    end
end

@testset "flip" begin
    @testset "LM" begin
		m = LM(0:1)
		@test ML(m) == ML(0:1)
		@test flip(m) == ML(0:1)
		@test LM(m) === m
    end
    @testset "ML" begin
		m = ML(0:1)
		@test LM(m) == LM(0:1)
		@test flip(m) == LM(0:1)
		@test ML(m) === m
    end
end

@testset "intersect" begin
	@testset "same l, different m" begin
		for T in [LM,ML]
			@eval begin
			    l1 = $T(3:3,0:2)
			    l2 = $T(3:3,1:3)
			    @test intersect(l1,l2) == $T(3:3,1:2)

			    l1 = $T(1:3)
			    l2 = $T(1:3,1:3)
			    @test intersect(l1,l2) == $T(1:3,1:3)
			    
			    for MT in [ZeroTo, ToZero]
				    local l1 = $T(1:3, MT)
				    local l2 = $T(1:3, MT)
				    @test intersect(l1,l2) == intersect(l2,l1) == $T(1:3, MT)

				    local l1 = $T(1:3, MT)
				    local l2 = $T(1:3, FullRange)
				    @test intersect(l1,l2) == intersect(l2,l1) == $T(1:3, MT)
				end

				l1 = $T(1:3, ZeroTo)
			    l2 = $T(1:3, ToZero)
			    @test intersect(l1,l2) == $T(1:3, ZeroTo(0))
			    @test intersect(l2,l1) == $T(1:3, ZeroTo(0))

			end
		end
	end
	@testset "different l, same m" begin
		for T in [LM,ML]
			@eval begin
			    l1 = $T(1:3,1:1)
			    l2 = $T(1:1,1:1)
			    @test intersect(l1,l2) == $T(1:1,1:1)

			    l1 = $T(2:2)
			    l2 = $T(1:3,-2:2)
			    @test intersect(l1,l2) == $T(2:2,-2:2)

			    l1 = $T(ZeroTo(2))
			    l2 = $T(1:3,-2:2)
			    @test intersect(l1,l2) == $T(1:2,-2:2)

			    l1 = $T(ZeroTo(2))
			    l2 = $T(ZeroTo(3),-2:2)
			    @test intersect(l1,l2) == $T(ZeroTo(2),-2:2)
			end
		end
	end
	@testset "different l, different m" begin
		for T in [LM,ML]
			@eval begin
			    l1 = $T(2:3,2:2)
			    l2 = $T(0:2)
			    @test intersect(l1,l2) == $T(2:2,2:2)

			    l1 = $T(2:3)
			    l2 = $T(1:10,-1:10)
			    @test intersect(l1,l2) == $T(2:3,-1:3)

			    l1 = $T(2:3)
			    l2 = $T(9:10, 5:5)
			    @test intersect(l1,l2) === nothing

			    for MT in [FullRange, ZeroTo, ToZero]
				    local l1 = $T(2:3,MT)
				    local l2 = $T(1:4,MT)
			    	@test intersect(l1,l2) == $T(2:3,MT)

			    	local l1 = $T(ZeroTo(3),MT)
				    local l2 = $T(ZeroTo(4),MT)
			    	@test intersect(l1,l2) == $T(ZeroTo(3),MT)
			    end
			end
		end
	end
end

@testset "show" begin
    io = IOBuffer()

    function testshow(io, x)
    	show(io, x)
    	show(io, MIME"text/plain"(), x)
    end
    
    testshow(io, SingleValuedRange(2))
    testshow(io, ZeroTo(2))
    testshow(io, ToZero(2))
    testshow(io, FullRange(2))

    testshow(io, LM(1:2,1:1))
    testshow(io, ML(1:2,1:1))
    testshow(io, L2L1Triangle(1:2,1))

    showerror(io, ModeMissingError(2,2,LM(1:1)))
    showerror(io, ModeMissingError(3,3,L2L1Triangle(1:1,1)))
end