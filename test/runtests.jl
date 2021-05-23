using Test,SphericalHarmonicModes
import SphericalHarmonicModes: ModeMissingError, SHModeRange,
check_if_lm_range_is_valid, flip

using Aqua
@testset "project quality" begin
    Aqua.test_all(SphericalHarmonicModes)
end

promote_typeof(x, y) = mapreduce(typeof, promote_type, (x, y))

@testset "constructors" begin

	@testset "Special Ranges" begin
	    @testset "ZeroTo" begin
	        r = ZeroTo(3)
	        @test first(r) == 0
	        @test last(r) == 3
	        @test !isempty(r)

	        r = ZeroTo(UInt8(3))
			@test first(r) == 0
	        @test last(r) == 3
	        @test !isempty(r)

            for rin in Any[0:2, ZeroTo(2), ZeroTo{true}(2), ZeroTo{false}(2)]
                r = ZeroTo(rin)
                @test first(r) == 0
                @test last(r) == 2
                r = ZeroTo{false}(rin)
                @test first(r) == 0
                @test last(r) == 2
            end
            for rin in Any[FullRange(0), ToZero(0), SingleValuedRange(0)]
                r = ZeroTo(rin)
                @test r isa ZeroTo
                @test first(r) == last(r) == 0
                r = ZeroTo{false}(rin)
                @test r isa ZeroTo{false}
                @test first(r) == last(r) == 0
            end
            @test_throws ArgumentError ZeroTo(1:2)
            @test_throws Exception ZeroTo{true}(ZeroTo(1))
            for r in Any[ToZero(1), FullRange(1), SingleValuedRange(1)]
                @test_throws Exception ZeroTo(r)
                @test_throws Exception ZeroTo{false}(r)
                @test_throws Exception ZeroTo{true}(r)
            end
	    end
	    @testset "FullRange" begin
	        r = FullRange(3)
	        @test first(r) == -3
	        @test last(r) == 3
	        @test !isempty(r)

            r = FullRange(-1:1)
            @test first(r) == -1
            @test last(r) == 1
            @test_throws ArgumentError FullRange(0:1)

            for rin in Any[-2:2, FullRange(2), FullRange{false}(2), FullRange{true}(2)]
                r = FullRange(rin)
                @test r isa FullRange
                @test first(r) == -2
                @test last(r) == 2
                r = FullRange{false}(rin)
                @test r isa FullRange{false}
                @test first(r) == -2
                @test last(r) == 2
            end
            for rin in Any[ZeroTo(0), ToZero(0), SingleValuedRange(0)]
                r = FullRange(rin)
                @test r isa FullRange
                @test first(r) == last(r) == 0
                r = FullRange{false}(rin)
                @test r isa FullRange{false}
                @test first(r) == last(r) == 0
            end
            @test_throws ArgumentError FullRange(1:2)
            @test_throws Exception FullRange{true}(FullRange(1))
            for r in Any[ToZero(1), ZeroTo(1), SingleValuedRange(1)]
                @test_throws Exception FullRange(r)
                @test_throws Exception FullRange{false}(r)
                @test_throws Exception FullRange{true}(r)
            end
	    end
	    @testset "ToZero" begin
	        r = ToZero(3)
	        @test first(r) == -3
	        @test last(r) == 0
	        @test !isempty(r)

            r = ToZero(-2:0)
            @test first(r) == -2
            @test last(r) == 0
            @test_throws ArgumentError ToZero(-2:-1)

            for rin in Any[-2:0, ToZero(2), ToZero{true}(2), ToZero{false}(2)]
                r = ToZero(rin)
                @test r isa ToZero
                @test first(r) == -2
                @test last(r) == 0
                r = ToZero{false}(rin)
                @test r isa ToZero{false}
                @test first(r) == -2
                @test last(r) == 0
            end
            @test_throws ArgumentError ToZero(1:2)
            @test_throws Exception ToZero{true}(ToZero(1))
            for rin in Any[ZeroTo(0), FullRange(0), SingleValuedRange(0)]
                r = ToZero(rin)
                @test r isa ToZero
                @test first(r) == last(r) == 0
                r = ToZero{false}(rin)
                @test r isa ToZero{false}
                @test first(r) == last(r) == 0
            end
            for r in Any[ZeroTo(1), FullRange(1), SingleValuedRange(1)]
                @test_throws Exception ToZero(r)
                @test_throws Exception ToZero{false}(r)
                @test_throws Exception ToZero{true}(r)
            end
	    end
	    @testset "SingleValuedRange" begin
	    	n = 3
	        r = SingleValuedRange(n)
	        @test r == n:n
	        @test first(r) == n
	        @test last(r) == n
	        @test length(r) == 1
	        @test !isempty(r)

            r = SingleValuedRange(n:n)
            @test first(r) == n
            @test last(r) == n
            @test_throws ArgumentError SingleValuedRange(1:2)

            for rin in Any[1:1, SingleValuedRange(1)]
                r = SingleValuedRange(rin)
                @test first(r) == 1
                @test last(r) == 1
            end
            @test_throws ArgumentError SingleValuedRange(1:2)
            for rin in Any[ZeroTo(0), FullRange(0), ToZero(0)]
                r = SingleValuedRange(rin)
                @test first(r) == last(r) == 0
            end
            for r in Any[ZeroTo(1), FullRange(1), ToZero(1)]
                @test_throws Exception SingleValuedRange(r)
            end
	    end
        @testset "promotion" begin
            @test promote_typeof(ZeroTo(1), ZeroTo(1)) == typeof(ZeroTo(1))
            @test promote_typeof(ZeroTo(1), SingleValuedRange(1)) == UnitRange{Int}
            @test promote_typeof(FullRange(1), SingleValuedRange(1)) == UnitRange{Int}
            @test promote_typeof(FullRange(1), ZeroTo(1)) == UnitRange{Int}
            @test promote_typeof(SingleValuedRange(1), SingleValuedRange(2)) == SingleValuedRange
        end
	end

	@testset "LM" begin
		@test eltype(LM(1:2)) == Tuple{Int,Int}
		@test eltype(LM(big(1):big(2))) == Tuple{BigInt,Int}

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

		@test LM(SingleValuedRange(3), 1:2) == LM(3:3, 1:2)
        @test LM(3, 1:2) == LM(3:3, 1:2)
        @test LM(3) == LM(3:3, -3:3)
        @test LM(3, 2) == LM(3:3, 2:2)

		@test_throws ArgumentError LM(1:0)
		@test_throws ArgumentError LM(1:1, 1:0)
		@test_throws ArgumentError LM(-1:1)
		@test_throws ArgumentError LM(0:1, 1:3)
		@test_throws ArgumentError LM(0:1, 3:3)
		@test_throws ArgumentError LM(0:1, -3:0)
		@test_throws ArgumentError LM(SingleValuedRange(1), 3:4)
		@test_throws ArgumentError LM(SingleValuedRange(1), ZeroTo(3))
		@test_throws ArgumentError LM(SingleValuedRange(1), ToZero(3))
		@test_throws ArgumentError LM(SingleValuedRange(1), FullRange(3))

		@testset "type-stability" begin
		    for LT in (:ZeroTo, :SingleValuedRange),
		    	MT in (:ZeroTo, :ToZero, :FullRange)
		    	@eval @test LM($LT(3), $MT) isa LM{<:$LT, <:$MT}
		    end
		    @test LM(ZeroTo(3), SingleValuedRange(2)) isa LM{UnitRange{Int}, SingleValuedRange}
		    @test LM(SingleValuedRange(3), SingleValuedRange(2)) isa LM{SingleValuedRange, SingleValuedRange}
		end

        @testset "promotion" begin
            @test promote_typeof(LM(1,1), LM(2,2)) == LM{SingleValuedRange, SingleValuedRange}
            @test promote_typeof(LM(1), LM(1)) == LM{SingleValuedRange, FullRange{true}}
            @test promote_typeof(LM(1:1), LM(1)) == LM{UnitRange{Int64}, FullRange{true}}
            @test promote_typeof(LM(1:1, ZeroTo), LM(1, ZeroTo)) == LM{UnitRange{Int}, ZeroTo{true}}
            @test promote_typeof(LM(1:1, ZeroTo), LM(1, ZeroTo(1))) == LM{UnitRange{Int}, UnitRange{Int}}
            @test promote_typeof(LM(1:1, 1:1), LM(1,1)) == LM{UnitRange{Int}, UnitRange{Int}}

            iterslist = Any[LM(1), LM(1:2), LM(1:2, 0:0), LM(1:1, ZeroTo), LM(1:2, ZeroTo(1)),
                        LM(1, ZeroTo), LM(1:1, FullRange), LM(1, FullRange), LM(ZeroTo(3), ZeroTo),
                        LM(ZeroTo(2)), LM(ZeroTo(2), FullRange(1))]

            for x in iterslist, y in iterslist
                @test isconcretetype(promote_typeof(x, y))
            end
        end
        @testset "conversion" begin
            for lr in Any[0:1, ZeroTo(1), SingleValuedRange(1)], mr in Any[0:1, ZeroTo(1), ToZero(1), SingleValuedRange(1)]
                convert(LM{UnitRange{Int}, UnitRange{Int}}, LM(lr, mr)) isa LM{UnitRange{Int}, UnitRange{Int}}
            end
            for LT in [UnitRange{Int}, ZeroTo{false}]
                for MT in [ZeroTo{false}, ZeroTo{true}]
                    @test convert(LM{LT, MT}, LM(0:1, 0:1)) isa LM{LT, MT}
                end
                for MT in [UnitRange{Int}, ToZero{false}, ToZero{true}]
                    @test convert(LM{LT, MT}, LM(0:1, -1:0)) isa LM{LT, MT}
                end
                for MT in [FullRange{false}, FullRange{true}]
                    @test convert(LM{LT, MT}, LM(0:1, -1:1)) isa LM{LT, MT}
                end
                @test convert(LM{LT, SingleValuedRange}, LM(0:1, 0:0)) isa LM{LT, SingleValuedRange}
            end
            for LT in [UnitRange{Int}, SingleValuedRange]
                for MT in [ZeroTo{false}, ZeroTo{true}]
                    @test convert(LM{LT, MT}, LM(1:1, 0:1)) isa LM{LT, MT}
                end
                for MT in [ToZero{false}, ToZero{true}]
                    @test convert(LM{LT, MT}, LM(1:1, -1:0)) isa LM{LT, MT}
                end
                for MT in [FullRange{false}, FullRange{true}]
                    @test convert(LM{LT, MT}, LM(1:1, -1:1)) isa LM{LT, MT}
                end
                @test convert(LM{LT, SingleValuedRange}, LM(1:1, 1:1)) isa LM{LT, SingleValuedRange}
            end
        end
	end

	@testset "ML" begin
		@test eltype(ML(1:2)) == Tuple{Int,Int}
		@test eltype(ML(big(1):big(2))) == Tuple{BigInt,Int}
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

		@test ML(SingleValuedRange(3), 1:2) == ML(3:3, 1:2)

		@test_throws ArgumentError ML(1:0)
		@test_throws ArgumentError ML(1:1, 1:0)
		@test_throws ArgumentError ML(-1:1)
		@test_throws ArgumentError ML(0:1, 1:3)
		@test_throws ArgumentError ML(0:1, 3:3)
		@test_throws ArgumentError ML(0:1, -3:0)
		@test_throws ArgumentError ML(SingleValuedRange(1), 3:4)
		@test_throws ArgumentError ML(SingleValuedRange(1), ZeroTo(3))
		@test_throws ArgumentError ML(SingleValuedRange(1), ToZero(3))
		@test_throws ArgumentError ML(SingleValuedRange(1), FullRange(3))

		@testset "type-stability" begin
		    for LT in (:ZeroTo, :SingleValuedRange),
		    	MT in (:ZeroTo, :ToZero, :FullRange)
		    	@eval @test ML($LT(3), $MT) isa ML{<:$LT, <:$MT}
		    end
		    @test ML(ZeroTo(3), SingleValuedRange(2)) isa ML{UnitRange{Int}, SingleValuedRange}
		    @test ML(SingleValuedRange(3), SingleValuedRange(2)) isa ML{SingleValuedRange, SingleValuedRange}
		end

        @testset "promotion" begin
            @test promote_typeof(ML(1,1), ML(2,2)) == ML{SingleValuedRange, SingleValuedRange}
            @test promote_typeof(ML(1), ML(1)) == ML{SingleValuedRange, FullRange{true}}
            @test promote_typeof(ML(1:1), ML(1)) == ML{UnitRange{Int64}, FullRange{true}}
            @test promote_typeof(ML(1:1, ZeroTo), ML(1, ZeroTo)) == ML{UnitRange{Int}, ZeroTo{true}}
            @test promote_typeof(ML(1:1, ZeroTo), ML(1, ZeroTo(1))) == ML{UnitRange{Int}, UnitRange{Int}}
            @test promote_typeof(ML(1:1, 1:1), ML(1,1)) == ML{UnitRange{Int}, UnitRange{Int}}

            iterslist = Any[ML(1), ML(1:2), ML(1:2, 0:0), ML(1:1, ZeroTo), ML(1:2, ZeroTo(1)),
                        ML(1, ZeroTo), ML(1:1, FullRange), ML(1, FullRange), ML(ZeroTo(3), ZeroTo),
                        ML(ZeroTo(2)), ML(ZeroTo(2), FullRange(1))]

            for x in iterslist, y in iterslist
                @test isconcretetype(promote_typeof(x, y))
            end
        end
        @testset "conversion" begin
            for lr in Any[0:1, ZeroTo(1), SingleValuedRange(1)], mr in Any[0:1, ZeroTo(1), ToZero(1), SingleValuedRange(1)]
                convert(ML{UnitRange{Int}, UnitRange{Int}}, ML(lr, mr)) isa ML{UnitRange{Int}, UnitRange{Int}}
            end
            for LT in [UnitRange{Int}, ZeroTo{false}]
                for MT in [ZeroTo{false}, ZeroTo{true}]
                    @test convert(ML{LT, MT}, ML(0:1, 0:1)) isa ML{LT, MT}
                end
                for MT in [UnitRange{Int}, ToZero{false}, ToZero{true}]
                    @test convert(ML{LT, MT}, ML(0:1, -1:0)) isa ML{LT, MT}
                end
                for MT in [FullRange{false}, FullRange{true}]
                    @test convert(ML{LT, MT}, ML(0:1, -1:1)) isa ML{LT, MT}
                end
                @test convert(ML{LT, SingleValuedRange}, ML(0:1, 0:0)) isa ML{LT, SingleValuedRange}
            end
            for LT in [UnitRange{Int}, SingleValuedRange]
                for MT in [ZeroTo{false}, ZeroTo{true}]
                    @test convert(ML{LT, MT}, ML(1:1, 0:1)) isa ML{LT, MT}
                end
                for MT in [ToZero{false}, ToZero{true}]
                    @test convert(ML{LT, MT}, ML(1:1, -1:0)) isa ML{LT, MT}
                end
                for MT in [FullRange{false}, FullRange{true}]
                    @test convert(ML{LT, MT}, ML(1:1, -1:1)) isa ML{LT, MT}
                end
                @test convert(ML{LT, SingleValuedRange}, ML(1:1, 1:1)) isa ML{LT, SingleValuedRange}
            end
        end
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
				@show mr, typeof(mr)
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

                mr = ML(l_min:l_max, T(0))
                testlength(iterated_length, mr)
                mr2 = ML(UnitRange(l_range(mr)), UnitRange(m_range(mr)))
                @test begin
                    res = length(mr) == length(mr2)
                    if !res
                        @show typeof(mr)
                    end
                    res
                end
                @test collect(mr) == collect(mr2)

                mr = ML(l_min:l_max, T(l_min))
                testlength(iterated_length, mr)
                mr2 = ML(UnitRange(l_range(mr)), UnitRange(m_range(mr)))
                @test length(mr) == length(mr2)
                @test collect(mr) == collect(mr2)

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

                mr = LM(l_min:l_max, T(0))
                testlength(iterated_length, mr)
                mr2 = LM(UnitRange(l_range(mr)), UnitRange(m_range(mr)))
                @test begin
                    res = length(mr) == length(mr2)
                    if !res
                        @show typeof(mr)
                    end
                    res
                end
                @test collect(mr) == collect(mr2)

                mr = LM(l_min:l_max, T(l_min))
                testlength(iterated_length, mr)
                mr2 = LM(UnitRange(l_range(mr)), UnitRange(m_range(mr)))
                @test length(mr) == length(mr2)
                @test collect(mr) == collect(mr2)

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
			m2 = ML(ZeroTo(l_min), SingleValuedRange(l_min))
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

	modeindex2(m, s::Integer, t::Integer) = modeindex2(m, (s,t))
    modeindex2(m, (s,t)::Tuple) = findfirst(isequal((s,t)), m)

	function testmodeindex(mr)
		for (ind,(s,t)) in enumerate(mr)
			@test begin
                res = modeindex(mr,(s,t)) == modeindex(mr,s,t) == modeindex2(mr,s,t) == ind
                if !res
                    @show mr, typeof(mr), s, t
                end
                res
            end
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

                mr = LM(ZeroTo(l_max), MT(l_max))
                testmodeindex(mr)

				mr = LM(SingleValuedRange(l_min), MT)
				testmodeindex(mr)
			end

            mr = LM(1:4, ZeroTo(4))
            @test modeindex(mr, (3,3)) == modeindex2(mr, (3,3))

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

		@test_throws ModeMissingError modeindex(LM(1:1), (2,2))
	end

	@testset "ML" begin
		for l_min=0:l_cutoff, l_max=l_min:l_cutoff

			for MT in [ZeroTo, ToZero, FullRange]
				mr = ML(l_min:l_max, MT)
				testmodeindex(mr)

				mr = ML(ZeroTo(l_max), MT)
				testmodeindex(mr)

                mr = ML(ZeroTo(l_max), MT(l_max))
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

		@test_throws ModeMissingError modeindex(ML(1:1), (2,2))
	end

    @testset "vector indexing" begin
        mr = ML(0:4, ZeroTo)
        (l,m) = first(mr)
        mr2 = ML(l,m)
        @test modeindex(mr, mr2) == modeindex(mr, l, m) == 1
        @test modeindex(flip(mr), mr2) == modeindex(flip(mr), l, m)

        for MT in [ZeroTo, FullRange, ToZero]
            mr = ML(0:4, MT)
            ms = ML(0:3, MT)
            @test modeindex(mr, ms) == [modeindex(mr, i) for i in ms]
            ms = ML(0:3, MT(3))
            @test modeindex(mr, ms) == [modeindex(mr, i) for i in ms]
        end

        mr = LM(0:4, ZeroTo)
        (l, m) = first(mr)
        mr2 = LM(l,m)
        @test modeindex(mr, mr2) == modeindex(mr, l, m) == 1
        @test modeindex(mr, LM(l:l,m)) == 1:1
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

    @testset "other iterators" begin
        r = collect((i,i) for i in 1:1000)
        for (ind, t) in enumerate(r)
            @test modeindex(r, t) == ind
            @test modeindex(r, t...) == ind
        end
        @test modeindex(r, (0,0)) === nothing
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
        @test convert(ML, m) == ML(0:1)
		@test flip(m) == ML(0:1)
		@test LM(m) === m
        @test convert(LM, m) === m
        @test flip(SphericalHarmonicModes.basetype(LM(1:2))) == ML
    end
    @testset "ML" begin
		m = ML(0:1)
		@test LM(m) == LM(0:1)
        @test convert(LM, m) == LM(0:1)
		@test flip(m) == LM(0:1)
		@test ML(m) === m
        @test convert(ML, m) === m
        @test flip(SphericalHarmonicModes.basetype(ML(1:2))) == LM
    end
end

@testset "ordering" begin
    for l_range in Any[0:1, ZeroTo(1)], m_range in Any[0:1, FullRange, ZeroTo]
        lm = LM(0:1)
        ml = SphericalHarmonicModes.flip(lm)
        @test SphericalHarmonicModes.ofordering(lm, ml) == lm
        @test SphericalHarmonicModes.ofordering(lm, lm) == lm
        @test SphericalHarmonicModes.ofordering(ml, lm) == ml
        @test SphericalHarmonicModes.ofordering(ml, lm) == ml
    end
end

@testset "intersect" begin
	@testset "same l, different m" begin
		for T in [LM, ML]
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
		for T in [LM, ML]
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

    @test intersect(FullRange(2), FullRange(1)) === FullRange(1)

    @test intersect(SingleValuedRange(1), -1:1) === SingleValuedRange(1)
    @test intersect(SingleValuedRange(1), FullRange(1)) === SingleValuedRange(1)
    @test intersect(SingleValuedRange(1), ZeroTo(1)) === SingleValuedRange(1)
    @test intersect(SingleValuedRange(1), 2:3) === nothing
    @test intersect(-1:1, SingleValuedRange(1)) === SingleValuedRange(1)
    @test intersect(FullRange(1), SingleValuedRange(1)) === SingleValuedRange(1)
    @test intersect(ZeroTo(1), SingleValuedRange(1)) === SingleValuedRange(1)
    @test intersect(2:3, SingleValuedRange(1)) === nothing
    @test intersect(SingleValuedRange(1), SingleValuedRange(1)) === SingleValuedRange(1)
    @test intersect(SingleValuedRange(1), SingleValuedRange(2)) === nothing
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

    showerror(io, ModeMissingError(LM(1:1), (2,2)) )
    showerror(io, ModeMissingError(L2L1Triangle(1:1,1), (3,3)) )
end

@testset "to_indices" begin
    modes = ML(0:3)
    @test to_indices(zeros(), (modes,), (:,)) == (1:length(modes),)
end
