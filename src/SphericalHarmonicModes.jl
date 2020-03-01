module SphericalHarmonicModes

export LM,ML,L₂L₁Δ
export modeindex,l_range,m_range,l₂_range,l₁_range, flip

abstract type ModeRange end
abstract type SHModeRange <: ModeRange end
abstract type ModeProduct <: ModeRange end

Base.eltype(::SHModeRange) = Tuple{Int,Int}

struct LM <: SHModeRange
	l_min :: Int
	l_max :: Int
	m_min :: Int
	m_max :: Int

	function LM(l_min,l_max,m_min,m_max)
		check_if_lm_range_is_valid(l_min,l_max,m_min,m_max)
		
		if max(m_min,-m_max) > l_min
			@info "Changing l_min from $(l_min) to $(max(m_min,-m_max))"*
			" to be consistent with "*
			" m_min = $(m_min) and m_max = $(m_max)"

			l_min = max(m_min,-m_max)
		end

		new(l_min,l_max,m_min,m_max)
	end
end

struct ML <: SHModeRange
	l_min :: Int
	l_max :: Int
	m_min :: Int
	m_max :: Int

	function ML(l_min,l_max,m_min,m_max)
		check_if_lm_range_is_valid(l_min,l_max,m_min,m_max)
		
		if max(m_min,-m_max) > l_min
			@info "Changing l_min from $(l_min) to $(max(m_min,-m_max))"*
			" to be consistent with "*
			" m_min = $(m_min) and m_max = $(m_max)"
			
			l_min = max(m_min,-m_max)
		end

		new(l_min,l_max,m_min,m_max)
	end
end

struct L₂L₁Δ <: ModeProduct
	l₁_min :: Int
	l₁_max :: Int
	Δl_max :: Int
	l₂_min :: Int
	l₂_max :: Int

	function L₂L₁Δ(l₁_min::Integer,l₁_max::Integer,
		Δl_max::Integer,l₂_min::Integer,l₂_max::Integer)

		map(check_if_non_negative,(l₁_min,l₁_max,Δl_max,l₂_min,l₂_max))

		(l₁_min > l₁_max) && throw(OrderError("l₁",l₁_min,l₁_max))
		(l₂_min > l₂_max) && throw(OrderError("l₂",l₂_min,l₂_max))
		
		(l₂_min > l₁_max + Δl_max) && 
		throw(ModeRangesInconsistentError(
			"l₂_min = $l₂_min is greater than l₁_max + Δl_max = $(l₁_max + Δl_max)"*
			" for l₁_max = $l₁_max and Δl_max = $Δl_max"))
		(l₂_max < l₁_min - Δl_max) && 
		throw(ModeRangesInconsistentError(
			"l₂_max = $l₂_max is less than l₁_min - Δl_max = $(l₁_min - Δl_max)"*
			" for l₁_max = $l₁_max and Δl_max = $Δl_max"))

		# Trim the range if necessary
		if l₂_min < l₁_min - Δl_max
			@info "Changing l₂_min from $(l₂_min) to $(l₁_min-Δl_max)"*
			" to be consistent with "*
			" l₁_min = $(l₁_min) and Δl_max = $(Δl_max)"
			l₂_min = l₁_min - Δl_max
		end
		if l₂_max > l₁_max + Δl_max
			@info "Changing l₂_max from $(l₂_max) to $(l₁_max + Δl_max)"*
			" to be consistent with "*
			" l₁_max=$(l₁_max) and Δl_max = $(Δl_max)"
			l₂_max = l₁_max + Δl_max
		end
		
		(l₂_min > l₂_max) && throw(OrderError("l₂",l₂_min,l₂_max))

		if l₁_min < l₂_min - Δl_max
			@info "Changing l₁_min from $(l₁_min) to $(l₂_min - Δl_max)"*
			" to be consistent with "*
			" l₂_min = $(l₂_min) and Δl_max = $(Δl_max)"
			l₁_min = l₂_min - Δl_max
		end
		if l₁_max > l₂_max + Δl_max
			@info "Changing l₁_max from $(l₁_max) to $(l₂_max + Δl_max)"*
			" to be consistent with "*
			" l₂_max=$(l₂_max) and Δl_max = $(Δl_max)"
			l₁_max = l₂_max + Δl_max
		end

		(l₁_min > l₁_max) && throw(OrderError("l₁",l₁_min,l₁_max))
		
		new(l₁_min,l₁_max,Δl_max,l₂_min,l₂_max)
	end
end
Base.eltype(::L₂L₁Δ) = Tuple{Int,Int}

# Throw errors if values are invalid
struct OrderError{T} <: Exception
	var :: String
	low :: T
	high :: T
end

Base.showerror(io::IO, e::OrderError) = print(io,e.var,"min = ",e.low,
	" is not consistent with ",e.var,"max = ",e.high)

struct mOutOfBoundsError <: Exception 
	l_max :: Integer
	m :: Integer
end

Base.showerror(io::IO, e::mOutOfBoundsError) = print(io," m = ", e.m,
		" does not satisfy ",-e.l_max," ⩽ m ⩽ ",e.l_max)

struct ModeMissingError{M,T} <: Exception
	firstmode :: T
	secondmode :: T
	itr :: M
end

Base.showerror(io::IO, e::ModeMissingError{<:SHModeRange}) = 
	print(io,"Mode with (l=",e.firstmode,",m=",e.secondmode,")",
	" is not included in the range given by ",e.itr)

Base.showerror(io::IO, e::ModeMissingError{L₂L₁Δ}) = 
	print(io,"Mode with (l₂=",e.firstmode,",l₁=",e.secondmode,")",
	" is not included in the range given by ",e.itr)

struct ModeRangesInconsistentError <: Exception
	msg :: String
end

Base.showerror(io::IO, e::ModeRangesInconsistentError) = print(io,e.msg)

struct NegativeDegreeError{T<:Integer} <: Exception
	l :: T
end

Base.showerror(io::IO, e::NegativeDegreeError) = print(io,"l = ",e.l,
	" does not correspond to a valid mode")

struct InvalidModeError{TL<:Integer,TM<:Integer} <: Exception
	l :: TL
	m :: TM
end

Base.showerror(io::IO, e::InvalidModeError) = print(io,"(l=",e.l,",m=",e.m,")",
	" is not a valid mode. |m| <= l is not satisfied")

struct NonContiguousError <: Exception end

Base.showerror(io::IO, e::NonContiguousError) = print(io,
	"ModeRanges only support contiguous indexing")

function check_if_non_negative(l::Integer)
	l >= 0 || throw(NegativeDegreeError(l))
end

function check_if_lm_range_is_valid(l_min,l_max,m_min,m_max)
	check_if_non_negative.((l_min,l_max))
	if m_min > m_max 
		throw(OrderError("m",m_min,m_max))
	end
	if l_min > l_max
		throw(OrderError("l",l_min,l_max))
	end
	if abs(m_max) > l_max
		throw(mOutOfBoundsError(l_max,m_max))
	end
	if abs(m_min) > l_max
		throw(mOutOfBoundsError(l_max,m_min))
	end
end

@inline function check_if_valid_mode(l::Integer,m::Integer)
	abs(m) <= l || throw(InvalidModeError(l,m))
end

@inline function check_if_mode_present(mr,l,m)
	(l,m) in mr || throw(ModeMissingError(l,m,mr))
end

# Constructors. Both ML and LM are constructed identically, 
# so we can dispatch on the supertype
(::Type{T})(l_min::Integer,l_max::Integer) where {T<:SHModeRange} = T(l_min,l_max,-l_max,l_max)
(::Type{T})(l::Integer) where {T<:SHModeRange} = T(l,l)

(::Type{T})(l_range::AbstractUnitRange{<:Integer},
	m_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(extrema(l_range)...,extrema(m_range)...)

(::Type{T})(l_range::AbstractUnitRange{<:Integer},m::Integer) where {T<:SHModeRange} = 
	T(extrema(l_range)...,m,m)

(::Type{T})(l::Integer,m_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(l,l,extrema(m_range)...)

(::Type{T})(l_range::AbstractUnitRange{<:Integer}) where {T<:SHModeRange} = 
	T(extrema(l_range)...)

# Spherical constructors to flip order
@inline LM(m::ML) = LM(m.l_min,m.l_max,m.m_min,m.m_max)
@inline ML(m::LM) = ML(m.l_min,m.l_max,m.m_min,m.m_max)
@inline ML(m::ML) = m
@inline LM(m::LM) = m
@inline flip(m::LM) = ML(m)
@inline flip(m::ML) = LM(m)

function L₂L₁Δ(l_min::Integer,l_max::Integer,Δl_max::Integer,
	l₂_min::Integer=max(l_min-Δl_max,0))

	L₂L₁Δ(l_min,l_max,Δl_max,l₂_min,l_max+Δl_max)
end

L₂L₁Δ(l_min::Integer,l_max::Integer,mr::SHModeRange,args...) = L₂L₁Δ(l_min,l_max,mr.l_max,args...)

L₂L₁Δ( l_range::AbstractUnitRange{<:Integer},Δl_max::Integer,
	l₂_range::AbstractUnitRange{<:Integer}) = 
	L₂L₁Δ(extrema(l_range)...,Δl_max,extrema(l₂_range)...)

L₂L₁Δ(l_min::Integer,l_max::Integer,Δl_max::Integer,
	l₂_range::AbstractUnitRange{<:Integer}) = 
	L₂L₁Δ(l_min,l_max,Δl_max,extrema(l₂_range)...)

L₂L₁Δ( l_range::AbstractUnitRange{<:Integer},Δl_max::Union{Integer,<:SHModeRange},
	args...) = L₂L₁Δ(extrema(l_range)...,Δl_max,args...)

# Get the ranges of the modes

@inline l_range(mr::SHModeRange) = mr.l_min:mr.l_max
@inline m_range(mr::SHModeRange) = mr.m_min:mr.m_max

@inline l₁_range(mr::L₂L₁Δ) = mr.l₁_min:mr.l₁_max
@inline l₂_range(mr::L₂L₁Δ) = mr.l₂_min:mr.l₂_max

@inline l_range(mr::SHModeRange,m::Integer) = max(abs(m),mr.l_min):mr.l_max

@inline m_range(mr::SHModeRange,l::Integer) = max(-l,mr.m_min):min(l,mr.m_max)

@inline function l₂_range(mr::L₂L₁Δ,l::Integer)
	max(l - mr.Δl_max,mr.l₂_min):min(l + mr.Δl_max,mr.l₂_max)
end

# Convenience function to generate the first step in the iteration
@inline first_m(mr::LM) = mr.m_min
@inline first_l(mr::LM) = first(l_range(mr,first_m(mr)))
@inline first_l(mr::ML) = mr.l_min
@inline first_m(mr::ML) = first(m_range(mr,first_l(mr)))

@inline first_l₁(mr::L₂L₁Δ) = max(mr.l₁_min,mr.l₂_min-mr.Δl_max)
@inline first_l₂(mr::L₂L₁Δ) = first(l₂_range(mr,first_l₁(mr)))

function Base.iterate(mr::LM, state=((first_l(mr),first_m(mr)), 1))

	(l,m), count = state

	if count > length(mr)
		return nothing
	end

	if l == mr.l_max
		next_m = m + 1
		next_l = max(mr.l_min,abs(next_m))
	else
		next_m = m
		next_l = l + 1
	end

	return (l,m), ((next_l,next_m), count + 1)
end

function Base.iterate(mr::ML, state=((first_l(mr),first_m(mr)), 1))
	
	(l,m), count = state

	if count > length(mr)
		return nothing
	end

	if m == min(mr.m_max,l)
		next_l = l + 1
		next_m = max(mr.m_min,-next_l)
	else
		next_l = l
		next_m = m + 1
	end

	return (l,m), ((next_l,next_m), count + 1)
end

function Base.iterate(mr::L₂L₁Δ,state=((first_l₂(mr),first_l₁(mr)), 1))

	(l₂,l₁),count = state
	if count > length(mr)
		return nothing
	end

	l₂_range_l₁ = l₂_range(mr,l₁)

	if l₂ == last(l₂_range_l₁)
		next_l₁ = l₁ + 1
		next_l₂ = first(l₂_range(mr,next_l₁))
	else
		next_l₁ = l₁
		next_l₂ = l₂ + 1
	end

	return (l₂,l₁),((next_l₂,next_l₁),count+1)
end

function _in((l,m)::Tuple{<:Integer,<:Integer},mr::ML)
	(abs(m)<=l) && (mr.l_min <= l <= mr.l_max) && (mr.m_min <= m <= mr.m_max) &&
	(m in m_range(mr,l))
end

function _in((l,m)::Tuple{<:Integer,<:Integer},mr::LM)
	(abs(m)<=l) && (mr.l_min <= l <= mr.l_max) && (mr.m_min <= m <= mr.m_max) &&
	(l in l_range(mr,m))
end

function _in((l₂,l₁)::Tuple{<:Integer,<:Integer},mr::L₂L₁Δ)
	(mr.l₁_min <= l₁ <= mr.l₁_max) && 
	(mr.l₂_min <= l₂ <= mr.l₂_max) && 
	(l₂ in l₂_range(mr,l₁))
end

Base.in(T::Tuple,mr::ModeRange) = _in(T,mr)

Base.last(mr::LM) = (last(l_range(mr,mr.m_max)),mr.m_max)
Base.last(mr::ML) = (mr.l_max,last(m_range(mr,mr.l_max)))
Base.last(mr::L₂L₁Δ) = (last(l₂_range(mr,mr.l₁_max)), mr.l₁_max)

function modeindex(mr::LM,l::Integer,m::Integer)
	# Check if the l and m supplied correspond to valid modes
	check_if_non_negative(l)
	check_if_valid_mode(l,m)
	check_if_mode_present(mr,l,m)

	Nskip = 0
	
	l_min,l_max = mr.l_min,mr.l_max
	m_min,m_max = mr.m_min,mr.m_max

	# Nskip = sum(length(l_range(mr,m_i)) fot m_i=mr.m_min:m-1)

	up = min(m - 1, -1)
	lo = max(m_min, -l_min)
	if up >= lo
		Nskip += (1 + l_max - l_min)*(1 - lo + up)
	end

	up = min(m - 1, l_min)
	lo = max(m_min, 0)
	if up >= lo
		Nskip += (1 + l_max - l_min)*(1 - lo + up)
	end

	up = min(m - 1, m_max, l_max)
	lo = max(m_min, l_min + 1)
	if up >= lo
		Nskip += div((-1 + lo - up)*(-2*(1 + l_max) + lo + up),2)
	end

	up = min(m - 1, -1, -l_min - 1)
	lo = max(m_min, -l_max)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 + 2l_max + lo + up)),2)
	end

	ind_l = l - first(l_range(mr,m)) + 1
	Nskip + ind_l
end

function modeindex(mr::LM,l::AbstractUnitRange{<:Integer},m::Integer)
	last(l) in l_range(mr,m) || throw(ModeMissingError(last(l),m,mr))
	start = modeindex(mr,first(l),m)
	start:start + length(l) - 1
end

modeindex(mr::LM,::Colon,m::Integer) = modeindex(mr,l_range(mr,m),m)

function modeindex(mr::ML,l::Integer,m::Integer)
	# Check if the l and m supplied correspond to valid modes
	check_if_non_negative(l)
	check_if_valid_mode(l,m)
	check_if_mode_present(mr,l,m)

	Nskip = 0
	
	l_min,l_max = mr.l_min,mr.l_max
	m_min,m_max = mr.m_min,mr.m_max

	# Nskip = sum(length(m_range(mr,l_i)) for l_i=mr.l_min:l-1)

	up = min(-m_min, m_max, l - 1)
	lo = l_min
	if up >= lo
		Nskip += -(-1 + lo - up)*(1 + lo + up)
	end

	up = min(-m_min, l - 1)
	lo = max(l_min, m_max + 1, -m_max)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 + 2m_max + lo + up)),2)
	end

	up = min(m_max, l - 1)
	lo = max(l_min, -m_min + 1, m_min)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 - 2m_min + lo + up)),2)
	end

	up = l - 1
	lo = max(-m_min + 1, m_max + 1, l_min)
	if up >= lo
		Nskip += (1 + m_max - m_min)*(1 - lo + up)
	end

	ind_m = m - first(m_range(mr,l)) + 1
	Nskip + ind_m
end

function modeindex(mr::ML,l::Integer,m::AbstractUnitRange{<:Integer})
	last(m) in m_range(mr,l) || throw(ModeMissingError(l,last(m),mr))
	start = modeindex(mr,l,first(m))
	start:start + length(m) - 1
end

modeindex(mr::ML,l::Integer,::Colon) = modeindex(mr,l,m_range(mr,l))

function modeindex(mr::L₂L₁Δ,l₂::Integer,l₁::Integer)
	# Check if l₁ and l₂ supplied correspond to valid modes
	map(check_if_non_negative,(l₁,l₂))
	check_if_mode_present(mr,l₂,l₁)

	Nskip = 0

	l_min,l_max,l₂_min,l₂_max = mr.l₁_min,mr.l₁_max,mr.l₂_min,mr.l₂_max
	Δl_max = mr.Δl_max

	up = min(l₁ - 1, l₂_max - Δl_max)
	lo = max(l_min, l₂_min + Δl_max)
	if up >= lo
		Nskip += (1 + 2Δl_max)*(1 - lo + up)
	end

	up = min(l₁ - 1, l₂_max + Δl_max)
	lo = max(l_min, l₂_min + Δl_max, l₂_max - Δl_max + 1)
	if up >= lo
		Nskip += div((-1 + lo - up)*(-2*(1 + Δl_max + l₂_max) + lo + up),2)
	end

	up = min(l₁ - 1, l₂_min + Δl_max - 1, l₂_max - Δl_max)
	lo = max(l_min, l₂_min - Δl_max)
	if up >= lo
		Nskip += div(-((-1 + lo - up)*(2 + 2Δl_max - 2l₂_min + lo + up)),2)
	end

	up = min(l₁ - 1, l₂_min + Δl_max - 1)
	lo = max(l_min, l₂_max - Δl_max + 1)
	if up >= lo
		Nskip += (1 + l₂_max - l₂_min)*(1 - lo + up)
	end

	ind_l₂ = l₂ - first(l₂_range(mr,l₁)) + 1
	Nskip + ind_l₂
end

function modeindex(mr::L₂L₁Δ,l₂::AbstractUnitRange{<:Integer},l₁::Integer)
	last(l₂) in l₂_range(mr,l₁) || throw(ModeMissingError(last(l₂),l₁,mr))
	start = modeindex(mr,first(l₂),l₁)
	start:start + length(l₂) - 1
end

modeindex(mr::L₂L₁Δ,::Colon,l₁::Integer) = modeindex(mr,l₂_range(mr,l₁),l₁)

modeindex(mr::ModeRange,T::Tuple{<:Any,<:Any}) = modeindex(mr,T...)
modeindex(mr::ModeRange,T::Vararg{<:Any,2}) = modeindex(mr,T...)
modeindex(mr::ModeRange,::Colon,::Colon) = Base.OneTo(length(mr))

# modeindex with two ranges works only if the values are contiguously stored in the iterator
# If the values are contiguous then it returns modeindex(l_min,m_min):modeindex(l_max,m_max)
# All (l,m) values need to be present in the iterator
# Some invalid values might be caught by internal modeindex calls
function modeindex(mr::ModeRange,l_range::AbstractUnitRange,m_range::AbstractUnitRange)
	m_min,m_max = extrema(m_range)
	l_min,l_max = extrema(l_range)
	firstind = modeindex(mr,l_min,m_min)
	lastind = modeindex(mr,l_max,m_max)

	if (lastind - firstind + 1) != length(l_range)*length(m_range)
		throw(NonContiguousError())
	end
	firstind:lastind
end

# Return the range of indices of m containing the entirety of mpart
# If mpart is contiguously stored in m then collect(m)[modeindex(mr,mpart)] == collect(mpart)
function modeindex(mr::T,mpart::T) where {T<:ModeRange}
	mode_start = first(mpart)
	mode_end = last(mpart)
	modeindex(mr,mode_start):modeindex(mr,mode_end)
end

# Add methods to Base functions to compute the size of the iterators
Base.length(mr::ModeRange) = modeindex(mr,last(mr))

Base.firstindex(mr::ModeRange) = 1
Base.lastindex(mr::ModeRange) = length(mr)

# Size and axes behave similar to vectors
@inline Base.size(mr::ModeRange) = (length(mr),)
@inline Base.size(mr::ModeRange,d::Integer) = d == 1 ? length(mr) : 1

@inline Base.axes(mr::ModeRange) = (Base.OneTo(length(mr)),)
@inline Base.axes(mr::ModeRange,d::Integer) = d == 1 ? Base.OneTo(length(mr)) : Base.OneTo(1)

# Display the iterators

function Base.show(io::IO, mr::SHModeRange)
	print(io,"(l=",l_range(mr),",m=",m_range(mr),")")
end

function Base.show(io::IO, mr::L₂L₁Δ)
	print(io,"(l₁=",l₁_range(mr),",Δl_max=",mr.Δl_max,
		",l₂=",l₂_range(mr),")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::LM)
	println("Spherical harmonic modes with l increasing faster than m")
	print(io,"(l_min = ",first(l_range(mr)),", l_max = ",last(l_range(mr)),
		", m_min = ",first(m_range(mr)),", m_max = ",last(m_range(mr)),")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::ML)
	println("Spherical harmonic modes with m increasing faster than l")
	print(io,"(l_min = ",first(l_range(mr)),", l_max = ",last(l_range(mr)),
		", m_min = ",first(m_range(mr)),", m_max = ",last(m_range(mr)),")")
end

function Base.show(io::IO, ::MIME"text/plain", mr::L₂L₁Δ)
	println("Spherical harmonic modes (l₂,l₁) where |l₁-Δl| ⩽ l₂ ⩽ l₁+Δl "*
		"for 0 ⩽ Δl ⩽ Δl_max, l₁_min ⩽ l₁ ⩽ l₁_max, and l₂_min ⩽ l₂ ⩽ l₂_max")
	print(io,"(",first(l₂_range(mr))," ⩽ l₂ ⩽ ",last(l₂_range(mr))," and ",
		first(l₁_range(mr))," ⩽ l₁ ⩽ ",last(l₁_range(mr)),", with Δl_max = ",mr.Δl_max,")")
end

end # module
