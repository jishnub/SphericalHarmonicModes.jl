# SphericalHarmonicModes.jl

[![Build Status](https://travis-ci.com/jishnub/SphericalHarmonicModes.jl.svg?branch=master)](https://travis-ci.com/jishnub/SphericalHarmonicModes.jl)
[![Build Status](https://ci.appveyor.com/api/projects/status/github/jishnub/SphericalHarmonicModes.jl?svg=true)](https://ci.appveyor.com/project/jishnub/SphericalHarmonicModes-jl)
[![Coverage Status](https://coveralls.io/repos/github/jishnub/SphericalHarmonicModes.jl/badge.svg?branch=master)](https://coveralls.io/github/jishnub/SphericalHarmonicModes.jl?branch=master)

This package provides a few iterators that are relevant in the context of spherical harmonics. The goal of this project is to convert multi-dimensional Cartesian indices to one-dimensional ones. They may therefore be used for indexing arrays, and would allow storing arrays of spherical harmonic coefficients contiguously. There is also the package [SphericalHarmonicArrays.jl](https://github.com/jishnub/SphericalHarmonicArrays.jl) that uses these iterators for indexing.

The iterators implemented currently are:

1. `LM` and `ML`: Two iterators to loop over spherical harmonic modes denoted by `(l,m)`, where `l` is the angular degree and `m` is the azimuthal order.
2. `L₂L₁Δ`: An iterator to loop over pairs of spherical harmonic degrees `l₁` and `l₂`, where `|l₁-Δl| <= l₂ <= l₁+Δl`. The iterator generates pairs of `(l₂,l₁)` for a specified range of `l₁` and all `Δl` that satisfy `0 ⩽ Δl ⩽ Δl_max` for a specified `Δl_max`. Optionally a bound on `l₂` may be specified.

## Getting Started

### Installing

```julia
] add SphericalHarmonicModes

julia> using SphericalHarmonicModes
```
## Usage

### Creating a spherical harmonic mode iterator

There are two different orderings possible to iterate over spherical harmonic modes, with either `l` or `m` increasing faster than the other. They are denoted by `LM` and `ML`, where --- going by the Julia convention of column-major arrays --- the first index increases faster than the second. Irrespective of which ordering is chosen, the modes are always returned as `(l,m)` when the iterators are looped over.

Both the iterators are created using the general syntax `itr(l_min,l_max,m_min,m_max)` where `itr` may be `LM` or `ML`. To create an iterator with `m` increasing faster than `l`:

```julia
julia> itr = ML(0,1,-1,1)
Spherical harmonic modes with m increasing faster than l
(l_min = 0, l_max = 1, m_min = -1, m_max = 1)

julia> collect(itr)
4-element Array{Tuple{Int64,Int64},1}:
 (0, 0) 
 (1, -1)
 (1, 0) 
 (1, 1)
```

To create an iterator with `l` increasing faster than `m`:

```julia
julia> itr = LM(0,1,-1,1)
Spherical harmonic modes with l increasing faster than m
(l_min = 0, l_max = 1, m_min = -1, m_max = 1)

julia> collect(itr)
4-element Array{Tuple{Int64,Int64},1}:
 (1, -1)
 (0, 0) 
 (1, 0) 
 (1, 1)
 ```

 Special constructors to include all `m`'s are available for convenience.

```julia
julia> LM(2) # only one l, and all valid m for that l
Spherical harmonic modes with l increasing faster than m
(l_min = 2, l_max = 2, m_min = -2, m_max = 2)

julia> LM(2,4) # a range in l, and all valid m for each l
Spherical harmonic modes with l increasing faster than m
(l_min = 2, l_max = 4, m_min = -4, m_max = 4)

julia> LM(2:4) == LM(2,4) # may specify the range as a UnitRange
true
```

 You may also choose a range of `m`'s.
```julia
julia> LM(2:4,0:2) # a range in l, and all valid m in range for each l
Spherical harmonic modes with l increasing faster than m
(l_min = 2, l_max = 4, m_min = 0, m_max = 2)
```

### Creating an (l₂,l₁) iterator

This iterator may be created as `L₂L₁Δ(l₁_min,l₁_max,Δl_max,l₂_min,l₂_max)`, for example

```julia
julia> itr = L₂L₁Δ(1,3,2,2,4)
Spherical harmonic modes (l₂,l₁) where |l₁-Δl| ⩽ l₂ ⩽ l₁+Δl for 0 ⩽ Δl ⩽ Δl_max, l₁_min ⩽ l₁ ⩽ l₁_max, and l₂_min ⩽ l₂ ⩽ l₂_max
(2 ⩽ l₂ ⩽ 4 and 1 ⩽ l₁ ⩽ 3, with Δl_max = 2)

julia> collect(itr)
8-element Array{Tuple{Int64,Int64},1}:
 (2, 1)
 (3, 1)
 (2, 2)
 (3, 2)
 (4, 2)
 (2, 3)
 (3, 3)
 (4, 3)
```

The ranges of `l₁` and `l₂` will be clipped to the maximal valid subset dictated by `Δl_max`. Several convenience constructors are available, such as 

```julia
julia> itr = L₂L₁Δ(1,2,2) # all valid l₂
Spherical harmonic modes (l₂,l₁) where |l₁-Δl| ⩽ l₂ ⩽ l₁+Δl for 0 ⩽ Δl ⩽ Δl_max, l₁_min ⩽ l₁ ⩽ l₁_max, and l₂_min ⩽ l₂ ⩽ l₂_max
(0 ⩽ l₂ ⩽ 4 and 1 ⩽ l₁ ⩽ 2, with Δl_max = 2)

julia> L₂L₁Δ(1:2,2) == L₂L₁Δ(1,2,2) # the range in l₁ may be specified as a UnitRange
true

julia> itr = L₂L₁Δ(1:2,2,2) # all valid l₂ that lie above the lower cutoff
Spherical harmonic modes (l₂,l₁) where |l₁-Δl| ⩽ l₂ ⩽ l₁+Δl for 0 ⩽ Δl ⩽ Δl_max, l₁_min ⩽ l₁ ⩽ l₁_max, and l₂_min ⩽ l₂ ⩽ l₂_max
(2 ⩽ l₂ ⩽ 4 and 1 ⩽ l₁ ⩽ 2, with Δl_max = 2)

julia> itr = L₂L₁Δ(1:2,2,2,2) # all valid l₂ in range
Spherical harmonic modes (l₂,l₁) where |l₁-Δl| ⩽ l₂ ⩽ l₁+Δl for 0 ⩽ Δl ⩽ Δl_max, l₁_min ⩽ l₁ ⩽ l₁_max, and l₂_min ⩽ l₂ ⩽ l₂_max
(2 ⩽ l₂ ⩽ 2 and 1 ⩽ l₁ ⩽ 2, with Δl_max = 2)

julia> L₂L₁Δ(1:2,2,2:2) == L₂L₁Δ(1:2,2,2,2) # the range in l₂ may be specified as a UnitRange
true
```

### Using the iterators

 The length of an iterator can be computed in `O(1)` time.
 
```julia
julia> itr = LM(0,20000000,-1000000,2000);

julia> @btime length($itr)
  0.029 ns (0 allocations: 0 bytes)
19540018501001
```

It is easy to check whether a mode is present in the iterator. This can also be checked in `O(1)` time.

```julia
julia> itr = LM(0,20000000,-1000000,2000);

julia> @btime (1000,1000) in $itr
  0.029 ns (0 allocations: 0 bytes)
true
```

The index at which a mode is present can be checked using `modeindex`. For example
```julia
julia> itr = ML(0,2,-1,2);

julia> collect(itr)
8-element Array{Tuple{Int64,Int64},1}:
 (0, 0) 
 (1, -1)
 (1, 0) 
 (1, 1) 
 (2, -1)
 (2, 0) 
 (2, 1) 
 (2, 2) 

julia> modeindex(itr,(1,0))
3

julia> modeindex(itr,(2,2))
8
```

This is also evaluated in `O(1)` time.

```julia
julia> itr = ML(0,20000);

julia> @btime modeindex($itr,(20000,20000))
  0.029 ns (0 allocations: 0 bytes)
400040001

julia> itr = LM(0,20000);

julia> @btime modeindex($itr,(20000,20000))
  0.029 ns (0 allocations: 0 bytes)
400040001

julia> itr = L₂L₁Δ(1:100,100);

julia> @btime modeindex($itr,(100,100))
  0.029 ns (0 allocations: 0 bytes)
15050
```

Indexing is not supported at the moment, but the last element can be obtained easily.

```julia
julia> itr = ML(0,2,-1,2);

julia> collect(itr)[end]
(2, 2)

julia> last(itr)
(2, 2)

julia> itr = ML(0,20000);

julia> @btime last($itr)
  0.029 ns (0 allocations: 0 bytes)
(20000, 20000)
```

The times were measured on an Intel® Core™ i7-8650U machine.

## License

This project is licensed under the MIT License - see the [LICENSE.md](https://github.com/jishnub/SphericalHarmonicModes.jl/blob/master/LICENSE) file for details.
