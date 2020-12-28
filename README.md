# SphericalHarmonicModes.jl

![CI](https://github.com/jishnub/SphericalHarmonicModes.jl/workflows/CI/badge.svg?branch=master)
[![Coverage Status](https://coveralls.io/repos/github/jishnub/SphericalHarmonicModes.jl/badge.svg?branch=master)](https://coveralls.io/github/jishnub/SphericalHarmonicModes.jl?branch=master)
[![Stable](https://img.shields.io/badge/docs-stable-blue.svg)](https://jishnub.github.io/SphericalHarmonicModes.jl/stable)
[![Dev](https://img.shields.io/badge/docs-dev-blue.svg)](https://jishnub.github.io/SphericalHarmonicModes.jl/dev)

This package provides a few iterators that are relevant in the context of spherical harmonics. The goal of this project is to convert multi-dimensional Cartesian indices to one-dimensional ones. They may therefore be used for indexing arrays, and would allow storing arrays of spherical harmonic coefficients contiguously. There is also the package [SphericalHarmonicArrays.jl](https://github.com/jishnub/SphericalHarmonicArrays.jl) that uses these iterators for indexing.

The iterators implemented currently are:

1. `LM` and `ML`: Two iterators to loop over spherical harmonic modes denoted by `(l,m)`, where `l` is the angular degree and `m` is the azimuthal order.
2. `L2L1Triangle`: An iterator to loop over pairs of spherical harmonic degrees `l2` and `l1` that satisfy the triangle condition `|l1-Δl| <= l2 <= l1+Δl`. The iterator generates pairs of `(l2,l1)` for a specified range of `l1` and all `Δl` that satisfy `0 ⩽ Δl ⩽ Δl_max` for a specified `Δl_max`. Optionally a bound on `l2` may be specified.

## Getting Started

### Installing

```julia
] add SphericalHarmonicModes

julia> using SphericalHarmonicModes
```
## Usage

### Creating a spherical harmonic mode iterator

There are two different orderings possible to iterate over spherical harmonic modes, with either `l` or `m` increasing faster than the other. They are denoted by `LM` and `ML`, where --- going by the Julia convention of column-major arrays --- the first index increases faster than the second. Irrespective of which ordering is chosen, the modes are always returned as `(l,m)` when the iterators are looped over.

Both the iterators are created using the general syntax `itr(l_range, m_range)` where `itr` may be `LM` or `ML`. To create an iterator with `m` increasing faster than `l`:

```julia
julia> itr = ML(0:1, -1:1)
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
julia> itr = LM(0:1, -1:1)
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
julia> LM(2:4) # a range in l, and all valid m for each l
Spherical harmonic modes with l increasing faster than m
(l_min = 2, l_max = 4, m_min = -4, m_max = 4)
```

### Creating an (l2,l1) iterator

This iterator may be created as `L2L1Triangle(l1_min,l1_max,Δl_max,l2_min,l2_max)`, for example

```julia
julia> itr = L2L1Triangle(1,3,2,2,4)
Spherical harmonic modes (l2,l1) that satisfy l1 - 2 ⩽ l2 ⩽ l1 + 2, with 2 ⩽ l2 ⩽ 4 and 1 ⩽ l1 ⩽ 3

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

The ranges of `l1` and `l2` will be clipped to the maximal valid subset dictated by `Δl_max`.

### Using the iterators

 The length of an iterator can be computed in `O(1)` time.
 
```julia
julia> @btime length(m) setup=(m=LM(0:rand(1:1000000)))
  3.197 ns (0 allocations: 0 bytes)
```

It is easy to check whether a mode is present in the iterator. This can also be checked in `O(1)` time.

```julia
julia> @btime el in m setup=(m=LM(0:rand(1:1000000)); el=(rand(1:100),rand(1:100)))
  7.307 ns (0 allocations: 0 bytes)
```

The index at which a mode is present can be checked using `modeindex`. For example
```julia
julia> itr = ML(0:2,-1:2);

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
julia> itr = ML(0:20000);

julia> @btime modeindex($itr,el) setup=(el=(rand(1000:20000),rand(1:1000)))
  6.386 ns (0 allocations: 0 bytes)

julia> itr = LM(0:20000);

julia> @btime modeindex($itr,el) setup=(el=(rand(1000:20000),rand(1:1000)))
  9.595 ns (0 allocations: 0 bytes)

julia> itr = L2L1Triangle(1:100, 100);

julia> @btime modeindex($itr,el) setup=(el=(rand(1:100),rand(1:100)))
  15.411 ns (0 allocations: 0 bytes)
```

Indexing is not supported at the moment, but the last element can be obtained easily.

```julia
julia> itr = ML(0:2,-1:2);

julia> collect(itr)[end]
(2, 2)

julia> last(itr)
(2, 2)

julia> itr = ML(0:20000);

julia> @btime last(m) setup=(m=ML(0:rand(1:20000)))
  3.734 ns (0 allocations: 0 bytes)
```

## License

This project is licensed under the MIT License - see the [LICENSE.md](https://github.com/jishnub/SphericalHarmonicModes.jl/blob/master/LICENSE) file for details.
