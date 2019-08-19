# SphericalHarmonicModes.jl

[![Build Status](https://travis-ci.com/jishnub/SphericalHarmonicModes.jl.svg?branch=master)](https://travis-ci.com/jishnub/SphericalHarmonicModes.jl)

An iterator to loop over spherical harmonic modes, typically denoted by `(l,m)`. We use the notation `(s,t)` in this package.

## Getting Started

### Installing

```julia
] add "https://github.com/jishnub/SphericalHarmonicModes.jl.git"

julia> using SphericalHarmonicModes
```
## Usage

### Creating an iterator

There are two different orderings possible to iterate over the modes, with either `s` or `t` increasing faster than the other. They are denoted by `st` and `ts`, where --- going by the Julia convention of column-major arrays --- the first index increases faster than the second. Irrespective of which ordering is chosen, the modes are always returned as `(s,t)`.

To create an iterator with `t` increasing faster than `s`: 

```julia
julia> m=ts(0,2,-1,2)
Spherical harmonic modes with t increasing faster than s
smin = 0, smax = 2, tmin = -1, tmax = 2

julia> collect(m)
8-element Array{Tuple{Int64,Int64},1}:
 (0, 0) 
 (1, -1)
 (1, 0) 
 (1, 1) 
 (2, -1)
 (2, 0) 
 (2, 1) 
 (2, 2)
```

To create an iterator with `s` increasing faster than `t`:

```julia
julia> m=st(0,2,-1,2)
Spherical harmonic modes with s increasing faster than t
smin = 0, smax = 2, tmin = -1, tmax = 2

julia> collect(m)
8-element Array{Tuple{Int64,Int64},1}:
 (1, -1)
 (2, -1)
 (0, 0) 
 (1, 0) 
 (2, 0) 
 (1, 1) 
 (2, 1) 
 (2, 2)
 ```

 Note that the modes are ordered by the slower index, so the first mode might not be the one for which `s==smin`.

 Special constructors to include all `t`s are available for convenience.

```julia
julia> st(2)
Spherical harmonic modes with s increasing faster than t
smin = 2, smax = 2, tmin = -2, tmax = 2

julia> st(2,4)
Spherical harmonic modes with s increasing faster than t
smin = 2, smax = 4, tmin = -4, tmax = 4

julia> ts(2)
Spherical harmonic modes with t increasing faster than s
smin = 2, smax = 2, tmin = -2, tmax = 2

julia> ts(2,4)
Spherical harmonic modes with t increasing faster than s
smin = 2, smax = 4, tmin = -4, tmax = 4
```

 The length of the iterator can be computed in `O(1)` time.
 
```julia
julia> m=st(0,20000000,-1000000,2000)
Spherical harmonic modes with s increasing faster than t
smin = 0, smax = 20000000, tmin = -1000000, tmax = 2000

julia> @btime length(m)
  20.630 ns (1 allocation: 16 bytes)
19540018501001
```

It is easy to check whether a mode is present in the iterator. This can also be checked in `O(1)` time.

```julia
julia> m=st(0,20000000,-1000000,2000)
Spherical harmonic modes with s increasing faster than t
smin = 0, smax = 20000000, tmin = -1000000, tmax = 2000

julia> (1000,1000) in m
true
```

The index at which a mode is present can be checked using `modeindex`. For example
```julia
julia> m=ts(0,2,-1,2)
Spherical harmonic modes with t increasing faster than s
smin = 0, smax = 2, tmin = -1, tmax = 2

julia> collect(m)
8-element Array{Tuple{Int64,Int64},1}:
 (0, 0) 
 (1, -1)
 (1, 0) 
 (1, 1) 
 (2, -1)
 (2, 0) 
 (2, 1) 
 (2, 2) 

julia> modeindex(m,(1,0))
3

julia> modeindex(m,(2,2))
8
```

This is also evaluated in `O(1)` time.

```julia
julia> m=ts(0,20000);

julia> @btime modeindex(m,(20000,20000))
  74.562 ns (1 allocation: 16 bytes)
400040001
```

Indexing is not supported at the moment.

## License

This project is licensed under the MIT License - see the [LICENSE.md](https://github.com/jishnub/SphericalHarmonicModes.jl/blob/master/LICENSE) file for details.
