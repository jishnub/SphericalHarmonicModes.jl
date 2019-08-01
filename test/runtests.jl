using Test,SphericalHarmonicModes

@testset "modes skipped" begin
	for smax=0:40,smin=0:smax,tmin=-smax:smax,tmax=tmin:smax
		
		@test SphericalHarmonicModes.neg_skip(smin,smax,tmin,tmax) >= 0
		@test SphericalHarmonicModes.pos_skip(smin,smax,tmin,tmax) >= 0

		@test SphericalHarmonicModes.neg_skip(smin,smax,tmin,tmax) == 
					count(t<tmin for s=smin:smax for t=-s:s)

		@test SphericalHarmonicModes.pos_skip(smin,smax,tmin,tmax) == 
					count(t>tmax for s=smin:smax for t=-s:s)
	end
end