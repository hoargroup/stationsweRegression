# stationsweRegression

The original purpose of this package was written to estimate SWE distribution using linear regression following the methods outlined in [Schneider and Molotch, 2016](!http://onlinelibrary.wiley.com/doi/10.1002/2016WR019067/full) with some minor differences:

~~fsca is used as a predictor instead of reconstructed SWE. If you look in the discussion of the paper, we show that this is almost as good as using reconstructed SWE. I've done some more tests and found that the accuracy trade off versus not having to run the reconstruction model for new domains is worthwhile for my immediate purposes.~~

As of v0.2 reconstructed swe can be used to estimate SWE using flag `SNOW_VAR='rcn'`. See the updated runfiles in the corresponding example_sweregression repo.

- the statistical model from the paper was upgraded from a step-wise linear regression to an elastic-net linear regression. in short, this means that all the predictor variables get used rather than dropping the variables with the lowest predictive ability or because of multicollinearity.

**NB** v0.3 provided the ability to simulate SWE in California using CDEC stations!

install with `devtools::install_github("hoargroup/stationsweRegression", build_vignettes = TRUE)`

Please read the vignettes for details regarding use of the package. These can be read from this github repository or from R with `browseVignettes("stationsweRegression")`.
