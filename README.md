# toxpiR <img src="man/figures/toxpiR-blue.png" width="150" align="right" />

<!-- badges: start -->
  [![codecov](https://codecov.io/gh/daynefiler/toxpiR/branch/master/graph/badge.svg?token=IWZVNP9NVH)](https://codecov.io/gh/daynefiler/toxpiR)
[![R-CMD-check](https://github.com/daynefiler/toxpiR/workflows/R-CMD-check/badge.svg)](https://github.com/daynefiler/toxpiR/actions)
<!-- badges: end -->

R package for the Toxicological Priority Index (ToxPi) prioritization algorithm. Package developed and maintained by the [Reif Lab](http://reif-lab.org). 

### Installation

Current stable release:

```r
install.packages("toxpiR")
```

Build from GitHub (current stable release):

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("ToxPi/toxpiR", 
                        dependencies = TRUE, 
                        build_vignettes = TRUE)
```

Build from GitHub (current dev version):

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("ToxPi/toxpiR",
                        ref = dev,
                        dependencies = TRUE, 
                        build_vignettes = TRUE)
```
