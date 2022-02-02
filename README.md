# toxpiR <img src="man/figures/toxpiR-blue.png" width="150" align="right" />

<!-- badges: start -->
  [![codecov](https://codecov.io/gh/ToxPi/toxpiR/branch/main/graph/badge.svg?token=7yocvT0KzZ)](https://codecov.io/gh/ToxPi/toxpiR)
[![R-CMD-check](https://github.com/ToxPi/toxpiR/workflows/R-CMD-check/badge.svg)](https://github.com/ToxPi/toxpiR/actions)
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
