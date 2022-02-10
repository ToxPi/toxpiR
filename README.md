# toxpiR <img src="man/figures/toxpiR-blue.png" width="150" align="right" />

<!-- badges: start -->
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![R-CMD-check](https://github.com/ToxPi/toxpiR/workflows/R-CMD-check/badge.svg)](https://github.com/ToxPi/toxpiR/actions)
  [![cran-version](https://www.r-pkg.org/badges/version-last-release/toxpiR)](https://cran.r-project.org/web/packages/toxpiR/index.html)
  [![downloads](https://cranlogs.r-pkg.org/badges/grand-total/toxpiR)](https://cranlogs.r-pkg.org/badges/grand-total/toxpiR)
  [![codecov](https://codecov.io/gh/ToxPi/toxpiR/branch/main/graph/badge.svg?token=7yocvT0KzZ)](https://codecov.io/gh/ToxPi/toxpiR)
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
