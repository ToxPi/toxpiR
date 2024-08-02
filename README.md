# toxpiR <img src="man/figures/logo-hex.png" width="150" align="right" />

<!-- badges: start -->
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![R-CMD-check](https://github.com/ToxPi/toxpiR/workflows/R-CMD-check/badge.svg)](https://github.com/ToxPi/toxpiR/actions)
  [![cran-version](https://www.r-pkg.org/badges/version-last-release/toxpiR?color=blue)](https://cran.r-project.org/web/packages/toxpiR/index.html)
  [![downloads](https://cranlogs.r-pkg.org/badges/grand-total/toxpiR)](https://cranlogs.r-pkg.org/badges/grand-total/toxpiR)
  [![codecov](https://codecov.io/gh/ToxPi/toxpiR/branch/main/graph/badge.svg?token=7yocvT0KzZ)](https://codecov.io/gh/ToxPi/toxpiR)
[![R-CMD-check](https://github.com/ToxPi/toxpiR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ToxPi/toxpiR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

R package for the Toxicological Priority Index (ToxPi) prioritization algorithm. 
Package developed and maintained by the [Reif Lab](http://reif-lab.org). 

### Installation

Current stable release (CRAN):

```r
install.packages("toxpiR")
```

Current stable release (Build from GitHub):

```r
remotes::install_github("ToxPi/toxpiR", 
                        dependencies = TRUE)
                        
Note: Users may need to ensure "remotes" package and packages
requiring "BiocManager" are installed before building package.

if (!require(remotes)) install.packages("remotes")

if (!require(BiocManager, quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c("S4Vectors","BiocGenerics"))

```

Current stable release (Build from GitHub with vignettes):

```r
remotes::install_github("ToxPi/toxpiR",
                        dependencies = TRUE, 
                        build_vignettes = TRUE)

Note: Building packages with vignettes requires the package
"pandoc" to be installed.
```
