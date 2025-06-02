# toxpiR <img src="man/figures/logo-hex.png" width="150" align="right" />

<!-- badges: start -->
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![R-CMD-check](https://github.com/ToxPi/toxpiR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ToxPi/toxpiR/actions/workflows/R-CMD-check.yaml)
  [![cran-version](https://www.r-pkg.org/badges/version-last-release/toxpiR?color=blue)](https://cran.r-project.org/web/packages/toxpiR/index.html)
  [![downloads](https://cranlogs.r-pkg.org/badges/grand-total/toxpiR)](https://cranlogs.r-pkg.org/badges/grand-total/toxpiR)
  [![codecov](https://codecov.io/gh/ToxPi/toxpiR/branch/main/graph/badge.svg?token=7yocvT0KzZ)](https://codecov.io/gh/ToxPi/toxpiR)

<!-- badges: end -->

R package for the Toxicological Priority Index (ToxPi) prioritization algorithm. 
Package developed and maintained by the [Reif Lab](http://reif-lab.org). 

### Installation

Current stable release (CRAN):

```r
Note: Users need to ensure packages requiring "BiocManager" are manually 
installed.

if (!require(BiocManager, quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c("S4Vectors","BiocGenerics"))

install.packages("toxpiR")
```

Current stable release (Build from GitHub):

```r
Note: Users need to ensure "remotes" package and packages
requiring "BiocManager" are installed before building package.

if (!require(remotes)) install.packages("remotes")

if (!require(BiocManager, quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c("S4Vectors","BiocGenerics"))

remotes::install_github("ToxPi/toxpiR", 
                        dependencies = TRUE)
```

Current stable release (Build from GitHub with vignettes):

```r
Note: Users need to ensure "remotes" package, "pandoc" package, and packages
requiring "BiocManager" are installed before building package.

if (!require(remotes)) install.packages("remotes")
if (!require(pandoc)) install.packages("pandoc")

if (!require(BiocManager, quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c("S4Vectors","BiocGenerics"))
                        
remotes::install_github("ToxPi/toxpiR",
                        dependencies = TRUE, 
                        build_vignettes = TRUE)
```
