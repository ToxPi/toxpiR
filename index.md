# toxpiR ![](reference/figures/logo-hex.png)

R package for the Toxicological Priority Index (ToxPi) prioritization
algorithm. Package developed and maintained by the [Reif
Lab](http://reif-lab.org) (Note that [David Reif has moved to
NIH)](https://www.niehs.nih.gov/research/atniehs/labs/ptb/staff/reif).

### Installation

Current stable release (CRAN):

``` r
install.packages("toxpiR")
```

Current stable release (Build from GitHub):

``` r
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

``` r
remotes::install_github("ToxPi/toxpiR",
                        dependencies = TRUE, 
                        build_vignettes = TRUE)

Note: Building packages with vignettes requires the package
"pandoc" to be installed.
```
