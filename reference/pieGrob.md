# Create a pie grob

Create a pie grob

## Usage

``` r
pieGrob(rads, fills = NULL, wts = NULL, name = NULL, vp = NULL, gp = NULL)

grid.pieGrob(rads, fills = NULL, wts = NULL, name = NULL, vp = NULL, gp = NULL)
```

## Arguments

- rads:

  Numeric, radius values for each slice from 0 to 1

- fills:

  Colors to fill the slices

- wts:

  Numeric, the relative portion of each slice

- name, vp, gp:

  Passed to [grid::gTree](https://rdrr.io/r/grid/grid-defunct.html)

## Value

`pieGrob` [grid::grob](https://rdrr.io/r/grid/grid-defunct.html) object

## Details

The default coloring can be set with `options("txp.fills")`.

## Examples

``` r
library(grid)

s <- seq(0.2, 1, by = 0.1)
grid.newpage()
grid.pieGrob(rads = s)

grid.newpage()
grid.pieGrob(rads = s, wts = s)


curr_txp_fills <- options()$txp.fills
options(txp.fills = 1:8)
grid.newpage()
grid.pieGrob(rads = s)

options(txp.fills = curr_txp_fills)

## Can edit
grid.newpage()
grid.pieGrob(rads = s, name = "myPie")

grid.ls() ## show the grid elements
#> myPie
#>   slice1
#>   slice2
#>   slice3
#>   slice4
#>   slice5
#>   slice6
#>   slice7
#>   slice8
#>   slice9
grid.edit("myPie", fills = 1:9, wts = 9:1)

```
