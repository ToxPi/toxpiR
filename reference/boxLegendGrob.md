# Create a filled-box legend

Create a filled-box legend

## Usage

``` r
boxLegendGrob(labels, fills, name = NULL, vp = NULL, gp = NULL)
```

## Arguments

- labels:

  Character, the legend labels

- fills:

  Colors to fill the slices

- name, vp, gp:

  Passed to [grid::frameGrob](https://rdrr.io/r/grid/grid.frame.html)

## Details

Not yet exported. Need to break out the creation of viewports and grobs
as done in the exported grobs. This will allow better grobEdit methods,
which also needs to be created for the boxLegendGrob. Also need to do
some input checks.

Also, if [`grid::legendGrob`](https://rdrr.io/r/grid/legendGrob.html)
gets updated to use the 'has.fill' option this function should be
removed and [`grid::legendGrob`](https://rdrr.io/r/grid/legendGrob.html)
can be used instead.
