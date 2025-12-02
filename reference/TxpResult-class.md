# ToxPi Result

S4 class to store ToxPi results

## Usage

``` r
# S4 method for class 'TxpResult'
txpScores(x)

# S4 method for class 'TxpResult'
txpSliceScores(x, adjusted = TRUE)

# S4 method for class 'TxpResult'
txpRanks(x)

# S4 method for class 'TxpResult'
txpMissing(x)

# S4 method for class 'TxpResult'
txpResultParam(x)

# S4 method for class 'TxpResult'
txpModel(x)

# S4 method for class 'TxpResult'
txpIDs(x)

# S4 method for class 'TxpResult'
txpIDs(x) <- value

# S4 method for class 'TxpResult'
txpWeights(x, adjusted = FALSE)

# S4 method for class 'TxpResult'
txpSlices(x)

# S4 method for class 'TxpResult'
txpTransFuncs(x, level, simplify = FALSE)

# S4 method for class 'TxpResult'
txpValueNames(x, simplify = FALSE)

# S4 method for class 'TxpResult,logical,missing'
x[i, j, ..., drop = FALSE]

# S4 method for class 'TxpResult,integer,missing'
x[i, j, ..., drop = FALSE]

# S4 method for class 'TxpResult,numeric,missing'
x[i, j, ..., drop = FALSE]

# S4 method for class 'TxpResult,character,missing'
x[i, j, ..., drop = FALSE]

# S4 method for class 'TxpResult'
length(x)

# S4 method for class 'TxpResult'
sort(x, decreasing = TRUE, na.last = TRUE, ...)

# S4 method for class 'TxpResult'
names(x)

# S4 method for class 'TxpResult'
names(x) <- value

# S4 method for class 'TxpResult'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  id.name = "id",
  score.name = "score",
  rank.name = "rank",
  adjusted = FALSE
)
```

## Arguments

- x:

  TxpResult object

- adjusted:

  Logical scalar, when `TRUE` the weights are adjusted to sum to 1 or
  the slice scores are scaled to their respective weight

- value:

  Replacement value

- level:

  `c('model', 'slices')`; indicates whether to retrieve `txpTransFuncs`
  slot from the model or underlying slices

- simplify:

  Logical scalar, flatten `txpValueNames` or `txpTransFunc` slots when
  retrieving slice-level information

- i:

  Subsetting index

- j, drop, optional:

  Not currently implemented

- ...:

  Passed to [base::data.frame](https://rdrr.io/r/base/data.frame.html)
  in `as.data.frame` or [base::sort](https://rdrr.io/r/base/sort.html)
  in `sort`

- decreasing, na.last:

  Passed to [base::sort](https://rdrr.io/r/base/sort.html)

- row.names:

  Passed to [base::data.frame](https://rdrr.io/r/base/data.frame.html)

- id.name, score.name, rank.name:

  Character scalar; when coercing to
  [base::data.frame](https://rdrr.io/r/base/data.frame.html), the name
  for the `txpIDs`, `txpScores`, and `txpRanks` columns, respectively

## Functions

- `txpScores(TxpResult)`: Return `txpScores` slot

- `txpSliceScores(TxpResult)`: Return `txpSliceScores` slot; default
  `adjusted = TRUE`, i.e. return slice scores adjusted for weight

- `txpRanks(TxpResult)`: Return `txpRanks` slot

- `txpMissing(TxpResult)`: Return `txpMissing` slot

- `txpResultParam(TxpResult)`: Return `txpResultParam` slot

- `txpModel(TxpResult)`: Return `txpModel` slot

- `txpIDs(TxpResult)`: Return `txpIDs` slot

- `txpWeights(TxpResult)`: Return `txpWeights` slot from model –
  shortcut for `txpWeights(txpModel(x))`; default `adjusted = FALSE`,
  i.e. return unadjusted weights

- `txpSlices(TxpResult)`: Return `txpSlices` slot from model – shortcut
  for `txpSlices(txpModel(x))`

- `txpTransFuncs(TxpResult)`: Return `txpTransFuncs` slot from model –
  shortcut for `txpTransFuncs(txpModel(x))`

- `txpValueNames(TxpResult)`: Return `txpValueNames` slot from slices –
  shortcut for `txpValueNames(txpSlices(txpModel(x)))`

- `length(TxpResult)`: Return the number of observations; shortcut for
  `length(txpScores(x))`

- `sort(TxpResult)`: Sort the “TxpResult\` object by their ranks

- `names(TxpResult)`: Returns IDs; equal to `txpIDs(x)`

- `as.data.frame(TxpResult)`: Coerce TxpResult to
  [base::data.frame](https://rdrr.io/r/base/data.frame.html) object with
  IDs, scores, ranks, and slice scores

## Slots

- `txpScores`:

  `vector(<numeric>)` of model scores

- `txpSliceScores`:

  `matrix(<numeric>)`, sample by slice `matrix` with individual slice
  scores

- `txpRanks`:

  `vector(<numeric>)` with rank of scores

- `txpMissing`:

  `vector(<numeric>)` with data missingness

- `txpModel`:

  [TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
  object

- `txpIDs`:

  `vector(<character>)` of observation IDs

- `txpResultParam`:

  [TxpResultParam](https://toxpi.github.io/toxpiR/reference/TxpResultParam-class.md)
  object

## See also

[txpCalculateScores](https://toxpi.github.io/toxpiR/reference/txpCalculateScores.md),
[plot](https://toxpi.github.io/toxpiR/reference/TxpResult-plot.md),
[TxpResultList](https://toxpi.github.io/toxpiR/reference/TxpResultList-class.md)

## Examples

``` r
## Load example dataset & model; see ?TxpModel for building model objects
data(txp_example_input, package = "toxpiR")
data(txp_example_model, package = "toxpiR")

## Calculate scores for single model; returns TxpResult object
res <- txpCalculateScores(model = txp_example_model, 
                          input = txp_example_input, 
                          id.var = "name")

## Accessors
txpScores(res)
#>  [1] 0.8633157 0.4148452 0.3479965 0.1640443 0.4252311 0.5857161 0.0000000
#>  [8] 0.7195122 0.7719789 0.4709989

txpSliceScores(res) ## adjusted for weight, by default
#>               s1          s2         s3         s4
#>  [1,] 0.25000000 0.009920635 0.37500000 0.22839506
#>  [2,] 0.03893443 0.103174603 0.22026699 0.05246914
#>  [3,] 0.07172131 0.105158730 0.17111650 0.00000000
#>  [4,] 0.02254098 0.000000000 0.02730583 0.11419753
#>  [5,] 0.10860656 0.047619048 0.11468447 0.15432099
#>  [6,] 0.05327869 0.125000000 0.28398058 0.12345679
#>  [7,] 0.00000000 0.000000000 0.00000000 0.00000000
#>  [8,] 0.19467213 0.099206349 0.23118932 0.19444444
#>  [9,] 0.21721311 0.075396825 0.22936893 0.25000000
#> [10,] 0.14549180 0.023809524 0.18750000 0.11419753
apply(txpSliceScores(res), 2, max, na.rm = TRUE)
#>    s1    s2    s3    s4 
#> 0.250 0.125 0.375 0.250 

txpSliceScores(res, adjusted = FALSE) ## each score should have maximum of 1
#>               s1         s2         s3        s4
#>  [1,] 1.00000000 0.07936508 1.00000000 0.9135802
#>  [2,] 0.15573770 0.82539683 0.58737864 0.2098765
#>  [3,] 0.28688525 0.84126984 0.45631068 0.0000000
#>  [4,] 0.09016393 0.00000000 0.07281553 0.4567901
#>  [5,] 0.43442623 0.38095238 0.30582524 0.6172840
#>  [6,] 0.21311475 1.00000000 0.75728155 0.4938272
#>  [7,] 0.00000000 0.00000000 0.00000000 0.0000000
#>  [8,] 0.77868852 0.79365079 0.61650485 0.7777778
#>  [9,] 0.86885246 0.60317460 0.61165049 1.0000000
#> [10,] 0.58196721 0.19047619 0.50000000 0.4567901
apply(txpSliceScores(res, adjusted = FALSE), 2, max, na.rm = TRUE)
#> s1 s2 s3 s4 
#>  1  1  1  1 

txpRanks(res)
#>  [1]  1  7  8  9  6  4 10  3  2  5

txpMissing(res)
#>    s1    s2    s3    s4 
#> 0.100 0.100 0.125 0.100 

txpModel(res)
#> TxpModel with 4 slices.
#>   txpSlices(4): s1 s2 s3 s4
#>   txpWeights(4): 2 1 3 2
#>   txpTransFuncs(4): NULL linear NULL NULL
identical(txpModel(res), txp_example_model)
#> [1] TRUE

txpIDs(res)
#>  [1] "chem01" "chem02" "chem03" "chem04" "chem05" "chem06" "chem07" "chem08"
#>  [9] "chem09" "chem10"
names(res) ## identical to txpIDs(res)
#>  [1] "chem01" "chem02" "chem03" "chem04" "chem05" "chem06" "chem07" "chem08"
#>  [9] "chem09" "chem10"
identical(txpIDs(res), names(res))
#> [1] TRUE

# Can access TxpModel slots directly
txpWeights(res)
#> [1] 2 1 3 2
txpWeights(res, adjusted = TRUE)
#> [1] 0.250 0.125 0.375 0.250
txpSlices(res)
#> TxpSliceList of length 4
#> names(4): s1 s2 s3 s4
# When retrieving transform functions, must specify level because both
# models and slices have transform functions
txpTransFuncs(res, level = "model")
#>   TxpTransFuncList of length 4: NULL linear NULL NULL

# Can access TxpSliceList slots directly
txpValueNames(res)
#> $s1
#> [1] "metric1" "metric2"
#> 
#> $s2
#> [1] "metric3"
#> 
#> $s3
#> [1] "metric4" "metric5" "metric6" "metric7"
#> 
#> $s4
#> [1] "metric8"
#> 
txpValueNames(res, simplify = TRUE)
#>       s11       s12        s2       s31       s32       s33       s34        s4 
#> "metric1" "metric2" "metric3" "metric4" "metric5" "metric6" "metric7" "metric8" 
txpTransFuncs(res, level = "slices")
#> $s1
#>   TxpTransFuncList of length 2: NULL NULL
#> 
#> $s2
#>   TxpTransFuncList of length 1: NULL
#> 
#> $s3
#>   TxpTransFuncList of length 4: linear linear linear linear
#> 
#> $s4
#>   TxpTransFuncList of length 1: linear
#> 
txpTransFuncs(res, level = "slices", simplify = TRUE)
#>   TxpTransFuncList of length 8: NULL NULL ... linear linear

## Subsetting
res[1]
#> TxpResult of length 1
#>   names(1): chem01
res[c("chem01", "chem09")]
#> TxpResult of length 2
#>   names(2): chem01 chem09
res[grepl("4|6", txpIDs(res))]
#> TxpResult of length 2
#>   names(2): chem04 chem06
if (FALSE) { # \dontrun{
res[c(TRUE, FALSE)] ## gets recycled with warning
} # }

## length -- returns number of observations
length(res)
#> [1] 10
length(res[1:5])
#> [1] 5

## sort
names(res)
#>  [1] "chem01" "chem02" "chem03" "chem04" "chem05" "chem06" "chem07" "chem08"
#>  [9] "chem09" "chem10"
names(sort(res))
#>  [1] "chem01" "chem09" "chem08" "chem06" "chem10" "chem05" "chem02" "chem03"
#>  [9] "chem04" "chem07"

txpScores(res)
#>  [1] 0.8633157 0.4148452 0.3479965 0.1640443 0.4252311 0.5857161 0.0000000
#>  [8] 0.7195122 0.7719789 0.4709989
txpScores(sort(res))
#>  [1] 0.8633157 0.7719789 0.7195122 0.5857161 0.4709989 0.4252311 0.4148452
#>  [8] 0.3479965 0.1640443 0.0000000
txpScores(sort(res, decreasing = FALSE))
#>  [1] 0.0000000 0.1640443 0.3479965 0.4148452 0.4252311 0.4709989 0.5857161
#>  [8] 0.7195122 0.7719789 0.8633157

## as.data.frame
as.data.frame(res)
#>        id     score rank         s1         s2         s3        s4
#> 1  chem01 0.8633157    1 1.00000000 0.07936508 1.00000000 0.9135802
#> 2  chem02 0.4148452    7 0.15573770 0.82539683 0.58737864 0.2098765
#> 3  chem03 0.3479965    8 0.28688525 0.84126984 0.45631068 0.0000000
#> 4  chem04 0.1640443    9 0.09016393 0.00000000 0.07281553 0.4567901
#> 5  chem05 0.4252311    6 0.43442623 0.38095238 0.30582524 0.6172840
#> 6  chem06 0.5857161    4 0.21311475 1.00000000 0.75728155 0.4938272
#> 7  chem07 0.0000000   10 0.00000000 0.00000000 0.00000000 0.0000000
#> 8  chem08 0.7195122    3 0.77868852 0.79365079 0.61650485 0.7777778
#> 9  chem09 0.7719789    2 0.86885246 0.60317460 0.61165049 1.0000000
#> 10 chem10 0.4709989    5 0.58196721 0.19047619 0.50000000 0.4567901
as.data.frame(res, id.name = "nm", score.name = "scr", rank.name = "rnk")
#>        nm       scr rnk         s1         s2         s3        s4
#> 1  chem01 0.8633157   1 1.00000000 0.07936508 1.00000000 0.9135802
#> 2  chem02 0.4148452   7 0.15573770 0.82539683 0.58737864 0.2098765
#> 3  chem03 0.3479965   8 0.28688525 0.84126984 0.45631068 0.0000000
#> 4  chem04 0.1640443   9 0.09016393 0.00000000 0.07281553 0.4567901
#> 5  chem05 0.4252311   6 0.43442623 0.38095238 0.30582524 0.6172840
#> 6  chem06 0.5857161   4 0.21311475 1.00000000 0.75728155 0.4938272
#> 7  chem07 0.0000000  10 0.00000000 0.00000000 0.00000000 0.0000000
#> 8  chem08 0.7195122   3 0.77868852 0.79365079 0.61650485 0.7777778
#> 9  chem09 0.7719789   2 0.86885246 0.60317460 0.61165049 1.0000000
#> 10 chem10 0.4709989   5 0.58196721 0.19047619 0.50000000 0.4567901
```
