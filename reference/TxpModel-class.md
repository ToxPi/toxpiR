# ToxPi Model

S4 class to store ToxPi models

## Usage

``` r
TxpModel(txpSlices, txpWeights = NULL, txpTransFuncs = NULL)

# S4 method for class 'TxpModel'
txpSlices(x)

# S4 method for class 'TxpModel'
txpSlices(x) <- value

# S4 method for class 'TxpModel'
txpWeights(x, adjusted = FALSE)

# S4 method for class 'TxpModel'
txpWeights(x) <- value

# S4 method for class 'TxpModel'
txpTransFuncs(x)

# S4 method for class 'TxpModel'
txpTransFuncs(x) <- value

# S4 method for class 'TxpModel'
txpValueNames(x, simplify = FALSE)

# S4 method for class 'TxpModel'
names(x)

# S4 method for class 'TxpModel'
names(x) <- value

# S4 method for class 'TxpModel'
length(x)

# S4 method for class 'TxpModel,TxpModel'
merge(x, y)
```

## Arguments

- txpSlices:

  Passed to `txpSlices` slot

- txpWeights:

  Passed to `txpWeights` slot

- txpTransFuncs:

  Passed to `txpTransFuncs` slot

- x, y:

  TxpModel object

- value:

  Replacement value

- adjusted:

  Scalar logical, should the returned weights be adjusted such that they
  sum to 1?

- simplify:

  Scalar logical, when `TRUE` the returned `list` is simplified

## Functions

- `txpSlices(TxpModel)`: Return `txpSlices` slot

- `txpWeights(TxpModel)`: Return `txpWeights` slot

- `txpTransFuncs(TxpModel)`: Return `txpTransFuncs` slot

- `txpValueNames(TxpModel)`: Return `list` of `txpValueNames` slots for
  the contained
  [TxpSliceList](https://toxpi.github.io/toxpiR/reference/TxpSliceList-class.md)
  object, or `vector` when `simplify = TRUE`

- `names(TxpModel)`: Return slice names; shortcut for
  `names(txpSlices(x))`

- `length(TxpModel)`: Return number of slices in model; shortcut for
  `length(txpSlices(x))`

- `merge(x = TxpModel, y = TxpModel)`: Merge two `TxpModel` objects into
  a single model

## Slots

- `txpSlices`:

  [TxpSliceList](https://toxpi.github.io/toxpiR/reference/TxpSliceList-class.md)
  object

- `txpWeights`:

  numeric vector specifying the relative weight of each slice; when
  NULL, defaults to 1 (equal weighting) for each slice

- `txpTransFuncs`:

  [TxpTransFuncList](https://toxpi.github.io/toxpiR/reference/TxpTransFuncList-class.md)
  object (or list of functions coercible to TxpTransFuncList)

## Examples

``` r
## Create TxpSliceList & TxpTransFuncList objects
s1 <- list(S1 = TxpSlice("inpt1"), S2 = TxpSlice("inpt2"))
tf <- list(NULL, sqrt = function(x) sqrt(x))

## Create TxpModel object
m1 <- TxpModel(txpSlices = s1, txpWeights = 2:1, txpTransFuncs = tf)
m1
#> TxpModel with 2 slices.
#>   txpSlices(2): S1 S2
#>   txpWeights(2): 2 1
#>   txpTransFuncs(2): NULL sqrt

## Access TxpModel slots
txpSlices(m1)
#> TxpSliceList of length 2
#> names(2): S1 S2
txpWeights(m1)
#> [1] 2 1
txpWeights(m1, adjusted = TRUE)
#> [1] 0.6666667 0.3333333
txpTransFuncs(m1)
#>   TxpTransFuncList of length 2: NULL sqrt

## length
length(m1) ## equal to length(txpSlices(m1))
#> [1] 2
length(m1) == length(txpSlices(m1))
#> [1] TRUE

## names
names(m1) ## equal to names(txpSlices(m1))
#> [1] "S1" "S2"
all(names(m1) == names(txpSlices(m1)))
#> [1] TRUE

## Replacement
m2 <- m1
txpSlices(m2) <- list(S3 = TxpSlice("inpt3"), S4 = TxpSlice("inpt4"))
m2
#> TxpModel with 2 slices.
#>   txpSlices(2): S3 S4
#>   txpWeights(2): 2 1
#>   txpTransFuncs(2): NULL sqrt
names(m2)[2] <- "hello"
names(m2)
#> [1] "S3"    "hello"
txpTransFuncs(m2) <- NULL
m2
#> TxpModel with 2 slices.
#>   txpSlices(2): S3 hello
#>   txpWeights(2): 2 1
#>   txpTransFuncs(2): NULL NULL
txpTransFuncs(m2)[[1]] <- function(x) x^2
names(txpTransFuncs(m2))[1] <- "sq"
m2
#> TxpModel with 2 slices.
#>   txpSlices(2): S3 hello
#>   txpWeights(2): 2 1
#>   txpTransFuncs(2): sq NULL

## merge
m3 <- merge(m1, m2)
m3
#> TxpModel with 4 slices.
#>   txpSlices(4): S1 S2 S3 hello
#>   txpWeights(4): 2 1 2 1
#>   txpTransFuncs(4): NULL sqrt sq NULL
```
