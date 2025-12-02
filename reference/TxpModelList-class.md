# List of TxpModel objects

Extension of
[S4Vectors::SimpleList](https://rdrr.io/pkg/S4Vectors/man/SimpleList-class.html)
that holds only
[TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
objects.

## Usage

``` r
TxpModelList(...)

# S4 method for class 'TxpModelList'
duplicated(x)

as.TxpModelList(x)
```

## Arguments

- ...:

  [TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
  object to create `TxpModelList` object

- x:

  `TxpModelList` object

## Functions

- `duplicated(TxpModelList)`: Returns logical vector of `length(x)`,
  where `TRUE` indicates a duplicate model in the list; see
  [base::duplicated](https://rdrr.io/r/base/duplicated.html)

- `as.TxpModelList()`: Coerce list or
  [TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
  objects to TxpModelList

## Examples

``` r
## Create some TxpModel objects; see ?TxpModel for more details
s1 <- list(S1 = TxpSlice("inpt1"), S2 = TxpSlice("inpt2"))
tf <- list(NULL, sqrt = function(x) sqrt(x))
m1 <- TxpModel(txpSlices = s1, txpWeights = 2:1, txpTransFuncs = tf)
m2 <- m1
txpSlices(m2) <- list(S3 = TxpSlice("inpt3"), S4 = TxpSlice("inpt4"))
m3 <- merge(m1, m2)

## Build a TxpModelList object
TxpModelList(m1 = m1, m2 = m2, m3 = m3)
#>   TxpModelList of length 3: m1 m2 m3

## Note: names are printed as '' when all are NULL
TxpModelList(m1, m2, m3)
#>   TxpModelList of length 3: '' '' ''
names(TxpModelList(m1, m2, m3))
#> NULL

## Test for duplicates
duplicated(TxpModelList(m1 = m1, m2 = m2, m3 = m3))
#> [1] FALSE FALSE FALSE
duplicated(TxpModelList(m1 = m1, m2 = m1, m3 = m3))
#> [1] FALSE  TRUE FALSE

## Coerce lists/TxpModel objects to TxpModelList
as(list(m1 = m1, m2 = m2, m3 = m3), "TxpModelList")
#>   TxpModelList of length 3: m1 m2 m3
as.TxpModelList(list(m1 = m1, m2 = m2, m3 = m3))
#>   TxpModelList of length 3: m1 m2 m3

as(m1, "TxpModelList")
#>   TxpModelList of length 1: ''
as.TxpModelList(m1)
#>   TxpModelList of length 1: ''
```
