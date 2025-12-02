# List of TxpResult objects

Extension of
[S4Vectors::SimpleList](https://rdrr.io/pkg/S4Vectors/man/SimpleList-class.html)
that holds only
[TxpResult](https://toxpi.github.io/toxpiR/reference/TxpResult-class.md)
objects.

## Usage

``` r
TxpResultList(...)

# S4 method for class 'TxpResultList'
duplicated(x)

as.TxpResultList(x)
```

## Arguments

- ...:

  [TxpResult](https://toxpi.github.io/toxpiR/reference/TxpResult-class.md)
  object to create `TxpResultList` object

- x:

  `TxpResultList` object

## See also

[TxpResult](https://toxpi.github.io/toxpiR/reference/TxpResult-class.md),
[txpCalculateScores](https://toxpi.github.io/toxpiR/reference/txpCalculateScores.md)

## Examples

``` r
## Load example dataset & model; see ?TxpModel for building model objects
data(txp_example_input, package = "toxpiR")
data(txp_example_model, package = "toxpiR")

## Calculate scores for list of models; returns TxpResultList object
txpCalculateScores(model = TxpModelList(m1 = txp_example_model, 
                                        m2 = txp_example_model), 
                   input = txp_example_input, 
                   id.var = "name")
#>   TxpResultList of length 2: m1 m2
resLst <- txpCalculateScores(model = list(m1 = txp_example_model, 
                                          m2 = txp_example_model), 
                             input = txp_example_input, 
                             id.var = "name")

## duplicated
duplicated(resLst)
#> [1] FALSE  TRUE

## Coercion
as(list(resLst[[1]], resLst[[2]]), "TxpResultList")
#>   TxpResultList of length 2: '' ''
as.TxpResultList(list(res1 = resLst[[1]], res2 = resLst[[2]]))
#>   TxpResultList of length 2: res1 res2

as(resLst[[1]], "TxpResultList")
#>   TxpResultList of length 1: ''
as.TxpResultList(resLst[[1]])
#>   TxpResultList of length 1: ''
```
