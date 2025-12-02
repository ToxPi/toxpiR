# Calculate ToxPi Scores for the given model and input data

Calculate ToxPi Scores for the given model and input data

## Usage

``` r
txpCalculateScores(model, input, ...)

# S4 method for class 'TxpModel,data.frame'
txpCalculateScores(
  model,
  input,
  id.var = NULL,
  rank.ties.method = c("average", "first", "last", "random", "max", "min"),
  negative.value.handling = c("keep", "missing")
)

# S4 method for class 'TxpModelList,data.frame'
txpCalculateScores(
  model,
  input,
  id.var = NULL,
  rank.ties.method = c("average", "first", "last", "random", "max", "min"),
  negative.value.handling = c("keep", "missing")
)

# S4 method for class 'list,data.frame'
txpCalculateScores(
  model,
  input,
  id.var = NULL,
  rank.ties.method = c("average", "first", "last", "random", "max", "min"),
  negative.value.handling = c("keep", "missing")
)
```

## Arguments

- model:

  [TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
  object or
  [TxpModelList](https://toxpi.github.io/toxpiR/reference/TxpModelList-class.md)
  object

- input:

  data.frame object containing the model input data

- ...:

  Included for extendability; not currently used

- id.var:

  Character scalar, column in 'input' to store in

- rank.ties.method:

  Passed to `rank.ties.method` slot

- negative.value.handling:

  Passed to `negative.value.handling` slot

## Value

[TxpResult](https://toxpi.github.io/toxpiR/reference/TxpResult-class.md)
or
[TxpResultList](https://toxpi.github.io/toxpiR/reference/TxpResultList-class.md)
object

## Details

`txpCalculateScores` is implemented as an S4 generic function with
methods for
[TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
and
[TxpModelList](https://toxpi.github.io/toxpiR/reference/TxpModelList-class.md).

Ranks are calculated such that the highest ToxPi score has a rank of 1.

Missingness is determined after applying input-level transformations but
before applying slice-level transformations.

## See also

[TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md),
[TxpResult](https://toxpi.github.io/toxpiR/reference/TxpResult-class.md),
[TxpResultParam](https://toxpi.github.io/toxpiR/reference/TxpResultParam-class.md)

## Examples

``` r
## Load example dataset & model; see ?TxpModel for building model objects
data(txp_example_input, package = "toxpiR")
data(txp_example_model, package = "toxpiR")

## Calculate scores for single model; returns TxpResult object
res <- txpCalculateScores(model = txp_example_model, 
                          input = txp_example_input, 
                          id.var = "name")

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
```
