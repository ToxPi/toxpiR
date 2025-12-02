# toxpiR data objects

Objects included in the toxpiR package, loaded with
[utils::data](https://rdrr.io/r/utils/data.html)

## Usage

``` r
data(txp_example_input, package = "toxpiR")

data(txp_example_model, package = "toxpiR")
```

## Source

<https://github.com/ToxPi/ToxPi-example-files>

## txp_example_input

Small example input data to be used with
[txpCalculateScores](https://toxpi.github.io/toxpiR/reference/txpCalculateScores.md)
in creating
[TxpResult](https://toxpi.github.io/toxpiR/reference/TxpResult-class.md)
objects. A [base::data.frame](https://rdrr.io/r/base/data.frame.html)
with 10 rows and 9 variables

- name:

  Observation names

- metric#:

  Input data for ToxPi models

## txp_example_model

Example
[TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
object intended for `txp_example_data`; model with 4 slices.

## Examples

``` r
data(txp_example_input, package = "toxpiR")
data(txp_example_model, package = "toxpiR")
txp_example_input
#>      name metric1 metric2 metric3 metric4 metric5 metric6 metric7 metric8
#> 1  chem01      74      77      25      74      77      97      25      77
#> 2  chem02      28      20      72      28      20      68      72      20
#> 3  chem03      61       3      73      61       3      24      73       3
#> 4  chem04      NA      40      20      NA      40      22      20      40
#> 5  chem05      29      53      44      29      53       4      44      53
#> 6  chem06      12      43      83      12      43      85      83      43
#> 7  chem07      29      NA      NA      29      NA      38      NA      NA
#> 8  chem08      58      66      70      58      66      NA      70      66
#> 9  chem09      51      84      58      51      84      NA      58      84
#> 10 chem10      60      40      32      60      40      38      32      40
txp_example_model
#> TxpModel with 4 slices.
#>   txpSlices(4): s1 s2 s3 s4
#>   txpWeights(4): 2 1 3 2
#>   txpTransFuncs(4): NULL linear NULL NULL

## Code to create txp_example_model
tf1 <- TxpTransFuncList(linear = function(x) x)
sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
                   s2 = TxpSlice("metric3"),
                   s3 = TxpSlice(sprintf("metric%d", 4:7),
                                 tf1[rep("linear", 4)]),
                   s4 = TxpSlice("metric8", tf1))
tf2 <- TxpTransFuncList(NULL, linear = function(x) x, NULL, NULL)
TxpModel(txpSlices = sl, txpWeights = c(2, 1, 3, 2), txpTransFuncs = tf2)
#> TxpModel with 4 slices.
#>   txpSlices(4): s1 s2 s3 s4
#>   txpWeights(4): 2 1 3 2
#>   txpTransFuncs(4): NULL linear NULL NULL
```
