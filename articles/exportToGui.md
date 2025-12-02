# Export ToxPi GUI Files

This vignette will show how to save a toxpiR model that will be
compatible with the ToxPi Java GUI, which can be downloaded from
[here](https://toxpi.org/). The toxpiR package and ToxPi Java GUI are
not directly compatible and there are several key differences to keep in
mind.

## Key differences between Java GUI and toxpiR

### Slice weights

The Java GUI only allows weights that are either integers or a ratio of
integers whereas the toxpiR package has no restrictions. The
[`txpExportGui()`](https://toxpi.github.io/toxpiR/reference/txpExportGui.md)
function requires all weights to be integers, so the user may need to
change the model weights to acceptable approximations prior to calling
the export function.

### Transformation/scaling functions

Currently the Java GUI only allows specific scaling functions and
applies them independently to every input within a slice. The toxpiR
package allows user-defined transformation functions at the input-level
and slice-level. To account for these differences, all input-level
transformation functions are applied before the data is exported. If
slice-level transformations are applied, then the export function will
create a data file that has the final slice scores rather than
input-level data.

The Java GUI does not allow negative input values and will treat them a
missing data. This causes a problem if negative values exist after
applying any user-defined transformations. If negative values occur
within a slice, then all values of that slice will be shifted up by a
constant so that no negative values remain. If a slice has both negative
transformed values and missing values, then missing values are replaced
with the added constant. In this last case, the toxpi and slice scores
will be computed correctly, however, the Java GUI should not be used to
compute bootstrapped confidence intervals because replacing missing data
during the export process will cause the resampling step to be
incorrect.

### Metrics in multiple slices

The Java GUI does not allow multiple columns to have the same name,
unless the data in those columns matches exactly. If a toxpiR model
includes an input column in multiple slices, then the name will be
appended with the slice index for each occurrence.

## Example use

First create a toxpiR model with accompanying data. Here we’ll load the
“Format C” data example using the
[`txpImportGui()`](https://toxpi.github.io/toxpiR/reference/txpImportGui.md)
function.

``` r
library(toxpiR)

# Load example model from "Import ToxPi GUI Files" vignette
data_format_C <- tempfile()
download.file(
  url = "https://raw.githubusercontent.com/ToxPi/ToxPi-example-files/main/format_C.csv",
  destfile = data_format_C,
  quiet = TRUE
)
gui1 <- txpImportGui(data_format_C)
#> Warning in method(object): The following 'input' columns are duplicated in the model:
#>     metric3, metric2, metric3, metric1, metric2
```

Now we can use to export function to create a new data file. Notice the
warnings for negative and missing values.

``` r
# Export back into GUI format
data_exported <- tempfile()
txpExportGui(
  fileName = data_exported,
  input = gui1$input,
  model = gui1$model,
  id.var = 'Name',
  fills = gui1$fills
)
#> Warning in txpExportGui(fileName = data_exported, input = gui1$input, model =
#> gui1$model, : Slice "Slice2" contains negative values after applying
#> transformations so all values were increased by x = 5.
#> Warning in txpExportGui(fileName = data_exported, input = gui1$input, model =
#> gui1$model, : Slice "Slice3" contains both missing and negative values after
#> applying transformations so missing values were replaced with 0 and then all
#> values were increased by x = 5.
```

### Compare the data files

Take a moment to observe differences between the original data file
(`data_format_C`) and the exported version (`data_exported`).

**`data_format_C`**

| V1                               | V2       | V3          | V4     | V5      | V6      | V7      | V8      |
|:---------------------------------|:---------|:------------|:-------|:--------|:--------|:--------|:--------|
| \# Slice1!4!0xFF69B4!-log10(x)+6 |          |             |        |         | x       | x       |         |
| \# Slice2!4!0x6959CD!-ln(x)      |          |             |        |         |         | x       |         |
| \# Slice3!4!0xCDC1C5!-ln(x)      |          |             |        | x       | x       | x       |         |
| \# Slice4!5!0xFF6347!-log10(x)+6 |          |             |        | x       | x       |         | x       |
| Row                              | Source   | CASRN       | Name   | metric1 | metric2 | metric3 | metric4 |
| 1                                | source01 | 11-111-1111 | chem01 | 25      | 91      | NA      | NA      |
| 2                                | source02 | 22-222-2222 | chem02 | NA      | 46      | 51      | 48      |
| 3                                | source03 | 33-333-3333 | chem03 | 44      | NA      | 9       | 34      |
| 4                                | source04 | 44-444-4444 | chem04 | 26      | 64      | 27      | 9       |
| 5                                | source05 | 55-555-5555 | chem05 | 33      | 36      | 69      | 88      |
| 6                                | source06 | 66-666-6666 | chem06 | 94      | 46      | NA      | 54      |
| 7                                | source07 | 77-777-7777 | chem07 | 37      | 31      | NA      | 7       |
| 8                                | source08 | 88-888-8888 | chem08 | 58      | 29      | 9       | 46      |
| 9                                | source09 | 99-999-9999 | chem09 | 95      | 24      | 78      | 46      |
| 10                               | source10 | 11-222-3333 | chem10 | 68      | 54      | 43      | 25      |

**`data_exported`**

| V1                             | V2             | V3             | V4             | V5             | V6             | V7             | V8             | V9             | V10     |
|:-------------------------------|:---------------|:---------------|:---------------|:---------------|:---------------|:---------------|:---------------|:---------------|:--------|
| \# Slice1!4!0xFF69B4!linear(x) | x              | x              |                |                |                |                |                |                |         |
| \# Slice2!4!0x6959CD!linear(x) |                |                | x              |                |                |                |                |                |         |
| \# Slice3!4!0xCDC1C5!linear(x) |                |                |                | x              | x              | x              |                |                |         |
| \# Slice4!5!0xFF6347!linear(x) |                |                |                |                |                |                | x              | x              | x       |
|                                | metric2_slice1 | metric3_slice1 | metric3_slice2 | metric1_slice3 | metric2_slice3 | metric3_slice3 | metric1_slice4 | metric2_slice4 | metric4 |
| chem01                         | 4.0410         | NA             | NA             | 1.7811         | 0.4891         | 5.0000         | 4.6021         | 4.0410         | NA      |
| chem02                         | 4.3372         | 4.2924         | 1.0682         | 5.0000         | 1.1714         | 1.0682         | NA             | 4.3372         | 4.3188  |
| chem03                         | NA             | 5.0458         | 2.8028         | 1.2158         | 5.0000         | 2.8028         | 4.3565         | NA             | 4.4685  |
| chem04                         | 4.1938         | 4.5686         | 1.7042         | 1.7419         | 0.8411         | 1.7042         | 4.5850         | 4.1938         | 5.0458  |
| chem05                         | 4.4437         | 4.1612         | 0.7659         | 1.5035         | 1.4165         | 0.7659         | 4.4815         | 4.4437         | 4.0555  |
| chem06                         | 4.3372         | NA             | NA             | 0.4567         | 1.1714         | 5.0000         | 4.0269         | 4.3372         | 4.2676  |
| chem07                         | 4.5086         | NA             | NA             | 1.3891         | 1.5660         | 5.0000         | 4.4318         | 4.5086         | 5.1549  |
| chem08                         | 4.5376         | 5.0458         | 2.8028         | 0.9396         | 1.6327         | 2.8028         | 4.2366         | 4.5376         | 4.3372  |
| chem09                         | 4.6198         | 4.1079         | 0.6433         | 0.4461         | 1.8219         | 0.6433         | 4.0223         | 4.6198         | 4.3372  |
| chem10                         | 4.2676         | 4.3665         | 1.2388         | 0.7805         | 1.0110         | 1.2388         | 4.1675         | 4.2676         | 4.6021  |

### Compare results

Although the data files are visually different, they will result in the
same toxpi and slice scores.

``` r
gui2 <- txpImportGui(data_exported)

res1 <- txpCalculateScores(gui1$model, gui1$input)
res2 <- txpCalculateScores(gui2$model, gui2$input)

all.equal(
  txpScores(res1),
  txpScores(res2)
)
#> [1] TRUE

all.equal(
  txpSliceScores(res1),
  txpSliceScores(res2)
)
#> [1] TRUE
```
