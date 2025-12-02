# Export comma-separated file intended for ToxPi GUI

Export comma-separated file intended for ToxPi GUI

## Usage

``` r
txpExportGui(
  fileName = "txpModel.csv",
  input,
  model,
  id.var = NULL,
  fills = NULL
)
```

## Arguments

- fileName:

  Character scalar, the path to the output file

- input:

  data.frame object containing the model input data

- model:

  [TxpModel](https://toxpi.github.io/toxpiR/reference/TxpModel-class.md)
  object or
  [TxpModelList](https://toxpi.github.io/toxpiR/reference/TxpModelList-class.md)
  object

- id.var:

  Character scalar, column in 'input' to store in

- fills:

  Colors to fill the slices

## Details

The GUI differs in two meaninful ways for exporting `toxpiR` models: (1)
the GUI only allows for integer weights; (2) the GUI applies
transformation functions differently.

`txpExporGui` will not work for models with non-integer weights.

The GUI only applies a single transformation function to every input
within a slice, and only functions from a pre-determined list; `toxpiR`
allows users to apply any valid function individually to each input,
then a second transformation function on the summed slice values.
Because of this complexity, any exported models with slice-level
transformation functions will not export at the input level. In other
words, the export will have only the final slice scores. Otherwise, all
input-level transformations will be performed, the and the export will
contain transformed input-level data with the `linear(x)` GUI
transformation.
