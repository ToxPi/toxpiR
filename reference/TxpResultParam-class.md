# ToxPi Result Parameters

S4 class to store ToxPi result calculation parameters

## Arguments

- rank.ties.method:

  Passed to `rank.ties.method` slot

- negative.value.handling:

  Passed to `negative.value.handling` slot

## Details

If more than one value is passed to `TxoResultParam` scalar options,
e.g. `rank.ties.method`, only the first value is kept.

The `rank.ties.method` slot is passed to
[base::rank](https://rdrr.io/r/base/rank.html) for calculating the ranks
of observations, with the highest-scoring observation having the rank of
1.

`negative.value.handling` indicates how to handle negative values in the
inputs. The ToxPi algorithm originally intended to accept non-negative
potency values; the GUI, therefore, treats negative values in the input
as missing. By default,
[txpCalculateScores](https://toxpi.github.io/toxpiR/reference/txpCalculateScores.md)
keeps negative values (`negative.value.handling = "keep"`). To replicate
the GUI behavior, users can set `negative.value.handling = "missing"`.

## Slots

- `rank.ties.method`:

  Character scalar, method used to calculate score ranks passed to
  [base::rank](https://rdrr.io/r/base/rank.html)

- `negative.value.handling`:

  Character scalar, how negative values are handled, see details

## See also

[txpCalculateScores](https://toxpi.github.io/toxpiR/reference/txpCalculateScores.md),
[TxpResult](https://toxpi.github.io/toxpiR/reference/TxpResult-class.md)
