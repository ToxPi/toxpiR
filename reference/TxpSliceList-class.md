# List of TxpSlice objects

Extension of
[S4Vectors::SimpleList](https://rdrr.io/pkg/S4Vectors/man/SimpleList-class.html)
that requires uniquely-named elements and holds only
[TxpSlice](https://toxpi.github.io/toxpiR/reference/TxpSlice-class.md)
objects.

## Usage

``` r
TxpSliceList(...)

# S4 method for class 'TxpSliceList'
txpValueNames(x, simplify = FALSE)

# S4 method for class 'TxpSliceList'
txpTransFuncs(x, simplify = FALSE)

# S4 method for class 'TxpSliceList'
duplicated(x)

as.TxpSliceList(x)
```

## Arguments

- ...:

  [TxpSlice](https://toxpi.github.io/toxpiR/reference/TxpSlice-class.md)
  object to create `TxpSliceList` object; MUST give unique names to each
  slice

- x:

  `TxpSliceList` object

- simplify:

  Scalar logical, when `TRUE` the returned `list` is simplified to a
  `vector`/[TxpTransFuncList](https://toxpi.github.io/toxpiR/reference/TxpTransFuncList-class.md)
  object

## Details

Note, there is no coercion for
[TxpSlice](https://toxpi.github.io/toxpiR/reference/TxpSlice-class.md)
to `TxpSliceList` because unique names are required.

## Functions

- `txpValueNames(TxpSliceList)`: Return `list` of `txpValueNames` slots
  for the contained
  [TxpSlice](https://toxpi.github.io/toxpiR/reference/TxpSlice-class.md)
  objects, or `vector` when `simplify = TRUE`

- `txpTransFuncs(TxpSliceList)`: Return `list` of `txpTransFuncs` slots
  for the contained
  [TxpSlice](https://toxpi.github.io/toxpiR/reference/TxpSlice-class.md)
  objects, or
  [TxpTransFuncList](https://toxpi.github.io/toxpiR/reference/TxpTransFuncList-class.md)
  when `simplify = TRUE`

- `duplicated(TxpSliceList)`: Returns logical vector of `length(x)`,
  where `TRUE` indicates a duplicate slice in the list; see
  [base::duplicated](https://rdrr.io/r/base/duplicated.html)

## Examples

``` r
## Create TxpSlice objects
s1 <- TxpSlice("input1", list(linear = function(x) x))
s2 <- TxpSlice(c("input2", "input3"), 
               list(log = function(x) log(x), sqrt = function(x) sqrt(x)))

## Create TxpSliceList
sl <- TxpSliceList(s1 = s1, s2 = s2)

## Accessors
txpValueNames(sl)
#> $s1
#> [1] "input1"
#> 
#> $s2
#> [1] "input2" "input3"
#> 
txpValueNames(sl, simplify = TRUE)
#>       s1      s21      s22 
#> "input1" "input2" "input3" 

txpTransFuncs(sl)
#> $s1
#>   TxpTransFuncList of length 1: linear
#> 
#> $s2
#>   TxpTransFuncList of length 2: log sqrt
#> 
txpTransFuncs(sl, simplify = TRUE)
#>   TxpTransFuncList of length 3: linear log sqrt

## Coercion
as(list(s1 = TxpSlice("hello"), s2 = TxpSlice("user")), "TxpSliceList")
#> TxpSliceList of length 2
#> names(2): s1 s2
as.TxpSliceList(c(s1 = TxpSlice("hello"), s2 = TxpSlice("user")))
#> TxpSliceList of length 2
#> names(2): s1 s2

## Concatenation
c(sl, TxpSliceList(s3 = TxpSlice("input4")))
#> TxpSliceList of length 3
#> names(3): s1 s2 s3

## Reduce TxpSliceList to single slice
Reduce(merge, sl)
#> TxpSlice with 3 inputs.
#>   txpValueNames(3): input1 input2 input3
#>   txpTransFuncs(3): linear log sqrt
```
