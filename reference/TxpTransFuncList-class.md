# List of TxpTransFunc objects

Extension of
[S4Vectors::SimpleList](https://rdrr.io/pkg/S4Vectors/man/SimpleList-class.html)
that holds only `NULL` or
[TxpTransFunc](https://toxpi.github.io/toxpiR/reference/TxpTransFunc-class.md)
objects.

## Usage

``` r
TxpTransFuncList(...)

as.TxpTransFuncList(x)
```

## Arguments

- ...:

  [TxpTransFunc](https://toxpi.github.io/toxpiR/reference/TxpTransFunc-class.md)
  object or function to create `TxpTransFuncList` object

- x:

  `list`, `function`, or
  [TxpTransFunc](https://toxpi.github.io/toxpiR/reference/TxpTransFunc-class.md)
  object to coerce to `TxpTransFuncList`

## Details

When `...` includes function objects, `TxpTransFuncList` will attempt to
coerce them to
[TxpTransFunc](https://toxpi.github.io/toxpiR/reference/TxpTransFunc-class.md)
and return an error if any of the elements cannot be coerced to
[TxpTransFunc](https://toxpi.github.io/toxpiR/reference/TxpTransFunc-class.md).

## Examples

``` r
## Create TxpTransFunc objects
tf1 <- TxpTransFunc(function(x) x)
tf2 <- TxpTransFunc(function(x) sqrt(x))

## Create TxpTransFuncList 
tfl <- TxpTransFuncList(linear = tf1, sqrt = tf2, cube = function(x) x^3)
tfl[[3]](3) == 27
#> [1] TRUE
tfl[["sqrt"]](4) == 2
#> [1] TRUE

## Concatenate
c(tfl, tfl)
#>   TxpTransFuncList of length 6: linear sqrt ... sqrt cube

## names
names(c(tfl, tfl))
#> [1] "linear" "sqrt"   "cube"   "linear" "sqrt"   "cube"  

# note: names are printed as '' when missing; NULL is printed when list item
# is NULL
names(TxpTransFuncList(function(x) x, NULL))
#> NULL
TxpTransFuncList(function(x) x, NULL)
#>   TxpTransFuncList of length 2: '' NULL

## coercion
as(function(x) x, "TxpTransFuncList")
#>   TxpTransFuncList of length 1: ''
as.TxpTransFuncList(function(x) x)
#>   TxpTransFuncList of length 1: ''

as(TxpTransFunc(function(x) x), "TxpTransFuncList")
#>   TxpTransFuncList of length 1: ''
as.TxpTransFuncList(TxpTransFunc(function(x) x))
#>   TxpTransFuncList of length 1: ''

as(list(function(x) x, sqrt = function(x) sqrt(x)), "TxpTransFuncList")
#>   TxpTransFuncList of length 2: '' sqrt
as.TxpTransFuncList(list(function(x) x, sqrt = function(x) sqrt(x)))
#>   TxpTransFuncList of length 2: '' sqrt
```
