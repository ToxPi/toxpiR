# ToxPi Slice

S4 class to store ToxPi slices

## Usage

``` r
TxpSlice(txpValueNames, txpTransFuncs = NULL)

# S4 method for class 'TxpSlice'
txpValueNames(x)

# S4 method for class 'TxpSlice'
txpValueNames(x) <- value

# S4 method for class 'TxpSlice'
txpTransFuncs(x)

# S4 method for class 'TxpSlice'
txpTransFuncs(x) <- value

# S4 method for class 'TxpSlice'
length(x)

# S4 method for class 'TxpSlice,TxpSlice'
merge(x, y)
```

## Arguments

- txpValueNames:

  Passed to `txpValueNames` slot

- txpTransFuncs:

  Passed to `txpTransFuncs` slot

- x, y:

  `TxpSlice` object

- value:

  Replacement value

## Details

If the user supplies `txpTransFuncs` a single
function/[TxpTransFunc](https://toxpi.github.io/toxpiR/reference/TxpTransFunc-class.md)
object, the given function will be recycled for each input with a
warning.

## Functions

- `txpValueNames(TxpSlice)`: Return `txpValueNames` slot

- `txpTransFuncs(TxpSlice)`: Return `txpTransFuncs` slot

- `length(TxpSlice)`: Return number of inputs in slice; shortcut for
  `length(txpValueNames(x))`

- `merge(x = TxpSlice, y = TxpSlice)`: Merge two `TxpSlice` objects into
  a single slice

## Slots

- `txpValueNames`:

  `vector(<character>)` specifying the input columns to include in the
  slice

- `txpTransFuncs`:

  [TxpTransFuncList](https://toxpi.github.io/toxpiR/reference/TxpTransFuncList-class.md)
  with one function per entry in `txpValueNames` or an object that can
  be coerced to `TxpTransFuncList`; when `NULL`, no transformation
  function applied

## Examples

``` r
## Create TxpSlice object 
# Without transform functions
TxpSlice(txpValueNames = c("sqrData", "expData")) 
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): sqrData expData
#>   txpTransFuncs(2): NULL NULL
# With transform functions
TxpSlice(txpValueNames = c("sqrData", "expData"),
         txpTransFuncs = c(sq = function(x) x^2, log = function(x) log(x)))
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): sqrData expData
#>   txpTransFuncs(2): sq log

# Transformation function recycled with warning when single function given
TxpSlice(txpValueNames = c("sqrData", "expData"), 
         txpTransFuncs = function(x) x^2) 
#> Warning: Recycling given 'txpTransFuncs' for each input.
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): sqrData expData
#>   txpTransFuncs(2): '' ''
         

## Access TxpSlice slots
sl <- TxpSlice(txpValueNames = c("sqrData", "expData"),
               txpTransFuncs = c(sq = function(x) x^2, 
                                 log = function(x) log(x)))
txpValueNames(sl)
#> [1] "sqrData" "expData"
txpTransFuncs(sl)
#>   TxpTransFuncList of length 2: sq log

## Replacement
txpValueNames(sl)[1] <- "hello"
sl
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): hello expData
#>   txpTransFuncs(2): sq log

txpTransFuncs(sl)[[2]](exp(1))
#> [1] 1
txpTransFuncs(sl)[[2]] <- function(x) sqrt(x)
txpTransFuncs(sl)[[2]](exp(1))
#> [1] 1.648721

# Note that replacing a single list element does NOT update the name
sl
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): hello expData
#>   txpTransFuncs(2): sq log
names(txpTransFuncs(sl))[2] <- "sqrt" 
sl
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): hello expData
#>   txpTransFuncs(2): sq sqrt

# Replacing the whole list DOES update the names
txpTransFuncs(sl) <- list(sqrt = function(x) sqrt(x), 
                          log = function(x) log(x))
sl
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): hello expData
#>   txpTransFuncs(2): sqrt log

## length -- returns number of inputs
length(TxpSlice(letters))
#> [1] 26

## merge
s1 <- TxpSlice("hello")
s2 <- TxpSlice("data")
merge(s1, s2)
#> TxpSlice with 2 inputs.
#>   txpValueNames(2): hello data
#>   txpTransFuncs(2): NULL NULL

# Note, input names still must be unique
if (FALSE) merge(s1, s1) # \dontrun{} ## produces error
```
