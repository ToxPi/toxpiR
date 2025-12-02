# Numeric transformation function

S4 class to store numeric transformation functions

## Usage

``` r
TxpTransFunc(x)
```

## Arguments

- x:

  function, see details

## Details

`TxpTransFunc` inherits from a standard R function, but specifies a
single input and a numeric output of the same length.

Functions can be passed directly to `TxpTransFuncList` list and the
functions will be coerced to `TxpTransFunc`.

We have an imperfect system for dealing with primitive functions (e.g.,
base::sqrt). To coerce primitives to TxpTransFunc's, we wrap them in
another function cal; wrapping the primitives obscures the original
function and requires the user to explore the function environment to
understand the primitive called. We recommend wrapping primitives in
separate functions to make the intent clear, .e.g.,
`mysqrt <- function(x) sqrt(x)`.

## Examples

``` r
f1 <- function(x) "hello"
f2 <- function(x) 3
f3 <- function(x) x + 5
if (FALSE) { # \dontrun{
t1 <- TxpTransFunc(x = f1) ## Produces error
t2 <- TxpTransFunc(x = f2) ## Produces error
} # }
t3 <- TxpTransFunc(x = f3)

## TxpTransFunc objects act as any other function
body(t3)
#> x + 5
formals(t3)
#> $x
#> 
#> 
t3(1:10)
#>  [1]  6  7  8  9 10 11 12 13 14 15

## Coercion from functions
if (FALSE) { # \dontrun{
TxpTransFuncList(f1, f2, f3) ## Produces error because f1, f3 not valid
} # }
```
