##----------------------------------------------------------------------------##
## methods-TxpTransFunc
##----------------------------------------------------------------------------##

#' @name TxpTransFunc-class
#' @title Numeric transformation function
#' @description S4 class to store numeric transformation functions
#' 
#' @param x function, see details
#' 
#' @details 
#' \code{TxpTransFunc} inherits from a standard R function, but specifies a 
#' single input and a numeric output of the same length.
#' 
#' Functions can be passed directly to \code{TxpTransFuncList} list and the 
#' functions will be coerced to \code{TxpTransFunc}.
#' 
#' We have an imperfect system for dealing with primitive functions (e.g., 
#' [base::sqrt]).
#' To coerce primitives to TxpTransFunc's, we wrap them in another function
#' cal; wrapping the primitives obscures the original function and requires
#' the user to explore the function environment to understand the primitive 
#' called.
#' We recommend wrapping primitives in separate functions to make the intent
#' clear, .e.g., `mysqrt <- function(x) sqrt(x)`.  
#' 
#' @examples 
#' f1 <- function(x) "hello"
#' f2 <- function(x) 3
#' f3 <- function(x) x + 5
#' \dontrun{
#' t1 <- TxpTransFunc(x = f1) ## Produces error
#' t2 <- TxpTransFunc(x = f2) ## Produces error
#' }
#' t3 <- TxpTransFunc(x = f3)
#' 
#' ## TxpTransFunc objects act as any other function
#' body(t3)
#' formals(t3)
#' t3(1:10)
#' 
#' ## Coercion from functions
#' \dontrun{
#' TxpTransFuncList(f1, f2, f3) ## Produces error because f1, f3 not valid
#' }

NULL

##----------------------------------------------------------------------------##
## constructor

#' @rdname TxpTransFunc-class
#' @export 

TxpTransFunc <- function(x) {
  if (missing(x)) return(new("TxpTransFunc"))
  if (is.primitive(x)) {
    somePrimitive <- x
    f <- .convertPrimitive(somePrimitive)
  }
  else f <- x
  new2("TxpTransFunc", f)
}

##----------------------------------------------------------------------------##
## validity

.TxpTransFunc.validity <- function(object) {
  msg <- NULL
  res1 <- try(object(1:5), silent = TRUE)
  res2 <- try(object(1:6), silent = TRUE)
  if (is(res1, "try-error") || is(res2, "try-error")) {
    msg <- c(msg, "TxpTransFunc returned error when given numeric input.")
    return(msg)
  }
  if (length(res1) != 5 || length(res2) != 6) {
    msg <- c(msg, "TxpTransFunc output length must equal input length.")
  }
  if (!class(res1) %in% c("numeric", "integer")) {
    msg <- c(msg, "TxpTransFunc output must be numeric for numeric inputs.")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2(Class = "TxpTransFunc", method = .TxpTransFunc.validity)

##----------------------------------------------------------------------------##
## coercion

.TxpTransFunc.coerce.from.function <- function(from) {
  if (is.primitive(from)) {
    somePrimitive <- from
    f <- .convertPrimitive(somePrimitive)
  }
  else f <- from
  TxpTransFunc(f)
}

setAs("function", "TxpTransFunc", .TxpTransFunc.coerce.from.function)

##----------------------------------------------------------------------------##
## concatenation

## Close, but rearranges the elements inappropriately:
# f <- TxpTransFunc(f)
# c(a = f, b = f, f)
## ALSO, causes errors with the 'c' method for TxpTransFuncList that would
## need correction
# .TxpTransFunc.concatenate <- function(x, ...) {
#   lst <- if (missing(x)) list(...) else list(x, ...)
#   do.call("TxpTransFuncList", lst)
# }
# setMethod("c", "TxpTransFunc", .TxpTransFunc.concatenate)

##----------------------------------------------------------------------------##
## utilities

#' @importFrom pryr substitute_q make_function

.convertPrimitive <- function(somePrimitive) {
  warning("Using primitive functions obscures behavior; ",
          "see ?TxpTransFunc for more details.")
  f <- function(y) somePrimitive(y)
  f <- make_function(formals(f), substitute_q(body(f), environment(f)))
}

##----------------------------------------------------------------------------##

