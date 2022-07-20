##----------------------------------------------------------------------------##
## methods-txpTransFuncList
##----------------------------------------------------------------------------##

#' @name TxpTransFuncList-class
#' @title List of TxpTransFunc objects
#' @description Extension of [S4Vectors::SimpleList] that holds only `NULL` or 
#' [TxpTransFunc] objects.
#' 
#' @param ... [TxpTransFunc] object or function to create `TxpTransFuncList` 
#' object
#' @param x `list`, `function`, or [TxpTransFunc] object to coerce to 
#' `TxpTransFuncList`
#' 
#' @details 
#' When `...` includes function objects, `TxpTransFuncList` will attempt to 
#' coerce them to [TxpTransFunc] and return an error if any of the elements
#' cannot be coerced to [TxpTransFunc].
#' 
#' @examples 
#' ## Create TxpTransFunc objects
#' tf1 <- TxpTransFunc(function(x) x)
#' tf2 <- TxpTransFunc(function(x) sqrt(x))
#' 
#' ## Create TxpTransFuncList 
#' tfl <- TxpTransFuncList(linear = tf1, sqrt = tf2, cube = function(x) x^3)
#' tfl[[3]](3) == 27
#' tfl[["sqrt"]](4) == 2
#' 
#' ## Concatenate
#' c(tfl, tfl)
#' 
#' ## names
#' names(c(tfl, tfl))
#' 
#' # note: names are printed as '' when missing; NULL is printed when list item
#' # is NULL
#' names(TxpTransFuncList(function(x) x, NULL))
#' TxpTransFuncList(function(x) x, NULL)
#' 
#' ## coercion
#' as(function(x) x, "TxpTransFuncList")
#' as.TxpTransFuncList(function(x) x)
#' 
#' as(TxpTransFunc(function(x) x), "TxpTransFuncList")
#' as.TxpTransFuncList(TxpTransFunc(function(x) x))
#' 
#' as(list(function(x) x, sqrt = function(x) sqrt(x)), "TxpTransFuncList")
#' as.TxpTransFuncList(list(function(x) x, sqrt = function(x) sqrt(x)))

NULL

##----------------------------------------------------------------------------##
## constructor

.TxpTransFuncList.toTransFunc <- function(x) {
  if (!is.null(x) && !inherits(x, "TxpTransFunc")) { 
    x <- try(TxpTransFunc(x), silent = TRUE)
  }
  x
}

#' @rdname TxpTransFuncList-class
#' @export 

TxpTransFuncList <- function(...) {
  listData <- lapply(list(...), .TxpTransFuncList.toTransFunc)
  valid <- vapply(listData, is, logical(1), "TxpTransFunc_OR_NULL")
  if (any(!valid)) {
    stop("Some of the given list items could not be coerced to 'TxpTransFunc'.")
  }
  new2("TxpTransFuncList", listData)
}

##----------------------------------------------------------------------------##
## validity

.TxpTransFuncList.validity <- function(object) {
  msg <- NULL
  valid <- vapply(object@listData, is, logical(1), "TxpTransFunc_OR_NULL")
  if (any(!valid)) {
    msg <- c(msg, "All TxpFuncList objects must be of class 'TxpTransFunc.'")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpTransFuncList", .TxpTransFuncList.validity)

##----------------------------------------------------------------------------##
## show

.TxpTransFuncList.show <- function(object) {
  lnms <- .listDisplayNames(object)
  .coolcat("  TxpTransFuncList of length %d: %s\n", lnms)
}

setMethod("show", "TxpTransFuncList", .TxpTransFuncList.show)

##----------------------------------------------------------------------------##
## coercion 

.TxpTransFuncList.from.list <- function(from) {
  do.call("TxpTransFuncList", from)
}

setAs("list", "TxpTransFuncList", .TxpTransFuncList.from.list)

.TxpTransFuncList.from.func <- function(from) {
  TxpTransFuncList(from)
}

setAs("function", "TxpTransFuncList", .TxpTransFuncList.from.func)

#' @rdname TxpTransFuncList-class
#' @export

as.TxpTransFuncList <- function(x) as(x, "TxpTransFuncList")

##----------------------------------------------------------------------------##

