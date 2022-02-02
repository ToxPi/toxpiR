##----------------------------------------------------------------------------##
## methods-txpSliceList
##----------------------------------------------------------------------------##

#' @name TxpSliceList-class
#' @title List of TxpSlice objects
#' @description Extension of [S4Vectors::SimpleList] that requires 
#' uniquely-named elements and holds only [TxpSlice] objects.
#' 
#' @param ... [TxpSlice] object to create `TxpSliceList` object; MUST give 
#' unique names to each slice
#' @param x `TxpSliceList` object
#' @param simplify Scalar logical, when `TRUE` the returned `list` is simplified
#' to a `vector`/[TxpTransFuncList] object
#' 
#' @details 
#' Note, there is no coercion for [TxpSlice] to `TxpSliceList` because unique
#' names are required.
#' 
#' @examples
#' ## Create TxpSlice objects
#' s1 <- TxpSlice("input1", list(linear = function(x) x))
#' s2 <- TxpSlice(c("input2", "input3"), 
#'                list(log = function(x) log(x), sqrt = function(x) sqrt(x)))
#' 
#' ## Create TxpSliceList
#' sl <- TxpSliceList(s1 = s1, s2 = s2)
#' 
#' ## Accessors
#' txpValueNames(sl)
#' txpValueNames(sl, simplify = TRUE)
#' 
#' txpTransFuncs(sl)
#' txpTransFuncs(sl, simplify = TRUE)
#' 
#' ## Coercion
#' as(list(s1 = TxpSlice("hello"), s2 = TxpSlice("user")), "TxpSliceList")
#' as.TxpSliceList(c(s1 = TxpSlice("hello"), s2 = TxpSlice("user")))
#' 
#' ## Concatenation
#' c(sl, TxpSliceList(s3 = TxpSlice("input4")))
#' 
#' ## Reduce TxpSliceList to single slice
#' Reduce(merge, sl)

NULL

##----------------------------------------------------------------------------##
## constructor

#' @rdname TxpSliceList-class
#' @export 

TxpSliceList <- function(...) {
  listData <- list(...)
  new2("TxpSliceList", listData)
}

##----------------------------------------------------------------------------##
## validity

.TxpSliceList.validity <- function(object) {
  msg <- NULL
  valid <- vapply(object@listData, is, logical(1), "TxpSlice")
  if (any(!valid)) {
    msg <- c(msg, "All TxpSlice objects must be of class 'TxpSlice.'")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpSliceList", .TxpSliceList.validity)

##----------------------------------------------------------------------------##
## accessors

#' @describeIn TxpSliceList-class Return `list` of `txpValueNames` slots for the 
#' contained [TxpSlice] objects, or `vector` when `simplify = TRUE`
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpValueNames", "TxpSliceList", function(x, simplify = FALSE) {
  stopifnot(is_scalar_logical(simplify))
  nms <- lapply(x, txpValueNames)
  if (simplify) nms <- unlist(nms)
  nms
})

#' @describeIn TxpSliceList-class Return `list` of `txpTransFuncs` slots for the 
#' contained [TxpSlice] objects, or [TxpTransFuncList] when `simplify = TRUE`
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpTransFuncs", "TxpSliceList", function(x, simplify = FALSE) { 
  stopifnot(is_scalar_logical(simplify))
  fxs <- lapply(x, txpTransFuncs)
  if (simplify) fxs <- Reduce(c, fxs)
  fxs
})

##----------------------------------------------------------------------------##
## duplicated

#' @describeIn TxpSliceList-class Returns logical vector of `length(x)`, where 
#' `TRUE` indicates a duplicate slice in the list; see [base::duplicated]
#' @export

setMethod("duplicated", "TxpSliceList", function(x) .dupList(x))

##----------------------------------------------------------------------------##
## coercion 

.TxpSliceList.from.list <- function(from) {
  do.call("TxpSliceList", from)
}

setAs("list", "TxpSliceList", .TxpSliceList.from.list)

#' @rdname TxpSliceList-class
#' @export

as.TxpSliceList <- function(x) as(x, "TxpSliceList")

##----------------------------------------------------------------------------##

