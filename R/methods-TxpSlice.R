##----------------------------------------------------------------------------##
## methods-Slice
##----------------------------------------------------------------------------##

#' @name TxpSlice-class
#' @title ToxPi Slice
#' @description S4 class to store ToxPi slices
#' 
#' @slot txpValueNames `vector(<character>)` specifying the input columns to 
#' include in the slice
#' @slot txpTransFuncs [TxpTransFuncList] with one function per entry in 
#' `txpValueNames` or an object that can be coerced to `TxpTransFuncList`; 
#' when `NULL`, no transformation function applied
#' 
#' 
#' @param txpValueNames Passed to `txpValueNames` slot
#' @param txpTransFuncs Passed to `txpTransFuncs` slot
#' @param x,y `TxpSlice` object
#' @param value Replacement value
#' 
#' @details 
#' If the user supplies `txpTransFuncs` a single function/[TxpTransFunc] object, 
#' the given function will be recycled for each input with a warning. 
#' 
#' @examples 
#' ## Create TxpSlice object 
#' # Without transform functions
#' TxpSlice(txpValueNames = c("sqrData", "expData")) 
#' # With transform functions
#' TxpSlice(txpValueNames = c("sqrData", "expData"),
#'          txpTransFuncs = c(sq = function(x) x^2, log = function(x) log(x)))
#' 
#' # Transformation function recycled with warning when single function given
#' TxpSlice(txpValueNames = c("sqrData", "expData"), 
#'          txpTransFuncs = function(x) x^2) 
#'          
#' 
#' ## Access TxpSlice slots
#' sl <- TxpSlice(txpValueNames = c("sqrData", "expData"),
#'                txpTransFuncs = c(sq = function(x) x^2, 
#'                                  log = function(x) log(x)))
#' txpValueNames(sl)
#' txpTransFuncs(sl)
#' 
#' ## Replacement
#' txpValueNames(sl)[1] <- "hello"
#' sl
#' 
#' txpTransFuncs(sl)[[2]](exp(1))
#' txpTransFuncs(sl)[[2]] <- function(x) sqrt(x)
#' txpTransFuncs(sl)[[2]](exp(1))
#' 
#' # Note that replacing a single list element does NOT update the name
#' sl
#' names(txpTransFuncs(sl))[2] <- "sqrt" 
#' sl
#' 
#' # Replacing the whole list DOES update the names
#' txpTransFuncs(sl) <- list(sqrt = function(x) sqrt(x), 
#'                           log = function(x) log(x))
#' sl
#' 
#' ## length -- returns number of inputs
#' length(TxpSlice(letters))
#' 
#' ## merge
#' s1 <- TxpSlice("hello")
#' s2 <- TxpSlice("data")
#' merge(s1, s2)
#' 
#' # Note, input names still must be unique
#' \dontrun{merge(s1, s1)} ## produces error

NULL

##----------------------------------------------------------------------------##
## constructor

.TxpSlice.handle.funcs <- function(vn, tf) {
  vnl <- length(vn)
  if (is.null(tf)) tf <- vector("list", vnl)
  if (class(tf) %in% c("function", "TxpTransFunc") && vnl > 1) {
    warning("Recycling given 'txpTransFuncs' for each input.")
    tf <- .repFunc(tf, vnl)
  }
  if (!is(tf, "TxpTransFuncList")) {
    tf <- as.TxpTransFuncList(tf)
  }
  tf
}

#' @rdname TxpSlice-class
#' @export 

TxpSlice <- function(txpValueNames, txpTransFuncs = NULL) {
  txpTransFuncs <- .TxpSlice.handle.funcs(txpValueNames, txpTransFuncs)
  new2("TxpSlice", txpValueNames = txpValueNames, txpTransFuncs = txpTransFuncs) 
}

##----------------------------------------------------------------------------##
## accessors

#' @describeIn TxpSlice-class Return `txpValueNames` slot
#' @export

setMethod("txpValueNames", "TxpSlice", function(x) { x@txpValueNames })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpValueNames", "TxpSlice", function(x, value) {
  x@txpValueNames <- value
  validObject(x)
  x
})

#' @describeIn TxpSlice-class Return `txpTransFuncs` slot
#' @export

setMethod("txpTransFuncs", "TxpSlice", function(x) { x@txpTransFuncs })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpTransFuncs", "TxpSlice", function(x, value) {
  value <- .TxpSlice.handle.funcs(txpValueNames(x), value)
  x@txpTransFuncs <- value
  validObject(x)
  x
})

#' @describeIn TxpSlice-class Return number of inputs in slice; shortcut for
#' `length(txpValueNames(x))`
#' @export

setMethod("length", "TxpSlice", function(x) { length(txpValueNames(x)) })

##----------------------------------------------------------------------------##
## validity

.TxpSlice.validity <- function(object) {
  msg <- NULL
  vl <- txpValueNames(object)
  fx <- txpTransFuncs(object)
  if (any(duplicated(vl))) {
    msg <- c(msg, "txpValueNames(<TxpSlice>) must be unique.")
  }
  if (length(vl) != length(fx)) {
    tmp <- paste("length(txpValueNames(<TxpSlice>)) !=",
                 "length(txpTransFuncs(<TxpSlice>))")
    msg <- c(msg, tmp)
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpSlice", .TxpSlice.validity)

##----------------------------------------------------------------------------##
## show

.TxpSlice.show <- function(object) {
  n <- length(txpValueNames(object))
  cat(sprintf("TxpSlice with %d input%s.\n", n, ifelse(n > 1, "s", "")))
  .coolcat("  txpValueNames(%d): %s\n", txpValueNames(object))
  fnms <- .listDisplayNames(txpTransFuncs(object))
  .coolcat("  txpTransFuncs(%d): %s\n", fnms)
}

setMethod("show", "TxpSlice", .TxpSlice.show)

##----------------------------------------------------------------------------##
## merge

.TxpSlice.merge <- function(x, y) {
  vns <- c(txpValueNames(x), txpValueNames(y))
  tfs <- c(txpTransFuncs(x), txpTransFuncs(y))
  TxpSlice(txpValueNames = vns, txpTransFuncs = tfs)
}

#' @describeIn TxpSlice-class Merge two `TxpSlice` objects into a single 
#' slice
#' @export

setMethod("merge", c("TxpSlice", "TxpSlice"), .TxpSlice.merge)

##----------------------------------------------------------------------------##

