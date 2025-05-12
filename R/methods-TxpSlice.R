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
#' @slot txpLowerNames `vector(<character>)` specifying the input columns to 
#' include in the slice lower confidence interval; when `NULL`, no lower bounds
#' will be calculated
#' @slot txpLowerFuncs [TxpTransFuncList] with one function per entry in 
#' `txpLowerNames` or an object that can be coerced to `TxpTransFuncList`; 
#' when `NULL`, no transformation function applied to lower metrics
#' @slot txpUpperNames `vector(<character>)` specifying the input columns to 
#' include in the slice upper confidence interval; when `NULL`, no upper bounds
#' will be calculated
#' @slot txpUpperFuncs [TxpTransFuncList] with one function per entry in 
#' `txpUpperNames` or an object that can be coerced to `TxpTransFuncList`; 
#' when `NULL`, no transformation function applied to upper metrics
#' 
#' @param txpValueNames Passed to `txpValueNames` slot
#' @param txpTransFuncs Passed to `txpTransFuncs` slot
#' @param txpLowerNames Passed to `txpLowerNames` slot
#' @param txpLowerFuncs Passed to `txpLowerFuncs` slot
#' @param txpUpperNames Passed to `txpUpperNames` slot
#' @param txpUpperFuncs Passed to `txpUpperFuncs` slot
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

TxpSlice <- function(txpValueNames, txpTransFuncs = NULL, 
                     txpLowerNames = NULL, txpLowerFuncs = NULL,
                     txpUpperNames = NULL, txpUpperFuncs = NULL) {
  txpTransFuncs <- .TxpSlice.handle.funcs(txpValueNames, txpTransFuncs)
  txpLowerFuncs <- .TxpSlice.handle.funcs(txpLowerNames, txpLowerFuncs)
  txpUpperFuncs <- .TxpSlice.handle.funcs(txpUpperNames, txpUpperFuncs)
  new2("TxpSlice", txpValueNames = txpValueNames, txpTransFuncs = txpTransFuncs, 
                   txpLowerNames = txpLowerNames, txpLowerFuncs = txpLowerFuncs,
                   txpUpperNames = txpUpperNames, txpUpperFuncs = txpUpperFuncs) 
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

setMethod("txpLowerNames", "TxpSlice", function(x) { x@txpLowerNames })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpLowerNames", "TxpSlice", function(x, value) {
  x@txpLowerNames <- value
  validObject(x)
  x
})

#' @describeIn TxpSlice-class Return `txpLowerFuncs` slot
#' @export

setMethod("txpLowerFuncs", "TxpSlice", function(x) { x@txpLowerFuncs })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpLowerFuncs", "TxpSlice", function(x, value) {
  value <- .TxpSlice.handle.funcs(txpLowerNames(x), value)
  x@txpLowerFuncs <- value
  validObject(x)
  x
})
setMethod("txpUpperNames", "TxpSlice", function(x) { x@txpUpperNames })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpUpperNames", "TxpSlice", function(x, value) {
  x@txpUpperNames <- value
  validObject(x)
  x
})

#' @describeIn TxpSlice-class Return `txpUpperFuncs` slot
#' @export

setMethod("txpUpperFuncs", "TxpSlice", function(x) { x@txpUpperFuncs })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpUpperFuncs", "TxpSlice", function(x, value) {
  value <- .TxpSlice.handle.funcs(txpUpperNames(x), value)
  x@txpUpperFuncs <- value
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
  ln <- txpLowerNames(object)
  lfx <- txpLowerFuncs(object)
  un <- txpUpperNames(object)
  ufx <- txpUpperFuncs(object)
  
  if (any(duplicated(vl))) {
    msg <- c(msg, "txpValueNames(<TxpSlice>) must be unique.")
  }
  if (length(vl) != length(fx)) {
    tmp <- paste("length(txpValueNames(<TxpSlice>)) !=",
                 "length(txpTransFuncs(<TxpSlice>))")
    msg <- c(msg, tmp)
  }
  if (any(duplicated(ln))) {
    msg <- c(msg, "txpLowerNames(<TxpSlice>) must be unique.")
  }
  if (length(ln) != length(lfx)) {
    tmp <- paste("length(txpLowerNames(<TxpSlice>)) !=",
                 "length(txpLowerFuncs(<TxpSlice>))")
    msg <- c(msg, tmp)
  }
  if (any(duplicated(un))) {
    msg <- c(msg, "txpUpperNames(<TxpSlice>) must be unique.")
  }
  if (length(un) != length(ufx)) {
    tmp <- paste("length(txpUpperNames(<TxpSlice>)) !=",
                 "length(txpUpperFuncs(<TxpSlice>))")
    msg <- c(msg, tmp)
  }
  if(any(duplicated(c(vl,ln,un)))){msg <- c(msg, "txpValueNames(<TxpSlice>), txpLowerNames(<TxpSlice>), and txpUpperNames(<TxpSlice>) must not intersect")}
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
  .coolcat("  txpLowerNames(%d): %s\n", txpLowerNames(object))
  if(length(txpLowerNames(object)) == 0) {
    fnms <- NULL
  } else {
    fnms <- .listDisplayNames(txpLowerFuncs(object))
  }
  .coolcat("  txpLowerFuncs(%d): %s\n", fnms)
  .coolcat("  txpUpperNames(%d): %s\n", txpUpperNames(object))
  if(length(txpUpperNames(object)) == 0) {
    fnms <- NULL
  } else {
    fnms <- .listDisplayNames(txpUpperFuncs(object))
  } 
  .coolcat("  txpUpperFuncs(%d): %s\n", fnms)
}

setMethod("show", "TxpSlice", .TxpSlice.show)

##----------------------------------------------------------------------------##
## merge

.TxpSlice.merge <- function(x, y) {
  vns <- c(txpValueNames(x), txpValueNames(y))
  tfs <- c(txpTransFuncs(x), txpTransFuncs(y))
  lns <- c(txpLowerNames(x), txpLowerNames(y))
  ltfs <- c(txpLowerFuncs(x), txpLowerFuncs(y))
  uns <- c(txpUpperNames(x), txpUpperNames(y))
  utfs <- c(txpUpperFuncs(x), txpUpperFuncs(y))
  TxpSlice(txpValueNames = vns, txpTransFuncs = tfs,
           txpLowerNames = lns, txpLowerFuncs = ltfs,
           txpUpperNames = uns, txpUpperFuncs = utfs)
}

#' @describeIn TxpSlice-class Merge two `TxpSlice` objects into a single 
#' slice
#' @export

setMethod("merge", c("TxpSlice", "TxpSlice"), .TxpSlice.merge)

##----------------------------------------------------------------------------##

