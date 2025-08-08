##----------------------------------------------------------------------------##
## methods-Slice
##----------------------------------------------------------------------------##

#' @name TxpSlice-class
#' @title ToxPi Slice
#' @description S4 class to store ToxPi slices
#' 
#' @slot txpValueNames `vector(<character>)` specifying the input columns to 
#' include in the slice; when `NULL`, no main slice will be calculated. In this 
#' case either/both of lower and upper bounds must be provided
#' @slot txpTransFuncs [TxpTransFuncList] with one function per entry in 
#' `txpValueNames` or an object that can be coerced to `TxpTransFuncList`; 
#' when `NULL`, no transformation function applied
#' @slot txpValueNamesLower `vector(<character>)` specifying the input columns to 
#' include in the slice lower confidence interval; when `NULL`, no lower bounds
#' will be calculated
#' @slot txpTransFuncsLower [TxpTransFuncList] with one function per entry in 
#' `txpValueNamesLower` or an object that can be coerced to `TxpTransFuncList`; 
#' when `NULL`, no transformation function applied to lower metrics
#' @slot txpValueNamesUpper `vector(<character>)` specifying the input columns to 
#' include in the slice upper confidence interval; when `NULL`, no upper bounds
#' will be calculated
#' @slot txpTransFuncsUpper [TxpTransFuncList] with one function per entry in 
#' `txpValueNamesUpper` or an object that can be coerced to `TxpTransFuncList`; 
#' when `NULL`, no transformation function applied to upper metrics
#' 
#' @param txpValueNames Passed to `txpValueNames` slot
#' @param txpTransFuncs Passed to `txpTransFuncs` slot
#' @param txpValueNamesLower Passed to `txpValueNamesLower` slot
#' @param txpTransFuncsLower Passed to `txpTransFuncsLower` slot
#' @param txpValueNamesUpper Passed to `txpValueNamesUpper` slot
#' @param txpTransFuncsUpper Passed to `txpTransFuncsUpper` slot
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

TxpSlice <- function(txpValueNames = NULL, txpTransFuncs = NULL, 
                     txpValueNamesLower = NULL, txpTransFuncsLower = NULL,
                     txpValueNamesUpper = NULL, txpTransFuncsUpper = NULL) {
  txpTransFuncs <- .TxpSlice.handle.funcs(txpValueNames, txpTransFuncs)
  txpTransFuncsLower <- .TxpSlice.handle.funcs(txpValueNamesLower, txpTransFuncsLower)
  txpTransFuncsUpper <- .TxpSlice.handle.funcs(txpValueNamesUpper, txpTransFuncsUpper)
  new2("TxpSlice", txpValueNames = txpValueNames, txpTransFuncs = txpTransFuncs, 
                   txpValueNamesLower = txpValueNamesLower, txpTransFuncsLower = txpTransFuncsLower,
                   txpValueNamesUpper = txpValueNamesUpper, txpTransFuncsUpper = txpTransFuncsUpper) 
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
  if(is.null(value)){
    x@txpTransFuncs <- TxpTransFuncList()
    validObject(x)
    warning("Setting <txpTransFuncs> to NULL to match <txpValueNames>")
  } else {
    needed <- 0
    if(length(x@txpValueNames) > length(x@txpTransFuncs)){
      needed <- length(x@txpValueNames) - length(x@txpTransFuncs)
      padded <- c(as.list(x@txpTransFuncs), rep(list(NULL), needed))
      x@txpTransFuncs <- do.call(TxpTransFuncList, padded)
    }
    if(length(x@txpValueNames) < length(x@txpTransFuncs)){
      needed <- length(x@txpValueNames) - length(x@txpTransFuncs)
      x@txpTransFuncs <- do.call(TxpTransFuncList, as.list(x@txpTransFuncs)[1:(length(x@txpTransFuncs) - length(x@txpValueNames))])
    }
    
    validObject(x)
    
    if(needed > 0){
      warning("Length of new <txpValueNames> greater than old length. Assuming extra have txpTransFuncs NULL. Please check txpTransFuncs<TxpSlice>.")
    }
    if(needed < 0){
      warning("Length of new <txpValueNames> less than old length. Removing excess txpTransFuncs. Please check txpTransFuncs<TxpSlice>.")
    }
  }
  
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

#' @describeIn TxpSlice-class Return `txpValueNamesLower` slot
#' @export

setMethod("txpValueNamesLower", "TxpSlice", function(x) { x@txpValueNamesLower })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpValueNamesLower", "TxpSlice", function(x, value) {
  x@txpValueNamesLower <- value
  if(is.null(value)){
    x@txpTransFuncsLower <- TxpTransFuncList()
    validObject(x)
    warning("Setting <txpTransFuncsLower> to NULL to match <txpValueNamesLower>")
  } else {
    needed <- 0
    if(length(x@txpValueNamesLower) > length(x@txpTransFuncsLower)){
      needed <- length(x@txpValueNamesLower) - length(x@txpTransFuncsLower)
      padded <- c(as.list(x@txpTransFuncsLower), rep(list(NULL), needed))
      x@txpTransFuncsLower <- do.call(TxpTransFuncList, padded)
    }
    if(length(x@txpValueNamesLower) < length(x@txpTransFuncsLower)){
      needed <- length(x@txpValueNamesLower) - length(x@txpTransFuncsLower)
      x@txpTransFuncsLower <- do.call(TxpTransFuncList, as.list(x@txpTransFuncsLower)[1:(length(x@txpTransFuncsLower) - length(x@txpValueNamesLower))])
    }
    
    validObject(x)
    
    if(needed > 0){
      warning("Length of new <txpValueNamesLower> greater than old length. Assuming extra have txpTransFuncsLower NULL. Please check txpTransFuncsLower<TxpSlice>.")
    }
    if(needed < 0){
      warning("Length of new <txpValueNamesLower> less than old length. Removing excess txpTransFuncsLower. Please check txpTransFuncsLower<TxpSlice>.")
    }
  }
  
  x
})

#' @describeIn TxpSlice-class Return `txpTransFuncsLower` slot
#' @export

setMethod("txpTransFuncsLower", "TxpSlice", function(x) { x@txpTransFuncsLower })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpTransFuncsLower", "TxpSlice", function(x, value) {
  value <- .TxpSlice.handle.funcs(txpValueNamesLower(x), value)
  x@txpTransFuncsLower <- value
  validObject(x)
  x
})

#' @describeIn TxpSlice-class Return `txpValueNamesUpper` slot
#' @export

setMethod("txpValueNamesUpper", "TxpSlice", function(x) { x@txpValueNamesUpper })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpValueNamesUpper", "TxpSlice", function(x, value) {
  x@txpValueNamesUpper <- value
  if(is.null(value)){
    x@txpTransFuncsUpper <- TxpTransFuncList()
    validObject(x)
    warning("Setting <txpTransFuncsUpper> to NULL to match <txpValueNamesUpper>")
  } else {
    needed <- 0
    if(length(x@txpValueNamesUpper) > length(x@txpTransFuncsUpper)){
      needed <- length(x@txpValueNamesUpper) - length(x@txpTransFuncsUpper)
      padded <- c(as.list(x@txpTransFuncsUpper), rep(list(NULL), needed))
      x@txpTransFuncsUpper <- do.call(TxpTransFuncList, padded)
    }
    if(length(x@txpValueNamesUpper) < length(x@txpTransFuncsUpper)){
      needed <- length(x@txpValueNamesUpper) - length(x@txpTransFuncsUpper)
      x@txpTransFuncsUpper <- do.call(TxpTransFuncList, as.list(x@txpTransFuncsUpper)[1:(length(x@txpTransFuncsUpper) - length(x@txpValueNamesUpper))])
    }
    
    validObject(x)
    
    if(needed > 0){
      warning("Length of new <txpValueNamesUpper> greater than old length. Assuming extra have txpTransFuncsUpper NULL. Please check txpTransFuncsUpper<TxpSlice>.")
    }
    if(needed < 0){
      warning("Length of new <txpValueNamesUpper> less than old length. Removing excess txpTransFuncsUpper. Please check txpTransFuncsUpper<TxpSlice>.")
    }
  }
  
  x
})

#' @describeIn TxpSlice-class Return `txpTransFuncsUpper` slot
#' @export

setMethod("txpTransFuncsUpper", "TxpSlice", function(x) { x@txpTransFuncsUpper })

#' @rdname TxpSlice-class
#' @export

setReplaceMethod("txpTransFuncsUpper", "TxpSlice", function(x, value) {
  value <- .TxpSlice.handle.funcs(txpValueNamesUpper(x), value)
  x@txpTransFuncsUpper <- value
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
  ln <- txpValueNamesLower(object)
  lfx <- txpTransFuncsLower(object)
  un <- txpValueNamesUpper(object)
  ufx <- txpTransFuncsUpper(object)
  
  if(is.null(c(vl, ln, un))){
    msg <- c(msg, "At least one of txpValueNames, txpValueNamesLower, or txpValueNamesUpper must be provided and non NULL.")
  }
  if (any(duplicated(vl))) {
    msg <- c(msg, "txpValueNames(<TxpSlice>) must be unique.")
  }
  if (length(vl) != length(fx)) {
    tmp <- paste("length(txpValueNames(<TxpSlice>)) !=",
                 "length(txpTransFuncs(<TxpSlice>))")
    msg <- c(msg, tmp)
  }
  if (any(duplicated(ln))) {
    msg <- c(msg, "txpValueNamesLower(<TxpSlice>) must be unique.")
  }
  if (length(ln) != length(lfx)) {
    tmp <- paste("length(txpValueNamesLower(<TxpSlice>)) !=",
                 "length(txpTransFuncsLower(<TxpSlice>))")
    msg <- c(msg, tmp)
  }
  if (any(duplicated(un))) {
    msg <- c(msg, "txpValueNamesUpper(<TxpSlice>) must be unique.")
  }
  if (length(un) != length(ufx)) {
    tmp <- paste("length(txpValueNamesUpper(<TxpSlice>)) !=",
                 "length(txpTransFuncsUpper(<TxpSlice>))")
    msg <- c(msg, tmp)
  }
  if(any(duplicated(c(unique(vl),unique(ln),unique(un))))){
    msg <- c(msg, "txpValueNames(<TxpSlice>), txpValueNamesLower(<TxpSlice>), and txpValueNamesUpper(<TxpSlice>) must not intersect")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpSlice", .TxpSlice.validity)

##----------------------------------------------------------------------------##
## show

.TxpSlice.show <- function(object) {
  n <- length(txpValueNames(object))
  ln <- length(txpValueNamesLower(object))
  un <- length(txpValueNamesUpper(object))
  cat(sprintf("TxpSlice with %d main input%s,", n, ifelse(n != 1, "s", "")),
      sprintf("%d lower bound input%s,", ln, ifelse(ln != 1, "s", "")),
      sprintf("%d upper bound input%s.\n", un, ifelse(un != 1, "s", "")))
  .coolcat("  txpValueNames(%d): %s\n", txpValueNames(object))
  if(length(txpValueNames(object)) == 0) {
    fnms <- NULL
  } else {
    fnms <- .listDisplayNames(txpTransFuncs(object))
  }  
  .coolcat("  txpTransFuncs(%d): %s\n", fnms)
  .coolcat("  txpValueNamesLower(%d): %s\n", txpValueNamesLower(object))
  if(length(txpValueNamesLower(object)) == 0) {
    fnms <- NULL
  } else {
    fnms <- .listDisplayNames(txpTransFuncsLower(object))
  }
  .coolcat("  txpTransFuncsLower(%d): %s\n", fnms)
  .coolcat("  txpValueNamesUpper(%d): %s\n", txpValueNamesUpper(object))
  if(length(txpValueNamesUpper(object)) == 0) {
    fnms <- NULL
  } else {
    fnms <- .listDisplayNames(txpTransFuncsUpper(object))
  } 
  .coolcat("  txpTransFuncsUpper(%d): %s\n", fnms)
}

setMethod("show", "TxpSlice", .TxpSlice.show)

##----------------------------------------------------------------------------##
## merge

.TxpSlice.merge <- function(x, y) {
  vns <- c(txpValueNames(x), txpValueNames(y))
  tfs <- c(txpTransFuncs(x), txpTransFuncs(y))
  lns <- c(txpValueNamesLower(x), txpValueNamesLower(y))
  ltfs <- c(txpTransFuncsLower(x), txpTransFuncsLower(y))
  uns <- c(txpValueNamesUpper(x), txpValueNamesUpper(y))
  utfs <- c(txpTransFuncsUpper(x), txpTransFuncsUpper(y))
  TxpSlice(txpValueNames = vns, txpTransFuncs = tfs,
           txpValueNamesLower = lns, txpTransFuncsLower = ltfs,
           txpValueNamesUpper = uns, txpTransFuncsUpper = utfs)
}

#' @describeIn TxpSlice-class Merge two `TxpSlice` objects into a single 
#' slice
#' @export

setMethod("merge", c("TxpSlice", "TxpSlice"), .TxpSlice.merge)

##----------------------------------------------------------------------------##

