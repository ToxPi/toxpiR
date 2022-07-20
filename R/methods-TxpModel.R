##----------------------------------------------------------------------------##
## methods-TxpModel
##----------------------------------------------------------------------------##

#' @name TxpModel-class
#' @title ToxPi Model
#' @description S4 class to store ToxPi models
#' 
#' @slot txpSlices [TxpSliceList] object
#' @slot txpWeights numeric vector specifying the relative weight of each slice; 
#' when NULL, defaults to 1 (equal weighting) for each slice 
#' @slot txpTransFuncs [TxpTransFuncList] object (or list of functions 
#' coercible to TxpTransFuncList)
#' 
#' @param txpSlices Passed to `txpSlices` slot
#' @param txpWeights Passed to `txpWeights` slot
#' @param txpTransFuncs Passed to `txpTransFuncs` slot
#' @param x,y TxpModel object
#' @param value Replacement value
#' @param adjusted Scalar logical, when `TRUE` weights are adjusted to sum to 1
#' @param simplify Scalar logical, when `TRUE` the returned `list` is simplified
#' 
#' @examples 
#' ## Create TxpSliceList & TxpTransFuncList objects
#' s1 <- list(S1 = TxpSlice("inpt1"), S2 = TxpSlice("inpt2"))
#' tf <- list(NULL, sqrt = function(x) sqrt(x))
#' 
#' ## Create TxpModel object
#' m1 <- TxpModel(txpSlices = s1, txpWeights = 2:1, txpTransFuncs = tf)
#' m1
#' 
#' ## Access TxpModel slots
#' txpSlices(m1)
#' txpWeights(m1)
#' txpWeights(m1, adjusted = TRUE)
#' txpTransFuncs(m1)
#' 
#' ## length
#' length(m1) ## equal to length(txpSlices(m1))
#' length(m1) == length(txpSlices(m1))
#' 
#' ## names
#' names(m1) ## equal to names(txpSlices(m1))
#' all(names(m1) == names(txpSlices(m1)))
#' 
#' ## Replacement
#' m2 <- m1
#' txpSlices(m2) <- list(S3 = TxpSlice("inpt3"), S4 = TxpSlice("inpt4"))
#' m2
#' names(m2)[2] <- "hello"
#' names(m2)
#' txpTransFuncs(m2) <- NULL
#' m2
#' txpTransFuncs(m2)[[1]] <- function(x) x^2
#' names(txpTransFuncs(m2))[1] <- "sq"
#' m2
#' 
#' ## merge
#' m3 <- merge(m1, m2)
#' m3

NULL

##----------------------------------------------------------------------------##
## constructor

#' @rdname TxpModel-class
#' @export 

TxpModel <- function(txpSlices, txpWeights = NULL, txpTransFuncs = NULL) {
  if (!is(txpSlices, "TxpSliceList")) txpSlices <- as.TxpSliceList(txpSlices)
  n <- length(txpSlices)
  if (is.null(txpWeights)) txpWeights <- rep(1, n)
  if (is.null(txpTransFuncs)) {
    txpTransFuncs <- as(List(vector("list", n)), "TxpTransFuncList")
  }
  if (!is(txpTransFuncs, "TxpTransFuncList")) {
    txpTransFuncs <- as.TxpTransFuncList(txpTransFuncs)
  }
  new2("TxpModel", 
       txpSlices = txpSlices, 
       txpWeights = txpWeights, 
       txpTransFuncs = txpTransFuncs) 
}

##----------------------------------------------------------------------------##
## accessors

#' @describeIn TxpModel-class Return `txpSlices` slot
#' @aliases TxpModel-txpSlices
#' @export

setMethod("txpSlices", "TxpModel", function(x) { 
  x@txpSlices 
})

#' @rdname TxpModel-class
#' @export

setReplaceMethod("txpSlices", "TxpModel", function(x, value) {
  if (!is(value, "TxpSliceList")) value <- as.TxpSliceList(value)
  x@txpSlices <- value
  validObject(x)
  x
})

#' @describeIn TxpModel-class Return `txpWeights` slot
#' @param adjusted Scalar logical, should the returned weights be adjusted 
#' such that they sum to 1?
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpWeights", "TxpModel", function(x, adjusted = FALSE) { 
  stopifnot(is_scalar_logical(adjusted))
  wts <- x@txpWeights 
  if (adjusted) wts <- wts/sum(wts)
  wts
})

#' @rdname TxpModel-class
#' @export

setReplaceMethod("txpWeights", "TxpModel", function(x, value) {
  x@txpWeights <- value
  validObject(x)
  x
})

#' @describeIn TxpModel-class Return `txpTransFuncs` slot 
#' @export

setMethod("txpTransFuncs", "TxpModel", function(x) { 
  x@txpTransFuncs 
})

#' @rdname TxpModel-class
#' @export

setReplaceMethod("txpTransFuncs", "TxpModel", function(x, value) {
  if (is.null(value)) value <- vector("list", length(x))
  if (!is(value, "TxpTransFuncList")) value <- as.TxpTransFuncList(value)
  x@txpTransFuncs <- value
  validObject(x)
  x
})

#' @describeIn TxpModel-class Return `list` of `txpValueNames` slots for the 
#' contained [TxpSliceList] object, or `vector` when `simplify = TRUE`
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpValueNames", "TxpModel", function(x, simplify = FALSE) {
  stopifnot(is_scalar_logical(simplify))
  nms <- txpValueNames(txpSlices(x), simplify = simplify)
  nms
})

#' @describeIn TxpModel-class Return slice names; shortcut for 
#' `names(txpSlices(x))`
#' @export

setMethod("names", "TxpModel", function(x) { 
  names(txpSlices(x))
})

#' @rdname TxpModel-class
#' @export

setReplaceMethod("names", "TxpModel", function(x, value) {
  names(x@txpSlices) <- value
  validObject(x, complete = TRUE)
  x
})

.TxpModel.calc <- function(model, input, 
                           id.var = NULL,
                           rank.ties.method = c("average", "first", "last", 
                                                "random", "max", "min"),
                           negative.value.handling = c("keep", "missing")) {
  .calculateScores(model = model, 
                   input = input, 
                   id.var = id.var, 
                   rank.ties.method = rank.ties.method,
                   negative.value.handling = negative.value.handling)
}

#' @describeIn TxpModel-class Return number of slices in model; shortcut for
#' `length(txpSlices(x))`
#' @export

setMethod("length", "TxpModel", function(x) { 
  length(txpSlices(x))
})

#' @rdname txpCalculateScores
#' @export

setMethod("txpCalculateScores", c("TxpModel", "data.frame"), .TxpModel.calc)

##----------------------------------------------------------------------------##
## validity

.TxpModel.validity <- function(object) {
  msg <- NULL
  sl <- txpSlices(object)
  wt <- txpWeights(object)
  tf <- txpTransFuncs(object)
  if (length(sl) != length(wt)) {
    tmp <- "length(txpSlices(<TxpModel>)) != length(txpWeights(<TxpModel>))"
    msg <- c(msg, tmp)
  }
  if (length(sl) != length(tf)) {
    tmp <- "length(txpSlices(<TxpModel>)) != length(txpTransFuncs(<TxpModel>))"
    msg <- c(msg, tmp)
  }
  valNms <- txpValueNames(sl, simplify = TRUE)
  valDup <- duplicated(valNms)
  if (any(valDup)) {
    dup <- valNms[valDup]
    wrn <- "The following 'input' columns are duplicated in the model:\n    %s"
    warning(sprintf(wrn, paste(dup, collapse = ", ")))
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpModel", .TxpModel.validity)

##----------------------------------------------------------------------------##
## show

.TxpModel.show <- function(object) {
  fnms <- .listDisplayNames(txpTransFuncs(object))
  cat(sprintf("TxpModel with %d slices.\n", length(txpSlices(object))))
  .coolcat("txpSlices(%d): %s\n", names(txpSlices(object)))
  .coolcat("txpWeights(%d): %s\n", txpWeights(object))
  .coolcat("txpTransFuncs(%d): %s\n", fnms)
}

setMethod("show", "TxpModel", .TxpModel.show)

##----------------------------------------------------------------------------##
## merge

.TxpModel.merge <- function(x, y) {
  sls <- c(txpSlices(x), txpSlices(y))
  wts <- c(txpWeights(x), txpWeights(y))
  tfs <- c(txpTransFuncs(x), txpTransFuncs(y))
  TxpModel(txpSlices = sls, txpWeights = wts, txpTransFuncs = tfs)
}

#' @describeIn TxpModel-class Merge two `TxpModel` objects into a single 
#' model
#' @export

setMethod("merge", c("TxpModel", "TxpModel"), .TxpModel.merge)

##----------------------------------------------------------------------------##
