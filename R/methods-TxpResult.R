##----------------------------------------------------------------------------##
## methods-TxpResult
##----------------------------------------------------------------------------##

#' @name TxpResult-class
#' @aliases TxpResult
#' @title ToxPi Result
#' @description S4 class to store ToxPi results
#'
#' @slot txpScores `vector(<numeric>)` of model scores
#' @slot txpSliceScores `matrix(<numeric>)`, sample by slice `matrix` with
#' individual slice scores
#' @slot txpSliceLows `matrix(<numeric>)` or NULL, sample by slice `matrix` with
#' individual slice lower confidence interval scores
#' @slot txpSliceUps `matrix(<numeric>)` or NULL, sample by slice `matrix` with
#' individual slice upper confidence interval scores
#' @slot txpRanks `vector(<numeric>)` with rank of scores
#' @slot txpMissing `vector(<numeric>)` with data missingness
#' @slot txpModel [TxpModel] object
#' @slot txpIDs `vector(<character>)` of observation IDs
#' @slot txpResultParam [TxpResultParam] object
#' 
#' @param x [TxpResult] object
#' @param value Replacement value
#' @param adjusted Logical scalar, when `TRUE` the weights are adjusted to sum
#' to 1 or the slice scores are scaled to their respective weight
#' @param level `c('model', 'slices')`; indicates whether to retrieve
#' `txpTransFuncs` slot from the model or underlying slices
#' @param simplify Logical scalar, flatten `txpValueNames` or `txpTransFunc`
#' slots when retrieving slice-level information
#' @param i Subsetting index
#' @param j,drop,optional Not currently implemented
#' @param decreasing,na.last Passed to [base::sort]
#' @param row.names Passed to [base::data.frame]
#' @param id.name,score.name,rank.name Character scalar; when coercing to
#' [base::data.frame], the name for the `txpIDs`, `txpScores`, and `txpRanks`
#' columns, respectively
#' @param ... Passed to [base::data.frame] in `as.data.frame` or [base::sort]
#' in `sort`
#'
#' @seealso [txpCalculateScores], [plot], [TxpResultList]
#'
#' @template roxgn-loadExamples
#' @template roxgn-calcTxpModel
#'
#' @examples
#' ## Accessors
#' txpScores(res)
#'
#' txpSliceScores(res) ## adjusted for weight, by default
#' apply(txpSliceScores(res), 2, max, na.rm = TRUE)
#'
#' txpSliceScores(res, adjusted = FALSE) ## each score should have maximum of 1
#' apply(txpSliceScores(res, adjusted = FALSE), 2, max, na.rm = TRUE)
#'
#' txpRanks(res)
#'
#' txpMissing(res)
#'
#' txpModel(res)
#' identical(txpModel(res), txp_example_model)
#'
#' txpIDs(res)
#' names(res) ## identical to txpIDs(res)
#' identical(txpIDs(res), names(res))
#'
#' # Can access TxpModel slots directly
#' txpWeights(res)
#' txpWeights(res, adjusted = TRUE)
#' txpSlices(res)
#' # When retrieving transform functions, must specify level because both
#' # models and slices have transform functions
#' txpTransFuncs(res, level = "model")
#'
#' # Can access TxpSliceList slots directly
#' txpValueNames(res)
#' txpValueNames(res, simplify = TRUE)
#' txpTransFuncs(res, level = "slices")
#' txpTransFuncs(res, level = "slices", simplify = TRUE)
#'
#' ## Subsetting
#' res[1]
#' res[c("chem01", "chem09")]
#' res[grepl("4|6", txpIDs(res))]
#' \dontrun{
#' res[c(TRUE, FALSE)] ## gets recycled with warning
#' }
#'
#' ## length -- returns number of observations
#' length(res)
#' length(res[1:5])
#'
#' ## sort
#' names(res)
#' names(sort(res))
#'
#' txpScores(res)
#' txpScores(sort(res))
#' txpScores(sort(res, decreasing = FALSE))
#'
#' ## as.data.frame
#' as.data.frame(res)
#' as.data.frame(res, id.name = "nm", score.name = "scr", rank.name = "rnk")

NULL

##----------------------------------------------------------------------------##
## constructor -- NOT exported

TxpResult <- function(txpScores, txpScoreLows, txpScoreUps, txpSliceScores, txpSliceLows, txpSliceUps, txpRanks, txpRankLows, txpRankUps, txpMissing,
                      txpModel, txpIDs = NULL, txpResultParam) {
  new2("TxpResult",
       txpScores = txpScores,
       txpScoreLows = txpScoreLows,
       txpScoreUps = txpScoreUps,
       txpSliceScores = txpSliceScores,
       txpSliceLows = txpSliceLows,
       txpSliceUps = txpSliceUps,
       txpRanks = txpRanks,
       txpRankLows = txpRankLows,
       txpRankUps = txpRankUps,
       txpMissing = txpMissing,
       txpModel = txpModel,
       txpIDs = txpIDs,
       txpResultParam = txpResultParam)
}

##----------------------------------------------------------------------------##
## accessors

#' @describeIn TxpResult-class Return `txpScores` slot
#' @export

setMethod("txpScores", "TxpResult", function(x) { x@txpScores })

#' @describeIn TxpResult-class Return `txpScoreLows` slot
#' @export

setMethod("txpScoreLows", "TxpResult", function(x) { x@txpScoreLows })

#' @describeIn TxpResult-class Return `txpScoreUps` slot
#' @export

setMethod("txpScoreUps", "TxpResult", function(x) { x@txpScoreUps })

#' @describeIn TxpResult-class Return `txpSliceScores` slot; default
#' `adjusted = TRUE`, i.e. return slice scores adjusted for weight
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpSliceScores", "TxpResult", function(x, adjusted = TRUE) {
  stopifnot(is_scalar_logical(adjusted))
  scr <- x@txpSliceScores
  if(is.null(scr)){return(NULL)}
  if (adjusted) {
    nms <- names(txpSlices(x))
    val_ind <- which(nms %in% colnames(scr))
    wts <- txpWeights(x, adjusted = TRUE)[val_ind]
    scr <- scr*rep(wts, each = NROW(scr))
  }
  scr
})

#' @describeIn TxpResult-class Return `txpSliceUps` slot; default
#' `adjusted = TRUE`, i.e. return slice upper confidence scores adjusted for weight
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpSliceUps", "TxpResult", function(x, adjusted = TRUE) {
  stopifnot(is_scalar_logical(adjusted))
  scr <- x@txpSliceUps
  if(is.null(scr)){return(NULL)}
  if (adjusted) {
    nms <- names(txpSlices(x))
    up_nms <- paste0(nms, "_up")
    val_ind <- which(up_nms %in% colnames(scr))
    wts <- txpWeights(x, adjusted = TRUE)[val_ind]
    scr <- scr*rep(wts, each = NROW(scr))
  }
  scr
})

#' @describeIn TxpResult-class Return `txpSliceLows` slot; default
#' `adjusted = TRUE`, i.e. return slice lower confidence scores adjusted for weight
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpSliceLows", "TxpResult", function(x, adjusted = TRUE) {
  stopifnot(is_scalar_logical(adjusted))
  scr <- x@txpSliceLows
  if(is.null(scr)){return(NULL)}
  if (adjusted) {
    nms <- names(txpSlices(x))
    low_nms <- paste0(nms, "_low")
    val_ind <- which(low_nms %in% colnames(scr))
    wts <- txpWeights(x, adjusted = TRUE)[val_ind]
    scr <- scr*rep(wts, each = NROW(scr))
  }
  scr
})

#' @describeIn TxpResult-class Return `txpRanks` slot
#' @export

setMethod("txpRanks", "TxpResult", function(x) { x@txpRanks })

#' @describeIn TxpResult-class Return `txpRankLows` slot
#' @export

setMethod("txpRankLows", "TxpResult", function(x) { x@txpRankLows })

#' @describeIn TxpResult-class Return `txpRanks` slot
#' @export

setMethod("txpRankUps", "TxpResult", function(x) { x@txpRankUps })

#' @describeIn TxpResult-class Return `txpMissing` slot
#' @export

setMethod("txpMissing", "TxpResult", function(x) { x@txpMissing })

#' @describeIn TxpResult-class Return `txpResultParam` slot
#' @export

setMethod("txpResultParam", "TxpResult", function(x) { 
  lifecycle::deprecate_warn(
    when = "1.3.0",
    what = "txpResultParam()",
    details = "This slot will be removed in a future version as the same information
    is now provided in txpModel(<txpResult>)."
  )
  x@txpResultParam 
})

#' @describeIn TxpResult-class Return `txpModel` slot
#' @export

setMethod("txpModel", "TxpResult", function(x) { x@txpModel })

#' @describeIn TxpResult-class Return `txpIDs` slot
#' @export

setMethod("txpIDs", "TxpResult", function(x) { x@txpIDs })

.TxpResult.replaceIDs <- function(x, value) {
  x@txpIDs <- value
  validObject(x)
  x
}

#' @rdname TxpResult-class
#' @export

setReplaceMethod("txpIDs", "TxpResult", .TxpResult.replaceIDs)

#' @describeIn TxpResult-class Return `txpWeights` slot from model -- shortcut
#' for `txpWeights(txpModel(x))`; default `adjusted = FALSE`, i.e. return
#' unadjusted weights
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpWeights", "TxpResult", function(x, adjusted = FALSE) {
  stopifnot(is_scalar_logical(adjusted))
  txpWeights(txpModel(x), adjusted = adjusted)
})

#' @describeIn TxpResult-class Return `txpSlices` slot from model -- shortcut
#' for `txpSlices(txpModel(x))`
#' @export

setMethod("txpSlices", "TxpResult", function(x) { txpSlices(txpModel(x)) })

.TxpResult.txpTransFuncs <- function(x, level, simplify = FALSE) {
  stopifnot(is_scalar_logical(simplify))
  level <- match.arg(level, c("model", "slices"))
  if (level == "model") {
    return(txpTransFuncs(txpModel(x)))
  } else {
    return(txpTransFuncs(txpSlices(txpModel(x)), simplify = simplify))
  }
}

#' @describeIn TxpResult-class Return `txpTransFuncs` slot from model --
#' shortcut for `txpTransFuncs(txpModel(x))`
#' @importFrom rlang is_scalar_logical
#' @export

setMethod("txpTransFuncs", "TxpResult", .TxpResult.txpTransFuncs)

#' @describeIn TxpResult-class Return `txpValueNames` slot from slices --
#' shortcut for `txpValueNames(txpSlices(txpModel(x)))`
#' @export

setMethod("txpValueNames", "TxpResult", function(x, simplify = FALSE) {
  txpValueNames(txpSlices(txpModel(x)), simplify = simplify)
})

.TxpResult.squareBracket <- function(x, i, j, ..., drop = FALSE) {
  ss <- txpSliceScores(x, adjusted = FALSE)[i, , drop = FALSE]
  ls <- txpSliceLows(x, adjusted = FALSE)[i, , drop = FALSE]
  us <- txpSliceUps(x, adjusted = FALSE)[i, , drop = FALSE]
  
  TxpResult(txpScores = txpScores(x)[i],
            txpScoreLows = txpScoreLows(x)[i],
            txpScoreUps = txpScoreUps(x)[i],
            txpSliceScores = ss,
            txpSliceLows = ls,
            txpSliceUps = us,
            txpRanks = txpRanks(x)[i],
            txpRankLows = txpRankLows(x)[i],
            txpRankUps = txpRankUps(x)[i],
            txpMissing = txpMissing(x),
            txpModel = txpModel(x),
            txpIDs = txpIDs(x)[i],
            txpResultParam = suppressWarnings(txpResultParam(x)))
}

#' @rdname TxpResult-class
#' @export

setMethod("[",
          c("TxpResult", "logical", "missing"),
          function(x, i, j, ..., drop = FALSE) {
  if (length(i) < length(x)) {
    warning("Length of logical vector less than length of object; ",
            "recycling vector")
  }
  .TxpResult.squareBracket(x, i)
})

#' @rdname TxpResult-class
#' @export

setMethod("[", c("TxpResult", "integer", "missing"), .TxpResult.squareBracket)

#' @rdname TxpResult-class
#' @export

setMethod("[", c("TxpResult", "numeric", "missing"), .TxpResult.squareBracket)

#' @rdname TxpResult-class
#' @export

setMethod("[",
          c("TxpResult", "character", "missing"),
          function(x, i, j, ..., drop = FALSE) {
  ids <- txpIDs(x)
  if (is.null(ids)) {
    stop("TxpResult object must have assigned names, e.g. txpIDs(), to ",
         "susbet using a character vector.")
  }
  ind <- match(i, ids)
  if (length(ind) == 1 && is.na(ind)) return(suppressWarnings(x[0]))
  .TxpResult.squareBracket(x, ind)
})


#' @describeIn TxpResult-class Return the number of observations; shortcut for
#' `length(txpScores(x))`
#' @export

setMethod("length", "TxpResult", function(x) { length(txpScores(x)) })

#' @describeIn TxpResult-class Sort the ``TxpResult` object by their ranks
#' @export

setMethod("sort", "TxpResult", function(x, decreasing = TRUE, level = "main",
                                        na.last = TRUE, ...) {
  if(level == "main"){
    ind <- order(txpScores(x), decreasing = decreasing, na.last = na.last, ...)
  } else if(level == "low"){
    ind <- order(txpScoreLows(x), decreasing = decreasing, na.last = na.last, ...)
  } else if(level == "up"){
    ind <- order(txpScoreUps(x), decreasing = decreasing, na.last = na.last, ...)
  } else {
    stop("Invalid level parameter for sort function. Options are 'main', 'low', or 'up'")
  }
  x[ind]
})

#' @describeIn TxpResult-class Returns IDs; equal to `txpIDs(x)`
#' @export

setMethod("names", "TxpResult", function(x) txpIDs(x))

#' @rdname TxpResult-class
#' @export

setReplaceMethod("names", "TxpResult", .TxpResult.replaceIDs)

##----------------------------------------------------------------------------##
## validity

.TxpResult.validity <- function(object) {
  msg <- NULL
  scores <- txpScores(object)
  scoreLows <- txpScoreLows(object)
  scoreUps <- txpScoreUps(object)
  
  if(all(is.null(list(scores, scoreLows, scoreUps)))){
    msg <- c(msg, "At least one scoring level must be non-NULL")
  }
  
  sliceScores <- txpSliceScores(object, adjusted = FALSE)
  sliceLows <- txpSliceLows(object, adjusted = FALSE)
  sliceUps <- txpSliceUps(object, adjusted = FALSE)
  
  ranks <- txpRanks(object)
  rankLows <- txpRankLows(object)
  rankUps <- txpRankUps(object)
  
  model <- txpModel(object)
  ids <- txpIDs(object)
  lengths <- NULL
  if(!is.null(scores)){
    if (!is(ranks[1], "numeric")) {
      msg <- c(msg, "Entries in txpRanks must be \"numeric\"")
    }    
    if (!is(sliceScores[1], "numeric")) {
      msg <- c(msg, "Entries in txpSliceScores must be \"numeric\"")
    }
    if (length(scores) != nrow(sliceScores)) {
      msg <- c(msg, "length(txpScores) != nrow(txpSliceScores)")
    }
    lengths <- c(lengths, length(scores))
  }
  
  if(!is.null(scoreLows)){
    if (!is(rankLows[1], "numeric")) {
      msg <- c(msg, "Entries in txpRankLows must be \"numeric\"")
    } 
    if (!is(sliceLows[1], "numeric")) {
      msg <- c(msg, "Entries in txpSliceLows must be \"numeric\"")
    }
    if (length(scoreLows) != nrow(sliceLows)) {
      msg <- c(msg, "length(txpScoreLows) != nrow(txpSliceLows)")
    }
    lengths <- c(lengths, length(scoreLows))
  }
  
  if(!is.null(scoreUps)){
    if (!is(rankUps[1], "numeric")) {
      msg <- c(msg, "Entries in txpRankUps must be \"numeric\"")
    } 
    if (!is(sliceUps[1], "numeric")) {
      msg <- c(msg, "Entries in txpSliceUps must be \"numeric\"")
    }
    if (length(scoreUps) != nrow(sliceUps)) {
      msg <- c(msg, "length(txpScoreUps) != nrow(txpSliceUps)")
    }
    lengths <- c(lengths, length(scoreUps))
  }
  
  length <- unique(lengths)
  if(length(length) != 1) { 
    msg <- c(msg, "non-NULL txpScores, txpScoreLows, txpScoreUps must have same length")
  } else if (length(ids) != length(scores)) {
    msg <- c(msg, "length(txpIDs) != length(object)")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpResult", .TxpResult.validity)

##----------------------------------------------------------------------------##
## coerce

#' @importFrom rlang is_scalar_character is_scalar_logical

.TxpResult.as.data.frame <- function(x,
                                     row.names = NULL,
                                     optional = FALSE,
                                     ...,
                                     id.name = "id",
                                     score.name = "score",
                                     rank.name = "rank",
                                     adjusted = FALSE) {
  stopifnot(is_scalar_character(id.name))
  stopifnot(is_scalar_character(score.name))
  stopifnot(is_scalar_character(rank.name))
  stopifnot(is_scalar_logical(adjusted))
  
  outCols <- c()
  n <- length(txpScores(x) %||% txpScoreLows(x) %||% txpScoreUps(x))
  df <- data.frame(row = seq_len(n))
  
  if(!is.null(txpScores(x))){
    df[[score.name]] <- txpScores(x)
    df[[rank.name]] <- txpRanks(x)
    df <- cbind(df, txpSliceScores(x, adjusted = adjusted))
    outCols <- c(outCols, score.name, rank.name, colnames(txpSliceScores(x)))
  }
  
  if(!is.null(txpSliceLows(x, adjusted = adjusted))){
    df[[paste0(score.name, "_low")]] <- txpScoreLows(x)
    df[[paste0(rank.name, "_low")]] <- txpRankLows(x)
    df <- cbind(df, txpSliceLows(x, adjusted = adjusted))
    outCols <- c(outCols, paste0(score.name, "_low"), paste0(rank.name, "_low"), colnames(txpSliceLows(x)))
  }
  
  if(!is.null(txpSliceUps(x, adjusted = adjusted))){
    df[[paste0(score.name, "_up")]] <- txpScoreUps(x)
    df[[paste0(rank.name, "_up")]] <- txpRankUps(x)
    df <- cbind(df, txpSliceUps(x, adjusted = adjusted))
    outCols <- c(outCols, paste0(score.name, "_up"), paste0(rank.name, "_up"), colnames(txpSliceUps(x)))
  }

  if (!is.null(txpIDs(x))) {
    df[[id.name]] <- txpIDs(x)
    outCols <- c(id.name, outCols)
  } else {
    warning("txpIDs(x) is NULL; no ID column in returned data.frame.")
  }
  df[ , outCols]
}

#' @describeIn TxpResult-class Coerce TxpResult to [base::data.frame] object
#' with IDs, scores, ranks, and slice scores
#' @export

setMethod("as.data.frame", "TxpResult", .TxpResult.as.data.frame)


##----------------------------------------------------------------------------##
## show

.TxpResult.show <- function(object) {
  cat(sprintf("TxpResult of length %s\n", length(object)))
  .coolcat("names(%d): %s\n", names(object))
}

setMethod("show", "TxpResult", .TxpResult.show)


##----------------------------------------------------------------------------##



