##----------------------------------------------------------------------------##
## txpCalculateScores
##----------------------------------------------------------------------------##

#' @name txpCalculateScores
#' @title Calculate ToxPi Scores for the given model and input data
#' @description Calculate ToxPi Scores for the given model and input data
#'
#' @param model [TxpModel] object or [TxpModelList] object
#' @param input data.frame object containing the model input data
#' @param id.var Character scalar, column in 'input' to store in
#' @inheritParams TxpResultParam-class
#' @inheritParams txpGenerics
#'
#' @details
#' `txpCalculateScores` is implemented as an S4 generic function with methods
#' for [TxpModel] and [TxpModelList].
#'
#' Ranks are calculated such that the highest ToxPi score has a rank of 1.
#'
#' Missingness is determined after applying input-level transformations but
#' before applying slice-level transformations.
#'
#' @seealso [TxpModel], [TxpResult], [TxpResultParam]
#'
#' @template roxgn-loadExamples
#' @template roxgn-calcTxpModel
#' @template roxgn-calcTxpModelList
#'
#' @return [TxpResult] or [TxpResultList] object
#'
#' @export

NULL

.sumNA <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

.z2o <- function(x) {
  (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
}

.sumSlice <- function(slice, input, negative.value.handling) {
  # Applies input-level transformation functions and sums the values to give
  # a raw slice score
  nms <- txpValueNames(slice)
  dat <- input[nms]
  if (negative.value.handling == "missing") dat[dat < 0]  <- NA
  tfs <- txpTransFuncs(slice)
  for (i in seq_along(nms)) {
    if (is.null(tfs[[i]])) next
    dat[[i]] <- tfs[[i]](dat[[i]])
  }
  x <- apply(dat, MARGIN = 1, .sumNA)
  dat <- unlist(dat)
  y <- sum(!is.finite(dat)) / length(dat)
  list(sum = x, mis = y)
}

.prepSlices <- function(model, input, param) {
  
  ## Clean up infinite in input
  input <- .rmInfinite(model = model, input = input)
  
  ## Calculate raw slice scores and missingness
  x <- lapply(
    txpSlices(model), .sumSlice, input = input,
    negative.value.handling = slot(param, "negative.value.handling"))
  slc <- sapply(x, "[[", "sum")
  mis <- sapply(x, "[[", "mis")
  
  ## Look for and apply slice-level transformation functions
  tfs <- txpTransFuncs(model)
  if (any(!sapply(tfs, is.null))) {
    for (i in 1:ncol(slc)) {
      if (is.null(tfs[[i]])) next
      slc[ , i] <- tfs[[i]](slc[ , i])
    }
  }
  
  ## Make infinite NaN
  slc[is.infinite(slc)] <- NaN
  
  ## Scale slice scores from 0 to 1
  slc <- apply(slc, 2, .z2o)
  
  ## Make NA 0
  slc[is.na(slc)] <- 0
  
  list(slc = slc, mis = mis)
}

.calculateScores <- function(model, input,
                             id.var = NULL,
                             rank.ties.method = c("average", "first", "last",
                                                  "random", "max", "min"),
                             negative.value.handling = c("keep", "missing")) {

  ## Test inputs
  .chkModelInput(model = model, input = input)
  param <- TxpResultParam(rank.ties.method = rank.ties.method,
                          negative.value.handling = negative.value.handling)

  ## Preprocess data, aggregate into slices, and determine missing data
  slcMis <- .prepSlices(model = model, input = input, param = param)
  slc <- slcMis$slc
  mis <- slcMis$mis

  ## Calculate ToxPi score
  wts <- txpWeights(model, adjusted = TRUE)
  score <- rowSums(slc*rep(wts, each = NROW(slc)), na.rm = TRUE)

  ## Calculate ToxPi ranks
  rnks <- rank(-score, ties.method = rank.ties.method)

  ## Assign IDs
  ids <- if (!is.null(id.var)) as.character(input[[id.var]]) else NULL

  TxpResult(txpScores = score,
            txpSliceScores = slc,
            txpRanks = rnks,
            txpMissing = mis,
            txpModel = model,
            txpIDs = ids,
            txpResultParam = param)

}

##----------------------------------------------------------------------------##

