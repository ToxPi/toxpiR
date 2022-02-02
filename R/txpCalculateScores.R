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
#' @param rank.ties.method Character scalar, passed to [base::rank]
#' @inheritParams txpGenerics
#' 
#' @details 
#' `txpCalculateScores` is implemented as an S4 generic function with methods
#' for [TxpModel] and [TxpModelList].
#' 
#' Ranks are calculated such that the highest ToxPi score has a rank of 1.
#' 
#' @seealso [TxpModel], [TxpResult]
#' 
#' @template roxgn-loadExamples
#' @template roxgn-calcTxpModel
#' @template roxgn-calcTxpModelList
#' 
#' @return [TxpResult] or [TxpResultList] object
#' 
#' @export 

NULL

.calculateScores <- function(model, input, 
                             id.var = NULL,
                             rank.ties.method = c("average", "first", "last", 
                                                  "random", "max", "min")) {
  
  ## Test inputs
  stopifnot(is(model, "TxpModel"))
  stopifnot(is.data.frame(input))
  valNms <- txpValueNames(txpSlices(model), simplify = TRUE)
  inptNms <- names(input)
  if (!all(valNms %in% inptNms)) {
    miss <- valNms[!valNms %in% inptNms]
    msg <- "'input' missing the following data specified by 'model':\n    %s"
    stop(sprintf(msg, paste(miss, collapse = ", ")))
  }
  tstClass <- function(x) is.numeric(input[[x]])
  inptCls <- sapply(valNms, tstClass)
  if (!all(inptCls)) {
    nc2n <- valNms[!inptCls]
    msg <- "The following 'input' columns not numeric:\n    %s"
    stop(sprintf(msg, paste(nc2n, collapse = ", ")))
  }
  
  ## Clean up infinite in input
  notFinite <- sapply(valNms, function(x) any(is.infinite(input[[x]])))
  if (any(notFinite)) {
    warning("Some of the given inputs contained infinite values.")
    for (i in valNms[notFinite]) input[[i]][is.infinite(input[[i]])] <- NaN
  }
   
  ## Helper functions
  mySum <- function(x) {
    if (all(is.na(x))) return(NA_real_)
    sum(x, na.rm = TRUE)
  }
  sumSlice <- function(x) {
    # Applies input-level transformation functions and sums the values to give
    # a raw slice score
    nms <- txpValueNames(x)
    dat <- input[nms]
    tfs <- txpTransFuncs(x)
    for (i in seq_along(nms)) {
      if (is.null(tfs[[i]])) next 
      dat[[i]] <- tfs[[i]](dat[[i]])
    }
    apply(dat, MARGIN = 1, mySum)
  }
  z2o <- function(x) (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
  
  ## Calculate raw slice scores
  slc <- sapply(txpSlices(model), sumSlice)
  
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
  slc <- apply(slc, 2, z2o)
  
  ## Make NA 0
  slc[is.na(slc)] <- 0
  
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
            txpModel = model,
            txpIDs = ids)
  
}

##----------------------------------------------------------------------------##

