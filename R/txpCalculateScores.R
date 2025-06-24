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

.sumNA <- function(x, level, max_cols) {
  if (all(is.na(x))) return(NA_real_)
  if(level == "up"){
    na_indices <- is.na(x)
    x[na_indices] <- max_cols[na_indices]
  }
  sum(x, na.rm = TRUE)
}

.z2o <- function(x) {
  (x - min(x, na.rm = TRUE))/diff(range(x, na.rm = TRUE))
}

.sumSlice <- function(slice, input, negative.value.handling) {
  # Applies input-level transformation functions and averages the values to give
  # a raw slice score
  .avgLevel <- function(nms, input, negative.value.handling, level){
    dat <- input[nms]
    if (negative.value.handling == "missing") dat[dat < 0]  <- NA
    tfs <- txpTransFuncs(slice)
    for (i in seq_along(nms)) {
      if (is.null(tfs[[i]])) next
      dat[[i]] <- tfs[[i]](dat[[i]])
    }
    max_cols <- sapply(dat, function(col) if (all(is.na(col))) NA_real_ else max(col, na.rm = TRUE))
    x <- apply(dat, MARGIN = 1, .sumNA, level, max_cols)
    x <- x/length(nms)
    
    dat <- unlist(dat)
    y <- sum(!is.finite(dat)) / length(dat)
    return(list(x = x,y = y))
  }
  
  #main score
  nms <- txpValueNames(slice)
  sum <- .avgLevel(nms, input, negative.value.handling, "mid")$x
  mis <- .avgLevel(nms, input, negative.value.handling, "mid")$y
  
  #lower confidence interval
  nms <- txpLowerNames(slice)
  if(!is.null(nms)){
    low_sum <- .avgLevel(nms, input, negative.value.handling, "low")$x
  } else {
    low_sum <- NULL
  }
  
  #upper confidence interval
  nms <- txpUpperNames(slice)
  if(!is.null(nms)){
    up_sum <- .avgLevel(nms, input, negative.value.handling, "up")$x
  } else {
    up_sum <- NULL
  }
  
  list(sum = sum, mis = mis, low_sum = low_sum, up_sum = up_sum)
}

.prepSlices <- function(model, input) {
  
  ## Clean up infinite in input
  input <- .rmInfinite(model = model, input = input)
  
  ## Calculate raw slice scores and missingness
  x <- lapply(
    txpSlices(model), .sumSlice, input = input,
    negative.value.handling = slot(model, "negativeHandling"))
  
  mis <- sapply(x, "[[", "mis")
  
  ##########TEMP FOR TESTING
  #x$s1$low_sum <- rep(10, length(x$s1$sum))
  #print(x)
  ##########
  cols <- c("sum", "low_sum", "up_sum")
  # Extract each component into a named list
  slc <- lapply(names(x), function(name) {
    val <- x[[name]]
    val <- setNames(lapply(cols, function(col) val[[col]]), paste0(name, c("", "_low", "_up")))
    val <- Filter(Negate(is.null), val)
    val <- as.matrix(as.data.frame(do.call(cbind, val)))
  })

  ## Look for and apply slice-level transformation functions
  tfs <- txpTransFuncs(model)
  if (any(!sapply(tfs, is.null))) {
    for (i in 1:length(slc)) {
      if (is.null(tfs[[i]])) next
      for (j in 1:length(slc[[i]])){
        slc[[i]][[j]] <- tfs[[i]](slc[[i]][[j]])
      }
    }
  }
  
  ## Make infinite NaN
  slc <- lapply(slc, function(x) {
    x[is.infinite(x)] <- NaN
    x
  })
  
  ## Scale slice scores from 0 to 1
  slc <- lapply(slc, .z2o)
  
  # Initialize empty matrices for low and up columns
  slc_main <- matrix(ncol = 0, nrow = nrow(slc[[1]]))
  slc_low <- matrix(ncol = 0, nrow = nrow(slc[[1]]))
  slc_up <- matrix(ncol = 0, nrow = nrow(slc[[1]]))
  
  # Loop through the list of matrices and extract the columns
  for (mat in slc) {
    mat_cols <- colnames(mat)
    main_cols <- intersect(names(x), mat_cols)
    low_cols  <- intersect(paste0(names(x), "_low"), mat_cols)
    up_cols   <- intersect(paste0(names(x), "_up"), mat_cols)

    if (length(main_cols)) slc_main <- cbind(slc_main, mat[, main_cols, drop = FALSE])
    if (length(low_cols))  slc_low  <- cbind(slc_low,  mat[, low_cols,  drop = FALSE])
    if (length(up_cols))   slc_up   <- cbind(slc_up,   mat[, up_cols,   drop = FALSE])
  }
  
  ## Make NA 0
  slc_main[is.na(slc_main)] <- 0
  slc_low[is.na(slc_low)] <- 0
  slc_up[is.na(slc_up)] <- 0

  list(slc_main = slc_main, mis = mis, slc_low = slc_low, slc_up = slc_up)
}

.calculateScores <- function(model, input,
                             id.var = NULL) {

  ## Test inputs
  .chkModelInput(model = model, input = input)

  ## Preprocess data, aggregate into slices, and determine missing data
  slcMis <- .prepSlices(model = model, input = input)
  mis <- slcMis$mis
  slc <- slcMis$slc_main
  if(ncol(slcMis$slc_main) == 0){slc <- NULL} else {slc <- slcMis$slc_main}
  if(ncol(slcMis$slc_low) == 0){slc_low <- NULL} else {slc_low <- slcMis$slc_low}
  if(ncol(slcMis$slc_up) == 0){slc_up <- NULL} else {slc_up <- slcMis$slc_up}

  ## Calculate ToxPi score
  wts <- txpWeights(model, adjusted = TRUE)
  
  if(is.null(slc)){score <- NULL} else {score <- rowSums(slc*rep(wts, each = NROW(slc)), na.rm = TRUE)}
  if(is.null(slc_low)){score_low <- NULL} else {score_low <- rowSums(slc_low*rep(wts, each = NROW(slc_low)), na.rm = TRUE)}
  if(is.null(slc_up)){score_up <- NULL} 
  else {
    slc_up_adjusted <- slc_up
    slc_up_adjusted[is.na(slc_up_adjusted)] <- 1
    score_up <- rowSums(slc_up_adjusted*rep(wts, each = NROW(slc_up_adjusted)))
  } 
  
  ## Calculate ToxPi ranks
  if(is.null(score)){rnks <- NULL} else{rnks <- rank(-score, ties.method = slot(model, "rankTies"))}
  if(is.null(score_low)){rnk_low <- NULL} else{rnk_low <- rank(-score_low, ties.method = slot(model, "rankTies"))}
  if(is.null(score_up)){rnk_up <- NULL} else{rnk_up <- rank(-score_up, ties.method = slot(model, "rankTies"))}
  
  ## Assign IDs
  ids <- if (!is.null(id.var)) as.character(input[[id.var]]) else NULL

  TxpResult(txpScores = score,
            txpScoreLows = score_low,
            txpScoreUps = score_up,
            txpSliceScores = slc,
            txpSliceLows = slc_low,
            txpSliceUps = slc_up,
            txpRanks = rnks,
            txpRankLows = rnk_low,
            txpRankUps = rnk_up,
            txpMissing = mis,
            txpModel = model,
            txpIDs = ids)

}

##----------------------------------------------------------------------------##

