##----------------------------------------------------------------------------##
## txpCalculateScores
##----------------------------------------------------------------------------##

#' @name txpCalculateScores
#' @title Calculate ToxPi Scores for the given model and input data
#' @description Calculate ToxPi Scores for the given model and input data
#'
#' @param model S4 [TxpModel] object or [TxpModelList] object. The txpModel object to be used in the calculation
#' @param input data.frame. The input dataframe to be analyzed that corresponds to the txpModel object provided
#' @param id.var Optional scalar character or numeric. The name or index of the column in the input containing
#' unique identifiers for the rows. If NULL, the row indices of the original data will be used as
#' unique names for identification
#' @param rank.ties.method Scalar character. Optionally overwrite `rankTies` in 
#' the provided txpModel with a new method for handling rank ties. Options are 
#' average', 'first', 'last', 'random', 'max', 'min'; when NULL keeps method stated in model
#' @param negative.value.handling Scalar character. Optionally overwrite `negativeHandling` in 
#' the provided txpModel with a new method for handling negative data. Options are 
#' 'keep' and missing'; when NULL keeps method stated in model
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

.sumSlice <- function(slice, input, model, negative.value.handling, slc_name) {
  # Applies input-level transformation functions and averages the values to give
  # a raw slice score
  .avgLevel <- function(nms, input, negative.value.handling, level){
    dat <- input[nms]
    if (negative.value.handling == "missing") dat[dat < 0]  <- NA
    if(level == "low"){ tfs <- txpLowerFuncs(slice)}
    else if(level == "up"){tfs <- txpUpperFuncs(slice)}
    else {tfs <- txpTransFuncs(slice)}
    for (i in seq_along(nms)) {
      if (is.null(tfs[[i]])) next
      dat[[i]] <- withCallingHandlers(
        tfs[[i]](dat[[i]]),
        warning = function(w) {
          message(sprintf("Warning in transformation for metric '%s' in slice '%s': %s", nms[[i]], slc_name, conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      )
    }

    .chkNonFiniteMetrics(dat, slc_name)
    max_cols <- sapply(dat, function(col) if (all(is.na(col))) NA_real_ else max(col, na.rm = TRUE))
    x <- apply(dat, MARGIN = 1, .sumNA, level, max_cols)
    x <- x/length(nms)
    dat <- unlist(dat)
    y <- sum(!is.finite(dat)) 
    return(list(x = x,y = y))
  }
  
  num_cols <- 0
                                     
  #main score
  nms <- txpValueNames(slice)
  if(!is.null(nms)){
    sumMis <- .avgLevel(nms, input, negative.value.handling, "mid")
    sum <- sumMis$x
    mis <- sumMis$y
    num_cols <- num_cols + length(nms)
  } else {
    sum <- NULL
    if(!is.null(txpValueNames(model))){
      mis <- nrow(input)
      num_cols <- num_cols + 1
    }
    else{mis <- NULL}
  }
  
  #lower confidence interval
  low_nms <- txpLowerNames(slice)
  if(!is.null(low_nms)){
    low_sumMis <- .avgLevel(low_nms, input, negative.value.handling, "low")
    low_sum <- low_sumMis$x
    low_mis <- low_sumMis$y
    num_cols <- num_cols + length(low_nms)
  } else {
    low_sum <- NULL
    if(!is.null(txpLowerNames(model))){
      low_mis <- nrow(input)
      num_cols <- num_cols + 1
    }
    else{low_mis <- NULL}
  }
  
  #upper confidence interval
  up_nms <- txpUpperNames(slice)
  if(!is.null(up_nms)){
    up_sumMis <- .avgLevel(up_nms, input, negative.value.handling, "up")
    up_sum <- up_sumMis$x
    up_mis <- up_sumMis$y
    num_cols <- num_cols + length(up_nms)
  } else {
    up_sum <- NULL
    if(!is.null(txpUpperNames(model))){
      up_mis <- nrow(input)
      num_cols <- num_cols + 1
    }
    else{up_mis <- NULL}
  }
  
  data_size <- num_cols*nrow(input)
  #missing data between all levels
  total_mis <- sum(mis, low_mis, up_mis)/data_size
  list(sum = sum, mis = total_mis, low_sum = low_sum, up_sum = up_sum)
}

.prepSlices <- function(model, input, id.var) {
  
  ## Clean up infinite in input
  input <- .rmInfinite(model = model, input = input)
  
  ## Calculate raw slice scores and missingness
  x <- Map(
    function(slice, slc_name) {
      .sumSlice(
        slice,
        model = model,
        input = input,
        negative.value.handling = slot(model, "negativeHandling"),
        slc_name = slc_name
      )
    },
    txpSlices(model),
    names(model)
  )

  mis <- sapply(x, "[[", "mis")
  
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

  ## Check for entirely missing slices
  .chkNonFiniteCols(slc)

  ## Scale slice scores from 0 to 1
  slc <- lapply(slc, .z2o)

  # Initialize empty matrices for confidence levels
  if (is.null(id.var)) {
    id_names <- as.character(1:nrow(slc[[1]]))
  } else {
    id_names <- input[[id.var]]
  }
  if(!is.null(txpValueNames(model))){
    slc_main <- matrix(ncol = length(model), nrow = nrow(slc[[1]]))
    colnames(slc_main) <- names(model)
    rownames(slc_main) <- id_names
  } else {
    slc_main <- NULL
  }
  if(!is.null(txpLowerNames(model))){
    slc_low <- matrix(ncol = length(model), nrow = nrow(slc[[1]]))
    colnames(slc_low) <- paste0(names(model), "_low")
    rownames(slc_low) <- id_names
  } else {
    slc_low <- NULL
  }
  if(!is.null(txpUpperNames(model))){
    slc_up <- matrix(ncol = length(model), nrow = nrow(slc[[1]]))
    colnames(slc_up) <- paste0(names(model), "_up")
    rownames(slc_up) <- id_names
  } else {
    slc_up <- NULL
  }
  
  # Loop through the list of matrices and extract the columns
  for (mat in slc) {
    mat_cols <- colnames(mat)
    main_col <- intersect(names(x), mat_cols)
    low_col  <- intersect(paste0(names(x), "_low"), mat_cols)
    up_col   <- intersect(paste0(names(x), "_up"), mat_cols)

    if (!is.null(slc_main)) {
      slc_main[, main_col] <- mat[, main_col]
    }
    if (!is.null(slc_low)) {
      slc_low[, low_col] <- mat[, low_col]
    }
    if (!is.null(slc_up)) {
      slc_up[, up_col] <- mat[, up_col]
    }
  }

  ## Make low NA 0, main NA lower bound or 0, upper NA 1
  if(!is.null(slc_low)){slc_low[is.na(slc_low)] <- 0}
  if (!is.null(slc_main)) {
    if (!is.null(slc_low)) {
      slc_main[is.na(slc_main)] <- slc_low[is.na(slc_main)]
    } else {
      slc_main[is.na(slc_main)] <- 0
    }
  }
  if(!is.null(slc_up)){slc_up[is.na(slc_up)] <- 1}
  list(slc_main = slc_main, mis = mis, slc_low = slc_low, slc_up = slc_up)
}

.calculateScores <- function(model, input,
                             id.var = NULL,
                             rank.ties.method = NULL,
                             negative.value.handling = NULL) {
  ## Check for overwritten model parameters 
  if(!is.null(rank.ties.method)){
    warning("Provided rank.ties.method overwriting rankTies slot in txpModel")
    model@rankTies <- rank.ties.method
  }
  if(!is.null(negative.value.handling)){
    warning("Provided negative.value.handling overwriting negativeHandling slot in txpModel")
    model@negativeHandling <- negative.value.handling
  }
  validObject(model)  

  ## Test inputs
  .chkModelInput(model = model, input = input, id.var = id.var)
  param <- TxpResultParam(rank.ties.method = rankTies(model),
                          negative.value.handling = negativeHandling(model))
  
  ## Preprocess data, aggregate into slices, and determine missing data
  slcMis <- .prepSlices(model = model, input = input, id.var = id.var)
  mis <- slcMis$mis
  slc <- slcMis$slc_main
  slc_low <- slcMis$slc_low
  slc_up <- slcMis$slc_up

  ## Calculate ToxPi score
  wts <- txpWeights(model, adjusted = TRUE)

  if (!is.null(slc)) {
    nms <- names(txpSlices(model))
    val_ind <- which(nms %in% colnames(slc))
    aligned_wts <- wts[val_ind]
    score <- rowSums(slc*rep(aligned_wts, each = NROW(slc)), na.rm = TRUE)
  } else {
    score <- NULL
  }
  if (!is.null(slc_low)) {
    nms <- paste0(names(txpSlices(model)), "_low")
    val_ind <- which(nms %in% colnames(slc_low))
    aligned_wts <- wts[val_ind]
    score_low <- rowSums(slc_low*rep(aligned_wts, each = NROW(slc_low)), na.rm = TRUE)
  } else {
    score_low <- NULL
  }
  if (!is.null(slc_up)) {
    nms <- paste0(names(txpSlices(model)), "_up")
    val_ind <- which(nms %in% colnames(slc_up))
    aligned_wts <- wts[val_ind]
    score_up <- rowSums(slc_up*rep(aligned_wts, each = NROW(slc_up)), na.rm = TRUE)
  } else {
    score_up <- NULL
  }
  
  ## Calculate ToxPi ranks
  if(is.null(score)){rnks <- NULL} else{rnks <- rank(-score, ties.method = slot(model, "rankTies"))}
  if(is.null(score_low)){rnk_low <- NULL} else{rnk_low <- rank(-score_low, ties.method = slot(model, "rankTies"))}
  if(is.null(score_up)){rnk_up <- NULL} else{rnk_up <- rank(-score_up, ties.method = slot(model, "rankTies"))}
  
  ## Assign IDs
  n <- length(score %||% score_low %||% score_up)
  if (!is.null(id.var)){
    ids <- as.character(input[[id.var]])
  } else {
    warning("id.var not provided. Generating unique identifiers for samples based on original row indices")
    ids <- as.character(c(1:n))
  }
  
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
            txpIDs = ids, 
            txpResultParam = param)
  
}

##----------------------------------------------------------------------------##

