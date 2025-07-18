##---------------------------------------------------------------------------##
## Non-exported, non-documented, package-wide utility functions
##---------------------------------------------------------------------------##

#' @importFrom S4Vectors coolcat

.coolcat <- function(...) coolcat(..., indent = 2)

.catslot <- function(x, object) {
  cat("  ", x, ":", " ", slot(object = object, name = x), "\n", sep = "")
}

.repFunc <- function(func, times) {
  lst <- vector(mode = "list", length = times)
  for (i in 1:times) lst[[i]] <- func
  do.call("TxpTransFuncList", lst)
}

.listDisplayNames <- function(x) {
  n <- length(x)
  lnms <- names(x)
  if (is.null(lnms)) lnms <- rep('', n)
  lnms[sapply(x, is.null)] <- "NULL"
  lnms
}

.dupList <- function(x) {
  duplicated(as.list(x))
}

.chkModelInput <- function(model, input, id.var) {
  stopifnot(is(model, "TxpModel"))
  stopifnot(is.data.frame(input))
  valNms <- txpValueNames(model, simplify = TRUE)
  lowNms <- txpLowerNames(model, simplify = TRUE)
  upNms <- txpUpperNames(model, simplify = TRUE)
  valNms <- c(valNms, lowNms, upNms)
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
  if(!is.null(id.var)){
    stopifnot(is.numeric(id.var) || is.character(id.var))
    if(is.character(id.var) && !(id.var %in% inptNms)){
      msg <- "Specified identifier column %s not found in 'input'"
      stop(sprintf(msg, id.var))
    }
    if(is.numeric(id.var) && !(id.var >=1 & id.var <= length(inptNms))){
      stop("Invalid column index specified for 'id.var' for the provided 'input'")
    }
    if(any(duplicated(input[[id.var]]))){
      msg <- paste("'input' identifier column", id.var, "has non-unique values")
      stop(msg)
    }
  }
}

.rmInfinite <- function(model, input) {
  ## Clean up infinite in input
  valNms <- txpValueNames(txpSlices(model), simplify = TRUE)
  notFinite <- sapply(valNms, function(x) any(is.infinite(input[[x]])))
  if (any(notFinite)) {
    warning("Some of the given inputs contained infinite values.")
    for (i in valNms[notFinite]) input[[i]][is.infinite(input[[i]])] <- NaN
  }
  input
}

#' @importFrom grDevices col2rgb rgb

.col2hex <- function(x) {
  mat <- col2rgb(x)
  rgb(red = mat[1, ], green = mat[2, ], blue = mat[3, ], maxColorValue = 255)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

.chkNonFiniteCols <- function(df_list) {
  bad_cols <- unlist(lapply(df_list, function(df) {
    df <- as.data.frame(df)
    colnames(df)[sapply(df, function(col) all(!is.finite(col)))]
  }))
  
  if (length(bad_cols) > 0) {
    msg <- sprintf(
      "The following slices contain only non-finite values after all transformations:\n  %s",
      paste(unique(bad_cols), collapse = ", ")
    )
    stop(msg, call. = FALSE)
  }
  
  return(NULL)
}

.chkNonFiniteMetrics <- function(dat, slc_name) {
  df <- as.data.frame(dat)
  bad_cols <- colnames(df)[sapply(df, function(col) all(!is.finite(col)))]

  if (length(bad_cols) > 0) {
    msg <- sprintf(
      paste("The following metrics in slice", slc_name, "contain only non-finite values after metric transformations:\n  %s"),
            paste(unique(bad_cols), collapse = ", ")
      )
      stop(msg, call. = FALSE)
  }
  return(NULL)
}
##----------------------------------------------------------------------------##

