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

.chkModelInput <- function(model, input) {
  stopifnot(is(model, "TxpModel"))
  stopifnot(is.data.frame(input))
  valNms <- txpValueNames(model, simplify = TRUE)
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

# replicated pryr functions

.to_env <- function(x, quiet = FALSE) {
  if (is.environment(x)) {
    x
  } else if (is.list(x)) {
    list2env(x)
  } else if (is.function(x)) {
    environment(x)
  } else if (length(x) == 1 && is.character(x)) {
    if (!quiet) message("Using environment ", x)
    as.environment(x)
  } else if (length(x) == 1 && is.numeric(x) && x > 0) {
    if (!quiet) message("Using environment ", search()[x])
    as.environment(x)
  } else {
    stop("Input can not be coerced to an environment", call. = FALSE)
  }
}

.substitute_q <- function(x, env) {
  stopifnot(is.language(x))
  env <- .to_env(env)
  
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
  
}

.make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  stopifnot(
    .all_named(args),
    is.language(body))
  env <- .to_env(env)
  
  eval(call("function", args, body), env)
}

.all_named <- function(x) {
  if (length(x) == 0) return(TRUE)
  !is.null(names(x)) && all(names(x) != "")
}
##----------------------------------------------------------------------------##

