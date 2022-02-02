##---------------------------------------------------------------------------##
## Non-exported, non-documented, package-wide utility functions
##---------------------------------------------------------------------------##

#' @importFrom S4Vectors coolcat

.coolcat <- function(...) coolcat(..., indent = 2)

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

##----------------------------------------------------------------------------##

