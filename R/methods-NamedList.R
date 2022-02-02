##----------------------------------------------------------------------------##
## methods-NamedList
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
## validity

#' @importFrom S4Vectors classNameForDisplay

.NamedList.validity <- function(object) {
  msg <- NULL
  cname <- classNameForDisplay(object)
  if (length(object) > 0 && is.null(names(object))) {
    msg <- c(msg, sprintf("%s must have names.", cname))
  }
  if (any(duplicated(names(object)))) {
    msg <- c(msg, sprintf("%s names must be unique.", cname))
  }
  if (any(is.na(names(object)))) {
    msg <- c(msg, sprintf("%s names must not be <NA>.", cname))
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("NamedList", .NamedList.validity)

##----------------------------------------------------------------------------##
