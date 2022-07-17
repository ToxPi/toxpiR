##----------------------------------------------------------------------------##
## methods-TxpResultParam
##----------------------------------------------------------------------------##

#' @name TxpResultParam-class
#' @aliases TxpResultParam
#' @title ToxPi Result Parameters
#' @description S4 class to store ToxPi result calculation parameters
#' 
#' @slot rank.ties.method Character scalar, method used to calculate score 
#' ranks passed to [base::rank]
#' @slot negative.value.handling Character scalar, how negative values are 
#' handled, see details
#' 
#' @param rank.ties.method Passed to `rank.ties.method` slot
#' @param negative.value.handling Passed to `negative.value.handling` slot
#' 
#' @seealso [txpCalculateScores], [TxpResult]
#' 

NULL

##----------------------------------------------------------------------------##
## constructor -- NOT exported

TxpResultParam <- function(rank.ties.method, negative.value.handling) {
  new2("TxpResultParam", 
       rank.ties.method = rank.ties.method, 
       negative.value.handling = negative.value.handling)
}

##----------------------------------------------------------------------------##
## validity

#' @importFrom rlang is_scalar_character

.TxpResultParam.validity <- function(object) {
  msg <- NULL
  rankMthd <- slot(object, "rank.ties.method")
  if (!is_scalar_character(rankMthd)) {
    msg <- c(msg, "rank.ties.method must be scalar character")
  }
  validRnkMthd <- c("average", "first", "last", "random", "max", "min")
  if (is_scalar_character(rankMthd) && !rankMthd %in% validRnkMthd)  {
    msg <- c(msg, "Invalid rank.ties.method; see ?base::rank")
  }
  negHndl <- slot(object, "negative.value.handling")
  if (!is_scalar_character(negHndl)) {
    msg <- c(msg, "negative.value.handling must be scalar character")
  }
  validNegHndl <- c("keep", "missing")
  if (is_scalar_character(negHndl) && !negHndl %in% validNegHndl)  {
    msg <- c(msg, "Invalid negative.value.handling; see ?TxpResultParam")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpResultParam", .TxpResultParam.validity)

##----------------------------------------------------------------------------##
## show

.TxpResultParam.show <- function(object) {
  cat("TxpResultParam:\n")
  sapply(names(getSlots("TxpResultParam")), .catslot, object = object)
}

setMethod("show", "TxpResultParam", .TxpResultParam.show)


##----------------------------------------------------------------------------##



