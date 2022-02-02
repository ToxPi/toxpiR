##----------------------------------------------------------------------------##
## methods-txpResultList
##----------------------------------------------------------------------------##

#' @name TxpResultList-class
#' @title List of TxpResult objects
#' @description Extension of [S4Vectors::SimpleList] that holds only [TxpResult] 
#' objects.
#' 
#' @param ... [TxpResult] object to create `TxpResultList` object
#' @param x `TxpResultList` object
#' 
#' @template roxgn-loadExamples
#' @template roxgn-calcTxpModelList
#' 
#' @seealso [TxpResult], [txpCalculateScores]
#' 
#' @examples
#' ## duplicated
#' duplicated(resLst)
#' 
#' ## Coercion
#' as(list(resLst[[1]], resLst[[2]]), "TxpResultList")
#' as.TxpResultList(list(res1 = resLst[[1]], res2 = resLst[[2]]))
#' 
#' as(resLst[[1]], "TxpResultList")
#' as.TxpResultList(resLst[[1]])

NULL

##----------------------------------------------------------------------------##
## constructor

#' @rdname TxpResultList-class
#' @export 

TxpResultList <- function(...) {
  listData <- list(...)
  new2("TxpResultList", listData)
}

##----------------------------------------------------------------------------##
## validity

.TxpResultList.validity <- function(object) {
  msg <- NULL
  valid <- vapply(object@listData, is, logical(1), "TxpResult")
  if (any(!valid)) {
    msg <- c(msg, "All TxpResult objects must be of class 'TxpResult.'")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpResultList", .TxpResultList.validity)

##----------------------------------------------------------------------------##
## show

.TxpResultList.show <- function(object) {
  lnms <- .listDisplayNames(object)
  .coolcat("  TxpResultList of length %d: %s\n", lnms)
}

setMethod("show", "TxpResultList", .TxpResultList.show)

##----------------------------------------------------------------------------##
## duplicated

#' @rdname TxpResultList-class
#' @export

setMethod("duplicated", "TxpResultList", function(x) .dupList(x))

##----------------------------------------------------------------------------##
## coercion 

.TxpResultList.from.list <- function(from) {
  do.call("TxpResultList", from)
}

setAs("list", "TxpResultList", .TxpResultList.from.list)

.TxpResultList.from.TxpResult <- function(from) {
  TxpResultList(from)
}

setAs("TxpResult", "TxpResultList", .TxpResultList.from.TxpResult)

#' @rdname TxpResultList-class
#' @export

as.TxpResultList <- function(x) as(x, "TxpResultList")

##----------------------------------------------------------------------------##


