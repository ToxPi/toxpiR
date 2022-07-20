##----------------------------------------------------------------------------##
## methods-txpModelList
##----------------------------------------------------------------------------##

#' @name TxpModelList-class
#' @title List of TxpModel objects
#' @description Extension of [S4Vectors::SimpleList] that holds only [TxpModel] 
#' objects.
#' 
#' @param ... [TxpModel] object to create `TxpModelList` object
#' @param x `TxpModelList` object
#' 
#' @examples
#' ## Create some TxpModel objects; see ?TxpModel for more details
#' s1 <- list(S1 = TxpSlice("inpt1"), S2 = TxpSlice("inpt2"))
#' tf <- list(NULL, sqrt = function(x) sqrt(x))
#' m1 <- TxpModel(txpSlices = s1, txpWeights = 2:1, txpTransFuncs = tf)
#' m2 <- m1
#' txpSlices(m2) <- list(S3 = TxpSlice("inpt3"), S4 = TxpSlice("inpt4"))
#' m3 <- merge(m1, m2)
#' 
#' ## Build a TxpModelList object
#' TxpModelList(m1 = m1, m2 = m2, m3 = m3)
#' 
#' ## Note: names are printed as '' when all are NULL
#' TxpModelList(m1, m2, m3)
#' names(TxpModelList(m1, m2, m3))
#' 
#' ## Test for duplicates
#' duplicated(TxpModelList(m1 = m1, m2 = m2, m3 = m3))
#' duplicated(TxpModelList(m1 = m1, m2 = m1, m3 = m3))
#' 
#' ## Coerce lists/TxpModel objects to TxpModelList
#' as(list(m1 = m1, m2 = m2, m3 = m3), "TxpModelList")
#' as.TxpModelList(list(m1 = m1, m2 = m2, m3 = m3))
#' 
#' as(m1, "TxpModelList")
#' as.TxpModelList(m1)

NULL

##----------------------------------------------------------------------------##
## constructor

#' @rdname TxpModelList-class
#' @export 

TxpModelList <- function(...) {
  listData <- list(...)
  new2("TxpModelList", listData)
}

##----------------------------------------------------------------------------##
## validity

.TxpModelList.validity <- function(object) {
  msg <- NULL
  valid <- vapply(object@listData, is, logical(1), "TxpModel")
  if (any(!valid)) {
    msg <- c(msg, "All TxpModel objects must be of class 'TxpModel.'")
  }
  if (is.null(msg)) return(TRUE)
  msg
}

setValidity2("TxpModelList", .TxpModelList.validity)

##----------------------------------------------------------------------------##
## txpCalculateScores

.TxpModelList.calc <- function(model, input, 
                               id.var = NULL,
                               rank.ties.method = c("average", "first", "last", 
                                                    "random", "max", "min"),
                               negative.value.handling = c("keep", "missing")) {
  if (is.list(model)) {
    model <- try(as.TxpModelList(model), silent = TRUE)
    if (is(model, "try-error")) {
      stop("Given list could not be coerced to TxpModelList")
    }
  }
  resLst <- lapply(model, .calculateScores,
                   input = input, 
                   id.var = id.var, 
                   rank.ties.method = rank.ties.method,
                   negative.value.handling = negative.value.handling)
  as.TxpResultList(resLst)
}

#' @rdname txpCalculateScores
#' @export

setMethod("txpCalculateScores", 
          c("TxpModelList", "data.frame"), 
          .TxpModelList.calc)

#' @rdname txpCalculateScores
#' @export

setMethod("txpCalculateScores", c("list", "data.frame"), .TxpModelList.calc)

##----------------------------------------------------------------------------##
## show

.TxpModelList.show <- function(object) {
  lnms <- .listDisplayNames(object)
  .coolcat("  TxpModelList of length %d: %s\n", lnms)
}

setMethod("show", "TxpModelList", .TxpModelList.show)

##----------------------------------------------------------------------------##
## duplicated

#' @describeIn TxpModelList-class Returns logical vector of `length(x)`, where 
#' `TRUE` indicates a duplicate model in the list; see [base::duplicated]
#' @export

setMethod("duplicated", "TxpModelList", function(x) .dupList(x))

##----------------------------------------------------------------------------##
## coercion 

.TxpModelList.from.list <- function(from) {
  do.call("TxpModelList", from)
}

setAs("list", "TxpModelList", .TxpModelList.from.list)

.TxpModelList.from.TxpModel <- function(from) {
  TxpModelList(from)
}

setAs("TxpModel", "TxpModelList", .TxpModelList.from.TxpModel)

#' @describeIn TxpModelList-class Coerce list or [TxpModel] objects to 
#' TxpModelList
#' @export

as.TxpModelList <- function(x) as(x, "TxpModelList")

##----------------------------------------------------------------------------##

