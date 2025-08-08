#' @importFrom BiocGenerics updateObject
#' @importFrom BiocGenerics sort
#' @importFrom BiocGenerics duplicated
#' @importFrom BiocGenerics as.data.frame

#' @name txpGenerics
#' @title toxpiR package generics
#' @description toxpiR package generics; see class man pages for associated
#' methods
#' @param x toxpiR S4 object
#' @param value Replacement value
#' @param ... Included for extendability; not currently used
#'
#' @return See specific methods for details.

NULL

#' @rdname txpGenerics
setGeneric("txpValueNames", function(x, ...) standardGeneric("txpValueNames"))

#' @rdname txpGenerics
setGeneric("txpValueNames<-",
           function(x, ..., value) standardGeneric("txpValueNames<-"))

#' @rdname txpGenerics
setGeneric("txpTransFuncs", function(x, ...) standardGeneric("txpTransFuncs"))

#' @rdname txpGenerics
setGeneric("txpTransFuncs<-",
           function(x, ..., value) standardGeneric("txpTransFuncs<-"))

#' @rdname txpGenerics
setGeneric("txpValueNamesLower", function(x, ...) standardGeneric("txpValueNamesLower"))

#' @rdname txpGenerics
setGeneric("txpValueNamesLower<-",
           function(x, ..., value) standardGeneric("txpValueNamesLower<-"))

#' @rdname txpGenerics
setGeneric("txpTransFuncsLower", function(x, ...) standardGeneric("txpTransFuncsLower"))

#' @rdname txpGenerics
setGeneric("txpTransFuncsLower<-",
           function(x, ..., value) standardGeneric("txpTransFuncsLower<-"))

#' @rdname txpGenerics
setGeneric("txpValueNamesUpper", function(x, ...) standardGeneric("txpValueNamesUpper"))

#' @rdname txpGenerics
setGeneric("txpValueNamesUpper<-",
           function(x, ..., value) standardGeneric("txpValueNamesUpper<-"))

#' @rdname txpGenerics
setGeneric("txpTransFuncsUpper", function(x, ...) standardGeneric("txpTransFuncsUpper"))

#' @rdname txpGenerics
setGeneric("txpTransFuncsUpper<-",
           function(x, ..., value) standardGeneric("txpTransFuncsUpper<-"))

#' @rdname txpGenerics
setGeneric("txpSlices", function(x, ...) standardGeneric("txpSlices"))

#' @rdname txpGenerics
setGeneric("txpSlices<-",
           function(x, ..., value) standardGeneric("txpSlices<-"))

#' @rdname txpGenerics
setGeneric("txpWeights", function(x, ...) standardGeneric("txpWeights"))

#' @rdname txpGenerics
setGeneric("txpWeights<-",
           function(x, ..., value) standardGeneric("txpWeights<-"))

#' @rdname txpGenerics
setGeneric("negativeHandling", function(x, ...) standardGeneric("negativeHandling"))

#' @rdname txpGenerics
setGeneric("negativeHandling<-",
           function(x, ..., value) standardGeneric("negativeHandling<-"))

#' @rdname txpGenerics
setGeneric("rankTies", function(x, ...) standardGeneric("rankTies"))

#' @rdname txpGenerics
setGeneric("rankTies<-",
           function(x, ..., value) standardGeneric("rankTies<-"))

#' @rdname txpCalculateScores
setGeneric("txpCalculateScores",
           function(model,
                    input,
                    id.var = NULL,
                    rank.ties.method = NULL,
                    negative.value.handling = NULL
           ) {
             standardGeneric("txpCalculateScores")
           }
)

#' @rdname txpGenerics
setGeneric("txpScores", function(x, ...) standardGeneric("txpScores"))

#' @rdname txpGenerics
setGeneric("txpScoresLower", function(x, ...) standardGeneric("txpScoresLower"))

#' @rdname txpGenerics
setGeneric("txpScoresUpper", function(x, ...) standardGeneric("txpScoresUpper"))

#' @rdname txpGenerics
setGeneric("txpSliceScores", function(x, ...) standardGeneric("txpSliceScores"))

#' @rdname txpGenerics
setGeneric("txpSliceScoresLower", function(x, ...) standardGeneric("txpSliceScoresLower"))

#' @rdname txpGenerics
setGeneric("txpSliceScoresUpper", function(x, ...) standardGeneric("txpSliceScoresUpper"))

#' @rdname txpGenerics
setGeneric("txpModel", function(x, ...) standardGeneric("txpModel"))

#' @rdname txpGenerics
setGeneric("txpIDs", function(x, ...) standardGeneric("txpIDs"))

#' @rdname txpGenerics
setGeneric("txpIDs<-", function(x, ..., value) standardGeneric("txpIDs<-"))

#' @rdname txpGenerics
setGeneric("txpRanks", function(x, ...) standardGeneric("txpRanks"))

#' @rdname txpGenerics
setGeneric("txpRanksLower", function(x, ...) standardGeneric("txpRanksLower"))

#' @rdname txpGenerics
setGeneric("txpRanksUpper", function(x, ...) standardGeneric("txpRanksUpper"))

#' @rdname txpGenerics
setGeneric("txpMissing", function(x, ...) standardGeneric("txpMissing"))

#' @rdname txpGenerics
setGeneric("txpResultParam", function(x, ...) standardGeneric("txpResultParam"))
