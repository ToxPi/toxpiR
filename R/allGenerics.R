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
setGeneric("txpLowerNames", function(x, ...) standardGeneric("txpLowerNames"))

#' @rdname txpGenerics
setGeneric("txpLowerNames<-",
           function(x, ..., value) standardGeneric("txpLowerNames<-"))

#' @rdname txpGenerics
setGeneric("txpLowerFuncs", function(x, ...) standardGeneric("txpLowerFuncs"))

#' @rdname txpGenerics
setGeneric("txpLowerFuncs<-",
           function(x, ..., value) standardGeneric("txpLowerFuncs<-"))

#' @rdname txpGenerics
setGeneric("txpUpperNames", function(x, ...) standardGeneric("txpUpperNames"))

#' @rdname txpGenerics
setGeneric("txpUpperNames<-",
           function(x, ..., value) standardGeneric("txpUpperNames<-"))

#' @rdname txpGenerics
setGeneric("txpUpperFuncs", function(x, ...) standardGeneric("txpUpperFuncs"))

#' @rdname txpGenerics
setGeneric("txpUpperFuncs<-",
           function(x, ..., value) standardGeneric("txpUpperFuncs<-"))

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
setGeneric("txpScoreLows", function(x, ...) standardGeneric("txpScoreLows"))

#' @rdname txpGenerics
setGeneric("txpScoreUps", function(x, ...) standardGeneric("txpScoreUps"))

#' @rdname txpGenerics
setGeneric("txpSliceScores", function(x, ...) standardGeneric("txpSliceScores"))

#' @rdname txpGenerics
setGeneric("txpSliceLows", function(x, ...) standardGeneric("txpSliceLows"))

#' @rdname txpGenerics
setGeneric("txpSliceUps", function(x, ...) standardGeneric("txpSliceUps"))

#' @rdname txpGenerics
setGeneric("txpModel", function(x, ...) standardGeneric("txpModel"))

#' @rdname txpGenerics
setGeneric("txpIDs", function(x, ...) standardGeneric("txpIDs"))

#' @rdname txpGenerics
setGeneric("txpIDs<-", function(x, ..., value) standardGeneric("txpIDs<-"))

#' @rdname txpGenerics
setGeneric("txpRanks", function(x, ...) standardGeneric("txpRanks"))

#' @rdname txpGenerics
setGeneric("txpRankLows", function(x, ...) standardGeneric("txpRankLows"))

#' @rdname txpGenerics
setGeneric("txpRankUps", function(x, ...) standardGeneric("txpRankUps"))

#' @rdname txpGenerics
setGeneric("txpMissing", function(x, ...) standardGeneric("txpMissing"))

#' @rdname txpGenerics
setGeneric("txpResultParam", function(x, ...) standardGeneric("txpResultParam"))
