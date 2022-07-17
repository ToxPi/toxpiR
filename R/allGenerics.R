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
setGeneric("txpSlices", function(x, ...) standardGeneric("txpSlices"))

#' @rdname txpGenerics
setGeneric("txpSlices<-", 
           function(x, ..., value) standardGeneric("txpSlices<-"))

#' @rdname txpGenerics
setGeneric("txpWeights", function(x, ...) standardGeneric("txpWeights"))

#' @rdname txpGenerics
setGeneric("txpWeights<-", 
           function(x, ..., value) standardGeneric("txpWeights<-"))

#' @rdname txpCalculateScores
setGeneric("txpCalculateScores", 
           function(model, input, ...) standardGeneric("txpCalculateScores"))

#' @rdname txpGenerics
setGeneric("txpScores", function(x, ...) standardGeneric("txpScores"))

#' @rdname txpGenerics
setGeneric("txpSliceScores", function(x, ...) standardGeneric("txpSliceScores"))

#' @rdname txpGenerics
setGeneric("txpModel", function(x, ...) standardGeneric("txpModel"))

#' @rdname txpGenerics
setGeneric("txpIDs", function(x, ...) standardGeneric("txpIDs"))

#' @rdname txpGenerics
setGeneric("txpIDs<-", function(x, ..., value) standardGeneric("txpIDs<-"))

#' @rdname txpGenerics
setGeneric("txpRanks", function(x, ...) standardGeneric("txpRanks"))

#' @rdname txpGenerics
setGeneric("txpResultParam", function(x, ...) standardGeneric("txpResultParam"))
