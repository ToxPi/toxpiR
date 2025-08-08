##----------------------------------------------------------------------------##
## All classes
##----------------------------------------------------------------------------##

#' @import methods
#' @importFrom S4Vectors setValidity2 new2
#' @importClassesFrom S4Vectors character_OR_NULL

NULL

##----------------------------------------------------------------------------##
## Virtual classes

#' @importClassesFrom S4Vectors SimpleList
#' @importClassesFrom S4Vectors List
#' @importFrom S4Vectors List

setClass("NamedList", contains = c("VIRTUAL", "SimpleList"))

##----------------------------------------------------------------------------##
## TxpTransFunc

#' @rdname TxpTransFunc-class
#' @exportClass TxpTransFunc

setClass("TxpTransFunc", contains = "function", prototype = function(x) x)

setClassUnion("TxpTransFunc_OR_NULL", members = c("TxpTransFunc", "NULL"))

##----------------------------------------------------------------------------##
## TxpTransFuncList

#' @rdname TxpTransFuncList-class
#' @exportClass TxpTransFuncList

setClass("TxpTransFuncList",
         contains = "SimpleList",
         prototype = prototype(elementType = "TxpTransFunc_OR_NULL"))

##----------------------------------------------------------------------------##
## TxpSlice

#' @rdname TxpSlice-class
#' @exportClass TxpSlice

setClass("TxpSlice",
         slots = c(txpValueNames = "character_OR_NULL",
                   txpTransFuncs = "TxpTransFuncList",
                   txpValueNamesLower = "character_OR_NULL",
                   txpTransFuncsLower = "TxpTransFuncList",
                   txpValueNamesUpper = "character_OR_NULL",
                   txpTransFuncsUpper = "TxpTransFuncList"))

setClassUnion("TxpSlice_OR_NULL", members = c("TxpSlice", "NULL"))

##----------------------------------------------------------------------------##
## TxpSliceList

#' @rdname TxpSliceList-class
#' @importClassesFrom S4Vectors SimpleList
#' @exportClass TxpSliceList

setClass("TxpSliceList",
         contains = "NamedList",
         prototype = prototype(elementType = "TxpSlice"))

##----------------------------------------------------------------------------##
## TxpModel

#' @rdname TxpModel-class
#' @exportClass TxpModel

setClass("TxpModel",
         slots = c(txpSlices = "TxpSliceList",
                   txpWeights = "numeric",
                   txpTransFuncs = "TxpTransFuncList",
                   negativeHandling = "character",
                   rankTies = "character"))

setClassUnion("TxpModel_OR_NULL", members = c("TxpModel", "NULL"))

##----------------------------------------------------------------------------##
## TxpModelList

#' @rdname TxpModelList-class
#' @importClassesFrom S4Vectors SimpleList
#' @exportClass TxpModelList

setClass("TxpModelList",
         contains = "SimpleList",
         prototype = prototype(elementType = "TxpModel"))

##----------------------------------------------------------------------------##
## TxpResultParam

#' @name TxpResultParam-class
#' @exportClass TxpResultParam

setClass("TxpResultParam",
         slots = c(rank.ties.method = "character",
                   negative.value.handling = "character"))

##----------------------------------------------------------------------------##
## TxpResult

#' @name TxpResult-class
#' @exportClass TxpResult

setClassUnion("matrix_OR_NULL", members = c("matrix", "NULL"))
setClassUnion("numeric_OR_NULL", members = c("numeric", "NULL"))
setClass("TxpResult",
         slots = c(txpScores = "numeric_OR_NULL",
                   txpScoresLower = "numeric_OR_NULL",
                   txpScoresUpper = "numeric_OR_NULL",
                   txpSliceScores = "matrix_OR_NULL",
                   txpSliceScoresLower = "matrix_OR_NULL",
                   txpSliceScoresUpper = "matrix_OR_NULL",
                   txpRanks = "numeric_OR_NULL",
                   txpRanksLower = "numeric_OR_NULL",
                   txpRanksUpper = "numeric_OR_NULL",
                   txpMissing = "numeric",
                   txpModel = "TxpModel",
                   txpIDs = "character_OR_NULL",
                   txpResultParam = "TxpResultParam"))

setClassUnion("TxpResult_OR_NULL", members = c("TxpResult", "NULL"))

##----------------------------------------------------------------------------##
## TxpResultList

#' @rdname TxpResultList-class
#' @importClassesFrom S4Vectors SimpleList
#' @exportClass TxpResultList

setClass("TxpResultList",
         contains = "SimpleList",
         prototype = prototype(elementType = "TxpResult"))







