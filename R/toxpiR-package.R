#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

TXP_FILLS = c("dodgerblue",
              "bisque",
              "darkolivegreen3",
              "darkorchid3",
              "mistyrose2",
              "darkgoldenrod1")


#' @name toxpiR-datasets
#' @title toxpiR data objects
#' @description Objects included in the toxpiR package, loaded with 
#' [utils::data]
#' @aliases txp_example_input txp_example_model
#' 
#' @usage data(txp_example_input, package = "toxpiR")
#' @usage data(txp_example_model, package = "toxpiR")
#' 
#' @section txp_example_input:
#' 
#' Small example input data to be used with [txpCalculateScores] in creating
#' [TxpResult] objects. A [base::data.frame] with 10 rows and 9 variables
#' \describe{
#'   \item{name}{Observation names}
#'   \item{metric#}{Input data for ToxPi models}
#' }
#' 
#' @source <https://github.com/ToxPi/ToxPi-example-files>
#' 
#' @section txp_example_model:
#' 
#' Example [TxpModel] object intended for `txp_example_data`; model with 4
#' slices.
#' 
#' @examples
#' data(txp_example_input, package = "toxpiR")
#' data(txp_example_model, package = "toxpiR")
#' txp_example_input
#' txp_example_model
#' 
#' ## Code to create txp_example_model
#' tf1 <- TxpTransFuncList(linear = function(x) x)
#' sl <- TxpSliceList(s1 = TxpSlice(sprintf("metric%d", 1:2)),
#'                    s2 = TxpSlice("metric3"),
#'                    s3 = TxpSlice(sprintf("metric%d", 4:7), 
#'                                  tf1[rep("linear", 4)]),
#'                    s4 = TxpSlice("metric8", tf1))
#' tf2 <- TxpTransFuncList(NULL, linear = function(x) x, NULL, NULL)
#' TxpModel(txpSlices = sl, txpWeights = c(2, 1, 3, 2), txpTransFuncs = tf2)
#' 
#' @importFrom utils data

NULL