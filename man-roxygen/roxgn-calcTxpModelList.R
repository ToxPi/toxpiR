#' @examples 
#' ## Calculate scores for list of models; returns TxpResultList object
#' txpCalculateScores(model = TxpModelList(m1 = txp_example_model, 
#'                                         m2 = txp_example_model), 
#'                    input = txp_example_input, 
#'                    id.var = "name")
#' resLst <- txpCalculateScores(model = list(m1 = txp_example_model, 
#'                                           m2 = txp_example_model), 
#'                              input = txp_example_input, 
#'                              id.var = "name")
#' 

