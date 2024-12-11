##----------------------------------------------------------------------------##
## txpModelBuilder
##----------------------------------------------------------------------------##

#' @name txpModelBuilder
#' @title Shiny App for building ToxPi models
#' @description Opens a Shiny App for building and exporting a ToxPi model built
#' from a specific dataset
#' 
#' @param dataFile A data frame or a file path to a `.csv` file. If the input is
#' a file path, the file must be a csv containing data structured as a typical 
#' CSV, where the first row is expected to be a header
#' 
#' @examples
#' library(toxpiR)
#' txpModelBuilder('data/size_small.csv')
#' 
#' @details 
#' This function takes a csv or r dataframe as input and opens a shiny 
#' application for building and exporting a ToxPi model.
#' 
#' The input is expected to contain samples as the rows, and corresponding
#' features/metrics as columns. For a csv, the first row must contain the column
#' names. For an r dataframe, `colnames` must be set to the corresponding names
#' 
#' @return No return. Models can be exported from the application as csv or rds 
#' files
#' 
#' @importFrom shiny shinyApp
#' @export

txpModelBuilder <- function(dataFile) {
  
  # Define the UI
  ui <- .buildModelUI()
  
  # Define the server logic
  server <- function(input, output, session) {
    data <- .readRawFile(dataFile)  
    
    .buildModelServer(input, output, session, data)
  }
  
  # Launch the app
  shinyApp(ui = ui, server = server)
}