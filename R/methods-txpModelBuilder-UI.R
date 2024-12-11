##----------------------------------------------------------------------------##
## txpModelBuilder-UI
##----------------------------------------------------------------------------##

#' @noRd
#' @title txpModelBuilder UI
#' @description Defines the UI logic for the `txpModelBuilder` Shiny app. 
#' 
#' @seealso [.buildModelServer] for corresponding server functions
#' @seealso [.buildSliceServer] and [.buildSliceUI] for slice modules
#' @seealso `txpModelBuilder` for server-ui integration
#' 
#' @import shiny
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyBS bsAlert

.buildModelUI <- function() {
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$script(
                  "shinyjs.getLegendWidth = function() {
                 var legendWidth = document.getElementById('profileLegend').offsetWidth;
                 Shiny.setInputValue('legendWidth', legendWidth);
               };
               $(document).on('shiny:connected', function() {
                 shinyjs.getLegendWidth();
               });
               $(window).resize(function() {
                 shinyjs.getLegendWidth();
               });"
      )
    ),
    sidebarLayout(
      sidebarPanel(
        width = 2, style = "height:calc(100vh - 70px - 53px); overflow-y:auto;margin:0px;padding:0px",
        tags$script(
          HTML('
            $(document).ready(function() {
              var selectInput1 = $("#sliceSelect");
              var selectInput2 = $("#sliceNames");
      
              selectInput1.on("scroll", function() {
                selectInput2.scrollTop(selectInput1.scrollTop());
              });
      
              selectInput2.on("scroll", function() {
                selectInput1.scrollTop(selectInput2.scrollTop());
              });
            });
          ')
        ),
        div(
          class = "center-button",
          style = "padding-top: 0px;",
          actionButton("addSlice", "Add Slice"),
        ),
        div(class = "center-button",
             selectInput(
               "sliceSelect",
               label = "Slice ID:",
               choices = "Slice1",
               selected = "Slice1",
               size = 8,
               selectize = FALSE
           )
        ),
        div(class = "center-button",
            selectInput(
              "sliceNames",
              label = "Slice Name:",
              choices = "Slice1",
              selected = "Slice1",
              size = 8,
              selectize = FALSE
            )
        ),
        div(class = "center-button",
            selectInput(
              "exportData",
              label = "Export Data Model:",
              choices = list("WebApp CSV File", "GUI CSV File", "S4 Object")
            )
        ),
        div(class = "center-button",
            downloadButton("exportDataButton", "Export Model")
        ),
        bsAlert("modelAlert"),
        div(class = "center-button",
            fileInput(
              "modelSelect",
              label = "Import toxpiR Model:",
              accept = c(
                "text/rdata",
                ".rdata"
              ),
              placeholder = "Select A toxpiR Model Object"
            )
        ),
        div(class = "center-button",
          selectInput(
            "rankTiesMethod",
            label = "Tie Handling",
            choices = c("average", "first", "last", "random", "max", "min")
          )
        ),
        div(class = "center-button",
          selectInput(
            "negativeValueHandling",
            label = "Negative Handling",
            choices = c("keep", "missing")
          )
        )
      ),
      mainPanel(
        width = 10,
        fluidRow(
          column(10, style = "padding:0px;color:red",
                 uiOutput("sliceInfo")
          ),
          column(2, style = "margin-bottom:0px;text-align:center;",
                 uiOutput("profilePreview", style = "color:red;"),
                 uiOutput("profileLegend",style = "overflow:auto;height:calc(100vh - 70px - 53px - 0.125 * 100vw);white-space: nowrap;")
          ),
        ),
        div(class = "main-footer",
            div(style = "margin-top: 10px;position:fixed; left:10px",
                actionButton("buildModelHelp", "Help")
            )
        )
      )
    )
  )
}