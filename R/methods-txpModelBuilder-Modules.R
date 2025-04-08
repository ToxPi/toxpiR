##----------------------------------------------------------------------------##
## txpModelBuilder-Modules
##----------------------------------------------------------------------------##

#' @noRd
#' @title Slice Information Panel UI
#' @description Defines and renders the UI for the slice information panel tab 
#' based on the current user inputs for the selected slice. Note: Parameters are
#' used to allow switching between modules by selecting a slice from a list 
#' developed outside the module code
#' 
#' @param id A string. The unique identifier of the selected slice.
#' @param name A string. The user-specified name for the slice.
#' @param sliceTF A string. An unsubmitted slice transformation in R syntax, 
#' representing a function of `x`.
#' @param sliceWeight A string. The numeric weight assigned to the 
#' slice, read in as a string.
#' @param color A string. A hexadecimal color code representing the slice color 
#' (e.g., "#FFFFFF").
#' @param selectedMetrics A character vector. The metrics currently included in 
#' the slice.
#' @param shownMetricTransform A string. The unsubmitted transformation for the 
#' currently selected metric.
#' @param currentMetricTransform A string. The most recently submitted 
#' transformation for the currently selected metric.
#' @param currentSelectedMetric A string. The metric currently selected within 
#' the slice.
#' @param currentSliceTransform A string. The most recently submitted 
#' transformation for the currently selected slice.
#' @param currentTab A string. The name of the tab currently selected by the 
#' user.
#' @return No return value. This function is called internally by the Shiny server.
#' 
#' @import shiny 
#' @importFrom sortable bucket_list add_rank_list sortable_options
#' @importFrom colourpicker colourInput
#' 
#' @seealso [.buildSliceServer] for corresponding server functions
#' @seealso [.buildModelServer] and [.buildModelUI] for integration with model 
#' building and slice selection
#' @examples
#' library(shiny)
#' library(sortable)
#' library(colourpicker)
#' library(ggplot2)
#' # Example usage in a Shiny app
#' shinyApp(
#'   ui = fluidPage(
#'     .buildSliceUI(id = "slice1", name = "Example Slice")
#'   ),
#'   server = function(input, output, session) {
#'     slices <- .buildSliceServer(
#'       id = "slice1",
#'       metrics = colnames(mtcars),
#'       data = mtcars,
#'       negativeHandling = reactive("keep")
#'     )
#'   }
#' )
.buildSliceUI <- function(id, name, sliceTF = "x", sliceWeight = "1",
                         color = "#FFFFFF", selectedMetrics = NULL, shownMetricTransform = "x",
                         currentMetricTransform = "x", currentSelectedMetric = "", currentSliceTransform = 'x', currentTab = "Slice Info") {
  ns <- NS(id)
  tags$div( 
    id = ns("sliceID"),
    tabsetPanel(selected = currentTab, id = "selectedTab",
                tabPanel(
                  "Slice Info",
                  div(style = "padding-right:2.5px; padding-left:0px",
                      wellPanel(style = "height:calc(100vh - 70px - 53px - 43px);margin:0px;",
                                fluidRow(
                                  column(
                                    4,
                                    textInput(ns("sliceName"), label = "Slice Name", value = name),
                                    colourInput(ns("sliceColor"), 
                                                             label = "Slice Color", 
                                                             value = color, 
                                                             allowTransparent = TRUE),
                                    textInput(ns("sliceWeight"), label = "Slice Weight", value = sliceWeight),
                                    div(
                                      class = "center-button",
                                      actionButton("moveUpID", label = "Move Up"),
                                      actionButton("moveDownID", label = "Move Down"),
                                    ),
                                    div(
                                      class = "center-button",
                                      style = "padding-top: 15px;",
                                      actionButton("removeButton", label = "Remove")
                                    )
                                  ),
                                  column(8,
                                                textInput(ns("sliceMetricFilter"), label = "Filter Metrics"),
                                                div(
                                                  class = "metric-bucket-list",
                                                  style = "overflow-y:auto; height:calc(100vh - 70px - 53px - 41px - 74px - 22px);",
                                                  uiOutput(ns("bucketPlaceholder"))
                                                )
                                  )
                                ))
                  )
                ),
                
                tabPanel(
                  "Data Transforms",
                  fluidRow(
                    column(style = "padding-right:2.5px",
                                  6,
                                  wellPanel(style = "height:calc(100vh - 70px - 53px - 43px);margin:0px;",
                                            column(12,
                                                          fluidRow(
                                                            textInput(ns("sliceTransformation"), label = "Slice Function", value = sliceTF)
                                                          )
                                            ),
                                            column(12,
                                                          fluidRow(
                                                            actionButton(ns("submitTransformation"), label = "Assign Function"),
                                                            p(paste("Current Transform: ", currentSliceTransform))
                                                          )
                                            ),
                                            fluidRow(
                                              column(
                                                6,
                                                renderText("Distribution before Transformation"),
                                                plotOutput(ns("histogram"), height = "150px"),
                                                style = "margin-bottom: 20px;"  # Adjust the margin-bottom as needed
                                              ),
                                              column(
                                                6,
                                                renderText("Distribution after Transformation"),
                                                plotOutput(ns("histogramTransformed"), height = "150px"),
                                                style = "margin-bottom: 20px;"  # Adjust the margin-bottom as needed
                                              )
                                            )
                                  )
                    ),
                    column(6, style = "padding-left:2.5px;",
                                  wellPanel(style = "height:calc(100vh - 70px - 53px - 43px);margin-bottom:0px;",
                                            selectInput(
                                              ns("metricTransformSelect"),
                                              label = "Select Metric",
                                              choices = selectedMetrics,
                                              selected = currentSelectedMetric
                                            ),
                                            
                                            textInput(
                                              ns("metricTransformation"),
                                              label = "Metric Function",
                                              value = shownMetricTransform
                                            ),
                                            column(
                                              12,
                                              fluidRow(
                                                actionButton(ns("submitMetricTransformation"), label = "Assign Function"),
                                                actionButton(ns("submitMetricTransformationAll"), label = "Assign To All")
                                              )
                                            ),
                                            fluidRow(
                                              column(
                                                12,
                                                p(paste("Current Transform: ", currentMetricTransform))
                                              )
                                            ),
                                            fluidRow(
                                              column(6,
                                                            renderText("Distribution before Transformation"),
                                                            plotOutput(ns("metricHistogram"), height = "150px"),
                                                            style = "margin-bottom: 20px;"  # Adjust the margin-bottom as needed
                                              ),
                                              column(6,
                                                            renderText("Distribution after Transformation"),
                                                            plotOutput(ns("metricHistogramTransformed"), height = "150px"),
                                                            style = "margin-bottom: 20px;"  # Adjust the margin-bottom as needed
                                              )
                                            )
                                            
                                  )
                    )
                  )
                )
    )
  )
}

#' @noRd
#' @title Server Logic for Slice Builder Module
#' @description
#' Defines and manages the server-side logic for the slice builder module. This 
#' function processes user inputs, updates reactive slice parameters, and 
#' integrates them into the server environment.
#' @keywords internal
#' 
#' @param id A string. The unique identifier for the selected slice.
#' @param metrics A character vector. The available metrics from which users can
#' select for their slice.
#' @param data A data frame. The dataset to which slice operations and 
#' transformations are applied.
#' @param negativeHandling A reactive string. Specifies how negative values in 
#' the dataset should be handled. Options are keep and missing.
#' @param preSlice A txpSlice object or `NULL`. Optional. An initial 
#' configuration for the slice including predefined slice parameters.
#' @param initialSliceTransform A string. The default slice transformation 
#' formula (in R syntax) applied to the slice. Default is `"x"`.
#' @param initialName A string or `NULL`. The initial name of the slice. 
#' Default is `NULL`, in which case the name will be the id.
#' @param initialColor A string. The default color for the slice, represented 
#' as a hexadecimal color code without #(e.g., `"f3622d"`). Default is 
#' `"f3622d"`.
#' @param initialWeight A numeric value. The initial weight assigned to the 
#' slice. Default is `1`.
#' @param initialMetricTransform A string. The default transformation formula 
#' for metrics within the slice. Default is `"x"`.
#' 
#' @return A reactive list. The function returns a list of reactive objects 
#' containing the updated slice parameters, including a txpSlice object, name, 
#' weight, slice transformation, color, and metric transformations. These 
#' objects are later used to build the model.
#' @import shiny
#' @importFrom ggplot2 ggplot
#' @seealso [.buildSliceUI] for the corresponding UI function.
#' 
#' @examples
#' library(shiny)
#' library(sortable)
#' library(colourpicker)
#' library(ggplot2)
#' # Example usage in a Shiny app
#' shinyApp(
#'   ui = fluidPage(
#'     .buildSliceUI(id = "slice1", name = "Example Slice")
#'   ),
#'   server = function(input, output, session) {
#'     slices <- .buildSliceServer(
#'       id = "slice1",
#'       metrics = colnames(mtcars),
#'       data = mtcars,
#'       negativeHandling = reactive("keep")
#'     )
#'   }
#' )
.buildSliceServer <- function(id, metrics, data, negativeHandling, preSlice = NULL, initialSliceTransform = "x", initialName = NULL, initialColor = NULL, initialWeight = 1, initialMetricTransform = "x") {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      if(is.null(initialColor)){initialColor <- "#f3622d"}
      
      #REACTIVE VALUES
      selectedMetrics <- reactiveVal(NULL) #metrics included in slice
      metricTransformationList <- reactiveValues()   #tf strings metric level
      metricTransformationResultList <- reactiveValues() #data to save transformed metrics
      sliceTransformation <- reactiveVal("x") #tf string slice level
      sliceWeight <- reactiveVal(initialWeight) #User defined slice weight

      #get values to initialize reactive values if a slice is provided
      if (!is.null(preSlice)) {
        #update selected metrics based on predefined slice 
        sliceMetrics <- txpValueNames(preSlice)
        selectedMetrics(sliceMetrics)

        #get metric transformations from predefined slice
        tfs <- txpTransFuncs(preSlice)
        names(tfs) <- sliceMetrics

        #get metric transformations as text, update reactives and transformed metric data
        index <- 1
        for (i in names(tfs)) {
          text <- .getFuncText(tfs[i]@listData[[index]]@.Data)
          metricTransformationList[[i]] <- text
          tmpf <- .createFunc(text)
          tmpSlice <- TxpSlice(i, tmpf)
          metricTransformationResultList[[i]] <- .sumSlice(tmpSlice, data, isolate(negativeHandling()))
        }

        #assign slice transformation to initial
        sliceTransformation(initialSliceTransform)
      }

      #REACTIVE EXPRESSIONS
      #assign selectable metrics reactively based on filter and removing already selected
      availableMetrics <- reactive({
        filtered <- metrics[grepl(input$sliceMetricFilter, metrics)]
        filtered <- filtered[!(filtered %in% selectedMetrics())]
        #filtered[c(min(1, length(filtered)):min(20, length(filtered)))]
      })

      #named list of metric transformations for creating a TxpSlice object
      tfList <- reactive({
        #get list of current transforms as text and remove any null
        transforms <- reactiveValuesToList(metricTransformationList, all.names = FALSE)
        transforms <- transforms[!sapply(transforms, is.null)]

        #convert transform text to functions
        tfs <- list()
        for (i in names(transforms)) {
          tfs[[i]] <- .createFunc(transforms[[i]])
        }
        tfs
      })

      #TxpSlice object
      slice <- reactive({
        if (length(selectedMetrics()) == 0) {
          NULL
        } else {
          #reorder tf list based on selected Metrics
          orderedTFs <- tfList()[selectedMetrics()]

          #get slice object
          suppressWarnings(
            TxpSlice(selectedMetrics(), do.call(TxpTransFuncList, orderedTFs))
          )
        }
      })

      #User defined slice name
      sliceName <- reactive({
        if (is.null(input$sliceName)) {
          initialName
        } else {
          input$sliceName
        }
      })

      #User defined slice color
      sliceColor <- reactive({
        if (is.null(input$sliceColor)) {
          initialColor
        } else {
          input$sliceColor
        }
      })

      #the metric transformation to be currently shown in the UI
      shownMetricTransformation <- reactiveVal({
        if (is.null(isolate(input$metricTransformation))) {
          initialMetricTransform
        } else {
          isolate(input$metricTransformation)
        }
      })

      #untransformed distribution of slice scores
      Distribution <- reactive({
        if (length(selectedMetrics()) == 1) {
          tmpData <- metricTransformationResultList[[selectedMetrics()]]
          if (negativeHandling() == "missing") tmpData[tmpData < 0] <- NA
          data.frame(setNames(list(tmpData),
                              "Distribution"))
        } else {
          #get transformed metric results
          tmpData <- data.frame(matrix(nrow = nrow(data)))
          for (i in selectedMetrics()) {
            tmpData <- cbind(tmpData, metricTransformationResultList[[i]])
          }
          if (negativeHandling() == "missing") tmpData[tmpData < 0] <- NA

          #aggregate metric results
          data.frame(setNames(list(apply(tmpData[, -1], MARGIN = 1, .sumNA)),
                              "Distribution"))
        }
      })

      #transformed distribution of slice scores
      transformedDistribution <- reactive({
        #transform aggregated data
        func <- .createFunc(sliceTransformation())
        if (is.null(func)) {
          Distribution()
        } else {
          data.frame(setNames(list(.createFunc(sliceTransformation())(Distribution())[[1]]),
                              "Distribution"))
        }
      })

      #REACTIVE UI OUTPUTS
      #untransformed metric distribution plot
      output$metricHistogram <- renderPlot({
        if (input$metricTransformSelect == "" || input$metricTransformSelect == " ") {
          ggplot()
        } else {
          ggplot(data.frame(setNames(list(data[, input$metricTransformSelect]),
                                     "Distribution")
          ),
                 aes(x = Distribution)) +
            geom_histogram(bins = 30,
                           fill = input$sliceColor)
        }
      })

      #transformed metric distribution plot
      output$metricHistogramTransformed <- renderPlot({
        if (input$metricTransformSelect == "" || input$metricTransformSelect == " ") {
          ggplot()
        } else {
          tmpData <- metricTransformationResultList[[input$metricTransformSelect]]
          tmpData <- tmpData[is.finite(tmpData)]
          if (negativeHandling() == "missing") tmpData[tmpData < 0] <- NA
          ggplot(data.frame(setNames(list(tmpData),
                                     "Distribution")),
                 aes(x = Distribution)) +
            geom_histogram(bins = 30,
                           fill = input$sliceColor)
        }
      })

      #untransformed slice distribution plot
      output$histogram <- renderPlot({
        if (length(selectedMetrics()) == 0) {
          ggplot()
        } else {
          ggplot(Distribution(), aes(x = Distribution)) +
            geom_histogram(bins = 30,
                           fill = input$sliceColor)
        }
      })

      #transformed slice distribution plot
      output$histogramTransformed <- renderPlot({
        if (length(selectedMetrics()) == 0) {
          ggplot()
        } else {
          ggplot(transformedDistribution(), aes(x = Distribution)) +
            geom_histogram(bins = 30,
                           fill = input$sliceColor)
        }
      })

      #selection bucket for metrics to include in the slice
      output$bucketPlaceholder <- renderUI({
        bucket_list(
          header = NULL,
          group_name = ns("metricBucketID"),
          orientation = "horizontal",
          add_rank_list(
            text = "Available Metrics",
            labels = availableMetrics(),
            input_id = ns("metricInputID"),
            options = sortable_options(multiDrag = FALSE)
          ),
          add_rank_list(
            text = "Selected Metrics",
            labels = selectedMetrics(),
            input_id = ns("metricOutputID"),
            options = sortable_options(multiDrag = FALSE)
          )
        )
      })

      #OBSERVABLE EVENTS
      #updating sliceWeight via text input change
      observeEvent(input$sliceWeight, {
        sliceWeight(input$sliceWeight)
      })

      #updating slice weight from outside module server by recalling server with new weights
      ## ie: weight estimation method
      observeEvent(initialWeight, {
        sliceWeight(initialWeight)
      })

      #slice level transformation submission
      observeEvent(input$submitTransformation, {
        #read input
        text <- isolate(input$sliceTransformation)

        #get function
        sliceTransformation(text)
      })

      #moving metric from or to selected bin - MAKE THIS IGNORE INITIALIZATION AND HANDLE MULTISELECTION MOVING
      observeEvent(input$metricOutputID, {
        req(length(selectedMetrics()) != length(input$metricOutputID))

        #update metric transformation information for moved metric
        if (length(selectedMetrics()) > length(input$metricOutputID)) { #metric was moved from selected
          removed = TRUE
          movedMetric <- selectedMetrics()[!(selectedMetrics() %in% input$metricOutputID)]
          metricTransformationList[[movedMetric]] <- NULL
        } else { #metric was moved to selected
          removed = FALSE
          movedMetric <- input$metricOutputID[!(input$metricOutputID %in% selectedMetrics())]
          metricTransformationList[[movedMetric]] <- "x"
          metricTransformationResultList[[movedMetric]] <- data[, movedMetric]
        }

        #update selected metrics
        selectedMetrics(input$metricOutputID)

        #update metric text inputs as follows:
        ##if removed:
        ##if the last metric was removed, become empty
        ##if currently shown was removed, switch to first in list
        ##otherwise keep same shown but update dropdown
        #NEED TO ACCOUNT FOR MULTIPLE SELECTION, SHORTEN THIS TO NOT REPEAT SO MANY TIMES
        if (length(selectedMetrics()) == 0) { #if none are selected now or initialization
          updateSelectInput("metricTransformSelect",
                            session = session,
                            label = "Select Metric",
                            choices = " ",
                            selected = " ")

          updateTextInput(session = session,
                          "metricTransformation",
                          label = "Function",
                          value = "")
        } else if (removed) {
          if (input$metricTransformSelect == movedMetric) { #if the one removed is the current transform
            updateSelectInput("metricTransformSelect",
                              session = session,
                              label = "Select Metric",
                              choices = selectedMetrics(),
                              selected = selectedMetrics()[1])

          } else {
            updateSelectInput("metricTransformSelect",
                              session = session,
                              label = "Select Metric",
                              choices = selectedMetrics(),
                              selected = input$metricTransformSelect)
          }
        } else if (length(selectedMetrics()) == 1) { #if first metric is added
          updateSelectInput("metricTransformSelect",
                            session = session,
                            label = "Select Metric",
                            choices = selectedMetrics(),
                            selected = movedMetric)
        } else {
          updateSelectInput("metricTransformSelect",
                            session = session,
                            label = "Select Metric",
                            choices = selectedMetrics(),
                            selected = input$metricTransformSelect)
        }
      })

      #change metric currently shown
      observeEvent(input$metricTransformSelect, {
        if (input$metricTransformSelect == " " || input$metricTransformSelect == "") {
          updateTextInput(session = session,
                          "metricTransformation",
                          label = "Function",
                          value = "")
          shownMetricTransformation("")
        } else {
          updateTextInput(session = session,
                          "metricTransformation",
                          label = "Function",
                          value = metricTransformationList[[isolate(input$metricTransformSelect)]])
          shownMetricTransformation(metricTransformationList[[isolate(input$metricTransformSelect)]])
        }
      })

      #submit currently typed metric transformation function
      observeEvent(input$submitMetricTransformation, {
        #update list of transformations
        metricTransformationList[[input$metricTransformSelect]] <- input$metricTransformation #tf function
        shownMetricTransformation(input$metricTransformation)
        tmpf <- .createFunc(input$metricTransformation)


        #create tmp slice object for a single metric
        suppressWarnings(
          tmpSlice <- TxpSlice(input$metricTransformSelect, tmpf)
        )

        #perform metric level transformation and assign to result list
        metricTransformationResultList[[input$metricTransformSelect]] <- .sumSlice(tmpSlice,
                                                                                   data,
                                                                                   negativeHandling()
        )
      })

      observeEvent(input$submitMetricTransformationAll, {
        text <- input$metricTransformation
        shownMetricTransformation(text)
        tmpf <- .createFunc(text)
        for (i in selectedMetrics()) {
          metricTransformationList[[i]] <- text

          #create tmp slice object for a single metric
          suppressWarnings(
            tmpSlice <- TxpSlice(i, tmpf)
          )

          #perform metric level transformation and assign to result list
          metricTransformationResultList[[i]] <- .sumSlice(tmpSlice,
                                                           data,
                                                           negativeHandling()
          )
        }
      })

      #return slice object and info
      return(list(object = reactive(slice()),
                  name = reactive(sliceName()),
                  weight = reactive(sliceWeight()),
                  sliceTransform = reactive(sliceTransformation()),
                  color = reactive(sliceColor()),
                  metricTransform = reactive(shownMetricTransformation()),
                  currentMetric = reactive(input$metricTransformSelect)))
    }
  )
}