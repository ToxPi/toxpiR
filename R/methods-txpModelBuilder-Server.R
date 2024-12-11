##----------------------------------------------------------------------------##
## txpModelBuilder-Server
##----------------------------------------------------------------------------##

#' @noRd
#' @title txpModelBuilder Server
#' @description Defines the Server logic for the `txpModelBuilder` Shiny app. 
#' 
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param data The data frame used for building the model upon.
#' 
#' @seealso [.buildModelUI] for corresponding UI
#' @seealso [.buildSliceServer] and [.buildSliceUI] for slice modules
#' @seealso `txpModelBuilder` for server-ui integration
#' 
#' @import shiny 
#' @importFrom shinyjs enable disable
#' @importFrom colorspace lighten

.buildModelServer <- function(input, output, session, data) {
  #Fixed variables for initializing first slice
  currentID <- 1
  sliceID <- "Slice1"

  #Get numeric columns for metric options
  tstCls <- function(x) is.numeric(data[[x]])
  inptCls <- sapply(colnames(data), tstClass)
  metricOptions <- colnames(data)[inptCls]
    
  #Reactive variables
  currentIDs <-reactiveVal(currentID) #Vector of all numeric IDs (1,2...) in model
  currentSlices <- reactiveVal(sliceID) #Vector of all slice ids (Slice1, ...) in model
  maxID <- reactiveVal(currentID) #Largest ID for tracking new slice IDs

  #List of all sliceIDs, each containing a list of required information about the slice
  sliceObjects <- reactiveValues()

  #list of colors for slices
  toxColors <- reactiveVal()

  #current index selected in panel
  currentSelectedIndex <- reactiveVal(1)

  #list of current slice names for slice selection panel
  currentNames <- reactive({
    tmpList <- list()
    for (i in currentSlices()) {
      tmpList <- c(tmpList, sliceObjects[[i]][["name"]]())
    }
    tmpList
  })

  #Reactives for slice information panel accessed from slice modules
  #slice id
  shownID <- reactive(input$sliceSelect)
  #all metrics in slice
  shownMetrics <- reactive({
    if (is.null(sliceObjects[[input$sliceSelect]][["object"]]())) {
      NULL
    } else {
      txpValueNames(sliceObjects[[input$sliceSelect]][["object"]]())
    }
  })
  #slice name
  shownName <- reactive(sliceObjects[[input$sliceSelect]][["name"]]())
  #slice transform function
  shownTF <- reactive(sliceObjects[[input$sliceSelect]][["sliceTransform"]]())
  #slice weight
  shownWeight <- reactive(sliceObjects[[input$sliceSelect]][["weight"]]())
  #slice color
  shownColor <- reactive(sliceObjects[[input$sliceSelect]][["color"]]())
  #currently selected metric for tf info
  shownMetric <- reactive(sliceObjects[[input$sliceSelect]][["currentMetric"]]())
  #metric transform for selected metric
  shownMetricTransform <- reactive(sliceObjects[[input$sliceSelect]][["metricTransform"]]())
  #tab shown
  currentSelectedTab <- reactiveVal("Slice Info")
  #shown text based on last tf submissions
  currentMetricTransform <- reactive({
    sliceObjects[[input$sliceSelect]][["metricTransform"]]()
  })
  currentSliceTransform <- reactive({
    sliceObjects[[input$sliceSelect]][["sliceTransform"]]()
  })

  #initialize first slice module server
  sliceObjects[[sliceID]] <- .buildSliceServer(id = sliceID,
                                              metrics = metricOptions,
                                              data = data,
                                              negativeHandling = reactive(input$negativeValueHandling),
                                              initialColor = "#f3622d",
                                              initialName = sliceID)


  #model preview
  previewModel <- reactive({
    #get txp model
    model <- .getTxpModel(currentSlices(), sliceObjects, preview = TRUE)
    #enable/disable buttons based on if an error is returned
    if (!(class(model) == "character")) {
      enable('exportDataButton')
    } else {
      disable('exportDataButton')
    }

    #if txp model returns error message return error
    validate(need(class(model) != "character", model))

    #return proper model
    model
  })


  #################### Logic for buildModel Tab ##################################
  ##### REACTIVE UI OUTPUTS
  #slice info panel ui
  output$sliceInfo <- renderUI(tryCatch(
                                        {
                                         .buildSliceUI(shownID(),
                                            name = isolate(shownName()),
                                            sliceTF = isolate(shownTF()),
                                            sliceWeight = isolate(shownWeight()),
                                            color = isolate(shownColor()),
                                            selectedMetrics = isolate(shownMetrics()),
                                            shownMetricTransform = isolate(shownMetricTransform()),
                                            currentMetricTransform = currentMetricTransform(),
                                            currentSelectedMetric = shownMetric(),
                                            currentSliceTransform = currentSliceTransform(),
                                            currentTab = currentSelectedTab())
                                          }, error = function(cond){
                                             paste("Please Add A Slice Or Import A toxpiR Model")
                                          }
                                        )
  )

  #profile preview plot based on current model, does error handling
  output$profilePreview <- renderUI({
    tryCatch(
      {
        #get model and plotting requirements
        radii <- rep(1, length(currentSlices()))
        fills <- .getTxpColors(currentSlices(), sliceObjects)

        #get missing data
        missing <- .getMissingness(data, previewModel(), input$negativeValueHandling)
        previewSliceScores <- matrix(data = radii, nrow = 1)
        colnames(previewSliceScores) <- names(previewModel())
        #
        previewResult <- new2("TxpResult",
                              txpScores = c(1),
                              txpSliceScores = previewSliceScores,
                              txpRanks = c(1),
                              txpMissing = missing,
                              txpModel = previewModel(),
                              txpIDs = c("Profile Preview"),
                              txpResultParam = new2("TxpResultParam",
                                                    rank.ties.method = "average",
                                                    negative.value.handling = "keep"))
        plot <- plot(previewResult, package = 'gg',
                     fills = fills,
                     showScore = FALSE) +
                theme(legend.position = 'none',
                      strip.text.x = element_blank())

        # Set plot dimensions dynamically based on legend width
        plotWidth <- input$legendWidth
        plotHeight <- plotWidth # Keep it square

        renderPlot(plot, width = plotWidth, height = plotHeight)
      }, error = function(cond) {
        previewModel()
      })
  })

  output$profileLegend <- renderUI({
    tryCatch(
      {
        previewModel()
        lapply(currentSlices(), function(i) {
          color <- sliceObjects[[i]][["color"]]()
          div(style = paste0("background-color:", color, ";height:20px;"),
              sliceObjects[[i]][["name"]]()
          )
        })
      },
      error = function(cond) {
        NULL
      }
    )
  })

  #export toxpi model as csv or rdata
  output$exportDataButton <- downloadHandler(
    filename = function() {
      if (input$exportData != "S4 Object") {
        paste("Model-", Sys.Date(), ".csv", sep = "")
      } else {
        paste("Model-", Sys.Date(), ".RData", sep = "")
      }
    },
    content = function(file) {
      model <- .getTxpModel(currentSlices(), sliceObjects)
      if (input$exportData == "WebApp CSV File") {
        txpExportWebApp(fileName = file,
                        input = confirmedData(),
                        model = model,
                        fills = .getTxpColors(currentSlices(), sliceObjects))
      } else if (input$exportData == "GUI CSV File") {
        txpExportGui(fileName = file,
                     input = confirmedData(),
                     model = model,
                     fills = .getTxpColors(currentSlices(), sliceObjects))
      } else {
        saveRDS(model, file)
      }
    }
  )

  ##### OBSERVABLE EVENTS

  ##slice selection list
  #add a new slice
  observeEvent(input$addSlice, {
    #get slice info
    newID <- maxID() + 1
    sliceID <- paste0("Slice", newID)
    if (length(currentSlices()) == 0) {
      defaultColor <- "#f3622d"
    } else {
      previousID <- paste0("Slice", maxID())
      previousColor <- sliceObjects[[previousID]][["color"]]()
      defaultColor <- lighten(previousColor)
    }

    #initialize slice module server
    sliceObjects[[sliceID]] <- .buildSliceServer(id = sliceID,
                                                metrics = metricOptions,
                                                data = data,
                                                negativeHandling = reactive(input$negativeValueHandling),
                                                initialColor = defaultColor,
                                                initialName = sliceID)

    #update reactive values
    currentSlices(c(currentSlices(), sliceID))
    currentIDs(c(currentIDs(), newID)) #add current ID to currentIDS
    maxID(newID)

    #update slice select list and selected
    updateSelectInput("sliceSelect",
                      session = session,
                      label = "Slice ID:",
                      choices = currentSlices(),
                      selected = sliceID
    )
  })
  #selection of a new slice based on id
  observeEvent(input$sliceSelect, {
    currentSelectedIndex(which(currentSlices() == input$sliceSelect))
  })
  #typing in a new slice name or observing change in selected slice id index
  observeEvent(list(currentNames(), currentSelectedIndex()), {
    updateSelectInput("sliceNames",
                      session = session,
                      label = "Slice Name:",
                      choices = currentNames(),
                      selected = currentNames()[currentSelectedIndex()])
  })
  #changing of the current slice names options
  observeEvent(input$sliceNames, {
    #get the current index selected
    currentSelectedIndex(which(currentNames() == input$sliceNames))
    #update list with new slice name options
    updateSelectInput("sliceSelect",
                      session = session,
                      label = "Slice ID:",
                      choices = currentSlices(),
                      selected = currentSlices()[currentSelectedIndex()]
    )
  })

  #move selected slice up in slice list
  observeEvent(input$moveUpID, {
    req(input$sliceSelect)
    #get selected info
    selectedSlice <- input$sliceSelect
    if (currentSelectedIndex() != 1) {
      currentSlices(.moveSlice(selectedSlice, "up", currentSlices(), session))
      currentSelectedIndex(currentSelectedIndex() - 1)
    }
  })

  #move selected slice down in slice list
  observeEvent(input$moveDownID, {
    req(input$sliceSelect)
    #get slice info
    selectedSlice <- input$sliceSelect
    if (currentSelectedIndex() != length(currentSlices())) {
      currentSlices(.moveSlice(selectedSlice, "down", currentSlices(), session))
      currentSelectedIndex(currentSelectedIndex() + 1)
    }
  })

  #remove selected slice from slice list
  observeEvent(input$removeButton, {
    req(input$sliceSelect)
    #get slice info
    selectedSlice <- input$sliceSelect
    selectedID <- substring(selectedSlice, 6)

    #remove slices and ids
    currentSlices(currentSlices()[currentSlices() != selectedSlice])
    currentIDs(currentIDs()[currentIDs() != selectedID])

    #remove ui
    removeUI(
      selector = paste0("#", selectedSlice, "-sliceID")
    )

    #update slice select list
    updateSelectInput("sliceSelect",
                      session = session,
                      label = "Slice ID:",
                      choices = currentSlices(),
                      selected = currentSlices()[1]
    )
  })


  ##current slice panel
  #selection of a new tab in slice ui panel
  observeEvent(input$selectedTab, {
    currentSelectedTab(input$selectedTab)
  })

  ##model parameters
  #upload a toxpiR model
  observeEvent(input$modelSelect, {
    tryCatch(
      {
        #load model
        model <- readRDS(input$modelSelect$datapath)

        #reset current slices
        currentIDs(NULL)
        currentSlices(NULL)

        #add and populate slices based on model
        sliceNames <- names(model)
        sliceWeights <- txpWeights(model)
        sliceTFs <- txpTransFuncs(model)

        #initialize all slice module servers
        index <- 1
        ID <- maxID() + 1
        lapply(
          X = txpSlices(model)@listData,
          FUN = function(slice, currentIndex = index, currentID = ID) {
            #get specific slice info at model level
            sliceID <- paste0("Slice", currentID)
            name <- sliceNames[index]
            sliceTF <- .getFuncText(sliceTFs[currentIndex]@listData[[name]]@.Data)
            sliceWeight <- sliceWeights[currentIndex]

            #get shown metric transform
            metricFuncs <- names(txpTransFuncs(slice))[1]
            initialMetricText <- .getFuncText(txpTransFuncs(slice)[[metricFuncs]])

            sliceObjects[[sliceID]] <<- .buildSliceServer(id = sliceID,
                                                         metrics = selectedMetrics$confirmed,
                                                         data = confirmedData(),
                                                         negativeHandling = reactive(input$negativeValueHandling),
                                                         preSlice = slice,
                                                         initialSliceTransform = sliceTF,
                                                         initialName = name,
                                                         initialWeight = sliceWeight,
                                                         initialColor = "#FFFFFF",
                                                         initialMetricTransform = initialMetricText)


            #update reactive values
            currentIDs(c(currentIDs(), currentID))
            currentSlices(c(currentSlices(), sliceID))
            maxID(currentID)

            #update counters
            index <<- currentIndex + 1
            ID <<- currentID + 1
          }
        )
        closeAlert(session, "modelAlertID")

        #Update sliceSelection list
        updateSelectInput("sliceSelect",
                          session = session,
                          label = "Slice ID:",
                          choices = currentSlices(),
                          selected = currentSlices()[1])
      }, error = function(cond){
        createAlert(session, "modelAlert", "modelAlertID",
                    title = "Oops",
                    content = "Uploaded file is not a valid toxpiR model. See help for details.",
                    append = FALSE)
      }
    )

  })

  observeEvent(input$buildModelHelp, {
    showModal(buildModelHelp())
  })
}