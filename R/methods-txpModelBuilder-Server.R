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

.buildModelServer <- function(input, output, session, data, initialModel, initialFills, id) {
  
  ######################## Model Initialization ################################
  #### APP-WIDE REACTIVES FOR MODEL ####
  sliceObjects <- reactiveValues() #Reactive list of slices as sliceIDs, each containing a list of required information about the slice
  #toxColors <- reactiveVal(fills) #List of colors for slices
  
  #### MODEL PARAMETER UI OUTPUTS ####
  output$negativeHandlingPlaceholder <- renderUI({
    selectInput(
      "negativeValueHandling",
      label = "Negative Handling",
      choices = c("keep", "missing"), 
      selected = ifelse(is.null(initialModel), "keep", slot(initialModel, "negativeHandling"))
    )
  })
  
  output$rankTiesPlaceholder <- renderUI({
    selectInput(
      "rankTiesMethod",
      label = "Tie Handling",
      choices = c("average", "first", "last", "random", "max", "min"),
      selected = ifelse(is.null(initialModel), "average", slot(initialModel, "rankTies"))
    )
  })
  
  #### INITIAL MODEL CREATION ####
  # Identify numeric-convertible columns
  isConvertible <- function(x) is.numeric(data[[x]])
  convertibleCols <- sapply(colnames(data), isConvertible)
  availableMetrics <- colnames(data)[convertibleCols]

  #Fixed variables for initializing first slice
  index <- 0
  initialSliceIDs <- c()
  # Initialize original slices from starting model 
  if(is.null(initialModel)){
    index <- 1
    sliceID <- "Slice1"
    initialSliceIDs <- c(initialSliceIDs, sliceID)
    
    #Prep a 1 slice default model
    sliceObjects[[sliceID]] <- .buildSliceServer(id = sliceID, 
                                                metrics = availableMetrics, 
                                                data = data,
                                                negativeHandling = reactive(input$negativeValueHandling),
                                                initialColor = initialFills,
                                                initialName = sliceID)
  } else {
    #get model level slice info
    sliceNames <- names(initialModel)
    sliceWeights <- txpWeights(initialModel)
    sliceTFs <- txpTransFuncs(initialModel)
    
    #initialize slice modules
    lapply(
      X = txpSlices(initialModel)@listData,
      FUN = function(slice, currentIndex = index+1){
        #get specific slice info at model level
        sliceID <- paste0("Slice", currentIndex) 
        name <- sliceNames[currentIndex]
        sliceTF <- .getFuncText(sliceTFs[currentIndex]@listData[[name]]@.Data)
        sliceWeight <- sliceWeights[currentIndex]
        color <- initialFills[currentIndex]
        
        #get shown metric transform
        metricFuncs <- names(txpTransFuncs(slice))[1]
        initialMetricText <- .getFuncText(txpTransFuncs(slice)[[metricFuncs]])
        
        #initialize slice module server
        sliceObjects[[sliceID]] <<- .buildSliceServer(id = sliceID, 
                                                     metrics = availableMetrics, 
                                                     data = data,
                                                     negativeHandling = reactive(ifelse(is.null(input$negativeValueHandling), "keep", input$negativeValueHandling)),
                                                     preSlice = slice,
                                                     initialSliceTransform = sliceTF,
                                                     initialName = name,
                                                     initialWeight = sliceWeight,
                                                     initialColor = color,
                                                     initialMetricTransform = initialMetricText)
        
        #update counters
        index <<- currentIndex
      }
    )
  }
  
  ####################### Slice Selection Pane #################################
  #### REACTIVE VARIABLES ####
  currentIDs <-reactiveVal(c(1:index)) #Vector of all numeric IDs (1,2...) in model
  currentSlices <- reactiveVal(paste0("Slice", c(1:index))) #Vector of all slice ids (Slice1, ...) in model
  maxID <- reactiveVal(index) #Largest ID for tracking new slice IDs
  currentSelectedIndex <- reactiveVal(1) #Current slice index selected in panel

  #### REACTIVE EXPRESSIONS ####
  currentNames <- reactive({ #list of current slice names for slice selection panel
    tmpList <- list()
    for (i in currentSlices()) {
      tmpList <- c(tmpList, sliceObjects[[i]][["name"]]())
    }
    tmpList
  })
  
  #### UI OUTPUTS ####
  output$sliceSelectPlaceholder <- renderUI({ #Slice selection UI by ID
    selectInput("sliceSelect",
                label = "Slice ID:",
                choices = currentSlices(),
                selected = currentSlices()[1],
                size = 8,
                selectize = FALSE)
  })
  
  output$sliceNamesPlaceholder <- renderUI({ #Slice selection UI by Name
    selectInput(
      "sliceNames",
      label = "Slice Name:",
      choices = currentNames(),
      selected = currentNames()[1],
      size = 8,
      selectize = FALSE
    )
  })
  
  #### OBSERVABLE EVENTS
  observeEvent(input$addSlice, {  #add a new slice to the model
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
                                                 metrics = availableMetrics,
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

  observeEvent(input$sliceSelect, { #selection of a new slice based on id
    currentSelectedIndex(which(currentSlices() == input$sliceSelect))
  })
  
  observeEvent(list(currentNames(), currentSelectedIndex()), { #changing an existing slice name or moving a slice to a new position
    updateSelectInput("sliceNames",
                      session = session,
                      label = "Slice Name:",
                      choices = currentNames(),
                      selected = currentNames()[currentSelectedIndex()])
  })
  
  observeEvent(input$sliceNames, { #changing of the current slice names options
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
  
  observeEvent(input$moveUpID, { #move a slice up
    req(input$sliceSelect)
    #get selected info
    selectedSlice <- input$sliceSelect
    if (currentSelectedIndex() != 1) {
      currentSlices(.moveSlice(selectedSlice, "up", currentSlices(), session))
      currentSelectedIndex(currentSelectedIndex() - 1)
    }
  })
  
  observeEvent(input$moveDownID, { #move a slice down
    req(input$sliceSelect)
    #get slice info
    selectedSlice <- input$sliceSelect
    if (currentSelectedIndex() != length(currentSlices())) {
      currentSlices(.moveSlice(selectedSlice, "down", currentSlices(), session))
      currentSelectedIndex(currentSelectedIndex() + 1)
    }
  })
  
  observeEvent(input$removeButton, { #remove a slice
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
  
  ######################## Slice Creation Pane #################################
  #### REACTIVE EXPRESSIONS ####
  shownID <- reactive(input$sliceSelect) #shown slice id
  shownName <- reactive(sliceObjects[[input$sliceSelect]][["name"]]()) #shown slice name
  shownTF <- reactive(sliceObjects[[input$sliceSelect]][["sliceTransform"]]()) #shown slice transform function
  shownWeight <- reactive(sliceObjects[[input$sliceSelect]][["weight"]]()) #shown slice weight
  shownColor <- reactive(sliceObjects[[input$sliceSelect]][["color"]]()) #shown slice color
  shownMetric <- reactive(sliceObjects[[input$sliceSelect]][["currentMetric"]]()) #currently shown metric
  shownMetricTransform <- reactive(sliceObjects[[input$sliceSelect]][["metricTransform"]]()) #shown metric transformation
  shownMetrics <- reactive({ #all metrics currently making up the slice
    if (is.null(sliceObjects[[input$sliceSelect]][["object"]]())) {
      NULL
    } else {
      txpValueNames(sliceObjects[[input$sliceSelect]][["object"]]())
    }
  })
  currentMetricTransform <- reactive({ #last submitted metric transformation
    sliceObjects[[input$sliceSelect]][["metricTransform"]]()
  })
  currentSliceTransform <- reactive({ #last submitted slice transformation
    sliceObjects[[input$sliceSelect]][["sliceTransform"]]()
  })
  
  #### REACTIVE VALUES ####
  currentSelectedTab <- reactiveVal("Slice Info") #current tab shown
  
  #### UI OUTPUTS ####
  output$sliceInfo <- renderUI({ #slice information panel (module)
    tryCatch({
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
    })
  })
  
  #### OBSERVABLE EVENTS ####
  observeEvent(input$selectedTab, { #selection of a new tab in slice ui panel
    currentSelectedTab(input$selectedTab)
  })
  
  ######################## Model Preview #######################################
  #### REACTIVE EXPRESSIONS ####
  previewModel <- reactive({ #Current TxpModel created
    req(input$negativeValueHandling)
    #get txp model
    model <- .getTxpModel(currentSlices(), sliceObjects, input$negativeValueHandling, input$rankTiesMethod, preview = TRUE)
    
    #enable/disable buttons based on if model is valid
    if (class(model) == "TxpModel") {
      enable('exportDataButton')
    } else {
      disable('exportDataButton')
    }

    #if txp model returns error message return a specific error
    validate(need(class(model) == "TxpModel", model))

    #return proper model
    model
  })
  
  #### UI OUTPUTS ####
  output$profilePreview <- renderUI({ #current model plot preview or error
    validate(need(class(previewModel()) == "TxpModel", previewModel()))
    tryCatch(
      {
        #get model and plotting requirements
        radii <- rep(1, length(currentSlices()))
        fills <- .getTxpColors(currentSlices(), sliceObjects)
        #get missing data
        missing <- .getMissingness(data, previewModel(), slot(previewModel(), "negativeHandling"))
        previewSliceScores <- matrix(data = radii, nrow = 1)
        colnames(previewSliceScores) <- names(previewModel())
        #
        previewResult <- TxpResult(
                              txpScores = c(1),
                              txpSliceScores = previewSliceScores,
                              txpRanks = c(1),
                              txpMissing = missing,
                              txpModel = previewModel(),
                              txpIDs = c("Profile Preview")
        )
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
  
  ############################ User Exports ####################################
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
      model <- .getTxpModel(currentSlices(), sliceObjects, input$negativeValueHandling, input$rankTiesMethod)
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

  ##### OBSERVABLE EVENTS ####
  observeEvent(input$modelSelect, { #upload a toxpiR model (remove?)
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
  
  observeEvent(input$calculateScores, { #calculate and save toxResult object
    #get model
    model <- getTxpModel(currentSlices(), sliceObjects)
    finalModel(model)
    toxColors(getTxpColors(currentSlices(), sliceObjects))
    missingness(getMissingness(confirmedData(), model, isolate(input$negativeValueHandling)))
    #missingness(txpMissin)
    finalNames(currentNames())
    finalWeights(txpWeights(model))
    
    #calculate results
    ##NOT WORKING WHEN 1 SAMPLE
    if (input$dataDirection == "positive") {
      toxResult(txpCalculateScores(model = model,
                                   input = confirmedData(),
                                   id.var = "Name",
                                   rank.ties.method = input$rankTiesMethod,
                                   negative.value.handling = input$negativeValueHandling))
    }
    else {
      toxResult(.calculateSignedScores(model = model,
                                       input = confirmedData(),
                                       id.var = "Name",
                                       rank.ties.method = input$rankTiesMethod))
    }
    df <- as.data.frame(toxResult())
    colnames(df)[colnames(df) == "score"] <- "ToxPi Score"
    colnames(df)[colnames(df) == "id"] <- "Name"
    colnames(df)[colnames(df) == "rank"] <- "Rank"
    toxResultDF(df)
    
    #update visible tabs and send to visualize results tab
    showTab('navbar', 'visualizeResultsTab')
    showTab('navbar', 'clusteringTab')
    shinyjs::show("calculateBootstrapCIs")
    shinyjs::enable("calculateBootstrapCIs")
    # updateCheckboxGroupInput(session, "plotOptions", "Plot Options:",
    #                    choiceNames = list(icon("chart-simple"), paste("NA"), icon("square-full"), icon("minus"), icon("circle"), paste("---"), icon("i"), icon("tag")),
    #                    choiceValues = list("barplot", "missing", "background", "outlines", "maxRadius", "sliceGuides", "bootstrapCIs", "labels"),
    #                    selected = c("missing", "background", "outlines", "maxRadius"),
    #                    inline = TRUE)
    runjs('$("#plotOptions input[value=\'bootstrapCIs\']").prop("checked", false);')
    
    runjs('$("#plotOptions input[type=checkbox]:eq(6)").prop("disabled", true);')
    #shinyjs::disable(selector = "#plotOptions input[value='bootstrapCIs']")
    shinyjs::disable("bootstrapCIsColor")
    updateSelectInput(inputId = "underlyingDataSelect",
                      session = session,
                      label = "Underlying Metric Data",
                      choices = currentNames())
    updateSelectInput(inputId = "rankSliceSelect",
                      session = session,
                      label = "Select Rank Method:",
                      choices = c("Overall", currentNames()))
    updateNavbarPage(session, "navbar", selected = "visualizeResultsTab")
    shinyjs::hide("resultsSelectShown")
    shinyjs::show("exportResults")
  })
  
  observeEvent(input$buildModelHelp, { #get help
    showModal(buildModelHelp())
  })
  
}