##---------------------------------------------------------------------------##
## Non-exported, non-documented, shiny-wide utility functions
##---------------------------------------------------------------------------##

#function for reading in data to the shiny app
.readRawFile <- function(dataFile) {
  if (is.character(dataFile) && file.exists(dataFile) && grepl("\\.csv$", dataFile)) {
    data <- read.csv(dataFile)
  }  else if (is.data.frame(dataFile)) {
    data <- dataFile
  }  else {
    stop("Input must be either a file path to a CSV file or an R dataframe.")
  }
  #Validate Data (Not Done)
  data
}

# Helper function for processing each slice
.processSlice <- function(sliceID, sliceObjects, preview = FALSE) {
  msg <- .validateSlice(sliceID, sliceObjects)
  if (!is.null(msg)) return(msg)
  slice <- sliceObjects[[sliceID]][["object"]]()
  weight <- as.numeric(sliceObjects[[sliceID]][["weight"]]())
  transform <- if (!preview) .createFunc(sliceObjects[[sliceID]][["sliceTransform"]]()) else NULL
  list(slice = slice, weight = weight, transform = transform)
}

.chkNames <- function(nmsList){
  repeatedNames <- nmsList[duplicated(nmsList)]
  if(length(repeatedNames != 0)){ #change to need validaate or try car=tc
    return(paste("Error:", repeatedNames[[1]], "is reused as a slice name. Please update the first slice containing this name."))
  }
  NULL
}

#validate parameters for a slice or determine specific error
.validateSlice <- function(sliceID, sliceObjects){
  name <- sliceObjects[[sliceID]][["name"]]()
  if(sliceObjects[[sliceID]][["name"]]() == ""){
    return(paste("Error: The current slice does not have a name."))
  }
  if(is.null(sliceObjects[[sliceID]][["object"]]())){
    return(paste("Error:", name, "does not contain any metrics."))
  } 
  weight <- suppressWarnings(as.numeric(sliceObjects[[sliceID]][["weight"]]()))
  if(is.na(weight)){
    return(paste("Error:", name, "does not contain a valid weight."))
  }
  if(is.null(.createFunc(sliceObjects[[sliceID]][["sliceTransform"]]()))){
    return(paste("Error:", name, "does not contain a valid slice transform."))
  }
  slice <- sliceObjects[[sliceID]][["object"]]()
  metricTFs <- txpTransFuncs(slice)
  nulls <- which(lengths(metricTFs)==0)
  if(!length(nulls)==0){
    return(paste("Error:", name, "contains an invalid transform for", 
                 txpValueNames(slice)[[nulls[1]]], "."))
  }
  NULL
}

.getTxpModel <- function(currentSlices, sliceObjects, preview = FALSE) {
  #$trycatch
  nmsList <- lapply(currentSlices, 
                    function(sliceID, sliceObjects){sliceObjects[[sliceID]][["name"]]()},
                    sliceObjects = sliceObjects)
  msg <- .chkNames(nmsList)
  if (!is.null(msg)) return(msg)
  results <- lapply(currentSlices, .processSlice, sliceObjects = sliceObjects, preview = preview)
  is_error <- vapply(results, function(slice) is.character(slice), logical(1))
  if (any(is_error)) return(results[is_error][[1]])
  sliceList <- setNames(lapply(results, `[[`, "slice"), nmsList)
  txpSliceList <- do.call(TxpSliceList, sliceList)
  weightList <- sapply(results, `[[`, "weight")
  model <- if (preview) {
    TxpModel(txpSlices = txpSliceList, txpWeights = weightList)
  } else {
    transformList <- setNames(lapply(results, `[[`, "transform"), sapply(results, `[[`, "name"))
    txpTransformList <- do.call(TxpTransFuncList, transformList)
    TxpModel(txpSlices = txpSliceList, txpWeights = weightList, txpTransFuncs = txpTransformList)
  }
  model
}

#function for obtaining color list from current slices
.getTxpColors<- function(currentSlices, sliceObjects){
  sapply(currentSlices, function(i) sliceObjects[[i]][["color"]]())
}

#function to get body text from a function
.getFuncText <- function(func){
  if(is.null(func)){
    return("x")
  }
  gsub(" ", "", paste0(deparse(body(func)), collapse = ""))
}

#function to create a new transform function from text
.createFunc <- function(text){
  isValid <- try(silent = TRUE, {
    func <- eval(parse(text = paste0("function(x) ", text)))
    suppressWarnings(
      tf <- TxpTransFunc(func)
    )
    func
  })
  if(class(isValid) == "try-error"){
    return(NULL)
  }
  isValid
}

#function to swap slice positions
.moveSlice <- function(selectedSlice, direction, currentSlices, session){
  selectedIndex <- which(currentSlices == selectedSlice)
  if(direction == "up"){
    if(selectedIndex == 1){
      return(currentSlices)
    } else {
      swapValue <- -1
    }
  } else if(direction == "down"){
    if(selectedIndex == length(currentSlices)){
      return(currentSlices)
    } else {
      swapValue <- 1
    }
  }
  
  #swap elements using tmp values
  swapSlice <- currentSlices[selectedIndex + swapValue]
  tmpSlices <- currentSlices
  tmpSlices[selectedIndex] <- swapSlice
  tmpSlices[selectedIndex + swapValue] <- selectedSlice
  
  #update slice select ui to change order
  updateSelectInput("sliceSelect",
                    session = session,
                    label = "Select A Slice:",
                    choices = tmpSlices,
                    selected = selectedSlice)
  tmpSlices
}


.getMissingness <- function(data, model, negativeHandling) {
  misVector <- sapply(txpSlices(model), function(slice) {
    .sumSlice(slice = slice, input = data, negative.value.handling = negativeHandling)$mis
  })
  names(misVector) <- names(txpSlices(model))
  misVector
}

##----------------------------------------------------------------------------##
