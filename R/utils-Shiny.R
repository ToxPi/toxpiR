##---------------------------------------------------------------------------##
## Non-exported, non-documented, shiny-wide utility functions
##---------------------------------------------------------------------------##

#function for reading in data to the shiny app
.readFileOrDF <- function(dataFile) {
  if(!is_scalar_character(dataFile) && !is.data.frame(dataFile)) {return("dataFile must be either an R dataframe or a file path to a compatible CSV file.")}
  
  # if dataFile is file path to csv
  if (is_scalar_character(dataFile)) { 
    #check for csv file
    if(!file.exists(dataFile)) {return("file not found")}
    if(tolower(tools::file_ext(dataFile)) != "csv") {return("dataFile must be either an R dataframe or a file path to a compatible CSV file.")}
    
    #read csv file and potential model header
    dataList <- .readTxpFormat(dataFile)
  #if dataFile is data.frame object
  }  else {
    data <- dataFile
    dataList <- list(model = NULL, input = data, fills = NULL)
  }
  dataList
}

#get data from any of three csv file formats (gui, webapp, no model header)
.readTxpFormat <- function(dataFile){
  indicator <- read.csv(file=dataFile, nrows=1, header = FALSE)[1]
  letter1 <- substring(indicator[[1]],1,1)
  if(!is.na(letter1) && letter1 == "#") {
    if(indicator == "#Generated From WebApp"){dataList <- txpImportWebApp(dataFile)}
    else{dataList <- txpImportGui(dataFile)}
  } else {
    data <- read.csv(dataFile)
    dataList <- list(model = NULL, input = data, fills = NULL)
  }
  dataList
}

# Function for validating file, can be a dataframe, gui csv, or webapp csv
.validateTxpData <- function(data, id){
  #check if id is a column in the dataset
  if(!is.null(id) && !(id %in% colnames(data))){
    return(paste0("id parameter ", id, " is not a valid column name found within the provided dataset"))
  }
  
  #see if any colnames can be converted to numeric
  numericColNames <- any(sapply(colnames(data), function(col) !is.na(suppressWarnings(as.numeric(col)))))
  #provide warning if more than one column name is empty or if any of the column names seem numeric
  if(("X" %in% colnames(data) && "X.1" %in% colnames(data)) || numericColNames){
    warning("Multiple columns in the dataFile had empty or numeric column names and thus might not contain the needed header. If this is not intentional, please make sure the csv file has a header row with column names or the r dataframe has column names properly assigned")
  }
  
  return(NULL)
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

.getTxpModel <- function(currentSlices, sliceObjects, negativeHandling, rankTies, preview = FALSE) {
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
    TxpModel(txpSlices = txpSliceList, txpWeights = weightList, negativeHandling = negativeHandling)
  } else {
    transformList <- setNames(lapply(results, `[[`, "transform"), sapply(results, `[[`, "name"))
    txpTransformList <- do.call(TxpTransFuncList, transformList)
    TxpModel(txpSlices = txpSliceList, 
             txpWeights = weightList, 
             txpTransFuncs = txpTransformList,
             negativeHandling = negativeHandling,
             rankTies = rankTies)
  }
  model
}

#function for obtaining color list from current slices
.getTxpColors<- function(currentSlices, sliceObjects){
  lapply(currentSlices, function(i) sliceObjects[[i]][["color"]]())
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
