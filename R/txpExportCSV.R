##----------------------------------------------------------------------------##
## txpExportCSV
##----------------------------------------------------------------------------##

#' @name txpExportCSV
#' @title Export comma-separated file containing txpModel and raw data
#' @description Export comma-separated file containing txpModel and raw data
#' 
#' @param fileName Character scalar, the path to the output file
#' @param input dataframe, raw data compatible with model
#' @param model txpModel object
#' @param id.var Character scalar, the column name containing unique ids for the data
#' @param fills Character vector, hex codes corresponding to slice colors for model
#' @inheritParams txpCalculateScores 
#' @inheritParams pieGrob
#' 
#' @details 
#' The GUI differs in two meaninful ways for exporting `toxpiR` models: (1) the
#' GUI only allows for integer weights; (2) the GUI applies transformation 
#' functions differently. 
#' 
#' `txpExportGui` will not work for models with non-integer weights.
#' 
#' The GUI only applies a single transformation function to every input within
#' a slice, and only functions from a pre-determined list; `toxpiR` allows 
#' users to apply any valid function individually to each input, then a second
#' transformation function on the summed slice values. Because of this 
#' complexity, any exported models with slice-level transformation functions
#' will not export at the input level. In other words, the export will have only
#' the final slice scores. Otherwise, all input-level transformations will be 
#' performed, the and the export will contain transformed input-level data with
#' the `linear(x)` GUI transformation.
#' 
#' @importFrom rlang is_scalar_character
#' @importFrom utils write.table
#' @export

txpExportCSV <- function(fileName = "txpModel.csv", 
                         input, 
                         model, 
                         id.var = NULL,
                         fills = NULL) {
  stopifnot(is_scalar_character(fileName))
  stopifnot(class(model) == "TxpModel")
  stopifnot(class(input) == "data.frame")

  slcWts <- txpWeights(model)
  
  ## Check for slice-level transformations 
  tfs <- txpTransFuncs(model)
  
  #get text for slice tfs
  tfsText <- lapply(
    X = tfs,
    FUN = function(func){
      gsub(" ", "", paste0(deparse(body(func)), collapse = ""))
    }
  )

  ## Notes: may duplicate inputs because the same input in different slices
  ## can have different transformation functions
  # vnmVec <-  txpValueNames(model, simplify = TRUE)
  # vnmVecLower <- txpLowerNames(model, simplify = TRUE)
  # vnmVecUpper <- txpUpperNames(model, simplify = TRUE)
  
  slcVec <- names(model)
  vnmLst <-  txpValueNames(model)
  vnmLstLower <- txpLowerNames(model)
  vnmLstUpper <- txpUpperNames(model)
  allVnm <- unique(unlist(c(vnmLst, vnmLstLower, vnmLstUpper)))
  
  itfsLst <- txpTransFuncs(txpSlices(model))
  itfsLstLower <- txpLowerFuncs(txpSlices(model))
  itfsLstUpper <- txpUpperFuncs(txpSlices(model))

  #CHANGE THIS LOGIC TO AVOID DOING ANY TRANSFORMATIONS
  mat <- matrix(NA_real_, nrow = NROW(input), ncol = 0)
  for (i in allVnm) {#looks like they do the transform here, take that out
    mat <- cbind(mat, input[,i])
  }

  ## Determine colors
  nSlices <- length(slcVec)
  
  #fills <- sub("^#", "0x", fills)
  
  ## Rename any duplicated column names
  # if (any(duplicated(vnmVec))) {
  #   dup <- unique(vnmVec[duplicated(vnmVec)])
  #   for (i in seq_along(vnmLst)) {
  #     vnmLst[[i]] <- gsub(paste0('^(', paste(dup, collapse = '|'), ')$'), paste0('\\1_slice', i), vnmLst[[i]])
  #   }
  #   vnmVec <- unlist(vnmLst)
  # }

  #get metrics funcs as text strings
  metricFuncsText <- list()
  metricFuncsTextLower <- list()
  metricFuncsTextUpper <- list()
  
  for(sliceName in slcVec){
    tmpList <- c()
    for(funcName in names(itfsLst[[sliceName]])){
      func <- itfsLst[[sliceName]][[funcName]]
      tmpList <- c(tmpList, gsub(" ", "", paste0(deparse(body(func)), collapse = "")))
    }
    metricFuncsText[[sliceName]] <- tmpList
    
    tmpList <- c()
    for(funcName in names(itfsLstLower[[sliceName]])){
      func <- itfsLstLower[[sliceName]][[funcName]]
      tmpList <- c(tmpList, gsub(" ", "", paste0(deparse(body(func)), collapse = "")))
    }
    metricFuncsTextLower[[sliceName]] <- tmpList
    
    tmpList <- c()
    for(funcName in names(itfsLstUpper[[sliceName]])){
      func <- itfsLstUpper[[sliceName]][[funcName]]
      tmpList <- c(tmpList, gsub(" ", "", paste0(deparse(body(func)), collapse = "")))
    }
    metricFuncsTextUpper[[sliceName]] <- tmpList
  }

  ## Prepare the header
  toxpiRIndicator <- paste0("# Generated From toxpiR!", negativeHandling(model), "!", rankTies(model))
  toxpiRIndicator <- c(toxpiRIndicator, rep('', length(allVnm)))
  slcMeta <- paste(slcVec, slcWts, fills, tfsText, sep = "!")
  slcMeta <- paste("#", slcMeta)
  slcVnmInd <- as.data.frame(matrix(nrow = nSlices, ncol = length(allVnm)+1))
  slcVnmInd[,1] <- slcMeta
  row.names(slcVnmInd) <- slcVec
  colnames(slcVnmInd) <- c("slcMeta", allVnm)
  for (i in seq_along(slcVec)) { 
    slice <- slcVec[[i]]
    for(j in seq_along(allVnm)){
      metric <- allVnm[[j]]
      pos <- which(vnmLst[[slice]] %in% metric)
      if(length(pos) != 0){
        func <- metricFuncsText[[slice]][[pos]]
      } else {
        pos <- which(vnmLstLower[[slice]] %in% metric)
        if(length(pos) != 0){
          func <- metricFuncsTextLower[[slice]][[pos]]
        } else {
          pos <- which(vnmLstUpper[[slice]] %in% metric)
          if(length(pos) != 0){
            func <- metricFuncsTextUpper[[slice]][[pos]]
          } else {
            func <- ""
          }
        }
      }
      slcVnmInd[i,j+1] <- func
    }
  }
  hdr <- slcVnmInd
  ## Default names
  ids <- if (is.null(id.var)) 1:NROW(input) else input[[id.var]]
  
  ## Make final output
  hdr <- rbind(toxpiRIndicator, hdr)
  dat <- rbind(c('', allVnm), cbind(ids, mat))
  colnames(dat) <- colnames(hdr)

  out <- rbind(hdr, dat)
  ## Write csv file
  write.table(x = out, 
              file = fileName, 
              quote = TRUE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
  
}

##----------------------------------------------------------------------------##
