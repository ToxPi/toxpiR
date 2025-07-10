##----------------------------------------------------------------------------##
## txpExportCSV
##----------------------------------------------------------------------------##

#' @name txpExportCSV
#' @title Export comma-separated file containing txpModel and raw data
#' @description Export comma-separated file containing txpModel and raw data
#' 
#' @param fileName Character scalar, the path to the output file
#' @param format Character scalar, export format for csv file. Options are 
#' 'toxpiR' and 'gui'. Defaults to 'toxpiR'.
#' @inheritParams txpCalculateScores 
#' @inheritParams pieGrob
#' 
#' @details 
#' This function exports data and models to csv files for easy sharing and 
#' recreation. The suggested format is toxpiR, whereas gui is provided for 
#' backwards compatibility with the toxpi GUI. The GUI format differs in several
#' meaningful ways: (1) the GUI only allows for integer weights; (2) the GUI can
#' only apply metric transformations; (3) all metrics within a slice undergo one 
#' common metric transformation; (4) metric transformations can only be chosen 
#' from a predefined subset; (5) negative handling can only be treated as 
#' missing; (6) the GUI doesn't allow for user provided confidence intervals 
#' 
#' `txpExportCSV` with `format = "gui"` will not work for models with 
#' non-integer weights, negative handling set to missing, nor slices containing 
#' upper/lower CIs
#' 
#' The GUI only applies a single transformation function to every input within
#' a slice, and only functions from a pre-determined list; `toxpiR` allows 
#' users to apply any valid function individually to each input, then a second
#' transformation function on the summed slice values. Because of this 
#' complexity, any exported models with slice-level transformation functions
#' will not export at the input level. In other words, the export will have only
#' the final slice scores. Otherwise, all input-level transformations will be 
#' performed, and the export will contain transformed input-level data with
#' the `linear(x)` GUI transformation.
#' 
#' @importFrom rlang is_scalar_character
#' @importFrom utils write.table
#' @export

txpExportCSV <- function(fileName = "txpModel.csv", 
                         input, 
                         model, 
                         id.var = NULL,
                         fills = NULL,
                         format = "toxpiR") {
  
  stopifnot(is_scalar_character(fileName))
  stopifnot(is_scalar_character(format))
  if(!(format %in% c("toxpiR", "gui"))){stop("Invalid format parameter. Valid options are 'toxpiR' and 'gui'.")}
  if (!is.null(id.var) && !(id.var %in% colnames(input))){stop(paste0("Provided id.var (", id.var, ") not found in provided input"))}
  
  ## Test inputs
  .chkModelInput(model = model, input = input, id.var = id.var)
  
  ## Clean up infinite in input
  input <- .rmInfinite(model, input)
  
  if(format == "toxpiR"){
    output <- .toToxpiR(input, 
                        model, 
                        id.var = id.var,
                        fills = fills)
  } else if(format == "gui"){
    output <- .toGUI(input, 
                     model, 
                     id.var = id.var,
                     fills = fills)
  }
  
  ## Write csv file
  write.table(x = output, 
              file = fileName, 
              quote = TRUE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
}

.toToxpiR <- function(input, 
                      model, 
                      id.var = NULL,
                      fills = NULL){
  slcWts <- txpWeights(model)
  
  ## Check for slice-level transformations 
  tfs <- txpTransFuncs(model)
  
  #get text for slice tfs
  tfsText <- lapply(
    tfs,
    function(func) {
      if (is.null(func)) return("x")
      gsub(" ", "", paste0(deparse(body(func)), collapse = ""))
    }
  )
  
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
  if (is.null(fills)) fills <- getOption("txp.fills", TXP_FILLS)
  if (nSlices > length(fills)) fills <- colorRampPalette(fills)(nSlices)
  if (nSlices < length(fills)) fills <- fills[1:nSlices]
  fills <- .col2hex(fills)
  
  #get metrics funcs as text strings
  metricFuncsText <- list()
  metricFuncsTextLower <- list()
  metricFuncsTextUpper <- list()

  for(sliceName in slcVec){
    tmpList <- c()

    for(index in seq_along(itfsLst[[sliceName]])){
      func <- itfsLst[[sliceName]][[index]]
      xfunc <- .setFuncArg(func, "x")
      tmpList <- c(tmpList, .getFuncBodyText(xfunc))
    }
    metricFuncsText[[sliceName]] <- tmpList
    
    tmpList <- c()
    for(index in seq_along(itfsLstLower[[sliceName]])){
      func <- itfsLstLower[[sliceName]][[index]]
      lfunc <- .setFuncArg(func, "l")
      tmpList <- c(tmpList, .getFuncBodyText(lfunc))
    }
    metricFuncsTextLower[[sliceName]] <- tmpList
    
    tmpList <- c()
    for(index in seq_along(itfsLstUpper[[sliceName]])){
      func <- itfsLstUpper[[sliceName]][[index]]
      ufunc <- .setFuncArg(func, "u")
      tmpList <- c(tmpList, .getFuncBodyText(ufunc))
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
}

.toGUI <- function(input, 
                   model, 
                   id.var = NULL,
                   fills = NULL){

  slcWts <- txpWeights(model)
  if (any(slcWts%%1 != 0)) {
    stop("ToxPi GUI only allows integer weights in the model.")
  }
  if (slot(model, "negativeHandling") != "missing") {
    stop("ToxPi GUI only compatible with models containing negativeHandling of 'missing'.")
  }
  if (any(!sapply(txpLowerNames(model), is.null)) || any(!sapply(txpUpperNames(model), is.null))) {
    stop("ToxPi GUI not compatible for models containing slices with confidence intervals.")
  }

  ## Check for slice-level transformations 
  tfs <- txpTransFuncs(model)
  if (any(!sapply(tfs, is.null))) {
    ## Output as completely transformed slice values
    warning("Model contains slice-level transformation; export will not ",
            "contain input-level data. See ?txpExportGui for more ",
            "information.")
    res <- .calculateScores(model = model, input = input, id.var = id.var)
    mat <- txpSliceScores(res)
    slcVec <- vnmVec <- colnames(mat)
  } else {
    ## Notes: may duplicate inputs because the same input in different slices
    ## can have different transformation functions
    vnmVec <-  txpValueNames(model, simplify = TRUE)
    slcVec <- names(model)
    vnmLst <-  txpValueNames(model)
    itfsLst <- txpTransFuncs(txpSlices(model))
    matLst <- list()
    for (i in seq_along(vnmLst)) {
      mat <- matrix(NA_real_, nrow = NROW(input), ncol = length(vnmLst[[i]]))
      for (j in seq_along(vnmLst[[i]])) {
        if (is.null(itfsLst[[i]][[j]])) {
          mat[ , j]  <- input[ , vnmLst[[i]][[j]]]
        } else {
          mat[ , j] <- itfsLst[[i]][[j]](input[ , vnmLst[[i]][[j]]])
        }
      }
      # Make sure transformed values are positive
      minMat <- min(mat[is.finite(mat)])
      if (minMat < 0) {
        x <- -floor(minMat)
        # If slices contain multiple components and any missing values, then those
        # missing values must be replaced with the added constant to produce the same
        # slice/toxpi scores because slice scores are computed by sum not mean
        # However, if all values are missing in a row, the leave it alone
        if (ncol(mat) > 1 & any(!is.finite(mat))) {
          idxNA <- apply(mat, 1, function(x) all(is.na(x)))
          mat[!idxNA & !is.finite(mat)] <- 0
          warning("Slice \"", slcVec[i], "\" contains both missing and negative values ",
                  "after applying transformations so missing values were replaced with 0 ",
                  "and then all values were increased by x = ", x, ".")
        } else {
          warning("Slice \"", slcVec[i], "\" contains negative values ",
                  "after applying transformations so all values were increased by x = ", x, ".")
        }
        # Shift all values by a constant to make them positive
        mat <- mat + x
      }
      matLst[[i]] <- mat
    }
    mat <- do.call(cbind, matLst)
  }
  
  ## Make infinite NaN
  mat[is.infinite(mat)] <- NaN
  
  ## Determine colors
  nSlices <- length(slcVec)
  if (is.null(fills)) fills <- getOption("txp.fills", TXP_FILLS)
  if (nSlices > length(fills)) fills <- colorRampPalette(fills)(nSlices)
  if (nSlices < length(fills)) fills <- fills[1:nSlices]
  
  fills <- .col2hex(fills)
  fills <- sub("^#", "0x", fills)
  
  ## Rename any duplicated column names
  vnmLst <- txpValueNames(model)
  if (any(duplicated(vnmVec))) {
    dup <- unique(vnmVec[duplicated(vnmVec)])
    for (i in seq_along(vnmLst)) {
      vnmLst[[i]] <- gsub(paste0('^(', paste(dup, collapse = '|'), ')$'), paste0('\\1_slice', i), vnmLst[[i]])
    }
    vnmVec <- unlist(vnmLst)
  }
  
  ## Prepare the header
  slcMeta <- paste(slcVec, slcWts, fills, "linear(x)", sep = "!")
  slcMeta <- paste("#", slcMeta)
  slcVnmInd <- vector(mode = "list", length = nSlices)
  names(slcVnmInd) <- slcVec
  for (i in slcVec) {
    slcVnmInd[[i]] <- rep('', length(vnmVec))
    slcVnmInd[[i]][vnmVec %in% vnmLst[[i]]] <- 'x' 
  }
  hdr <- cbind(slcMeta, do.call(rbind, slcVnmInd))
  
  ## Default names
  ids <- if (is.null(id.var)) 1:NROW(input) else input[[id.var]]
  
  ## Make final output
  out <- rbind(hdr, c('', vnmVec), cbind(ids, mat))
  
  return(out)
}

.setFuncArg <- function(func, arg_name) {
  if (is.null(func)) {
    arg_sym <- as.name(arg_name)
    return(eval(call("function", setNames(as.pairlist(alist(x = )), arg_name), arg_sym)))
  }
  
  orig_arg <- names(formals(func))[1]
  new_formals <- setNames(as.pairlist(alist(x = )), arg_name)
  new_body <- do.call(substitute, list(body(func), setNames(list(as.name(arg_name)), orig_arg)))
  eval(call("function", new_formals, new_body))
}

.getFuncBodyText <- function(func) {
  gsub(" ", "", paste0(deparse(body(func)), collapse = ""))
}
##----------------------------------------------------------------------------##
