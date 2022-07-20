##----------------------------------------------------------------------------##
## txpExportGui
##----------------------------------------------------------------------------##

#' @name txpExportGui
#' @title Export comma-separated file intended for ToxPi GUI
#' @description Export comma-separated file intended for ToxPi GUI
#' 
#' @param fileName Character scalar, the path to the output file
#' @inheritParams txpCalculateScores 
#' @inheritParams pieGrob
#' 
#' @details 
#' The GUI differs in two meaninful ways for exporting `toxpiR` models: (1) the
#' GUI only allows for integer weights; (2) the GUI applies transformation 
#' functions differently. 
#' 
#' `txpExporGui` will not work for models with non-integer weights.
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

txpExportGui <- function(fileName = "txpModel.csv", 
                         input, 
                         model, 
                         id.var = NULL,
                         fills = NULL) {
  
  ## TODO: fileName checks, can it be written? does it already exist? etc.
  
  stopifnot(is_scalar_character(fileName))
  
  ## Test inputs
  .chkModelInput(model = model, input = input)
  
  ## Clean up infinite in input
  input <- .rmInfinite(model, input)
  
  slcWts <- txpWeights(model)
  if (any(slcWts%%1 != 0)) {
    stop("ToxPi GUI only allows integer weights in the model.")
  }
  
  ## Check for slice-level transformations 
  tfs <- txpTransFuncs(model)
  if (any(!sapply(tfs, is.null))) {
    ## Output as completely transformed slice values
    warning("Model contains slice-level transformation; export will not ",
            "contain input-level data. See ?txpExportGui for more ",
            "information.")
    res <- .calculateScores(model = model, input = input)
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
  
  ## Write csv file
  write.table(x = out, 
              file = fileName, 
              quote = FALSE, 
              sep = ",", 
              row.names = FALSE, 
              col.names = FALSE)
  
}

##----------------------------------------------------------------------------##

