##----------------------------------------------------------------------------##
## pieGridGrob
##----------------------------------------------------------------------------##

#' @name pieGridGrob
#' @title Make grid of pieGrobs
#' @description Make grid of pieGrobs
#' 
#' @param radMat `matrix(<numeric>)`, observations by slice radii
#' @param wts `vector(<numeric>)`, relative weights of each slice
#' @param fills Vector of colors to fill slices
#' @param labels `vector(<character>)`, (optional) label for each observation
#' @param showRadSum Logical scalar, when `TRUE` show the weighted sum of slices
#' below the label
#' @param nrow,ncol Integer scalar, number of rows and columns for the grid
#' @param byrow Logical scalar, fill the grid by rows when `TRUE`
#' @param name,gp,vp Passed to [grid::gTree]
#' 
#' @examples 
#' \donttest{
#' library(grid)
#' 
#' s <- seq(0.2, 1, by = 0.1)
#' smat <- do.call("rbind", replicate(20, s, simplify = FALSE))
#' grid.newpage()
#' grid.pieGridGrob(radMat = smat)
#' 
#' rownames(smat) <- sprintf("obs%02d", 1:20)
#' grid.newpage()
#' grid.pieGridGrob(radMat = smat, wts = s)
#' grid.newpage()
#' grid.pieGridGrob(radMat = smat, wts = s, showRadSum = TRUE, labels = FALSE)
#' grid.newpage()
#' grid.pieGridGrob(radMat = smat, labels = "hello")
#' grid.newpage()
#' grid.pieGridGrob(radMat = smat, labels = 1:20)
#' 
#' ## Can edit like normal grid objects
#' grid.newpage()
#' grid.pieGridGrob(radMat = smat, wts = s, showRadSum = TRUE)
#' grid.ls() ## shows grid elements
#' grid.edit("pie-20", fills = 1:9)
#' grid.edit("pie-19-label", gp = gpar(font = 2, col = "red"))
#' grid.edit("pie-1", wts = rep(1, 9), rads = rep(1, 9))
#' for (s in sprintf("pie-%d-radSum", 2:4)) {
#'   grid.edit(s, gp = gpar(font = 2, col = "blue"))
#' }
#' }
#' 
#' @return `pieGrob` [grid::grob] object
#' 
#' @import grid
#' @importFrom rlang is_scalar_integerish is_scalar_logical
#' @importFrom grDevices colorRampPalette
#' @export

pieGridGrob <- function(radMat, wts = NULL, fills = NULL, labels = NULL, 
                        showRadSum = FALSE, ncol = NULL, nrow = NULL, 
                        byrow = TRUE, name = NULL, gp = NULL, vp = NULL) {
  
  nPie <- NROW(radMat)
  if (is.null(wts)) wts <- rep(1, NCOL(radMat))
  wts <- wts/sum(wts)
  if (is.null(labels) || (is.logical(labels) && labels)) {
    labels <- rownames(radMat) 
  } else {
    if (is.logical(labels) && !labels) labels <- NULL
  }
  pos <- makePieGridPos(nPie = nPie, nrow = nrow, ncol = ncol, byrow = byrow)
  pieGridVp <- makePieGridViewport(pos = pos, nPie = nPie)
  gTree(radMat = radMat,
        wts = wts,
        fills = fills,
        labels = labels,
        nPie = nPie,
        pos = pos,
        name = name,
        gp = gp,
        vp = vp,
        children = makePieGridGrob(radMat = radMat, 
                                   pos = pos, 
                                   wts = wts, 
                                   fills = fills, 
                                   labels = labels, 
                                   showRadSum = showRadSum,
                                   vp = pieGridVp),
        childrenvp = pieGridVp,
        cls = "pieGridGrob")
  
}

#' @rdname pieGridGrob
#' @export

grid.pieGridGrob <- function(radMat, wts = NULL, fills = NULL, labels = NULL, 
                             showRadSum = FALSE, ncol = NULL, nrow = NULL, 
                             byrow = TRUE, name = NULL, gp = NULL, vp = NULL) {
  g <- pieGridGrob(radMat = radMat,
                   wts = wts, 
                   fills = fills,
                   labels = labels,
                   showRadSum = showRadSum,
                   ncol = ncol,
                   nrow = nrow,
                   byrow = byrow,
                   name = name,
                   vp = vp,
                   gp = gp)
  grid.draw(g)
}

makePieGridPos <- function(nPie, nrow = NULL, ncol = NULL, byrow = TRUE) {
  stopifnot(is_scalar_integerish(nPie))
  stopifnot(is.null(ncol) || is_scalar_integerish(ncol))
  stopifnot(is.null(nrow) || is_scalar_integerish(nrow))
  stopifnot(is_scalar_logical(byrow))
  if (is.null(nrow) && is.null(ncol)) {
    ncol <- ceiling(sqrt(nPie))
    nrow <- ceiling(nPie/ncol)
  } else {
    if (is.null(nrow)) nrow <- ceiling(nPie/ncol)
    if (is.null(ncol)) ncol <- ceiling(nPie/nrow)
  }
  
  if(byrow) {
    pos <- expand.grid(col = 1:ncol, row = 1:nrow) 
  } else {
    pos <- expand.grid(row = 1:nrow, col = 1:ncol)
  }
  pos
}

makePieGridViewport <- function(pos, nPie) {
  nrow <- max(pos$row)
  ncol <- max(pos$col)
  gl <- grid.layout(nrow = nrow, 
                    ncol = ncol, 
                    widths = unit(rep_len(1, ncol), "null"),
                    heights = unit(rep(1, nrow), "null"))
  gridVp <- viewport(layout = gl)
  pieBoxes <- vector("list", nPie)
  for (i in 1:nPie) {
    pieBoxes[[i]] <- viewport(name = sprintf("pieBox-%s", i),
                              layout.pos.row = pos[i, "row"],
                              layout.pos.col = pos[i, "col"])
  }
  vpTree(gridVp, do.call("vpList", pieBoxes))
}

makePieGridGrob <- function(radMat, pos, wts = NULL, fills = NULL, 
                            labels = NULL, showRadSum = FALSE, vp = NULL) {
  
  nPie <- NROW(radMat)
  pies <- vector("list", nPie)
  
  for (i in 1:nPie) {
    rads <- radMat[i, ]
    pies[[i]] <- frameGrob(vp = vpStack(vp$parent, vp$children[[i]]))
    pies[[i]] <- packGrob(frame = pies[[i]],
                          grob = pieGrob(rads = rads,
                                         fills = fills,
                                         wts = wts,
                                         name = sprintf("pie-%s", i)))
    if (showRadSum) {
      rsum <- round(sum(rads*wts, na.rm = TRUE), 4)
      pies[[i]] <- packGrob(frame = pies[[i]], 
                            grob = textGrob(label = rsum,
                                            name = sprintf("pie-%s-radSum", i)),
                            side = "top",
                            height = unit(1.15, units = "char"))
    }
    
    if (!is.null(labels)) {
      pies[[i]] <- packGrob(frame = pies[[i]], 
                            grob = textGrob(label = labels[i],
                                            name = sprintf("pie-%s-label", i)),
                            side = "top",
                            height = unit(1.15, units = "char"))
    }
  }
  
  do.call("gList", pies)
  
}

##----------------------------------------------------------------------------##

