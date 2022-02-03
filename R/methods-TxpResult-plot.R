##----------------------------------------------------------------------------##
## plot methods for TxpResult
##----------------------------------------------------------------------------##

#' @name TxpResult-plot
#' @title Plot TxpResult objects
#' @description Plot [TxpResult] objects
#' @aliases plot
#' 
#' @param x [TxpResult] object
#' @param y Rank vector, i.e. `txpRanks(x)`
#' @param fills Vector of colors to fill slices
#' @param showScore Logical scalar, overall score printed below the name when
#' `TRUE`
#' @param labels Integer vector, indices of `x` to label in the rank plot
#' @param margins Passed to [grid::plotViewport]; only affects the scatterplot
#' region margins
#' @param gp,vp,name Passed to [grid::frameGrob] when creating the plotting 
#' area
#' @param newpage Logical scalar, [grid::grid.newpage] called prior to plotting
#' when `TRUE`
#' @param ... Passed to [pieGridGrob] when plotting ToxPi and to pointsGrob
#' when plotting ranks
#' 
#' @details 
#' It is strongly recommended to use a specific device (e.g., [grDevices::png],
#' [grDevices::pdf]) when creating rank plots. 
#' Using a GUI device will likely lead to inaccurate labeling, and any changes
#' to the device size WILL lead to inaccurate labeling. 
#' 
#' The plotting is built on the [grid::grid-package], and can be adjusted or
#' edited as such. 
#' 
#' If the labels are running of the device, the top or bottom margins can be 
#' increased with the `margins` parameter. 
#' 
#' @template roxgn-loadExamples
#' @template roxgn-calcTxpModel
#' 
#' @examples 
#' library(grid)
#' plot(res)
#' 
#' plot(res, txpRanks(res))
#' plot(res, txpRanks(res), pch = 16, size = unit(0.75, "char"))
#' 
#' ## Will likely make inaccurate labels within a GUI, e.g. RStudio
#' ## use png, pdf, etc. to get accurate labels
#' \dontrun{
#' tmpPdf <- tempfile()
#' pdf(tmpPdf)
#' plot(res, txpRanks(res), labels = c(10, 4, 2), pch = 16)
#' dev.off()
#' }
#' 
#' @return No return value; called for side effect (i.e. drawing in current
#' graphics device.)

NULL

#' @import grid

.TxpResult.toxpiPlot <- function(x, 
                                 fills = NULL, 
                                 showScore = TRUE,
                                 gp = NULL,
                                 vp = NULL,
                                 name = NULL,
                                 newpage = TRUE,
                                 ...) {
  
  if (is.null(fills)) fills <- getOption("txp.fills", TXP_FILLS)
  sNames <- names(txpSlices(x))
  pg <- pieGridGrob(txpSliceScores(x, adjusted = FALSE), 
                    wts = txpWeights(x),
                    labels = txpIDs(x),
                    fills = fills, 
                    showRadSum = showScore,
                    ...)
  lg <- boxLegendGrob(labels = sNames, fills = fills)
  wids <- unit(c(10, 1), "grobwidth", lg)
  fg <- frameGrob(layout = grid.layout(nrow = 1, ncol = 2, widths = wids), 
                  name = name, 
                  gp = gp, 
                  vp = vp)
  fg <- placeGrob(frame = fg, grob = pg, row = 1, col = 1)
  fg <- placeGrob(frame = fg, grob = lg, row = 1, col = 2)
  if (newpage) grid.newpage()
  grid.draw(fg)
  
}

#' @describeIn TxpResult-plot Plot ToxPi diagrams
#' @export

setMethod("plot", c("TxpResult", "missing"), .TxpResult.toxpiPlot)

#' @importFrom rlang is_named is_integerish is_scalar_logical
#' @import grid

.TxpResult.rankPlot <- function(x, y, labels = NULL, newpage = TRUE,
                                margins = c(4, 0, 1, 1),
                                name = NULL, gp = NULL, vp = NULL, ...) {
  
  stopifnot(is_scalar_logical(newpage))
  stopifnot(is.null(labels) || is_integerish(labels))
  
  drawLabels <- !is.null(labels)
  
  if (newpage) grid.newpage()
  
  if (drawLabels) {
    stopifnot(is_named(x))
    names(labels) <- txpIDs(x[labels])
    labelWidth <- .maxStrWidth(names(labels)) + unit(5, "char")
  } else {
    labelWidth <- unit(0, "mm")
  }
  
  gl <- grid.layout(nrow = 1, ncol = 2, unit.c(labelWidth, unit(1, "null")))
  
  fg <- frameGrob(layout = gl, name = name, gp = gp, vp = vp)
  
  rnk <- annScatterGrob(x = txpScores(x), 
                        y = y,
                        ann = if (drawLabels) labels else NULL,
                        yscale = rev(extendrange(range(y))),
                        yaxis = FALSE,
                        xlab = "ToxPi Score",
                        margins = margins,
                        ...)
  
  fg <- placeGrob(frame = fg, grob = rnk, row = 1, col = 2)
  grid.draw(fg)
  
  if (drawLabels) {
    lblGrob <- .refLabel(names(labels), labelWidth)
    fg <- placeGrob(frame = fg, grob = lblGrob, row = 1, col = 1)
    grid.draw(fg$children[fg$childrenOrder[2]])
  }
  
}

#' @describeIn TxpResult-plot Plot ToxPi ranks
#' @export

setMethod("plot", c("TxpResult", "numeric"), .TxpResult.rankPlot)

.maxStrWidth <- function(x) {
  wids <- lapply(x, stringWidth)
  wids[[which.max(sapply(wids, convertWidth, "inches"))]]
}

.refLabel <- function(lbl, xloc) {
  
  yloc <- do.call("unit.c", sapply(lbl, .getDeviceLoc)["y", ])
  ord <- order(yloc)
  yloc <- yloc[ord]
  lbl <- lbl[ord]
  
  n <- length(lbl)
  ypos <- yloc
  wd <- convertUnit(unit(1, "char"), "in")
  ht <- wd*1.2
  mid <- (n + 1) %/% 2
  # ypos[mid] <- yloc[mid]
  if (n > 1) {
    for (i in (mid + 1):n) {
      ypos[i] <- max(yloc[i], ypos[i - 1] + ht)
    }
  }
  if (n > 2) {
    for (i in (mid - 1):1) {
      ypos[i] <- min(yloc[i], ypos[i + 1] - ht)
    }
  }
  
  x1 <- rep(xloc, n)
  x2 <- x1 - 0.5*wd
  x3 <- x2 - 2*wd
  x4 <- x3 - 0.5*wd
  
  s1 <- segmentsGrob(x0 = unit(x1, "npc"), 
                     y0 = unit(yloc, "npc"), 
                     x1 = unit(x2, "npc"), 
                     y1 = unit(yloc, "npc"))
  s2 <- segmentsGrob(x0 = unit(x2, "npc"), 
                     y0 = unit(yloc, "npc"), 
                     x1 = unit(x3, "npc"), 
                     y1 = unit(ypos, "npc"))
  s3 <- segmentsGrob(x0 = unit(x3, "npc"), 
                     y0 = unit(ypos, "npc"), 
                     x1 = unit(x4, "npc"), 
                     y1 = unit(ypos, "npc"))
  tg <- textGrob(label = lbl, x = wd, y = ypos, just = "left")
  
  gTree(children = gList(s1, s2, s3, tg))
  
}

.getDeviceLoc <- function(x, units = "npc") {
  xPth <- grid.grep(x, viewports = TRUE, global = TRUE)[[1]]
  depth <- downViewport(attr(xPth, "vpPath"))
  xGrb <- grid.get(xPth)
  loc <- deviceLoc(xGrb$x, xGrb$y)
  upViewport(depth)
  loc
}

##----------------------------------------------------------------------------##

