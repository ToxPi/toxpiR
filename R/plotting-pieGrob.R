##----------------------------------------------------------------------------##
## pieGrob
##----------------------------------------------------------------------------##

#' @name pieGrob
#' @title Create a pie grob
#' @description Create a pie grob
#' @param rads Numeric, radius values for each slice from 0 to 1
#' @param fills Colors to fill the slices
#' @param wts Numeric, the relative portion of each slice
#' @param name,vp,gp Passed to [grid::gTree]
#' 
#' @details 
#' The default coloring can be set with `options("txp.fills")`.
#' 
#' 
#' @examples
#' library(grid)
#' 
#' s <- seq(0.2, 1, by = 0.1)
#' grid.newpage()
#' grid.pieGrob(rads = s)
#' grid.newpage()
#' grid.pieGrob(rads = s, wts = s)
#' 
#' curr_txp_fills <- options()$txp.fills
#' options(txp.fills = 1:8)
#' grid.newpage()
#' grid.pieGrob(rads = s)
#' options(txp.fills = curr_txp_fills)
#' 
#' ## Can edit
#' grid.newpage()
#' grid.pieGrob(rads = s, name = "myPie")
#' grid.ls() ## show the grid elements
#' grid.edit("myPie", fills = 1:9, wts = 9:1)
#' 
#' @return `pieGrob` [grid::grob] object
#' 
#' @import grid 
#' @export

pieGrob <- function(rads, fills = NULL, wts = NULL, 
                    name = NULL, vp = NULL, gp = NULL) {
  pieVp <- makePieViewport()
  gTree(name = name,
        rads = rads,
        fills = fills,
        wts = wts,
        gp = gp,
        vp = vp,
        childrenvp = pieVp,
        children = makePieGrob(rads = rads, 
                               fills = fills, 
                               wts = wts, 
                               vp = pieVp),
        cl = "pieGrob")
}

#' @rdname pieGrob
#' @export

grid.pieGrob <- function(rads, fills = NULL, wts = NULL, 
                         name = NULL, vp = NULL, gp = NULL) {
  g <- pieGrob(rads = rads, 
               fills = fills, 
               wts = wts, 
               name = name,
               vp = vp,
               gp = gp)
  grid.draw(g)
}

#' @export

editDetails.pieGrob <- function(x, specs) {
  if (any(c("rads", "fills", "wts") %in% names(specs))) {
    newRads <- if (is.null(specs$rads)) x$rads else specs$rads
    newFills <- if (is.null(specs$fills)) x$fills else specs$fills
    newWts <- if (is.null(specs$wts)) x$wts else specs$wts
    x <- setChildren(x, 
                     makePieGrob(rads = newRads, 
                                 fills = newFills, 
                                 wts = newWts,
                                 vp = x$childrenvp))
  }
  x
}

makeSliceGrob <- function(rad, th0, th1, fill, name = NULL, vp = NULL) {
  th <- c(seq(th0, th1, by = pi/360), th1)
  x <- c(0, cos(th))*rad
  y <- c(0, sin(th))*rad
  polygonGrob(x = x, 
              y = y, 
              name = name, 
              gp = gpar(fill = fill, col = NA),
              default.units = "native",
              vp = vp)
}

makePieGrob <- function(rads, fills = NULL, wts = NULL, vp = NULL) {
  nSlices <- length(rads)
  if (is.null(wts)) wts <- rep(1, nSlices)
  wts <- wts/sum(wts)
  ths <- cumsum(c(0, 2*pi*wts))
  
  if (is.null(fills)) fills <- getOption("txp.fills", TXP_FILLS)
  if (nSlices > length(fills)) fills <- colorRampPalette(fills)(nSlices)
  if (nSlices < length(fills)) fills <- fills[1:nSlices]
  
  slices <- vector("list", nSlices)
  for (i in seq_along(rads)) {
    slices[[i]] <- makeSliceGrob(rad = rads[i], 
                                 th0 = ths[i], 
                                 th1 = ths[i + 1], 
                                 fill = fills[i], 
                                 name = gPath(sprintf("slice%s", i)),
                                 vp = vp)
  }
  do.call("gList", slices)
}

makePieViewport <- function() {
  vpStack(viewport(layout = grid.layout(nrow = 1, ncol = 1, respect = TRUE)),
          viewport(name = "pievp",
                   layout.pos.row = 1,
                   layout.pos.col = 1,
                   xscale = c(-1, 1),
                   yscale = c(-1, 1)))
}

##----------------------------------------------------------------------------##

