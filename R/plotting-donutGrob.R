##----------------------------------------------------------------------------##
## donutGrob
##----------------------------------------------------------------------------##

#' @name donutGrob
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
#' grid.donutGrob(rads = s)
#' grid.newpage()
#' grid.donutGrob(rads = s, wts = s)
#' 
#' curr_txp_fills <- options()$txp.fills
#' options(txp.fills = 1:8)
#' grid.newpage()
#' grid.donutGrob(rads = s)
#' options(txp.fills = curr_txp_fills)
#' 
#' ## Can edit
#' grid.newpage()
#' grid.donutGrob(rads = s, name = "myPie")
#' grid.ls() ## show the grid elements
#' grid.edit("myPie", fills = 1:9, wts = 9:1)
#' 
#' @return `donutGrob` [grid::grob] object
#' 
#' @import grid 
#' @export

donutGrob <- function(rads, fills = NULL, wts = NULL, 
                      name = NULL, vp = NULL, gp = NULL) {
  vpname <- if (is.null(name)) "donutvp" else paste0(name, "-vp")
  donutVp <- makeSquareViewport(vpname)
  gTree(name = name,
        rads = rads,
        fills = fills,
        wts = wts,
        gp = gp,
        vp = vp,
        childrenvp = pieVp,
        children = makedonutGrob(rads = rads, 
                                 fills = fills, 
                                 wts = wts, 
                                 vp = donutVp),
        cl = "donutGrob")
}

#' @rdname donutGrob
#' @export

grid.donutGrob <- function(rads, fills = NULL, wts = NULL, 
                           name = NULL, vp = NULL, gp = NULL) {
  g <- donutGrob(rads = rads, 
                 fills = fills, 
                 wts = wts, 
                 name = name,
                 vp = vp,
                 gp = gp)
  grid.draw(g)
}

#' @export

editDetails.donutGrob <- function(x, specs) {
  if (any(c("rads", "fills", "wts") %in% names(specs))) {
    newRads <- if (is.null(specs$rads)) x$rads else specs$rads
    newFills <- if (is.null(specs$fills)) x$fills else specs$fills
    newWts <- if (is.null(specs$wts)) x$wts else specs$wts
    x <- setChildren(x, 
                     makedonutGrob(rads = newRads, 
                                   fills = newFills, 
                                   wts = newWts,
                                   vp = x$childrenvp))
  }
  x
}

makeDonutSliceGrob <- function(ir, or, t0, t1, fill, name = NULL, vp = NULL) {
  th <- c(seq(t0, t1, by = pi/360), t1)
  x <- c(0, cos(th))*rad
  y <- c(0, sin(th))*rad
  polygonGrob(x = x, 
              y = y, 
              name = name, 
              gp = gpar(fill = fill, col = NA),
              default.units = "native",
              vp = vp)
}

makedonutGrob <- function(rads, fills = NULL, wts = NULL, vp = NULL) {
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

##----------------------------------------------------------------------------##

