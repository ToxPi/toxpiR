##----------------------------------------------------------------------------##
## annScatterGrob -- not yet exported
##----------------------------------------------------------------------------##

#' @importFrom grDevices extendrange
#' @import grid

annScatterGrob <- function(x, y, ann = NULL, xlab = NULL, ylab = NULL,
                           xscale = NULL, yscale = NULL,
                           xaxis = TRUE, yaxis = TRUE,
                           margins = c(5.1, 4.1, 4.1, 2.1),
                           name = NULL, gp = NULL, vp = NULL, ...) {
  
  if (is.null(xscale)) xscale <- extendrange(range(x))
  if (is.null(yscale)) yscale <- extendrange(range(y))
  
  pltVp <- plotViewport(margins = margins, name = "annSctrPlotVp")
  datVp <- viewport(xscale = xscale, yscale = yscale, name = "annSctrDataVp")
  annSctrVp <- vpTree(pltVp, vpList(datVp))
  annSctr <- makeAnnScatter(x = x, 
                            y = y, 
                            ann = ann, 
                            xaxs = xaxis, 
                            yaxs = yaxis, 
                            xlab = xlab, 
                            ylab = ylab, 
                            vp = annSctrVp, 
                            ...)
  gTree(name = name,
        ann = ann,
        xscale = xscale,
        yscale = yscale,
        vp = vp,
        childrenvp = annSctrVp,
        children = annSctr,
        cl = "annScatterGrob")
  
}

grid.annScatterGrob <- function(x, y, ann = NULL, xlab = NULL, ylab = NULL,
                                xscale = NULL, yscale = NULL,
                                xaxis = TRUE, yaxis = TRUE,
                                margins = c(5.1, 4.1, 4.1, 2.1),
                                name = NULL, gp = NULL, vp = NULL, ...) {
  
  g <- annScatterGrob(x = x, 
                      y = y, 
                      ann = ann, 
                      xlab = xlab, 
                      ylab = ylab,
                      xaxis = xaxis, 
                      yaxis = yaxis,
                      xscale = xscale, 
                      yscale = yscale, 
                      margins = margins,
                      name = name, 
                      gp = gp, 
                      vp = vp, 
                      ...)
  grid.draw(g)
  
}

makeAnnScatter <- function(x, y, ann, xaxs, yaxs, xlab, ylab, vp, ...) {
  
  annLst <- vector(mode = "list", length = length(ann)) 
  nms <- names(ann)
  if (is.null(nms)) nms <- sprintf("ann-%s", seq_along(ann))
  for (i in seq_along(ann)) {
    ind <- ann[i]
    annLst[[i]] <- nullGrob(x = unit(x[ind], "native"), 
                            y = unit(y[ind], "native"), 
                            name = nms[i])
  }
  grbLst <- gList()
  grbLst[['annotations']] <- gTree(name = "annotations", 
                                   vp = vp,
                                   children = do.call("gList", annLst))
  grbLst[['sctr']] <- pointsGrob(x = x, y = y, vp = vp, ...)
  if (xaxs) grbLst[['xaxs']] <- grid.xaxis(draw = FALSE, vp = vp)
  if (yaxs) grbLst[['yaxs']] <- grid.yaxis(draw = FALSE, vp = vp)
  if (!is.null(xlab)) {
    grbLst[['xlab']] <- textGrob(xlab, y = unit(-3, "line"), vp = vp)
  }
  if (!is.null(ylab)) {
    grbLst[['ylab']] <- textGrob(ylab, 
                                 x = unit(-3, "line"), 
                                 vp = vp, 
                                 rot = 90)
  }
  
  grbLst
  
}
