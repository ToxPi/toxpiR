##----------------------------------------------------------------------------##
## boxLegendGrob
##----------------------------------------------------------------------------##

#' @name boxLegendGrob
#' @title Create a filled-box legend
#' @description Create a filled-box legend
#' @param labels Character, the legend labels
#' @param fills Colors to fill the slices
#' @param name,vp,gp Passed to [grid::frameGrob]
#' 
#' @details 
#' Not yet exported. Need to break out the creation of viewports and grobs as
#' done in the exported grobs. This will allow better grobEdit methods, which
#' also needs to be created for the boxLegendGrob.
#' Also need to do some input checks.
#' 
#' Also, if \code{grid::legendGrob} gets updated to use the 'has.fill' option 
#' this function should be removed and \code{grid::legendGrob} can be used
#' instead.
#' 
#' @import grid 

boxLegendGrob <- function(labels, fills, name = NULL, vp = NULL, gp = NULL) {
  
  wids <- c(unit(1.5, "char"), unit(max(nchar(labels)), "char"))
  hgts <- unit(rep_len(1.5, length(labels)), "char")
  fg <- frameGrob(layout = grid.layout(ncol = 2, 
                                       nrow = length(labels),
                                       widths = wids,
                                       heights = hgts),
                  vp = vp, 
                  name = name, 
                  gp = gp)
  for (i in seq_along(labels)) {
    fg <- placeGrob(frame = fg, 
                    grob = textGrob(label = labels[i]), 
                    col = 2, 
                    row = i)
    fg <- placeGrob(frame = fg, 
                    grob = rectGrob(width = unit(1, "char"),
                                    height = unit(1, "char"),
                                    gp = gpar(fill = fills[i], col = NA)),
                    col = 1, 
                    row = i)
  }
  
  fg

}

##----------------------------------------------------------------------------##

