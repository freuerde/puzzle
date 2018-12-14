#' Plots several ggplots in one
#'
#' This function plots several ggplots in one single graphic with user defined number of columns.
#' @param plotlist List of ggplots.
#' @param cols Number of columns within the desired plot.
#' @param layout User defined layout.
#' @author Dennis Freuer
#' @import ggplot2 grid
#' @export
#'
multiplot <- function(plotlist = NULL, cols = 1, layout = NULL) {
  plots <- plotlist

  numPlots = length(plots)

  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots == 1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
