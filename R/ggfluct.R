#' Fluctuation plot
#'
#' This function generates a fluctuation plot in ggplot-style.
#' @param tbl A 2-dim table (cross-table).
#' @param type Represents the type of boxes within the plot.
#' @param boxFill String that represents the colour of boxes within the plot.
#' @param floor Controls the min-size of the boxes within the plot.
#' @param ceiling Controls the max-size of the boxes within the plot.
#' @param xlab Character string representing the label of  the x-axis.
#' @param ylab Character string representing the label of  the y-axis.
#' @author Dennis Freuer
#' @import ggplot2
#' @export
#'
ggfluct <-function(table, type = "size", boxFill = "grey90",
                   floor = 0, ceiling = max(table$freq,na.rm = TRUE),
                   xlab="", ylab=""){
  require(ggplot2)
  # base code from ggplot2:ggfluctionation
  
  if (is.table(table))
    table <- as.data.frame(t(table))
  oldnames <- names(table)
  names(table) <- c("x", "y", "result")
  table <- transform(table, x = as.factor(x), y = as.factor(y),
                     freq = result)
  if (type == "size") {
    table <- transform(table, freq = sqrt(pmin(freq, ceiling)/ceiling),
                       border = boxFill)
    table[is.na(table$freq), "freq"] <- 1
    table <- subset(table, freq * ceiling >= floor)
  }
  if (type == "size") {
    nx <- length(levels(table$x))
    ny <- length(levels(table$y))
    p <- ggplot(table, aes_string(x = "x", y = "y", height = "freq",
                                  width = "freq", fill = "border")) +
      
      geom_tile(colour = "black") +
      
      scale_fill_identity() + theme(aspect.ratio = ny/nx) +
      xlab(xlab) + ylab(ylab) +
      theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
  }
  else {
    p <- ggplot(table, aes_string(x = "x", y = "y", fill = "freq")) +
      geom_tile(colour = "grey50") + scale_fill_gradient2(low = "white",
                                                          high = "darkgreen")
  }
  p$xlabel <- oldnames[1]
  p$ylabel <- oldnames[2]
  p
}
