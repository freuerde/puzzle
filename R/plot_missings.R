#' Positions and number of missings within a dataset
#'
#' This function plots the position of missings within a given dataset and
#' print the variables with the number of missings in an ordered way.
#' @param d Data frame.
#' @param print_res Controls whether the column names and counted missings should be printed at the console.
#' @param decreasing If TRUE, the variable names will be shown at the console
#' in a decreasing way. In the same order as in the data frame otherwise.
#' @author Dennis Freuer
#' @return Returns a list of length two - plot and data frame ordered by correlations.
#' @import ggplot2
#' @export
#'
plot_missings <- function(d, print_res=TRUE, decreasing=TRUE){
  d <- d[order(-as.integer(row.names(d))),]
  ina <- is.na(d)
  ina <- ifelse(ina==TRUE, 1, 0)
  yticks <- which(rowSums(ina) > 0)
  numb_nas <- colSums(ina)
  xlabs <- names(d)
  ina <- as.data.frame(ina)
  ina$rownr <- rownames(ina)
  suppressMessages(ina <- melt(ina))

  if(print_res){print(sort(numb_nas, decreasing = decreasing))}

  return( ggplot(data=ina, aes(x=variable, y=rownr, fill=value)) +
            geom_tile(show.legend=F) + ylab("rows") + xlab("number of NA's") +
            theme(axis.text.y=element_blank()) +
            scale_x_discrete(labels = paste0(xlabs, "\n", numb_nas)) +
            scale_y_discrete(breaks=yticks) )
}
