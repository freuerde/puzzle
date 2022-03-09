#' Heatmap and ordered table of pairwise correlations
#'
#' This function is an easy way to get ordered bivariate correlations in a decreasing way and
#' a visualisation in the form of a heatmap. It is very useful for detection of the strongest and weakest
#' correlations, especially if the given dataset contains many variables. The function divides
#' automatically the data into numerical and categorical variables and applies the pearson
#' correlation on the numerical variables (categorical variables are allowed and will be ignored).
#' @param df A data frame (categorical variables are allowed and will be ignored).
#' @param print_cors If TRUE, the pairwise correlation will be printed to the console.
#' @param rnd The number of digits of the decimal place.
#' @author Dennis Freuer
#' @return Returns a list of length two - plot and data frame ordered by correlations.
#' @import ggplot2
#' @importFrom stats cor as.dist hclust
#' @export
#'
cormat <- function(df, print_cors=FALSE, rnd=NULL, method = c("pearson", "kendall", "spearman")) {
  library(data.table)
  if(!is.data.table(df)) { df <- data.table(df)}
  # divide the data into numerical and categorical variables:
  sa <- sapply(df, class)
  ind <- which(sa %in% "numeric")
  
  dnum <- df[, ind, with=FALSE] # all numerical variables
  dcat <- df[, -ind, with=FALSE] # all categorical variables
  
  ### correlations:
  cors <- cor(dnum, use="pairwise.complete.obs", method=method[1])
  
  # cluster correlations:
  dd <- as.dist((1-cors)/2)
  hc <- hclust(dd)
  cors <- cors[hc$order, hc$order]
  
  cors[upper.tri(cors, diag=TRUE)] <- NA # delete the upper triangle
  cors <- reshape2::melt(cors, na.rm=TRUE)
  
  g <- ggplot(data=cors, aes(x=Var1, y=Var2, fill=value)) + geom_tile() + 
    labs(x = NULL, y = NULL) #+ scale_fill_gradient("cor",limits = c(-1,1))
  
  
  cors <- cors[order(abs(cors$value), decreasing=TRUE),]
  names(cors)[3] <- "Pearson_Cor" 
  
  if(!is.null(rnd)) {cors$Pearson_Cor <- round(cors$Pearson_Cor, rnd)}
  names(cors)[3] <- paste0(method[1],"_cor")
  if(print_cors) {print(cors)}
  
  ### chi-square tests: (WAS IST MIT ALPHAKUMMULIERUNG???)
  
  
  return(list(plot=g,decreasing_correlations=cors))
}
