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
cormat <- function(df, print_cors=FALSE, rnd=NULL) {
  if(!data.table::is.data.table(df)) { df <- data.table::data.table(df)}
  sa <- sapply(df, class)
  ind <- which(sa %in% c("numeric","integer"))

  dnum <- df[, ind, with=FALSE]
  dcat <- df[, -ind, with=FALSE]

  cors <- cor(dnum, use="pairwise.complete.obs")

  dd <- as.dist((1-cors)/2)
  hc <- hclust(dd)
  cors <- cors[hc$order, hc$order]

  cors[upper.tri(cors, diag=TRUE)] <- NA
  cors <- reshape::melt(cors, na.rm=TRUE)

  g <- ggplot(data=cors, aes(x=Var1, y=Var2, fill=value)) +
    geom_tile()

  cors <- cors[order(abs(cors$value), decreasing=TRUE),]
  names(cors)[3] <- "Pearson_Cor"

  if(!is.null(rnd)) {cors$Pearson_Cor <- round(cors$Pearson_Cor, rnd)}
  if(print_cors) {print(cors)}

  return(list(plot=g,decreasing_correlations=cors))
}
