#' Rowwise missing values
#'
#' This function counts for each row of a dataset the number of missing values (NAs).
#' @param dt data frame or data table with missing values in any row.
#' @author Dennis Freuer
#' @return Returns a numerical vector of lenghth nrow(d).
#' @import data.table
#' @export
#'
rowwise_na <- function(dt) {
  if(!is.data.table(dt)){dt <- as.data.table(dt)}
  dt[,numb_na := apply(apply(dt, MARGIN = 1, is.na), MARGIN = 2, sum)]
  return(dt$numb_na)
}
