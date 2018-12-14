#' Creates ggplot2 default colours
#'
#' Creates the colour code for the first n ggplot2 default colours.
#' @param n An integer value representing the desired number of colours.
#' @param h A numeric vector for adjusting the colours.
#' @return Returns a character vector of length n that contains the colour code.
#' @author Dennis Freuer
#' @importFrom grDevices hcl
#' @export
#'
ggcol <- function(n=3, h=c(0, 360) +15){
  if ((diff(h)%%360) < 1) h[2] <- h[2] - 360/n
  hcl(h = (seq(h[1], h[2], length = n)), c = 100, l = 65)
}
