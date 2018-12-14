#' Mosaic plot in ggplot-style
#'
#' Creates a nice-looking mosaic plot in ggplot-style.
#' @param data A dataset.
#' @param x .
#' @param y .
#' @param useNA If "no", the default.
#' @param ... Further inputs.
#' @author Dennis Freuer
#' @import ggplot2
#' @export
ggmosaic <- function(data, x, y, useNA="no", ...){
  xvar <- deparse(substitute(x))
  yvar <- deparse(substitute(y))
  mydata <- data[c(xvar, yvar)];
  mytable <- table(mydata, useNA=useNA);
  widths <- c(0, cumsum(apply(mytable, 1, sum)));
  heights <- apply(mytable, 1, function(x){c(0, cumsum(x/sum(x)))});

  alldata <- data.frame();
  allnames <- data.frame();
  for(i in 1:nrow(mytable)){
    for(j in 1:ncol(mytable)){
      alldata <- rbind(alldata, c(widths[i], widths[i+1], heights[j, i], heights[j+1, i]));
    }
  }
  colnames(alldata) <- c("xmin", "xmax", "ymin", "ymax")

  alldata[[xvar]] <- rep(dimnames(mytable)[[1]],rep(ncol(mytable), nrow(mytable)));
  alldata[[yvar]] <- rep(dimnames(mytable)[[2]],nrow(mytable));

  ggplot(alldata, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax)) +
    geom_rect(color="black", aes_string(fill=yvar)) +
    xlab(paste(xvar, "(count)")) + ylab(paste(yvar, "(proportion)"));
}
