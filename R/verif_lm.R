#' Advanced residual plots and tests for linear models
#'
#' Function for creating residual plots in ggplot style and all necessary tests,
#' so that the assumptions of linear regression models can be verified.
#' @param mdl A linear model of class lm.
#' @param results Choose "print", if results should be printed, but not returned.
#' Choose "return" if results should be returned in a list, but not printed.
#' @return Returns, if desired, a list of length 6 with an summary of tests,
#' all calculated tests and plots.
#' @author Dennis Freuer
#' @importFrom MASS stdres
#' @importFrom stats dnorm shapiro.test
#' @importFrom lmtest bptest dwtest
#' @importFrom gridExtra grid.arrange
#' @importFrom car vif
#' @import ggplot2 ggfortify
#' @export
#'
verif_lm <- function(mdl, results=c("print","return")){
  sres <- stdres(mdl)
  bp <- bptest(mdl)
  dw <- dwtest(mdl, alternative="two.sided")
  sw <- shapiro.test(sres)

  g <- ggplot2::autoplot(mdl, which = 1:6, ncol = 3, label.size = 3)
  g2 <- ggplot(data=data.frame(), aes(sres)) +
    geom_histogram(aes(y=..density..), fill="grey40", color="black", bins=30) +
    ggtitle("Residuals vs Normal") +
    stat_function(fun=function(x) dnorm(x), color="blue") +
    ylab("aa") + xlab("Standardized Residuals")

  grid.arrange(g[[1]],g[[2]],g2,g[[3]],g[[5]],g[[6]], nrow=2)

  sw$method <- "Shapiro-Wilk normality test of standardised residuals"
  bp$method <- "Breusch-Pagan homoscedasticity test"
  dw$method <- "Durbin-Watson autocorrelation test"
  sw$data.name <- as.character(mdl$call)
  bp$data.name <- as.character(mdl$call)
  dw$data.name <- as.character(mdl$call)
  v <- vif(mdl)

  if(results[1]=="print"){
    print(sw)
    print(bp)
    print(dw)
    cat("\nVariance Inflation Factors (VIF)\ndue to multicollinearity:\n")
    print(round(sort(v, decreasing=TRUE), 2))
  } else if(results[1]=="return"){
    mx <- matrix(nrow=5, ncol=4)
    colnames(mx) <- c("Test", "Tests", "p_value", "Decision")
    mx[1:4,1] <- c("Shapiro-Wilk", "Breusch-Pagan","Durbin-Watson",
                   "Variance Inflation Factor")
    mx[1:4,2] <- c("Normality of stand. residuals", "Homoscedasticity",
                   "Autocorrelation", "Muticollinearity")
    mx[1:3,3] <- round(c(sw$p.value, bp$p.value, dw$p.value),3)
    mx[1,4] <- ifelse(sw$p.value < 0.05, "Normality cannot be assumed",
                      "Normality can be assumed")
    mx[2,4] <- ifelse(bp$p.value < 0.05, "Homoscedasticity cannot be assumed",
                      "Homoscedasticity can be assumed")
    mx[3,4] <- ifelse(dw$p.value < 0.05, "Observations are autocorrelated",
                      "Observations are independent")
    v <- sort(v[(apply(mdl$model,2,class) %in% c("numeric","integer"))],
              decreasing=TRUE)
    mx[4,4] <- ifelse(v[1] >= 5, "Num covariates are multicorrelated",
                      "Num covariates are not multicorrelated")

    mx <- rbind(mx, c("VIF (decreasing):", rep("",3)))
    v <- round(v,2)
    for(i in 1:length(v)){
      mx <- rbind(mx, c(paste0("VIF: ",v[i],
                               " (",names(v[i]),")"), rep("",3)))
      mx[is.na(mx)] <- ""
    }
    return(list("test_summary"=mx, "sw_test"=sw, "bp_test"=bp, "dw_test"=dw,
                "vif"=v, "plot_resid"=g, "plot_hist"=g2))
  }
}
