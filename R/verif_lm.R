#' Advanced residual plots and tests for linear models
#'
#' Function for creating residual plots in ggplot style and all necessary tests,
#' so that the assumptions of linear regression models can be verified.
#' @param mdl A linear model of class lm.
#' @param dat The dataset on which the model was applied
#' @param results "markdown" creates a markdown summary document.
#' "return" returns all analyses in a list.
#' "print" prints the output on the console and shows the plot.
#' @param mkd_path Can be used to set the path, where the created markdown document should be stored.
#' If NULL, the current active path (getwd()) will be used to save the markdown document. 
#' @return Returns, if desired, a list of length 6 with an summary of tests,
#' all calculated tests and plots.
#' @author Dennis Freuer
#' @importFrom MASS stdres
#' @importFrom stats dnorm shapiro.test
#' @importFrom lmtest bptest dwtest
#' @importFrom gridExtra grid.arrange
#' @importFrom car vif
#' @import ggplot2 ggfortify dplyr
#' @export
#'
verif_lm <- function(mdl, dat=NULL, results=c("markdown","return","print"), mkd_path=NULL){
  library(rms); library(Hmisc); library(dplyr)
  library(MASS); library(lmtest); library(car); library(ggfortify)
  
  if(any(class(mdl) %in% "orm")){ # convert Harrel's orm model in a lm object
    if(is.null(dat)){ stop("Dataset required in case of Harrel's orm model") }
    mdl <- lm(mdl, data=dat)
  }
  
  sres <- MASS::stdres(mdl)
  bp <- bptest(mdl)
  dw <- dwtest(mdl, alternative="two.sided")
  sw <- shapiro.test(sres)
  gof <- broom::glance(mdl)
  gof <- bind_cols("n"=nobs(mdl), gof[,c(1,2,5,8,9)])
  
  sw$method <- "Shapiro-Wilk normality test of standardised residuals"
  bp$method <- "Breusch-Pagan homoscedasticity test"
  dw$method <- "Durbin-Watson autocorrelation test"
  sw$data.name <- as.character(mdl$call)
  bp$data.name <- as.character(mdl$call)
  dw$data.name <- as.character(mdl$call)
  v <- vif(mdl)
  
  g <- tryCatch({
    ggplot2::autoplot(mdl, which = 1:6, ncol = 3, label.size = 3) # doesn't work for spline models
  }, warning = function(w){ message(w) 
  }, error = function(e){ # Create manually residual-plots:
    list(
      ggplot(data.frame(), aes(x=mdl$fitted.values, y=mdl$residuals)) + 
        geom_hline(yintercept=0, linetype="dashed", colour="darkgrey") + geom_point() + 
        ggtitle("Residuals vs Fitted") + ylab("Residuals") + xlab("Fitted values"),
      # QQ-Plot:
      ggplot(data.frame(), aes(sample = MASS::stdres(mdl))) + stat_qq() + stat_qq_line(),
      # Standardized residuals
      ggplot(data.frame(), aes(x=mdl$fitted.values, y=sqrt(abs(MASS::stdres(mdl))))) + 
        geom_point() + ggtitle("Scale-Location") + ylab("sqrt(abs(stand residuals))")+ 
        xlab("Fitted values"),
      # empty content:
      ggplot() + geom_point(),
      # Cook's D:
      ggplot(data.frame(), aes(x=hatvalues(mdl), y=MASS::stdres(mdl))) + 
        geom_hline(yintercept=0, linetype="dashed", colour="darkgrey") + geom_point() + 
        ggtitle("Residuals vs Leverage") + ylab("Standardized residuals") + 
        xlab(paste("Leverage, max Cooks D =", round(max(cooks.distance(mdl)),2))),
      # Cook's D:
      ggplot(data.frame(), aes(x=hatvalues(mdl), y=cooks.distance(mdl))) + 
        geom_hline(yintercept=0, linetype="dashed", colour="darkgrey") + 
        geom_vline(xintercept=0, linetype="dashed", colour="darkgrey") +
        geom_point() + xlab("Leverage") +
        ggtitle("Cook's dist vs Leverage") + ylab("Cook's distance")
    )
  })
  
  g2 <- ggplot2::ggplot(data=data.frame(), ggplot2::aes(sres)) +
    ggplot2::geom_histogram(ggplot2::aes(y=..density..), fill="grey40", color="black", bins=30) +
    ggplot2::ggtitle("Residuals vs Normal") +
    ggplot2::stat_function(data=data.frame(), fun=function(x) dnorm(x), color="blue") +
    ggplot2::ylab(NULL) + ggplot2::xlab("Standardized Residuals")
  
  
  if(results[1]=="print"){
    cat("Goodness of fit:\n")
    print(gof)
    print(sw)
    print(bp)
    print(dw)
    cat("\nVariance Inflation Factors (VIF)\ndue to multicollinearity:\n")
    print(round(sort(v, decreasing=TRUE), 2))
    gridExtra::grid.arrange(g[[1]],g[[2]],g2,g[[3]],g[[4]],g[[6]], nrow=2)
    
  } else{
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
    v <- sort(v[(sapply(mdl$model,class) %in% c("numeric","integer"))],
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
    
    if(results[1]=="return"){
      return(list("test_summary"=mx, "sw_test"=sw, "bp_test"=bp, "dw_test"=dw,
                  "vif"=v, "plot_resid"=g, "plot_hist"=g2, "gof"=gof, "mdl"=mdl))
      
    } else if(results[1] == "markdown"){
      ll <- list()
      ll[[as.character(formula(mdl)[2])]] <- 
        list("test_summary"=mx, "sw_test"=sw, "bp_test"=bp, "dw_test"=dw,
             "vif"=v, "plot_resid"=g, "plot_hist"=g2, "gof"=gof, "mdl"=mdl)
      
      mkd_lm_verif(ll, mkd_path)
      
    }
  }
}
