#' Creates a markdown document showing the linear regression results 
#' 
#'
#' Creates a markdown document for a (named) list of linear models (from rms::ols() or lm())
#' showing clearly the summary statistics and estimates in tables that can be presented.
#' @param lm_list A (named) list object including linear regression.
#' @param mkd_path  Path to the folder, where the markdown document should be stored.
#' If NULL, the current path will be used.
#' @param open Should the HTML output be opened immediately after saving? Default is FALSE. 
#' @param overwrite Should an already existing file be overwritten? Default is TRUE.
#' @return Save a HTML document without returning anything.
#' @details This function is also used within the verif_lm() function.
#' @author Dennis Freuer
#' @export
#'
mkd_lm_results <- function(lm_list, dat=NULL, mkd_path=NULL, open=FALSE, overwrite=TRUE){
  if(class(lm_list) != "list"){ lm_list <- list("Model"=lm_list) }
  for(i in 1:length(lm_list)){ # convert Harrel's orm models into lm objects
    if(any(class(lm_list[[i]]) %in% "orm")){ 
      if(is.null(dat)){ stop("Dataset required in case of Harrel's orm model") }
      lm_list[[i]] <- lm(lm_list[[i]], data=dat)
    }
    
    est <- cbind(summary(lm_list[[i]])$coefficients, "CI lower"=confint(lm_list[[i]])[,1], "CI upper"=confint(lm_list[[i]])[,2])
    est <- est[,c(1,5,6,4)]
    dimnames(est)[[2]][4] <- "Pr(>\\|t\\|)"
    
    lm_list[[i]]$gof <- broom::glance(lm_list[[i]])[,c(12,1,2,5,8,9)] 
    lm_list[[i]]$mdl <- paste(formula(lm_list[[i]])[2],"~",formula(lm_list[[i]])[3])
    lm_list[[i]]$est <- est
  }
  
  
  mo <- c('---
title: "Linear regression results"
output: html_document
---

```{r setup, include=FALSE, message=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```


```{r results="asis"}
require(gridExtra)
require(pander)
panderOptions("table.split.table", Inf)
cnt <- 1

for(i in 1:length(lm_list)){
    cat(paste0("## ", cnt, ". ", names(lm_list)[i]))
  cat("\\nModel:", lm_list[[i]]$mdl, "\\n") 
  cat(pander(lm_list[[i]]$gof, style = "rmarkdown"))
  cat(pander(lm_list[[i]]$est, round=3, style = "rmarkdown"))
  
  if("plot" %in% names(lm_list[[i]])){
    print(lm_list[[i]]$plot)
  }
  cat(". \\n \\n***\\n")
  cnt <- cnt + 1
}


```') 
  
  
  if(is.null(mkd_path)){ 
    mkd_path = 'lin_regression_results' 
  } else{
    mkd_path <- gsub("\\..*","", mkd_path)
  }
  
  if(! dir.exists("markdown_tmp_files")){ dir.create("markdown_tmp_files") }
  cat(mo, file=paste0("markdown_tmp_files/lin_regression_results.Rmd"))      # create a R markdown dokument
  rmarkdown::render(paste0("markdown_tmp_files/lin_regression_results.Rmd")) # run the R markdown dokument
  file.copy(from=paste0("markdown_tmp_files/lin_regression_results.html"), to=paste0(mkd_path,".html"), overwrite=overwrite)
  unlink("markdown_tmp_files", recursive=TRUE)

  if(open){ browseURL(paste0(mkd_path,".html")) }
}
# creates a markdown document with results for a (named) list of linear models
# mkd_path  can be used to set the path, where the created markdown document should be stored
