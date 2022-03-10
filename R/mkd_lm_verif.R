#' Creates a markdown document to validate linear model assumptions 
#' 
#'
#' Creates a markdown document for a (named) list of linear model validations from verif_lm() function output.
#' @param verif_lm_list A list object including either the result or a (named) list 
#' of multiple results from the verif_lm() function.
#' @param mkd_path  Path to the folder, where the markdown document should be stored.
#' If NULL, the current path will be used.
#' @param open Should the HTML output be opened immediately after saving? Default is FALSE. 
#' @param overwrite Should an already existing file be overwritten? Default is TRUE.
#' @return Save a HTML document without returning anything.
#' @details This function is also used within the verif_lm() function.
#' @author Dennis Freuer
#' @export
#'
mkd_lm_verif <- function(verif_lm_list, mkd_path=NULL, open=FALSE, overwrite=TRUE){
  if(sum(names(verif_lm_list) %in% c("test_summary","plot_resid","mdl")) == 3){
    verif_lm_list <- list("Regression model"=verif_lm_list)
  }
  mo <- c('---
title: "Linear regression assumptions"
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

for(i in 1:length(verif_lm_list)){
    cat(paste0("## ", cnt, ". ", names(verif_lm_list)[i]))
  r <- verif_lm_list[[i]]
  cat("\\nModel:", paste(formula(r$mdl)[2],"~",formula(r$mdl)[3]), "\\n")
  cat(pander(r$gof, style = "rmarkdown"), " ")
  cat(pander(r$test_summary[1:4,], style = "rmarkdown"), " ")
  grid.arrange(r$plot_resid[[1]], r$plot_resid[[2]],
               r$plot_hist, r$plot_resid[[3]], r$plot_resid[[4]], 
               r$plot_resid[[6]], nrow=2)
  cat(". \\n \\n***\\n")
  cnt <- cnt + 1
}
```')
  
  if(is.null(mkd_path)){ 
    mkd_path = 'lin_regression_assumptions.html' 
  } else{
    mkd_path <- gsub("\\..*","", mkd_path)
  }
  
  if(! dir.exists("markdown_tmp_files")){ dir.create("markdown_tmp_files") }
  cat(mo, file=paste0("markdown_tmp_files/lin_regression_assumptions.Rmd"))      # create a R markdown dokument
  rmarkdown::render(paste0("markdown_tmp_files/lin_regression_assumptions.Rmd")) # run the R markdown dokument
  file.copy(from=paste0("markdown_tmp_files/lin_regression_assumptions.html"), to=paste0(mkd_path,".html"), overwrite=overwrite)
  unlink("markdown_tmp_files", recursive=TRUE)
  
  if(open){ browseURL(paste0(mkd_path,".html")) }
}
