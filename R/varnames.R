#' Variable names of a dataset with a substring of interest
#'
#' This function searches within a given dataset for variable names that contain one or more specific substrings.
#' @param d Dataset that contains the variable names of interest.
#' @param what Character vector of substrings are assumed to be in the variable names.
#' @param case_sensitive If FALSE, the default, R doesn't distinquish between upper and lower case.
#' @return Returns a character vector with variable names of interest.
#' @author Dennis Freuer
#' @export
#'
varnames <- function(d, what, case_sensitive=FALSE) {
  return(unlist(lapply(what, function(w){
    if(case_sensitive){
      names(d)[grepl(w,names(d))]
    } else {
      names(d)[grepl(w,tolower(names(d)))]
    }
  })))
}
