#' Returns the current path
#'
#' Returns the current path to the folder, where the current R-script is stored.
#' @param names Character vector that contains german umlaute.
#' @param and Character vector.If not NULL the vector "and"
#' (default: c("-"," ")) will be replaced by the element "by"
#' @param by Character element. Set to NULL, if no replacement is desired.
#' @return Returns the current path as a string.
#' @author Dennis Freuer
#' @export
#'
current_path <- function(){ return(dirname(rstudioapi::getSourceEditorContext()$path)) }

#' @describeIn crnt_path() and current_path() are two identical functions, but with different notation
crnt_path <- function(){ return(dirname(rstudioapi::getSourceEditorContext()$path)) }
