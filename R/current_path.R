#' Replaces german umlaute
#'
#' This function replaces german umlaute in an appropriate way to avoid
#' issues with special characters. In addition, user defined elements
#' can be replaced by an desired element, too.
#' @param names Character vector that contains german umlaute.
#' @param and Character vector.If not NULL the vector "and"
#' (default: c("-"," ")) will be replaced by the element "by"
#' @param by Character element. Set to NULL, if no replacement is desired.
#' @return Returns a character vector with replaced german umlaute.
#' @author Dennis Freuer
#' @export
#'
current_path <- function(){ return(dirname(rstudioapi::getSourceEditorContext()$path)) }

#' @describeIn crnt_path() and current_path() are two identical functions, but with different notation
crnt_path <- function(){ return(dirname(rstudioapi::getSourceEditorContext()$path)) }
