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
replace_umlaute <- function(names, and=c("-"," "), by=c("_")) {
  #sprintf("%X", as.integer(charToRaw("ß")))
  names <- gsub("\u00E4","ae",names) #ä
  names <- gsub("\u00FC","ue",names) #ü
  names <- gsub("\u00F6","oe",names) #ö
  names <- gsub("\u00DC","Ue",names) #Ü
  names <- gsub("\u00C4","Ae",names) #Ä
  names <- gsub("\u00D6","Oe",names) #Ö
  names <- gsub("\u00DF","ss",names) #ß
  if(!(is.null(and) | is.null(by))){
    for(i in and){names <- gsub(i,by,names)}
  }
  return(names)
}
