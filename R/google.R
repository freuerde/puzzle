#' Direct Google-search
#'
#' This function allows a Google-search directly from R-shell.
#' @param what Character string for searching within Google search engine.
#' @author Dennis Freuer
#' @importFrom utils browseURL
#' @export
#'
google <- function(what){
  browseURL(paste0("https://www.google.com/search?q=",tolower(gsub(" ", "+", what))))
}
