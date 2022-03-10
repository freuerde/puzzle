#' Loads and installs multiple packages at once
#'
#' Loads and installs (if necessary) several packages at once.
#' @param pkgs  vector of package names as character strings.
#' @param install_new_pgs Should unavailable packages be installed
#' automatically using the install.packages() function? Default is TRUE. 
#' @author Dennis Freuer
#' @export
#'
load_pkgs <- function(pkgs, install_new_pgs=TRUE){
  if(install_new_pgs){
    install.packages(setdiff(pkgs, rownames(installed.packages())))
  }
  lapply(pkgs, library, character.only = TRUE)
}

