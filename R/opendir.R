#' File access direclty from R
#'
#' Short function for opening a file directory within any operating system (Windows, MAC OS, etc.) from R directly.
#' @param dir Path, that should be opened with the current working directory as default.
#' @author Dennis Freuer
#' @export
#'
opendir <- function(dir = getwd()){
  if (.Platform['OS.type'] == "windows"){
    shell.exec(dir)
  } else {
    system(paste(Sys.getenv("R_BROWSER"), dir))
  }
}
