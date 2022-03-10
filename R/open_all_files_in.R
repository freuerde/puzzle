#' Opens all disred files in a folder
#'
#' Opens all files with a specific file type in a folder.
#' @param path_to_folder  The path to the folder as character string.
#' @param file_type The file type that should be opened.It is also possible to 
#' use a specific string indicating the file names of interest.
#' @author Dennis Freuer
#' @export
#'
open_all_files_in <- function(path_to_folder, file_type){
  fls <- list.files(path_to_folder, pattern = file_type)
  fls <- paste0(path_to_folder, "/",fls)
  lapply(fls, browseURL)
}

