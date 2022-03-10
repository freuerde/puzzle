#' Changes the RStudio-theme
#'
#' Changes the RStudio-theme in two ways. Either by giving a specific RStudio-theme or
#' just changing between dark and light mode,if theme=NULL (the default).
#' @param theme  A specific theme-name (e.g. Pastel on dark, Textmate (default), etc.).
#' @description If theme=NULL, then the function switches between the "Pastel on dark"
#' and "Textmate (default)" themes.
#' @author Dennis Freuer
#' @export
#'
change_theme <- function(theme=NULL){
  if(! is.null(theme)){
    rstudioapi::applyTheme(theme)
  } else{
    if(rstudioapi::getThemeInfo()$dark) {
      rstudioapi::applyTheme("Textmate (default)")
    } else{
      rstudioapi::applyTheme("Pastel on dark")
    }
  }
}

