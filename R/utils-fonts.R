#' Download and install Google fonts.
#'
#' @param .name_
#'
#' @return
#' @export
#'
#' @examples
import_plots_fonts <- function(.name_ = "Source Sans Pro") {
  install.packages("pacman")
  pacman::p_install(
    package = "sysfonts",
    force = FALSE)

  # fonts_path_ <- glue::glue(
  #   "C:/Users/{Sys.getenv('USERNAME')}/AppData/Local/Microsoft/Windows/Fonts"
  # )
  # font_import(paths = fonts_path_)
  sysfonts::font_add_google(
    name = .name_)

}
