#' Assign extra arguments/parameters in parent function
#'
#' @param .default_args_ # A list containing default arguments names and
#' their values.
#' @param .env_ # Environment object grabbed from the parent function's
#' environment to correctly assign arguments to that function.
#' @param .args_ # A list containing supplied/additional arguments names
#' and their values. Arguments in .default_args_ but existing in .args_
#' will be assigned values from .args_ and vice versa.
#'
#' @return This function assigns variables/objects in the parent's function
#' environment, hence it returns nothing.
#' @export
#'
#' @examples
assign_extraArgs_ <- function(.default_args_, .env_, .args_) {
  # Grab default arguments' names:
  if(is.null(names(.default_args_)))
    stop(".default_args_ should contain named objects")
  if(length(names(.default_args_)) != length(.default_args_))
    stop("all arguments in .default_args_ should be named")
  expected_args_names <- names(.default_args_)
  # Grab additional arguments' names:
  supplied_args_names <- names(.args_)
  # Let the user know if any of the supplied arguments were unrecognised:
  if(any(!supplied_args_names %in% expected_args_names))
    message("Argument(s) [",
            paste(supplied_args_names[!supplied_args_names %in%
                                        expected_args_names]),
            "] is/are unknown, and therefore ignored")
  # Set additional arguments:
  purrr::walk(
    .x = expected_args_names,
    .f = function(.arg) {
      assign(.arg,
             if(is.null(.args_[[.arg]])) {
               .default_args_[[.arg]]
             } else {
               .args_[[.arg]]
             }, envir = .env_)
    })
}

#' Run the example Shiny app that quires the healthsites api.
#'
#' @param example_app The example shiny app to run.
#'
#' @return Runs the example shiny app
#' @export
#'
#' @examples
#' \dontrun{
#' run_API_demo_App()
#' }
run_API_demo_App <- function(example_app = "one") {
  appFolder <- switch(example_app,
                      one = "calibrationApp"#,
                      #wb_dhs = "WBandDHS"
                      )
  appDir <- system.file("shiny-examples", appFolder,
                        package = "calibrater")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `calibrater`.",
         call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
