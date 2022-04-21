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

#' Convert logit to probability
#'
#' @param .logit_ The logit to be transformed
#'
#' @return The probability corresponding to the passed logit
#' @export
#'
#' @examples
logit_to_prob <- function(.logit_) {
  odds_ <- exp(.logit_)
  prob_ <- odds_ / (1 + odds_)
  # if odds_ are Inf, set prob_ to 1 to avoid returning `NaN`:
  prob_[odds_ == Inf] <- 1

  return(prob_)
}

#' Convert probability to logit
#'
#' @param .prob_ The probability to be transformed
#'
#' @return The logit transformation of the passed probability
#' @export
#'
#' @examples
prob_to_logit <- function(.prob_) {
  logit_ <- qlogis(.prob_)

  return(logit_)
}

#' Back transform parameters values
#'
#' @param .t_data_ A dataset with data on the transformed scale
#' @param .l_params_ A list with required parameters' information,
#' including; names and functions to back transform the parameters to their
#' original/desired scale.
#'
#' @return
#' @export
#'
#' @examples
backTransform <- function(.t_data_, .l_params_) {
  # Prepare inputs list:
  l_bTransform <- list(
    'v_params_names' = .l_params_$v_params_names,
    'bckTransFunc' = .l_params_$backTransform)

  # Back-transform the columns of interest:
  data_ <- purrr::map2_dfc(
    .x = l_bTransform$v_params_names,
    .y = l_bTransform$bckTransFunc,
    .f = function(name_ = .x, func_ = .y) {
      name_ = exec(.fn = func_,
                  .t_data_ %>%
                    select(.data[[name_]]))
    }
  ) %>% # Bind remaining columns:
    bind_cols(.t_data_ %>%
            select(- l_bTransform$v_params_names)) %>%
    select(
      colnames(.t_data_)[!colnames(.t_data_) %in%
                           l_bTransform$v_params_names], everything())

  return(data_)
}

#' Run the example shiny app.
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
run_demo_App <- function(example_app = "one") {
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
