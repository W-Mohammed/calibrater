#' R6 Class representing a calibrateR_R6 calibration machine.
#'
#' @description
#' An instance of this class is expected to calibrate a user-defined model
#' and produce summary plots/tables.
#' @format An [R6::R6Class] object.
#' @name calibrateR_R6
NULL
#'
#' @rdname calibrateR_R6
#' @export
#'
#' @examples
#' \dontrun{
#' }
#'
calibrateR_R6 <- R6::R6Class(
  # Object name:
  classname = "calibrateR_R6",
  # Public elements:
  public = list(
    #' @field calibration_model a Decision-Analytic model under calibration
    calibration_model = NULL,
    #' @field prior_samples a Decision-Analytic model under calibration
    prior_samples = NULL,

    #' @description
    #' Initialisation method (triggered when a new object is created).
    #' Summary plots and table(s) are created alongside the construction
    #' of the object.
    #'
    #' @param .n_samples
    #'
    #' @param .l_params
    #'
    #' @param ...
    #'
    #' @inheritParams calibrater::sample_prior_LHS
    #'
    #' @return A new `calibrateR_R6` object.
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    sampleR = function(.n_samples = 1, .l_params, ...) {
      self$prior_samples <- private$sampleR_(
        .n_samples = .n_samples,
        .l_params = .l_params,
        ...
        )

      invisible(self)

    }

  ),

  private = list(
    sampleR_ = function(...) {
      calibrater::sample_prior_LHS(...)
    }
  )
)
