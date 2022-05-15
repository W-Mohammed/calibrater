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
  # Object name:----
  classname = "calibrateR_R6",
  # Public elements:----
  public = list(
    #' @field calibration_model a Decision-Analytic model under calibration
    calibration_model = NULL,
    #' @field calibration_parameters calibration parameters' data
    calibration_parameters = NULL,
    #' @field calibration_targets calibration targets' data
    calibration_targets = NULL,
    #' @field random_calibration random calibration methods results
    random_calibration = NULL,
    #' @field prior_samples a Decision-Analytic model under calibration
    prior_samples = NULL,

    #' @description
    #' Sample prior distribution(s) using one or more sampling method. This
    #' function currently supports: LHS, FGS and RGS.
    #'
    #' @param .model The decision analytic model under calibration
    #' @param .params Calibration parameters
    #' @param .targets Calibration targets
    #'
    #' @return Object of class `CalibrateR_R6`
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    initialize = function(.model, .params, .targets) {
      # save passed arguments to dedicated internal objects
      self$calibration_model <- .model
      self$calibration_parameters <- .params
      self$calibration_targets <- .targets

      invisible(self)

    },

    #' @description
    #' Sample prior distribution(s) using one or more sampling method. This
    #' function currently supports: LHS, FGS and RGS.
    #'
    #' @param .n_samples An integer specifying the number of samples to be
    #' generated.
    #' @param .sampling_method The sampling method(s) to use. The function
    #' currently supports LHS, RGS, and FGS.
    #' @param .ssed_no random seed number
    #'
    #' @return Executes the required sampling method and populates the
    #' "prior samples" internal object.
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    sampleR = function(.n_samples = 1, .sampling_method, .ssed_no = NULL) {
      # pass arguments to the private function (wrapper function):
      private$sampleR_(
        .sampling_method = .sampling_method,
        .n_samples = .n_samples,
        .l_params = self$calibration_parameters,
        .ssed_no = .ssed_no
      )

      invisible(self)

    },

    #' @description
    #' Calibrate the model using one or more calibration method.
    #'
    #' @return Executes the required calibration method and populates
    #' the samples internal object.
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    calibrateR = function(...) {

    },

    #' @description
    #' Calibrate the model using one or more calibration method.
    #'
    #' @param .args A list of arguments to be passed to calibration model
    #' @param .optim Logical for whether the function is used by an
    #' optimisation algorithm. Default is \code{FALSE}.
    #' @param .maximise Logical for whether the output of the function is
    #' used in a maximising optimisation function. Default is \code{TRUE}.
    #' @param .weighted Logical for whether the SSR was to be weighted,
    #' default is \code{TRUE}. The weight used by function is
    #' \code{1/(sd^2)}.
    #' @param .sample_method The method used to sample from the prior
    #' distribution.
    #' @param .calibration_method The calibration process.
    #'
    #' @return Executes the required calibration method and populates
    #' the samples internal object.
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    calibrateR_random = function(.args, .optim, .maximise,
                                 .weighted = NULL, .sample_method,
                                 .calibration_method) {
      private$calibrateR_random_(
        .func = self$calibration_model,
        .args = .args,
        .optim = .optim,
        .maximise = .maximise,
        .weighted = .weighted,
        .sample_method = .sample_method,
        .samples = self$prior_samples[[.sample_method]],
        .l_targets = self$calibration_targets,
        .calibration_method = .calibration_method
      )
    },

    #' @description
    #' Calibrate the model using one or more calibration method.
    #'
    #' @return Executes the required calibration method and populates
    #' the samples internal object.
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    calibrateR_directed = function(...) {

    },

    #' @description
    #' Calibrate the model using one or more calibration method.
    #'
    #' @return Executes the required calibration method and populates
    #' the samples internal object.
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    calibrateR_bayesian = function(...) {

    }

  ),

  # Private elements:----
  private = list(
    ## Sample prior distribution(s):----
    sampleR_ = function(.sampling_method, ...) {
      ### FGS:----
      if("FGS" %in% .sampling_method)
        self$prior_samples["FGS"] <- list(
          sample_prior_FGS(...)
        )
      ### RGS:----
      if("RGS" %in% .sampling_method)
        self$prior_samples["RGS"] <- list(
          sample_prior_RGS(...)
        )
      ### LHS:----
      if("LHS" %in% .sampling_method)
        self$prior_samples["LHS"] <- list(
          sample_prior_LHS(...)
        )
    },
    ## Calibration methods:----
    ### Random:----
    calibrateR_random_ = function(.calibration_method, ...) {
      #### SSE:----
      if("SSE" %in% .calibration_method)
        self$random_calibration["SSE"] <- list(
          wSSE_GOF(...)
        )
      #### LLK:----
      if("LLK" %in% .calibration_method)
        self$random_calibration["LLK"] <- list(
          LLK_GOF(...)
        )

    },
    ### Directed:----
    calibrateR_directed_ = function() {

    },
    ### Bayesian:----
    calibrateR_bayesian_ = function() {

    }
  )
)
