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
    #' @field calibration_results calibration interim results
    calibration_results = NULL,
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
    #' Calibrate the model using one or more random search method(s).
    #'
    #' @param .args A list of arguments to be passed to calibration model.
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
    calibrateR_random = function(.args,
                                 .optim = FALSE,
                                 .maximise = TRUE,
                                 .weighted = NULL,
                                 .sample_method = 'LHS',
                                 .calibration_method = 'LLK') {
      private$calibrateR_random_(
        .func = self$calibration_model,
        .args = .args,
        .optim = .optim,
        .maximise = .maximise,
        .weighted = .weighted,
        .sample_method = .sample_method,
        .l_targets = self$calibration_targets,
        .calibration_method = .calibration_method
      )
    },

    #' @description
    #' Calibrate the model using one or more directed search method(s).
    #'
    #' @param .args A list of arguments to be passed to .func.
    #' @param .gof A goodness-of-fit function, default is log-likelihood.
    #' @param .n_samples Number of starting values (gausses) to use.
    #' @param .max_iterations Maximum number of algorithm iterations.
    #' @param temp SANN algorithm tuning parameter.
    #' @param tmax SANN algorithm tuning parameter.
    #' @param .calibration_method The calibration process.
    #' @param .sample_method The method used to sample from the prior
    #' distribution.
    #'
    #' @return Executes the required calibration method and populates
    #' the samples internal object.
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    calibrateR_directed = function(.args,
                                   .gof = 'LLK',
                                   .n_samples = 1,
                                   .max_iterations = 1000,
                                   temp = 10,
                                   tmax = 10,
                                   .calibration_method = 'NM',
                                   .sample_method = 'LHS') {
      private$calibrateR_directed_(
        .args = .args,
        .gof = .gof,
        .n_samples = .n_samples,
        .calibration_method = .calibration_method,
        .sample_method = .sample_method,
        .max_iterations = .max_iterations,
        temp = temp,
        tmax = tmax
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
    calibrateR_random_ = function(.calibration_method, .sample_method,
                                  ...) {
      if("RGS" %in% .sample_method){
        #### SSE:----
        if("SSE" %in% .calibration_method)
          self$calibration_results$random["SSE_RGS"] <- list(
            wSSE_GOF(
              .samples = self$prior_samples[["RGS"]],
              ...
            )
          )
        #### LLK:----
        if("LLK" %in% .calibration_method)
          self$calibration_results$random["LLK_RGS"] <- list(
            LLK_GOF(
              .samples = self$prior_samples[["RGS"]],
              ...
            )
          )
      }
      if("FGS" %in% .sample_method){
        #### SSE:----
        if("SSE" %in% .calibration_method)
          self$calibration_results$random["SSE_FGS"] <- list(
            wSSE_GOF(
              .samples = self$prior_samples[["FGS"]],
              ...
            )
          )
        #### LLK:----
        if("LLK" %in% .calibration_method)
          self$calibration_results$random["LLK_FGS"] <- list(
            LLK_GOF(
              .samples = self$prior_samples[["FGS"]],
              ...
            )
          )
      }
      if("LHS" %in% .sample_method){
        #### SSE:----
        if("SSE" %in% .calibration_method)
          self$calibration_results$random["SSE_LHS"] <- list(
            wSSE_GOF(
              .samples = self$prior_samples[["LHS"]],
              ...
            )
          )
        #### LLK:----
        if("LLK" %in% .calibration_method)
          self$calibration_results$random["LLK_LHS"] <- list(
            LLK_GOF(
              .samples = self$prior_samples[["LHS"]],
              ...
            )
          )
      }
    },
    ### Directed:----
    calibrateR_directed_ = function(.gof,
                                    .args,
                                    .n_samples,
                                    .calibration_method,
                                    .sample_method,
                                    .max_iterations,
                                    temp,
                                    tmax) {
      if("RGS" %in% .sample_method) {
        #### Nelder-Mead:----
        if("NM" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["NM_LLK_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = "LLK",
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["NM_SSE_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = "SSE",
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### BFGS:----
        if("BFGS" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["BFGS_LLK_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'BFGS',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["BFGS_SSE_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'BFGS',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### SANN:----
        if("SANN" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["SANN_LLK_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'SANN',
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["SANN_SSE_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'SANN',
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### GA:----
        if("GA" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["GA_LLK_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'GA',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["GA_SSE_RGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'GA',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
      }
      if("FGS" %in% .sample_method) {
        #### Nelder-Mead:----
        if("NM" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["NM_LLK_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = "LLK",
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["NM_SSE_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = "SSE",
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### BFGS:----
        if("BFGS" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["BFGS_LLK_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'BFGS',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["BFGS_SSE_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'BFGS',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### SANN:----
        if("SANN" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["SANN_LLK_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'SANN',
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["SANN_SSE_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'SANN',
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### GA:----
        if("GA" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["GA_LLK_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'GA',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["GA_SSE_FGS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'GA',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
      }
      if("LHS" %in% .sample_method) {
        #### Nelder-Mead:----
        if("NM" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["NM_LLK_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = "LLK",
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["NM_SSE_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = "SSE",
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### BFGS:----
        if("BFGS" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["BFGS_LLK_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'BFGS',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["BFGS_SSE_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'BFGS',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### SANN:----
        if("SANN" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["SANN_LLK_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'SANN',
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["SANN_SSE_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'SANN',
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
        #### GA:----
        if("GA" %in% .calibration_method) {
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["GA_LLK_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'LLK',
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'GA',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### SSE:----
          if("SSE" %in% .gof)
            self$calibration_results$directed[["GA_SSE_LHS"]] <-
              calibrateModel_directed(
                .func = self$calibration_model,
                .args = .args,
                .gof = 'SSE',
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = 'GA',
                maxit = .max_iterations,
                .maximise = TRUE,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
        }
      }
    },
    ### Bayesian:----
    calibrateR_bayesian_ = function() {

    }
  )
)
