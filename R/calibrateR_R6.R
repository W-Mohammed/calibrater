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
    #' @field calibration_model_args arguments passed to the calibration
    #' model
    calibration_model_args = NULL,
    #' @field calibration_parameters calibration parameters' data
    calibration_parameters = NULL,
    #' @field calibration_targets calibration targets' data
    calibration_targets = NULL,
    #' @field calibration_results calibration interim results
    calibration_results = NULL,
    #' @field PSA_samples calibration and un-calibration parameters
    #' samples
    PSA_samples = NULL,
    #' @field prior_samples a Decision-Analytic model under calibration
    prior_samples = NULL,
    #' @field transform_parameters boolean for whether to back transform
    #' parameters
    transform_parameters = FALSE,

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
    initialize = function(.model, .args, .params, .targets, .transform) {
      # save passed arguments to dedicated internal objects
      self$calibration_model <- .model
      self$calibration_model_args <- .args
      self$calibration_parameters <- .params
      self$calibration_targets <- .targets
      self$transform_parameters <- .transform

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
    calibrateR_random = function(.optim = FALSE,
                                 .maximise = TRUE,
                                 .weighted = NULL,
                                 .sample_method = 'LHS',
                                 .calibration_method = 'LLK') {
      private$calibrateR_random_(
        .func = self$calibration_model,
        .args = self$calibration_model_args,
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
    calibrateR_directed = function(.gof = 'LLK',
                                   .n_samples = 1,
                                   .max_iterations = 1000,
                                   temp = 10,
                                   tmax = 10,
                                   .calibration_method = 'NM',
                                   .sample_method = 'LHS') {
      private$calibrateR_directed_(
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
    #' @param .b_method Bayesian calibration method
    #' @param .n_resample Desired number of draws from the posterior
    #' @param .IMIS_iterations Maximum number of IMIS iterations
    #' @param .IMIS_sample IMIS sample size at each iteration
    #'
    #' @return Executes the required calibration method and populates
    #' the samples internal object.
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    calibrateR_bayesian = function(.b_method = 'SIR',
                                   .n_resample = 1000,
                                   .IMIS_iterations = 10,
                                   .IMIS_sample = 100) {
      private$calibrateR_bayesian_(
        .b_method = .b_method,
        .n_resample = .n_resample,
        .IMIS_iterations = .IMIS_iterations,
        .IMIS_sample = .IMIS_sample
      )
    }

  ),

  # Private elements:----
  private = list(
    ## Sample prior distribution(s):----
    sampleR_ = function(.sampling_method,
                        ...) {
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
    calibrateR_random_ = function(.calibration_method,
                                  .sample_method,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
                .args = self$calibration_model_args,
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
    calibrateR_bayesian_ = function(.b_method,
                                    .n_resample,
                                    .IMIS_iterations,
                                    .IMIS_sample) {
      #### IMIS:----
      if('IMIS' %in% .b_method) {
        self$calibration_results$bayesian[["IMIS"]] <-
          private$calibrateModel_beyesian(
          .b_method = 'IMIS',
          .func = self$calibration_model,
          .args = self$calibration_model_args,
          .transform = self$transform_parameters,
          .n_resample = .n_resample,
          .IMIS_iterations = .IMIS_iterations,
          .IMIS_sample = .IMIS_sample,
          .l_params = self$calibration_parameters,
          .l_targets = self$calibration_targets
        )
      }
      #### SIR:----
      if('SIR' %in% .b_method) {
        samples_ = private$sample_prior_IMIS(.n_samples = .n_resample)
        self$calibration_results$bayesian[["SIR"]] <-
          private$calibrateModel_beyesian(
          .b_method = 'SIR',
          .func = self$calibration_model,
          .args = self$calibration_model_args,
          .n_resample = .n_resample,
          .samples = samples_,
          .l_params = self$calibration_parameters,
          .l_targets = self$calibration_targets
        )
      }
    },
    ### Bayesian helper functions:----
    #### Sample prior:----
    # Sample from prior using Latin Hypercube Sampling (LHS) method
    #
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .n_samples An integer specifying the number of samples to be
    # generated.
    #
    sample_prior_IMIS = function(
      .n_samples,
      .l_params = self$calibration_parameters) {
      # Get the number of parameters:
      n_params <- length(.l_params[["v_params_names"]])
      # Get LHS samples:
      tbl_lhs_unit <- lhs::randomLHS(.n_samples, n_params) %>%
        as_tibble(~ vctrs::vec_as_names(...,
                                        repair = "unique",
                                        quiet = TRUE))
      # Define inputs list:
      l_lhs <- list(.l_params[['v_params_names']],
                    paste0('q', .l_params[['v_params_dists']]),
                    tbl_lhs_unit,
                    .l_params[['args']],
                    .l_params[['v_params_dists']])
      # Make sure parameter names are in a named vector:
      names(l_lhs[[1]]) <- l_lhs[[1]]
      # Map over parameters to scale up LHS samples to appropriate values:
      tbl_lhs_samp <- pmap_dfc(
        .l = l_lhs,
        .f = function(.name, .func, p, .arg, .dist) {
          assign(.name,
                 exec(.func,
                      p,
                      !!!.arg)
          )
        })

      return(tbl_lhs_samp %>% as.matrix())
    },
    #### Prior densities:----
    # Calculate log prior
    #
    # @param .samples A vector/dataset containing sampled/proposed values.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    log_priors = function(
      .samples,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {

      v_params_names <- .l_params[['v_params_names']]
      names(.l_params[['v_params_names']]) <- v_params_names
      # Ensure .samples is of appropriate class and named properly:
      if(is.null(dim(.samples))) # If vector, change to matrix
        .samples <- t(.samples)
      if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
        .samples <- .samples %>%
        as_tibble(~ vctrs::vec_as_names(...,
                                        repair = "unique",
                                        quiet = TRUE)) %>%
        `colnames<-`(v_params_names)
      # Get appropriate distributions' and distributions' parameters' objects:
      params_dists <- 'v_params_dists'
      params_args <- 'args'
      # Transform the sampled parameters back to their original scale:
      if(.transform) {
        .samples <- .samples %>%
          backTransform(.t_data_ = ., .l_params_ = .l_params)
        params_dists <- 'v_true_params_dists'
        params_args <- 'true_args'
      }
      # Define inputs list for the pmap function:
      l_lprior <- list(.l_params[['v_params_names']],
                       paste0('d', .l_params[[params_dists]]),
                       .l_params[[params_dists]],
                       .l_params[[params_args]],
                       .samples)
      # Estimate the log prior:
      v_lprior <- rowSums(pmap_df(
        .l = l_lprior,
        .f = function(.name, .func, .dist, .arg, .param) {
          exec(.func, .param, !!!.arg, log = TRUE)
        }
      ))

      return(v_lprior)

    },
    # Calculate log prior (one set of parameters at a time)
    #
    # @param .samples A vector/dataset containing sampled/proposed values.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    log_prior = function(
      .samples,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {

      v_params_names <- .l_params[['v_params_names']]
      names(.l_params[['v_params_names']]) <- v_params_names
      # Ensure .samples is of appropriate class and named properly:
      if(is.null(dim(.samples))) # If vector, change to matrix
        .samples <- t(.samples)
      if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
        .samples <- .samples %>%
        as_tibble(~ vctrs::vec_as_names(...,
                                        repair = "unique",
                                        quiet = TRUE)) %>%
        `colnames<-`(v_params_names)
      # Get appropriate distributions' and distributions' parameters' objects:
      params_dists <- 'v_params_dists'
      params_args <- 'args'
      # Transform the sampled parameters back to their original scale:
      if(.transform) {
        .samples <- .samples %>%
          backTransform(.t_data_ = ., .l_params_ = .l_params)
        params_dists <- 'v_true_params_dists'
        params_args <- 'true_args'
      }
      # Define inputs list for the pmap function:
      l_lprior <- list(.l_params[['v_params_names']],
                       paste0('d', .l_params[[params_dists]]),
                       .l_params[[params_dists]],
                       .l_params[[params_args]],
                       .samples)
      # Estimate the log prior:
      v_lprior <- sum(pmap_dbl(
        .l = l_lprior,
        .f = function(.name, .func, .dist, .arg, .param) {
          exec(.func, .param, !!!.arg, log = TRUE)
        }
      ))

      return(v_lprior)
    },
    # Calculate prior densities
    #
    # @param .samples A vector/dataset containing sampled/proposed values.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    calculate_priors = function(
      .samples,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {

      v_prior <-  exp(private$log_priors(
        .samples = .samples,
        .l_params = .l_params,
        .transform = .transform
      ))

      return(v_prior)
    },
    # Calculate prior (one set of parameters at a time)
    #
    # @param .samples A vector/dataset containing sampled/proposed values.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    calculate_prior = function(
      .samples,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {

      v_prior <-  exp(private$log_prior(
        .samples = .samples,
        .l_params = .l_params,
        .transform = .transform
      ))

      return(v_prior)
    },
    #### Likelihood:----
    # Calculate log likelihood (LLK)
    #
    # @param .samples A table or vector of sampled parameter values
    # @param .func A function defining the model to be calibrated
    # @param .args A list of arguments to be passed to .func
    # @param .l_targets A list containing a vector of targets' names, a
    # vector of targets' weights, a vector of targets' distributions, and
    # a table for each target that contains the values (column name
    # 'value') and standard errors (column name 'sd') of the corresponding
    # target.
    #
    log_likelihood = function(
      .samples,
      .func = self$calibration_model,
      .args = self$calibration_model_args,
      .l_targets = self$calibration_targets) {
      # Ensure .samples is of appropriate class and named properly:
      if(is.null(dim(.samples))) # If vector, change to matrix
        .samples <- t(.samples)
      if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
        .samples <- .samples %>%
          as_tibble(~ vctrs::vec_as_names(...,
                                          repair = "unique",
                                          quiet = TRUE))
      # Run the model using each set of sampled parameters:
      model_results <- pmap(
        .l = as.list(.samples),
        .f = function(...) {
          dots <- list(...)
          exec(.func, dots, !!!.args)
        })
      # Define inputs list for the pmap function:
      l_llik <- list(.l_targets[['v_targets_names']],
                     paste0('d', .l_targets[['v_targets_dists']]),
                     .l_targets[['v_targets_dists']],
                     .l_targets[['v_targets_weights']])

      # Estimate the overall log likelihood for each model output:
      overall_lliks <- map_dbl(
        .x = model_results,
        .f = function(.mod_res) {
          # Log likelihood (for each target):
          overall_llik <- pmap(
            .l = l_llik,
            .f = function(.name, .func, .dist, .weight) {
              tryCatch({
                if(.dist == 'norm') {
                  sum( # Sum all values of that one target, if many
                    exec(.func,
                         .l_targets[[.name]]$value, # target's sd
                         .mod_res[[.name]], # mean value
                         .l_targets[[.name]]$se, # sd value (target's sd)
                         log = TRUE)
                  ) * .weight # target weight
                } else if(.dist == 'binom') {
                  sum( # Sum all values of that one target, if many
                    exec(.func,
                         prob = .mod_res[[.name]],
                         x = .l_targets[[.name]]$x,
                         size = .l_targets[[.name]]$size,
                         log = TRUE)
                  ) * .weight # target weight
                } else if(.dist == 'lnorm') {
                  sum( # Sum all values of that one target, if many
                    exec(.func,
                         .l_targets[[.name]]$value, # target's mean
                         log(.mod_res[[.name]]) - (1/2) *
                           .l_targets[[.name]]$se^2, # mean value
                         .l_targets[[.name]]$se, # sd value (target's sd)
                         log = TRUE)
                  ) * .weight # target weight
                }
              },
              error = function(e) -Inf
              )
            }
          )
          # Overall log likelihood (for all targets):
          overall_llik <- overall_llik %>%
            reduce(`+`, .init = 0)
        })

      # Set NaN values to -Inf to avoid other functions from crashing:
      overall_lliks[is.na(overall_lliks)] <- -Inf

      return(overall_lliks)
    },
    # Calculate likelihood
    #
    # @param .samples A table or vector of sampled parameter values
    # @param .func A function defining the model to be calibrated
    # @param .args A list of arguments to be passed to .func
    # @param .l_targets A list containing a vector of targets' names, a
    # vector of targets' weights, a vector of targets' distributions, and
    # a table for each target that contains the values (column name
    # 'value') and standard errors (column name 'sd') of the corresponding
    # target.
    #
    calculate_likelihood = function(
      .samples,
      .func = self$calibration_model,
      .args = self$calibration_model_args,
      .l_targets = self$calibration_targets) {

      v_likelihood <-  exp(private$log_likelihood(
        .samples = .samples,
        .func = .func,
        .args = .args,
        .l_targets = .l_targets
      ))

      return(v_likelihood)
    },
    #### Posterior:----
    # Calculate log posteriors
    #
    # @param .samples A table or vector of sampled parameter values
    # @param .func A function defining the model to be calibrated
    # @param .args A list of arguments to be passed to .func
    # @param .l_targets A list containing a vector of targets' names, a
    # vector of targets' weights, a vector of targets' distributions, and
    # a table for each target that contains the values (column name
    # 'value') and standard errors (column name 'sd') of the corresponding
    # target.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    log_posteriors = function(
      .samples,
      .func = self$calibration_model,
      .args = self$calibration_model_args,
      .l_targets = self$calibration_targets,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {
      # calculate log prior:
      l_prior <- private$log_priors(
        .samples = .samples,
        .l_params = .l_params,
        .transform = .transform
      )
      # calculate log likelihood:
      l_lilk <- private$log_likelihood(
        .samples = .samples,
        .func = .func,
        .args = .args,
        .l_targets = .l_targets
      )
      # calculate log posterior:
      l_posterior <- l_prior + l_lilk

      return(l_posterior)
    },
    # Calculate log posterior (one set of parameters at a time)
    #
    # @param .samples A table or vector of sampled parameter values
    # @param .func A function defining the model to be calibrated
    # @param .args A list of arguments to be passed to .func
    # @param .l_targets A list containing a vector of targets' names, a
    # vector of targets' weights, a vector of targets' distributions, and
    # a table for each target that contains the values (column name
    # 'value') and standard errors (column name 'sd') of the corresponding
    # target.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    log_posterior = function(
      .samples,
      .func = self$calibration_model,
      .args = self$calibration_model_args,
      .l_targets = self$calibration_targets,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {
      # calculate log prior:
      l_prior <- private$log_prior(
        .samples = .samples,
        .l_params = .l_params,
        .transform = .transform
      )
      # calculate log likelihood:
      l_lilk <- private$log_likelihood(
        .samples = .samples,
        .func = .func,
        .args = .args,
        .l_targets = .l_targets
      )
      # calculate log posterior:
      l_posterior <- l_prior + l_lilk

      return(l_posterior)
    },
    # Calculate posterior densities
    #
    # @param .samples A table or vector of sampled parameter values
    # @param .func A function defining the model to be calibrated
    # @param .args A list of arguments to be passed to .func
    # @param .l_targets A list containing a vector of targets' names, a
    # vector of targets' weights, a vector of targets' distributions, and
    # a table for each target that contains the values (column name
    # 'value') and standard errors (column name 'sd') of the corresponding
    # target.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    calculate_posteriors = function(
      .samples,
      .func = self$calibration_model,
      .args = self$calibration_model_args,
      .l_targets = self$calibration_targets,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {
      # calculate the posterior:
      posterior <- exp(private$log_posteriors(
        .samples = .samples,
        .func = .func,
        .args = .args,
        .l_targets = .l_targets,
        .l_params = .l_params,
        .transform = .transform
      ))

      return(posterior)
    },
    # Calculate posterior (one set of parameters at a time)
    #
    # @param .samples A table or vector of sampled parameter values
    # @param .func A function defining the model to be calibrated
    # @param .args A list of arguments to be passed to .func
    # @param .l_targets A list containing a vector of targets' names, a
    # vector of targets' weights, a vector of targets' distributions, and
    # a table for each target that contains the values (column name
    # 'value') and standard errors (column name 'sd') of the corresponding
    # target.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    calculate_posterior = function(
      .samples,
      .func = self$calibration_model,
      .args = self$calibration_model_args,
      .l_targets = self$calibration_targets,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters) {
      # calculate the posterior:
      posterior <- exp(private$log_posterior(
        .samples = .samples,
        .func = .func,
        .args = .args,
        .l_targets = .l_targets,
        .l_params = .l_params,
        .transform = .transform
      ))

      return(posterior)
    },
    #### Bayesian helper function:----
    # Calibrate models using Bayesian methods - employing local IMIS_()
    #
    # @param .b_method Character defining the Bayesian method to use in
    # the calibration process. Currently supported methods are `SIR` and
    # `IMIS`.
    # @param .func A function defining the decision analytic model to be
    # calibrated.
    # @param .args A list of arguments passed to the model function.
    # @param .l_targets A list containing a vector of targets' names, a
    # vector of targets' weights, a vector of targets' distributions, and
    # a table for each target that contains the values (column name
    # 'value') and standard errors (column name 'sd') of the corresponding
    # target.
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .samples A table or vector of sampled parameter values
    # @param .n_resample the desired number of draws from the posterior
    # @param .IMIS_sample the incremental sample size at each IMIS
    # iteration
    # @param .IMIS_iterations the maximum number of iterations in IMIS
    # @param .transform Logical for whether to back-transform parameters
    # to their original scale.
    #
    calibrateModel_beyesian = function(
      .b_method = "SIR",
      .func = self$calibration_model,
      .args = self$calibration_model_args,
      .l_targets = self$calibration_targets,
      .l_params = self$calibration_parameters,
      .transform = self$transform_parameters,
      .samples,
      .n_resample = 1000,
      .IMIS_sample = 1000,
      .IMIS_iterations = 30) {
      # Ensure that .b_method is supported by the function:
      stopifnot(".b_method is supported by the function" =
                  any(.b_method %in% c('SIR', 'IMIS', 'MCMC')))

      if(.b_method == 'IMIS' & is.null(.IMIS_sample))
        .IMIS_sample <- 1000

      # SIR:
      if(.b_method == 'SIR') {
        if(nrow(.samples) != .n_resample)
          stop(paste("Please pass", .n_resample, "samples to the function."))
        ## Calculate log-likelihood for each sample value:
        llik <- private$log_likelihood(
          .samples = .samples,
          .func = .func,
          .args = .args,
          .l_targets = .l_targets
        )

        ## Calculate weights for the re-sample:
        # Note: subtracting off the maximum log-likelihood before
        # exponentiating helps avoid numerical under/overflow, which would
        # result in weights of Inf or 0.
        weight <- exp(llik - max(llik)) / sum(exp(llik - max(llik)))
        ## Re-sample from samples with wt as sampling weights:
        SIR_resample  <- sample.int(
          .n_resample,
          replace = TRUE,
          prob = weight
        )
        posterior_SIR <- .samples[SIR_resample, ]
        ## Combine log-likelihood & posterior probability of each sample:
        SIR_results <- cbind(posterior_SIR,
                             "Overall_fit" = llik[SIR_resample],
                             "Posterior_prob" = weight[SIR_resample]) %>%
          as_tibble() %>%
          arrange(desc(Overall_fit))

        return(list('Results' = SIR_results, 'Method' = "SIR"))

      } else if(.b_method == 'IMIS') { # IMIS:
        ## Define function inputs:
        # .l_params_ <<- .l_params # prior/sample.prior
        # .func_ <<- .func # calculate_likelihood
        # .args_ <<- .args # calculate_likelihood
        # .l_targets_ <<- .l_targets # calculate_likelihood
        # .transform_ <<- .transform # prior
        ## Run IMIS:
        fit_IMIS <- IMIS_(
          B = .IMIS_sample, # incremental sample size at each iteration
          B.re = .n_resample, # desired posterior sample size
          number_k = .IMIS_iterations, # maximum iterations
          D = 1, # use optimizer >= 1, do not use = 0.
          sample.prior = private$sample_prior_IMIS,
          prior = private$calculate_prior,
          priors = private$calculate_priors,
          likelihood = private$calculate_likelihood
        )
        ## Obtain draws from posterior:
        m_calib_res <- fit_IMIS$resample
        Overall_fit <- private$log_likelihood(
          .samples = m_calib_res,
          .func = .func,
          .args = .args,
          .l_targets = .l_targets)
        Posterior_prob <- private$calculate_posteriors(
          .samples = m_calib_res, .func = .func, .args = .args,
          .l_targets = .l_targets, .l_params = .l_params)
        ## Calculate log-likelihood (overall fit) and posterior probability:
        IMIS_results <- m_calib_res %>%
          as_tibble(~ vctrs::vec_as_names(...,
                                          repair = "unique",
                                          quiet = TRUE)) %>%
          mutate(
            "Overall_fit" = Overall_fit,
            "Posterior_prob" = Posterior_prob) %>%
          arrange(desc(Overall_fit))
        ## Name column names IMIS stats object:
        stats <- fit_IMIS$stat %>%
          as_tibble(~ vctrs::vec_as_names(...,
                                          repair = "unique",
                                          quiet = TRUE)) %>%
          `colnames<-`(c("MargLike", "UniquePoint", "MaxWeight", "ESS",
                         "ImpWt", "ImpWtVar"))

        return(list('Results' = IMIS_results,
                    'Method' = "IMIS",
                    'Fit'  = fit_IMIS,
                    'Stats' = stats))

      } else {

      }
    }
  )
)
