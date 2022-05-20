#' R6 Class representing a calibrateR_R6 calibration machine.
#'
#' @description
#' An instance of this class is expected to calibrate a user-defined model
#' and produce summary plots/tables.
#' @format An [R6::R6Class] object.
#' @name calibR_R6
NULL
#'
#' @rdname calibR_R6
#' @export
#'
#' @examples
#' \dontrun{
#' }
calibR_R6 <- R6::R6Class(
  # Object name:----
  classname = "calibR_R6",
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
    #' @field transform_parameters boolean for whether to back transform
    #' parameters
    transform_parameters = FALSE,
    #' @field prior_samples a Decision-Analytic model under calibration
    prior_samples = NULL,
    #' @field PSA_samples calibration and un-calibration parameters
    #' samples
    PSA_samples = NULL,
    #' @field PSA_results PSA results
    PSA_results = NULL,
    #' @field PSA_summary PSA results' summary
    PSA_summary = NULL,
    #' @field plots summary plots
    plots = NULL,

    #' @description
    #' Sample prior distribution(s) using one or more sampling method.
    #' This function currently supports: LHS, FGS and RGS.
    #'
    #' @param .model The decision analytic model under calibration
    #' @param .params Calibration parameters
    #' @param .targets Calibration targets
    #' @param .args Calibration model arguments
    #' @param .transform Logical for whether the model will use
    #' transformed parameters. This allows some functions to back
    #' transform the parameters to their original scale before using them.
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
    #' @param .b_methods Bayesian calibration method(s)
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
    calibrateR_bayesian = function(.b_methods = 'SIR',
                                   .n_resample = 1000,
                                   .IMIS_iterations = 10,
                                   .IMIS_sample = 100) {
      private$calibrateR_bayesian_(
        .b_methods = .b_methods,
        .n_resample = .n_resample,
        .IMIS_iterations = .IMIS_iterations,
        .IMIS_sample = .IMIS_sample
      )
    },

    #' @description
    #' Sample PSA values for the calibration parameters
    #'
    #' @param .calibration_methods Bayesian calibration method(s)
    #' @param .PSA_samples Number of PSA sets to sample
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    sample_PSA_values = function(.calibration_methods,
                                 .PSA_samples) {
      private$sample_PSA_values_(
        .calibration_methods = .calibration_methods,
        .PSA_samples = .PSA_samples
      )
    },

    #' @description
    #' Run PSA
    #'
    #' @param .PSA_unCalib_values_ PSA values for un-calibrated parameters
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    run_PSA = function(.PSA_unCalib_values_ = NULL) {
      private$run_PSA_(
        .PSA_unCalib_values_ = NULL
      )
    },

    #' @description
    #' Summarise PSA results
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    summarise_PSA = function() {
      private$summarise_PSA_()
    },

    #' @description
    #' Draw plots
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    draw_plots = function() {
      private$draw_priors_posteriors()
      private$draw_pair_correlations()
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
          calibR::sample_prior_FGS(...)
        )
      ### RGS:----
      if("RGS" %in% .sampling_method)
        self$prior_samples["RGS"] <- list(
          calibR::sample_prior_RGS(...)
        )
      ### LHS:----
      if("LHS" %in% .sampling_method)
        self$prior_samples["LHS"] <- list(
          calibR::sample_prior_LHS(...)
        )
    },
    ## Calibration methods:----
    ### Random:----
    calibrateR_random_ = function(
      .calibration_method,
      .sample_method,
      ...) {
      if("RGS" %in% .sample_method){
        #### SSE:----
        if("SSE" %in% .calibration_method)
          self$calibration_results$random["SSE_RGS"] <- list(
            calibR::wSSE_GOF(
              .samples = self$prior_samples[["RGS"]],
              .sample_method = "RGS",
              ...
            )
          )
        #### LLK:----
        if("LLK" %in% .calibration_method)
          self$calibration_results$random["LLK_RGS"] <- list(
            calibR::LLK_GOF(
              .samples = self$prior_samples[["RGS"]],
              .sample_method = "RGS",
              ...
            )
          )
      }
      if("FGS" %in% .sample_method){
        #### SSE:----
        if("SSE" %in% .calibration_method)
          self$calibration_results$random["SSE_FGS"] <- list(
            calibR::wSSE_GOF(
              .samples = self$prior_samples[["FGS"]],
              .sample_method = "FGS",
              ...
            )
          )
        #### LLK:----
        if("LLK" %in% .calibration_method)
          self$calibration_results$random["LLK_FGS"] <- list(
            calibR::LLK_GOF(
              .samples = self$prior_samples[["FGS"]],
              .sample_method = "FGS",
              ...
            )
          )
      }
      if("LHS" %in% .sample_method){
        #### SSE:----
        if("SSE" %in% .calibration_method)
          self$calibration_results$random["SSE_LHS"] <- list(
            calibR::wSSE_GOF(
              .samples = self$prior_samples[["LHS"]],
              .sample_method = "LHS",
              ...
            )
          )
        #### LLK:----
        if("LLK" %in% .calibration_method)
          self$calibration_results$random["LLK_LHS"] <- list(
            calibR::LLK_GOF(
              .samples = self$prior_samples[["LHS"]],
              .sample_method = "LHS",
              ...
            )
          )
      }
    },
    ### Directed:----
    calibrateR_directed_ = function(
      .gof,
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
              calibR::calibrateModel_directed(
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
    calibrateR_bayesian_ = function(.b_methods,
                                    .n_resample,
                                    .IMIS_iterations,
                                    .IMIS_sample) {
      #### IMIS:----
      if('IMIS' %in% .b_methods) {
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
      if('SIR' %in% .b_methods) {
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
        dplyr::as_tibble(~ vctrs::vec_as_names(...,
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
      tbl_lhs_samp <- purrr::pmap_dfc(
        .l = l_lhs,
        .f = function(.name, .func, p, .arg, .dist) {
          assign(.name,
                 purrr::exec(.func,
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
        dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                        repair = "unique",
                                        quiet = TRUE)) %>%
        `colnames<-`(v_params_names)
      # Get appropriate distributions' and distributions' parameters' objects:
      params_dists <- 'v_params_dists'
      params_args <- 'args'
      # Transform the sampled parameters back to their original scale:
      if(.transform) {
        .samples <- .samples %>%
          calibR::backTransform(.t_data_ = ., .l_params_ = .l_params)
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
      v_lprior <- rowSums(purrr::pmap_df(
        .l = l_lprior,
        .f = function(.name, .func, .dist, .arg, .param) {
          purrr::exec(.func, .param, !!!.arg, log = TRUE)
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
        dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                        repair = "unique",
                                        quiet = TRUE)) %>%
        `colnames<-`(v_params_names)
      # Get appropriate distributions' and distributions' parameters' objects:
      params_dists <- 'v_params_dists'
      params_args <- 'args'
      # Transform the sampled parameters back to their original scale:
      if(.transform) {
        .samples <- .samples %>%
          calibR::backTransform(.t_data_ = ., .l_params_ = .l_params)
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
      v_lprior <- sum(purrr::pmap_dbl(
        .l = l_lprior,
        .f = function(.name, .func, .dist, .arg, .param) {
          purrr::exec(.func, .param, !!!.arg, log = TRUE)
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
          dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                          repair = "unique",
                                          quiet = TRUE))
      # Run the model using each set of sampled parameters:
      model_results <- purrr::pmap(
        .l = as.list(.samples),
        .f = function(...) {
          dots <- list(...)
          purrr::exec(.func, dots, !!!.args)
        })
      # Define inputs list for the pmap function:
      l_llik <- list(.l_targets[['v_targets_names']],
                     paste0('d', .l_targets[['v_targets_dists']]),
                     .l_targets[['v_targets_dists']],
                     .l_targets[['v_targets_weights']])

      # Estimate the overall log likelihood for each model output:
      overall_lliks <- purrr::map_dbl(
        .x = model_results,
        .f = function(.mod_res) {
          # Log likelihood (for each target):
          overall_llik <- purrr::pmap(
            .l = l_llik,
            .f = function(.name, .func, .dist, .weight) {
              tryCatch({
                if(.dist == 'norm') {
                  sum( # Sum all values of that one target, if many
                    purrr::exec(.func,
                         .l_targets[[.name]]$value, # target's sd
                         .mod_res[[.name]], # mean value
                         .l_targets[[.name]]$se, # sd value (target's sd)
                         log = TRUE)
                  ) * .weight # target weight
                } else if(.dist == 'binom') {
                  sum( # Sum all values of that one target, if many
                    purrr::exec(.func,
                         prob = .mod_res[[.name]],
                         x = .l_targets[[.name]]$x,
                         size = .l_targets[[.name]]$size,
                         log = TRUE)
                  ) * .weight # target weight
                } else if(.dist == 'lnorm') {
                  sum( # Sum all values of that one target, if many
                    purrr::exec(.func,
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
            purrr::reduce(`+`, .init = 0)
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
    #### Bayesian calibration function:----
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
          dplyr::as_tibble() %>%
          dplyr::arrange(dplyr::desc(Overall_fit))

        return(list('Results' = SIR_results, 'Method' = "SIR"))

      } else if(.b_method == 'IMIS') { # IMIS:
        ## Run IMIS:
        fit_IMIS <- calibR::IMIS_(
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
          dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                          repair = "unique",
                                          quiet = TRUE)) %>%
          dplyr::mutate(
            "Overall_fit" = Overall_fit,
            "Posterior_prob" = Posterior_prob) %>%
          dplyr::arrange(dplyr::desc(Overall_fit))
        ## Name column names IMIS stats object:
        stats <- fit_IMIS$stat %>%
          dplyr::as_tibble(~ vctrs::vec_as_names(...,
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
    },
    ## PSA:----
    ### Sample PSA values for calibration parameters:----
    # Sample PSA values for the calibration parameters
    #
    # @param .calibration_methods calibration methods
    # @param .PSA_samples number of PSA samples
    #
    sample_PSA_values_ = function(
      .calibration_methods,
      .PSA_samples) {
      # Random calibration methods:
      if('Random' %in% .calibration_methods) {
        self$PSA_samples$random <- calibR::PSA_calib_values(
          .l_calib_res_lists = self$calibration_results$random,
          .search_method = 'Random',
          .PSA_samples = .PSA_samples,
          .transform_ = self$transform_parameters,
          .l_params = self$calibration_parameters
        )
      }
      # Directed calibration methods:
      if('Directed' %in% .calibration_methods) {
        self$PSA_samples$directed <- calibR::PSA_calib_values(
          .l_calib_res_lists = self$calibration_results$directed,
          .search_method = 'Directed',
          .PSA_samples = .PSA_samples,
          .transform_ = self$transform_parameters,
          .l_params = self$calibration_parameters
        )
      }
      # Bayesian calibration methods:
      if('Bayesian' %in% .calibration_methods) {
        self$PSA_samples$bayesian <- calibR::PSA_calib_values(
          .l_calib_res_lists = self$calibration_results$bayesian,
          .search_method = 'Bayesian',
          .PSA_samples = .PSA_samples,
          .transform_ = self$transform_parameters,
          .l_params = self$calibration_parameters
        )
      }
    },
    ### Run PSA:----
    # Perform PSA analysis
    #
    # @param .PSA_unCalib_values_ PSA values for un-calibrated parameters
    #
    run_PSA_ = function(
      .PSA_unCalib_values_) {
      self$PSA_results <- calibR::run_PSA(
        .func_ = self$calibration_model,
        .PSA_calib_values_ = c(self$PSA_samples$random,
                               self$PSA_samples$directed,
                               self$PSA_samples$bayesian),
        .args_ = c(self$calibration_model_args,
                   "calibrate_" = FALSE),
        .PSA_unCalib_values_ = .PSA_unCalib_values_
      )
    },
    ### Summarise PSA:----
    # Summarise PSA results
    #
    summarise_PSA_ = function() {
      self$PSA_summary <- purrr::map_df(
        .x = self$PSA_results,
        .f = function(PSA) {
          data_ <- dplyr::tibble(
            'mean_inc_Costs' = mean(PSA$inc_cost, na.rm = TRUE),
            'mean_inc_LY' = mean(PSA$inc_LY, na.rm = TRUE),
            'iNMB' = (mean_inc_LY * 30000) - mean_inc_Costs,
            'calibration_method' = if(nrow(PSA) == 1) paste(PSA$Label[[1]], "_*") else PSA$Label[[1]],
            'goodness_of_fit' = mean(PSA$Overall_fit, na.rm = TRUE)
          )
        }
      )
    },
    ## Plots:----
    ### Prior-posterior plots:----
    draw_priors_posteriors = function(sample_method = "LHS") {
      data_ <- c(self$PSA_samples$random,
                 self$PSA_samples$directed,
                 self$PSA_samples$bayesian) %>%
        purrr::transpose() %>%
        .[['PSA_calib_draws']] %>%
        dplyr::bind_rows() %>%
        dplyr::select(
          !!!dplyr::all_of(
            dplyr::syms(
              self$calibration_parameters$v_params_names)), Label)

      data_ <- if(self$transform_parameters) {
        data_ %>%
          dplyr::bind_rows(
            self$prior_samples[[sample_method]] %>%
              dplyr::mutate(Label = 'Prior') %>%
              calibR::backTransform(
                .t_data_ = .,
                .l_params_ = self$calibration_parameters
              )
          )
      } else {
        data_ %>%
          dplyr::bind_rows(
            self$prior_samples[[sample_method]] %>%
              dplyr::mutate(Label = 'Prior')
          )
      }

      data2_ = data_ %>%
        tidyr::pivot_longer(
          cols = self$calibration_parameters$v_params_names,
          names_to = "Parameter",
          values_to = "Distribution draws")

      self$plots$prior_posterior <- purrr::map(
        .x = self$calibration_parameters$v_params_names,
        .f = function(.parameter_) {
          data_ %>%
            dplyr::filter(!Label %in% "Prior") %>%
            dplyr::rename(Method = Label) %>%
            ggplot2::ggplot(
              ggplot2::aes(
                x = .data[[.parameter_]])) +
            ggplot2::geom_density(
              fill = "cadetblue",
              col = "blue",
              alpha = 0.2) +
            ggplot2::geom_density(data = data_ %>%
                           dplyr::filter(Label %in% "Prior") %>%
                           dplyr::rename(Prior = Label),
                         ggplot2::aes(
                           x = .data[[.parameter_]]),
                         fill = "red",
                         col = "red",
                         alpha = 0.2) +
            ggplot2::ylab(NULL) +
            ggplot2::coord_cartesian(xlim = c(0, 4)) +
            trelliscopejs::facet_trelliscope(
              facets = ~Method,
              nrow = 2,
              ncol = 4
            )
        }
      )
      names(self$plots$prior_posterior) <-
        self$calibration_parameters$v_params_names

      self$plots$prior_posterior2 <- data2_ %>%
        dplyr::filter(!Label %in% "Prior") %>%
        dplyr::rename(Method = Label) %>%
        ggplot2::ggplot(
          ggplot2::aes(
            x = `Distribution draws`)) +
        ggplot2::geom_density(
          fill = "cadetblue",
          col = "blue",
          alpha = 0.2) +
        ggplot2::geom_density(data = data2_ %>%
                       dplyr::filter(Label %in% "Prior") %>%
                       dplyr::rename(Prior = Label),
                     ggplot2::aes(
                       x = `Distribution draws`),
                     fill = "red",
                     col = "red",
                     alpha = 0.2) +
        ggplot2::ylab(NULL) +
        ggplot2::coord_cartesian(xlim = c(0, 4)) +
        trelliscopejs::facet_trelliscope(
          self_contained = TRUE,
          facets = Parameter~Method,
          nrow = 2,
          ncol = 5
        )
    },
    ### Correlation plots:----
    draw_pair_correlations = function() {
      data_ <- c(self$PSA_samples$random,
                 self$PSA_samples$directed,
                 self$PSA_samples$bayesian) %>%
        purrr::transpose() %>%
        .[['PSA_calib_draws']] %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(Label) %>%
        dplyr::mutate(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::filter(Count != 1) %>%
        dplyr::select(-Count)

      self$plots$correlations <- GGally::ggpairs(
        data = data_,
        columns = colnames(
          data_ %>%
            dplyr::select(-c(Overall_fit, Label))
        ),
        ggplot2::aes(color = Label),
        upper = list(continuous = GGally::wrap('cor',
                                               size = 3)),
        lower = list(combo = GGally::wrap("facethist",
                                          bins = 30)),
        diag = list(continuous = GGally::wrap("densityDiag",
                                              alpha = 0.5)),
        title = "Scatterplot matrix of calibration parameters grouped by calibration method"
      )

      self$plots$correlations2 <- data_ %>%
        dplyr::select(-c(Overall_fit, Label)) %>%
        psych::pairs.panels()

    }
  )
)
