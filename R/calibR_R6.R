#' R6 Class representing a calibR_R6 calibration machine.
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
    #' @field model_predictions simulated outputs
    model_predictions = NULL,
    #' @field prior_samples a Decision-Analytic model under calibration
    prior_samples = NULL,
    #' @field prior_samples a Decision-Analytic model under calibration
    maximum_a_posteriori = NULL,
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
      self$model_predictions$truth <- self$calibration_model()

      invisible(self)

    },

    #' @description
    #' Sample prior distribution(s) using one or more sampling method.
    #' This function currently supports: LHS, FGS and RGS.
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
    #' @param .calibration_method goodness-of-fit method name.
    #' @param .calibration_function goodness-of-fit function.
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
                                 .calibration_method = 'LLK',
                                 .calibration_function = NULL) {
      private$calibrateR_random_(
        .func = self$calibration_model,
        .args = self$calibration_model_args,
        .optim = .optim,
        .maximise = .maximise,
        .weighted = .weighted,
        .sample_method = .sample_method,
        .l_targets = self$calibration_targets,
        .calibration_method = .calibration_method,
        .calibration_function = .calibration_function
      )
    },

    #' @description
    #' Calibrate the model using one or more directed search method(s).
    #'
    #' @param .gof Name of goodness-of-fit function, default is
    #' log-likelihood.
    #' @param .gof_func Goodness-of-fit function; if NULL (default) the
    #' supported function defined by \code{.gof} will be used
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
                                   .gof_func = NULL,
                                   .n_samples = 1,
                                   .max_iterations = 1000,
                                   temp = 10,
                                   tmax = 10,
                                   .calibration_method = 'NM',
                                   .sample_method = 'LHS',
                                   .maximise = TRUE) {
      private$calibrateR_directed_(
        .gof = .gof,
        .gof_func = .gof_func,
        .n_samples = .n_samples,
        .calibration_method = .calibration_method,
        .sample_method = .sample_method,
        .max_iterations = .max_iterations,
        temp = temp,
        tmax = tmax,
        .maximise = .maximise
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
    #' @param prior_sample_method Sampling method used to generate prior
    #' samples.
    #' @param print_pair_correlations Print pair-wise correlations to
    #' console.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    draw_plots = function(prior_sample_method = "LHS",
                          print_pair_correlations = FALSE) {
      private$draw_priors_posteriors(
        prior_sample_method = prior_sample_method)
      private$draw_pair_correlations()
      if(isTRUE(print_pair_correlations))
        private$print_pair_correlations()
      private$draw_targets()

    },

    #' Print
    #'
    #' @return
    #' @export
    #'
    #' @examples
    print_test = function() {
      # if(!is.null(self$calibration_results$random)) {
      #   tmp <- purrr::map_df(
      #     .x = c('random', 'directed', 'bayesian'),
      #     .f = function(.category_) {
      #       print(.category_)
      #       private$get_MAP_values(
      #         .l_calib_res_lists = self$calibration_results[[.category_]],
      #         .search_method = .category_)
      #     }
      #   )
      #   print(tmp)
      # }
      # if(!is.null(self$calibration_results$directed)) {
      #   tmp <- private$get_MAP_values(
      #     .l_calib_res_lists = self$calibration_results$directed,
      #     .search_method = "directed")
      #   print(tmp)
      # }
      # if(!is.null(self$calibration_results$bayesian)) {
      #   tmp <- private$get_MAP_values(
      #     .l_calib_res_lists = self$calibration_results$bayesian,
      #     .search_method = "bayesian")
      #   print(tmp)
      # }
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
    .calibration_function,
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
        #### others:----
        # if(!.calibration_method %in% c("LLK", "SSE"))
        #   self$calibration_results$
        #     random[[paste0(.calibration_method, "RGS")]] <- list(
        #       .calibration_function(
        #         .samples = self$prior_samples[["RGS"]],
        #       )
        #     )
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
    .gof_func,
    .n_samples,
    .calibration_method,
    .sample_method,
    .max_iterations,
    temp,
    tmax,
    .maximise = TRUE) {
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("NM_", .gof, "_RGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("BFGS_", .gof, "_RGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "BFGS",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("SANN_", .gof, "_RGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "SANN",
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("GA_", .gof, "_RGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["RGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "GA",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("NM_", .gof, "_FGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("BFGS_", .gof, "_FGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "BFGS",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("SANN_", .gof, "_FGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "SANN",
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("GA_", .gof, "_FGS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["FGS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "GA",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("NM_", .gof, "_LHS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "NM",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("BFGS_", .gof, "_LHS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "BFGS",
                maxit = .max_iterations,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("SANN_", .gof, "_LHS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "SANN",
                maxit = .max_iterations,
                temp = temp,
                tmax = tmax,
                .maximise = .maximise,
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
                .maximise = .maximise,
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
                .maximise = .maximise,
                .l_params = self$calibration_parameters,
                .l_targets = self$calibration_targets
              )
          ##### others:----
          if(!.gof %in% c("LLK", "SSE"))
            self$calibration_results$
              directed[[paste0("GA_", .gof, "_LHS")]] <-
              calibR::calibrateModel_directed(
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .gof = .gof,
                .gof_func = .gof_func,
                .samples = self$prior_samples[["LHS"]] %>%
                  dplyr::slice_sample(n = .n_samples),
                .s_method = "GA",
                maxit = .max_iterations,
                .maximise = .maximise,
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
      self$PSA_results <-
        # if the model supports parameter transformation:
        if(!is.null(self$transform_parameters)) {
          calibR::run_PSA(
            .func_ = self$calibration_model,
            .PSA_calib_values_ = c(self$PSA_samples$random,
                                   self$PSA_samples$directed,
                                   self$PSA_samples$bayesian),
            .args_ = c(self$calibration_model_args,
                       "calibrate_" = FALSE,
                       "transform_" = FALSE),
            .PSA_unCalib_values_ = .PSA_unCalib_values_
          )
        } else {
          calibR::run_PSA(
            .func_ = self$calibration_model,
            .PSA_calib_values_ = c(self$PSA_samples$random,
                                   self$PSA_samples$directed,
                                   self$PSA_samples$bayesian),
            .args_ = c(self$calibration_model_args,
                       "calibrate_" = FALSE),
            .PSA_unCalib_values_ = .PSA_unCalib_values_
          )
        }
    },
    ### Summarise PSA:----
    # Summarise PSA results
    #
    summarise_PSA_ = function() {
      self$PSA_summary <- purrr::map_df(
        .x = self$PSA_results,
        .f = function(PSA) {
          data_ <- dplyr::tibble(
            'calibration_method' = if(nrow(PSA) == 1) paste(PSA$Label[[1]], "_*") else PSA$Label[[1]],
            'mean_inc_Costs' = mean(PSA$inc_cost, na.rm = TRUE),
            'mean_inc_LY' = mean(PSA$inc_LY, na.rm = TRUE),
            'iNMB' = (mean_inc_LY * 30000) - mean_inc_Costs,
            'PSA_samples' = nrow(PSA),
            'mean_goodness_of_fit' = mean(PSA$Overall_fit, na.rm = TRUE),
            'effective_sample_size' =
              if(PSA$Label[[1]] %in% c("SIR", "IMIS")) {
                calibR::effective_sample_size(
                  bayes_calib_output_list = self$calibration_results$
                    bayesian[[PSA$Label[[1]]]])
              } else {
                NA
              }
          )
        }
      )
      self$PSA_summary <- self$PSA_summary %>%
        dplyr::arrange(dplyr::desc(mean_goodness_of_fit))
    },
    ## Plots:----
    ### Target plots:----
    # Draw true and predicted  targets
    #
    # @param error_ percentage difference between mean target value and
    # upper/lower values.
    #
    draw_targets = function(error_ = 0.1) {
      # Get MAP values:
      self$maximum_a_posteriori$parameters <- purrr::map_df(
        .x = c('random', 'directed', 'bayesian'),
        .f = function(.category_) {
          private$get_MAP_values(
            .l_calib_res_lists = self$calibration_results[[.category_]],
            .search_method = .category_)
        }
      )
      # Run model using MAP values
      self$maximum_a_posteriori$simulated_results <- purrr::pmap(
        .l = self$maximum_a_posteriori$parameters,
        .f = function(...) {
          params = list(...)
          self$calibration_model(params)
        }
      )
      # set names:
      names(self$maximum_a_posteriori$simulated_results) <-
        self$maximum_a_posteriori$parameters$Label
      # Combine MAP results for same calibration targets
      self$maximum_a_posteriori$simulated_endpoints <- purrr::map(
        .x = self$calibration_targets$v_targets_names,
        .f = function(target_) {
          purrr::map_dfc(
            .x = self$maximum_a_posteriori$simulated_results,
            .f = function(results_) {
              results_[[target_]]
            }
          )
        }
      )
      names(self$maximum_a_posteriori$simulated_endpoints) <-
        self$calibration_targets$v_targets_names
      # Merge MAP simulated targets with
      targets <- purrr::map(
        .x = self$calibration_targets$v_targets_names,
        .f = function(target) {
          self$calibration_targets[[target]] %>%
            dplyr::bind_cols(
              self$maximum_a_posteriori$simulated_endpoints[[target]]
            )
        }
      )
      names(targets) <- self$calibration_targets$v_targets_names
      # Plot targets:
      ## geom_col:
      targets_plots <- purrr::map(
        .x = self$calibration_targets$v_targets_names,
        .f = function(target_) {
          data_ <- targets[[target_]] %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::rename(target = value)
          # add lb and ub if missing:
          if(is.null(data_$lb)) {
            data$lb = data$target * (1 - error_)
            data$ub = data$target * (1 + error_)
          }
          # query x and y axis variables:
          x_var = self$calibration_targets$v_targets_axis[[target_]]$x
          y_var = self$calibration_targets$v_targets_axis[[target_]]$y
          # prepare data:
          data_ <- data_  %>%
            tidyr::pivot_longer(
              cols = c(target,
                       self$maximum_a_posteriori$parameters$Label),
              names_to = "Method",
              values_to = "Values")
          # plot line or bar based on number of data points per target
          if(nrow(targets[[target_]]) < 4) {
            data_ %>%
              ggplot2::ggplot() +
              ggplot2::geom_col(
                ggplot2::aes(
                  x = Values,
                  y = Method,
                  fill = as.factor(.data[[y_var]]),
                  colour = as.factor(.data[[y_var]])),
            alpha = 0.4,
                show.legend = TRUE,
                position = "dodge2") +
              ggplot2::geom_errorbarh(
              # ggplot2::geom_linerange(
                data = data_,
                ggplot2::aes(
                  y = Method,
                  xmax = ub,
                  xmin = lb),
                colour = "black",
                position = "dodge2") +
              ggplot2::labs(
                title = "Observed and simulated maximum-a-posteriori target(s)",
                subtitle = if(!is.null(self$
                                       calibration_targets$
                                       v_targets_labels[[target_]])) {
                  self$calibration_targets$
                    v_targets_labels[[target_]]
                } else {
                  target_
                },
                caption = "Black bars represent target's 95% confidence internval",
                fill = y_var,
                colour = y_var
              ) +
              ggplot2::theme(
                plot.title.position = "plot",
                plot.subtitle = ggplot2::element_text(
                  face = "italic"),
                plot.caption.position= "plot")
          } else {

          }
        }
      )
      names(targets_plots) <- self$calibration_targets$v_targets_names

      self$plots$targets$col_plots <- targets_plots

      ## geom_point:
      targets_plots <- purrr::map(
        .x = self$calibration_targets$v_targets_names,
        .f = function(target_) {
          data_ <- targets[[target_]] %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::rename(target = value)
          # add lb and ub if missing:
          if(is.null(data_$lb)) {
            data$lb = data$target * (1 - error_)
            data$ub = data$target * (1 + error_)
          }
          # query x and y axis variables:
          x_var = self$calibration_targets$v_targets_axis[[target_]]$x
          y_var = self$calibration_targets$v_targets_axis[[target_]]$y
          if(is.null(x_var)) x_var = id
          if(is.null(y_var)) y_var = id
          # prepare data:
          data_ <- data_ %>%
            tidyr::pivot_longer(
              cols = c(target,
                       self$maximum_a_posteriori$parameters$Label),
              names_to = "Method",
              values_to = "Values")
          # plot line or bar based on number of data points per target
          if(nrow(targets[[target_]]) < 4) {
            data_ %>%
              dplyr::filter(!Method == "target") %>%
              ggplot2::ggplot() +
              ggplot2::geom_point(
                ggplot2::aes(
                  x = .data[[y_var]],
                  y = Values,
                  fill = Method,
                  colour = Method),
                alpha = 0.4,
                show.legend = TRUE) +
              ggplot2::geom_line(
                ggplot2::aes(
                  x = .data[[y_var]],
                  y = Values,
                  colour = Method),
                alpha = 0.4,
                show.legend = FALSE) +
              ggplot2::geom_errorbar(
                data = data_ %>%
                  dplyr::filter(Method == "target"),
                ggplot2::aes(
                  x = .data[[y_var]],
                  ymax = ub,
                  ymin = lb,
                  colour = Method),
                colour = "black",
                position = "dodge2") +
              ggplot2::labs(
                title = "Observed and simulated maximum-a-posteriori target(s)",
                subtitle = if(!is.null(self$
                                       calibration_targets$
                                       v_targets_labels[[target_]])) {
                  self$calibration_targets$
                    v_targets_labels[[target_]]
                } else {
                  target_
                },
                caption = "Black bars represent target's 95% confidence internval"
              ) +
              ggplot2::theme(
                plot.title.position = "plot",
                plot.subtitle = ggplot2::element_text(
                  face = "italic"),
                plot.caption.position= "plot")
          } else {

          }
        }
      )
      names(targets_plots) <- self$calibration_targets$v_targets_names

      self$plots$targets$point_plots <- targets_plots

      # geom vlines:
      ## geom_point:
      targets_plots <- purrr::map(
        .x = self$calibration_targets$v_targets_names,
        .f = function(target_) {
          data_ <- targets[[target_]] %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::rename(target = value)
          # add lb and ub if missing:
          if(is.null(data_$lb)) {
            data$lb = data$target * (1 - error_)
            data$ub = data$target * (1 + error_)
          }
          # query x and y axis variables:
          x_var = self$calibration_targets$v_targets_axis[[target_]]$x
          y_var = self$calibration_targets$v_targets_axis[[target_]]$y
          # prepare data:
          data_ <- data_ %>%
            tidyr::pivot_longer(
              cols = c(target,
                       self$maximum_a_posteriori$parameters$Label),
              names_to = "Method",
              values_to = "Values")
          # plot line or bar based on number of data points per target
          if(nrow(targets[[target_]]) < 4) {
            data_ %>%
              dplyr::filter(!Method == "target") %>%
              ggplot2::ggplot() +
              ggplot2::geom_point(
                ggplot2::aes(
                  x = Values,
                  y = .data[[y_var]],
                  group = Method,
                  colour = Method),
                show.legend = TRUE) +
              # 95% CI lower bound:
              ggplot2::geom_vline(
                data = data_ %>%
                  dplyr::filter(Method == "target"),
                ggplot2::aes(
                  xintercept = lb),
                lty = 2,
                colour = "black") +
              # target mean value:
              ggplot2::geom_vline(
                data = data_ %>%
                  dplyr::filter(Method == "target"),
                ggplot2::aes(
                  xintercept = Values),
                lty = 1,
                colour = "black") +
              # 95% CI upper bound:
              ggplot2::geom_vline(
                data = data_ %>%
                  dplyr::filter(Method == "target"),
                ggplot2::aes(
                  xintercept = ub),
                lty = 2,
                colour = "black") +
              ggplot2::labs(
                title = "Observed and simulated maximum-a-posteriori target(s)",
                subtitle = if(!is.null(self$
                                       calibration_targets$
                                       v_targets_labels[[target_]])) {
                  self$calibration_targets$
                    v_targets_labels[[target_]]
                } else {
                  target_
                },
                caption = "Black bars represent target's 95% confidence internval") +
              ggplot2::theme(
                plot.title.position = "plot",
                plot.subtitle = ggplot2::element_text(
                  face = "italic"),
                plot.caption.position= "plot")
          } else {

          }
        }
      )
      names(targets_plots) <- self$calibration_targets$v_targets_names

      self$plots$targets$vline_plots <- targets_plots

    },
    ### Prior-posterior plots:----
    # Create combined prior and posterior line plots
    #
    # @param data_ Data set containing prior and posterior data
    # @param ggplot_ Boolean, \code{TRUE} to generate ggplot2 otherwise
    # trelliscopejs
    # @param plots_row Number of rows in faceted plot or faceted plot page
    # @param plots_col Number of columns in faceted plot or faceted plot
    # page
    # @param log_scaled TRUE to use log scale
    #
    draw_priors_posteriors_line_plots = function(data_,
                                                 ggplot_,
                                                 plots_row,
                                                 plots_col,
                                                 log_scaled = TRUE) {
      plots_ <- purrr::map(
        .x = self$calibration_parameters$v_params_names,
        .f = function(.parameter_) {
          plot_ <- data_ %>%
            dplyr::filter(!Label %in% "Prior") %>%
            dplyr::rename(Method = Label) %>%
            ggplot2::ggplot() +
            ggplot2::geom_histogram(
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ..density..),
              bins = 30,
              fill = "white",
              colour = "darkgrey") +
            ggplot2::geom_density(
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ..density..),
              fill = "cadetblue",
              col = "blue",
              alpha = 0.4) +
            ggplot2::geom_histogram(
              data = data_ %>%
                dplyr::filter(Label %in% "Prior") %>%
                dplyr::rename(Method = Label),
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ..density..),
              bins = 30,
              fill = "white",
              colour = "darkgrey") +
            ggplot2::geom_density(
              data = data_ %>%
                dplyr::filter(Label %in% "Prior") %>%
                dplyr::rename(Method = Label),
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ..density..),
              fill = "red",
              col = "red",
              alpha = 0.2) +
            ggplot2::theme(
              plot.title.position = "plot",
              plot.subtitle = ggplot2::element_text(
                face = "italic"),
              axis.ticks.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_blank(),
              axis.title.x = ggplot2::element_blank()) +
            ggplot2::labs(
              title = "Prior and posterior(s) plots",
              subtitle = paste0(
                "x-axis shows \"", .parameter_, "\" values"
              ))

          # if log scale to be used
          if(log_scaled) {
            plot_ <- plot_ +
              ggplot2::scale_x_log10() +
              ggplot2::labs(
                title = "Prior and posterior(s) plots",
                subtitle = paste0(
                  "x-axis shows \"", .parameter_, "\" values on a logarithmic scale"
                ))
          }
          # If true values are known
          if(!is.null(self$calibration_parameters$
                      v_params_true_values[[.parameter_]])) {
            plot_ <- plot_ +
              ggplot2::geom_vline(
                xintercept = self$calibration_parameters$
                  v_params_true_values[[.parameter_]],
                show.legend = TRUE) +
              ggplot2::labs(
                caption = paste0(
                  "The black vertical lines represent \"",
                  .parameter_,
                  "\"'s ",
                  "true value: (",
                  round(self$calibration_parameters$
                    v_params_true_values[[.parameter_]], 2),
                  ")"
                ))
          }
          # ggplot2 or trelliscopejs
          if(ggplot_) {
            plot_ <- plot_ +
              ggplot2::facet_wrap(
                facets = ~ Method,
                nrow = plots_row,
                ncol = plots_col,
                scales = "free")
          } else {
            plot_ <- plot_ +
              trelliscopejs::facet_trelliscope(
                facets = ~ Method,
                nrow = 2,
                ncol = 3,
                self_contained = TRUE)
          }
        }
      )

      return(plots_)
    },
    # Create combined prior and posterior line plots
    #
    # @param data_ Data set containing prior and posterior data
    # @param ggplot_ Boolean, \code{TRUE} to generate ggplot2 otherwise
    # trelliscopejs
    # @param plots_row Number of rows in faceted plot or faceted plot page
    # @param plots_col Number of columns in faceted plot or faceted plot
    # page
    # @param log_scaled TRUE to use log scale
    #
    draw_density_line_plots = function(data_,
                                       ggplot_,
                                       plots_row,
                                       plots_col,
                                       log_scaled = FALSE) {
      plots_ <- purrr::map(
        .x = self$calibration_parameters$v_params_names,
        .f = function(.parameter_) {
          plot_ <- data_ %>%
            dplyr::rename(Method = Label) %>%
            ggplot2::ggplot(
              ggplot2::aes(
                x = .data[[.parameter_]])
            ) +
            ggplot2::geom_density(
              ggplot2::aes(
                colour = Method,
                fill = Method),
              alpha = 0.4,
              show.legend = FALSE) +
            ggplot2::geom_histogram(
              ggplot2::aes(
                y = ..density..),
              binwidth = 5,
              fill = "white") +
            ggplot2::theme(
              plot.title.position = "plot",
              plot.subtitle = ggplot2::element_text(
                face = "italic"),
              axis.ticks.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_blank(),
              axis.title.x = ggplot2::element_blank()) +
            ggplot2::labs(
              title = "Prior and posterior(s) plots",
              subtitle = paste0(
                "x-axis shows \"", .parameter_, "\" values"
              ))
          # if log scale to be used
          if(log_scaled) {
            plot_ <- plot_ +
              ggplot2::scale_x_log10() +
              ggplot2::labs(
                title = "Prior and posterior(s) plots",
                subtitle = paste0(
                  "x-axis shows \"", .parameter_, "\" values on a logarithmic scale"
                ))
          }
          # If true values are known
          if(!is.null(self$calibration_parameters$
                      v_params_true_values[[.parameter_]])) {
            plot_ <- plot_ +
              ggplot2::geom_vline(
                xintercept = self$calibration_parameters$
                  v_params_true_values[[.parameter_]],
                show.legend = TRUE) +
              ggplot2::labs(
                caption = paste0(
                  "The black vertical lines represent \"",
                  .parameter_,
                  "\"'s ",
                  "true value: (",
                  round(self$calibration_parameters$
                          v_params_true_values[[.parameter_]], 2),
                  ")"
                ))
          }
          # ggplot2 or trelliscopejs
          if(ggplot_) {
            plot_ <- plot_ +
              ggplot2::facet_wrap(
                facets = ~ Method,
                nrow = plots_row,
                ncol = plots_col,
                scales = "free")
          } else {
            plot_ <- plot_ +
              trelliscopejs::facet_trelliscope(
                facets = ~ Method,
                nrow = 2,
                ncol = 3,
                self_contained = TRUE)
          }
        }
      )

      return(plots_)
    },
    # Create combined prior and posterior box plots
    #
    # @param data_ Data set containing prior and posterior data
    # @param ggplot_ Boolean, \code{TRUE} to generate ggplot2 otherwise
    # trelliscopejs
    # @param plots_row Number of rows in faceted plot or faceted plot page
    # @param plots_col Number of columns in faceted plot or faceted plot
    # page
    # @param log_scaled TRUE to use log scale
    # @param facet_scale_ "fixed" or "free" facet scale
    #
    draw_priors_posteriors_box_plots = function(data_,
                                                ggplot_,
                                                plots_row,
                                                plots_col,
                                                log_scaled = TRUE,
                                                facet_scale_ = "fixed") {
      plots_ <- purrr::map(
        .x = self$calibration_parameters$v_params_names,
        .f = function(.parameter_) {
          plot_ <- data_ %>%
            dplyr::rename(Method = Label) %>%
            dplyr::mutate(
              Method = reorder(
                Method,
                .data[[.parameter_]],
                FUN = function(x) {
                  diff(range(x))
                }
              )) %>%
            ggplot2::ggplot() +
            ggplot2::geom_boxplot(
              ggplot2::aes(
                x = .data[[.parameter_]],
                group = Method,
                fill = Method,
                col = Method),
              alpha = 0.4,
              show.legend = FALSE) +
            ggplot2::theme(
              plot.title.position = "plot",
              plot.subtitle = ggplot2::element_text(
                face = "italic"),
              axis.title.x = ggplot2::element_blank(),
              axis.title.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              axis.text.y = ggplot2::element_blank(),
              panel.border = ggplot2::element_rect(
                colour = "black",
                fill = NA)) +
            ggplot2::labs(
              title = "Prior and posterior(s) box-plots",
              subtitle = paste0(
                "x-axis shows \"", .parameter_, "\" values"
              ))
          # If true values are known
          if(!is.null(self$calibration_parameters$
                      v_params_true_values[[.parameter_]])) {
            plot_ <- plot_ +
              ggplot2::geom_vline(
                xintercept = self$calibration_parameters$
                  v_params_true_values[[.parameter_]],
                show.legend = TRUE) +
              ggplot2::labs(
                caption = paste0(
                  "The black vertical lines represent \"",
                  .parameter_,
                  "\"'s ",
                  "true value: (",
                  round(self$calibration_parameters$
                          v_params_true_values[[.parameter_]], 2),
                  ")"
                ))
          }
          # if log scale to be used
          if(log_scaled) {
            plot_ <- plot_ +
              ggplot2::scale_x_log10() +
              ggplot2::labs(
                subtitle = paste0(
                  "x-axis shows \"", .parameter_, "\" values on a logarithmic scale"
                ))
          }
          # ggplot2 or trelliscopejs
          if(ggplot_) {
            plot_ +
              ggplot2::facet_wrap(
                facets = ~ Method,
                scales = facet_scale_,
                ncol = 1,
                strip.position = "left") +
              ggplot2::theme(
                strip.text.y.left = ggplot2::element_text(angle = 0))
          } else {
            plot_ <- plot_ +
              trelliscopejs::facet_trelliscope(
                facets = ~ Method,
                nrow = 2,
                ncol = 3,
                self_contained = TRUE)
          }
        }
      )

      return(plots_)
    },
    # Draw priors and posteriors plots
    #
    # @param prior_sample_method
    # @param plots_row
    # @param plots_col
    #
    draw_priors_posteriors = function(prior_sample_method = "LHS") {
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
            self$prior_samples[[prior_sample_method]] %>%
              dplyr::mutate(Label = 'Prior') %>%
              calibR::backTransform(
                .t_data_ = .,
                .l_params_ = self$calibration_parameters
              )
          )
      } else {
        data_ %>%
          dplyr::bind_rows(
            self$prior_samples[[prior_sample_method]] %>%
              dplyr::mutate(Label = 'Prior')
          )
      }

      data2_ = data_ %>%
        tidyr::pivot_longer(
          cols = self$calibration_parameters$v_params_names,
          names_to = "Parameter",
          values_to = "Distribution draws")
      # If true values are known
      if(!is.null(self$calibration_parameters$v_params_true_values)) {
        data2_ = data2_ %>%
          dplyr::bind_rows(
            self$calibration_parameters$
              v_params_true_values %>%
              dplyr::as_tibble(rownames = "Parameter") %>%
              dplyr::rename(`Distribution draws` = value) %>%
              dplyr::mutate(Label = "True"))
      }
      # Make a quick effort to get the best faceted plot:
      tot_plots <- length(
        c(self$PSA_samples$random,
          self$PSA_samples$directed,
          self$PSA_samples$bayesian) %>%
          purrr::transpose() %>%
          .[['PSA_calib_draws']]) + 1
      if(tot_plots > ceiling(sqrt(tot_plots))^2 -
         ceiling(sqrt(tot_plots))) {
        p_row <- p_col <- ceiling(sqrt(tot_plots))
      } else {
        p_row <- ceiling(sqrt(tot_plots))
        p_col <- ceiling(sqrt(tot_plots)) - 1
      }

      # One parameter at a time:
      ## ggplot2:
      self$plots$prior_posterior$parameters_ggplot$line_log <-
        private$draw_priors_posteriors_line_plots(
          data_ = data_,
          ggplot_ = TRUE,
          plots_row = p_row,
          plots_col = p_row,
          log_scaled = TRUE
        )
      self$plots$prior_posterior$parameters_ggplot$line <-
        private$draw_priors_posteriors_line_plots(
          data_ = data_,
          ggplot_ = TRUE,
          plots_row = p_row,
          plots_col = p_row,
          log_scaled = FALSE
        )
      self$plots$prior_posterior$parameters_ggplot$box <-
        private$draw_priors_posteriors_box_plots(
          data_ = data_,
          ggplot_ = TRUE,
          plots_row = p_row,
          plots_col = p_col,
          log_scaled = TRUE,
          facet_scale_ = "free"
        )

      names(self$plots$prior_posterior$parameters_ggplot$line_log) <-
        names(self$plots$prior_posterior$parameters_ggplot$line) <-
        names(self$plots$prior_posterior$parameters_ggplot$box) <-
        self$calibration_parameters$v_params_names
      ## trelliscopejs:
      self$plots$prior_posterior$parameters_trelli$line_log <-
        private$draw_priors_posteriors_line_plots(
          data_ = data_,
          ggplot_ = FALSE,
          plots_row = p_row,
          plots_col = p_col,
          log_scaled = TRUE
        )
      self$plots$prior_posterior$parameters_trelli$line <-
        private$draw_density_line_plots(
          data_ = data_,
          ggplot_ = FALSE,
          plots_row = p_row,
          plots_col = p_col,
          log_scaled = FALSE
        )
      self$plots$prior_posterior$parameters_trelli$box <-
        private$draw_priors_posteriors_box_plots(
          data_ = data_,
          ggplot_ = FALSE,
          plots_row = p_row,
          plots_col = p_col
        )
      names(self$plots$prior_posterior$parameters_trelli$line_log) <-
        names(self$plots$prior_posterior$parameters_trelli$line) <-
        names(self$plots$prior_posterior$parameters_trelli$box) <-
        self$calibration_parameters$v_params_names

      # All parameters at once:
      ## ggplot2:
      self$plots$prior_posterior$all_ggplot$line_log <-
        gridExtra::marrangeGrob(
          grobs = self$plots$prior_posterior$parameters_ggplot$line_log,
          nrow = 1,
          ncol = 1)
      self$plots$prior_posterior$all_ggplot$line <-
        gridExtra::marrangeGrob(
          grobs = self$plots$prior_posterior$parameters_ggplot$line,
          nrow = 1,
          ncol = 1)
      self$plots$prior_posterior$all_ggplot$box <-
        gridExtra::marrangeGrob(
          grobs = self$plots$prior_posterior$parameters_ggplot$box,
          nrow = 1,
          ncol = 1)
      ## trelliscopejs:
      all_plots <- data2_ %>%
        dplyr::filter(!Label %in% c("Prior", "True")) %>%
        dplyr::rename(Method = Label) %>%
        ggplot2::ggplot() +
        ggplot2::geom_density(
          ggplot2::aes(
            x = `Distribution draws`,
            y = ..scaled..),
          fill = "cadetblue",
          col = "blue",
          alpha = 0.4,
          show.legend = TRUE) +
        ggplot2::geom_density(
          data = data2_ %>%
            dplyr::filter(Label %in% "Prior") %>%
            dplyr::rename(Prior = Label),
          ggplot2::aes(
            x = `Distribution draws`,
            y = ..scaled..),
          fill = "red",
          col = "red",
          alpha = 0.2,
          show.legend = TRUE) +
        ggplot2::theme(
          axis.ticks.y = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank()) +
        ggplot2::scale_x_log10()
      # If true values are known
      # if(!is.null(self$calibration_parameters$v_params_true_values)) {
      #   all_plots <- all_plots +
      #     ggplot2::geom_vline(
      #       data = data2_ %>%
      #         dplyr::filter(Label %in% "TRUE") %>%
      #         dplyr::rename(True = Label),
      #       ggplot2::aes(
      #         xintercept = `Distribution draws`
      #       )
      #     )
      # }


      self$plots$prior_posterior$all_trelli <- all_plots +
        trelliscopejs::facet_trelliscope(
          facets = Parameter ~ Method,
          nrow = 2,
          ncol = 5,
          self_contained = TRUE)
    },
    ### Correlation plots:----
    draw_pair_correlations = function() {
      # pair correlations by calibration methods category:
      for(method in c('random', 'directed', 'bayesian')) {
        tryCatch(
          expr = {
            data_ <- self$PSA_samples[[method]] %>%
              purrr::transpose() %>%
              .[['PSA_calib_draws']] %>%
              dplyr::bind_rows() %>%
              dplyr::group_by(Label) %>%
              dplyr::mutate(Count = dplyr::n()) %>%
              dplyr::ungroup() %>%
              dplyr::filter(Count != 1) %>%
              dplyr::select(-Count)

            self$plots$correlations[[method]] <- GGally::ggpairs(
              data = data_,
              columns = colnames(
                data_ %>%
                  dplyr::select(-c(Overall_fit, Label))
              ),
              ggplot2::aes(color = Label, fill = Label),
              upper = list(
                continuous = GGally::wrap(
                  'cor',
                  size = 3)),
              lower = list(
                continuous = GGally::wrap(
                  "points",
                  alpha = 0.5)),
              diag = list(
                continuous = GGally::wrap(
                  "densityDiag",
                  alpha = 0.5))
              )

            self$plots$correlations[[method]] <-
              self$plots$correlations[[method]] +
              ggplot2::labs(
                title = "Pair-wise correlation matirx of calibration parameters",
                subtitle =paste(
                  "grouped by",
                  method,
                  "calibration method(s)")) +
              ggplot2::theme(
                plot.title.position = "plot",
                plot.subtitle = ggplot2::element_text(
                  face = "italic"))

          }, error = function(e) {
            message(paste0("\r", e))

            NULL
          }
        )
      }
      # all pair correlations:
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

      self$plots$correlations$all <- GGally::ggpairs(
        data = data_,
        columns = colnames(
          data_ %>%
            dplyr::select(-c(Overall_fit, Label))
        ),
        ggplot2::aes(color = Label, fill = Label),
        upper = list(
          continuous = GGally::wrap(
            'cor',
            size = 3)),
        lower = list(
          continuous = GGally::wrap(
            "points",
            alpha = 0.5)),
        diag = list(
          continuous = GGally::wrap(
            "densityDiag",
            alpha = 0.5))
      )

      self$plots$correlations$all <-
        self$plots$correlations$all +
        ggplot2::labs(
          title = "Pair-wise correlation matirx of calibration parameters",
          subtitle = "grouped by calibration method(s)") +
        ggplot2::theme(
          plot.title.position = "plot",
          plot.subtitle = ggplot2::element_text(
            face = "italic"))

    },
    ### Printable correlation plot:----
    print_pair_correlations = function() {
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

      data_ %>%
        dplyr::select(-c(Overall_fit, Label)) %>%
        psych::pairs.panels()
    },
    ## Helper functions:----
    # Get Maximum a-posteriori (MAP) values from calibration methods
    #
    # @param .l_calib_res_lists
    # @param .search_method
    # @param .l_params
    # @param .transform_
    #
    get_MAP_values = function(.l_calib_res_lists,
                              .search_method,
                              .l_params = self$calibration_parameters,
                              .transform_ = self$transform_parameters) {
      # Apply appropriate extraction method:
      if(.search_method == 'directed') {
        results <- purrr::map_df(
          .x = .l_calib_res_lists,
          .f = function(.list_ = .x) {
            max_a_posteriori <- .list_[[1]][["Estimate"]] %>%
              t() %>%
              dplyr::as_tibble(~ vctrs::vec_as_names(
                ...,
                repair = "unique",
                quiet = TRUE)) %>%
              `colnames<-`(.list_[[1]][["Params"]]) %>%
              dplyr::mutate(
                Label =
                  .list_[[1]][["Calibration method"]],
                Overall_fit =
                  round(.list_[[1]][["GOF value"]], 2)) %>%
              dplyr::select(Label, Overall_fit, dplyr::everything())
          })
      } else if(.search_method == 'random') {
        results <- purrr::map_df(
          .x = .l_calib_res_lists,
          .f = function(.data_ = .x) {
            max_a_posteriori = .data_ %>%
              dplyr::slice_max(
                order_by = Overall_fit,
                n = 1) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::select(Label, Overall_fit, dplyr::everything())
          }
        )
      } else {
        results <- purrr::map_df(
          .x = .l_calib_res_lists,
          .f = function(.list_ = .x) {
            max_a_posteriori <- .list_[["Results"]] %>%
              dplyr::mutate(Label = .list_[["Method"]]) %>%
              dplyr::select(-Posterior_prob) %>%
              dplyr::slice_max(
                order_by = Overall_fit,
                n = 1) %>%
              dplyr::slice_head(n = 1) %>%
              dplyr::select(Label, Overall_fit, dplyr::everything())
          }
        )
      }

      return(results)
    },
    # Get the names of the calibration methods used
    #
    get_calibration_methods = function() {
      methods_names <- names(
        c(self$PSA_samples$random,
          self$PSA_samples$directed,
          self$PSA_samples$bayesian)
      )

      return(methods_names)
    }
  )
)
