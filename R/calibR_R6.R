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
    #' @field model_interventions model interventions to run cost-effectiveness
    #' analysis
    model_interventions = NULL,
    #' @field transform_parameters logical for whether to back transform
    #' parameters
    transform_parameters = FALSE,
    #' @field GOF_measure_plot log likelihood values for plot
    GOF_measure_plot = NULL,
    #' @field model_predictions simulated outputs
    model_predictions = NULL,
    #' @field prior_samples samples from parameters' priors
    prior_samples = NULL,
    #' @field maximum_a_posteriori parameter set with maximum posterior
    #' probability
    maximum_a_posteriori = NULL,
    #' @field PSA_samples calibration and un-calibration parameters
    #' samples
    PSA_samples = NULL,
    #' @field PSA_results Probabilistic Sensitivity Analysis (PSA) results
    PSA_results = NULL,
    #' @field PSA_summary PSA results' summary
    PSA_summary = NULL,
    #' @field CEA_results_tables Cost-Effectiveness Analysis (CEA) results' tables
    CEA_results_tables = NULL,
    #' @field simulated_targets PSA results
    simulated_targets = NULL,
    #' @field plots summary plots
    plots = NULL,
    #' @field data data storage list
    data = NULL,

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
    #' @param .intervs A list containing the information about the considered
    #' interventions built into the model.
    #'
    #' @return Object of class `CalibrateR_R6`
    #'
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    initialize = function(.model, .args, .params, .targets, .transform,
                          .intervs = NULL) {
      # save passed arguments to dedicated internal objects
      self$calibration_model <- .model
      self$calibration_model_args <- .args
      self$calibration_parameters <- .params
      self$calibration_targets <- .targets
      self$transform_parameters <- .transform
      self$model_interventions <- .intervs
      self$model_predictions$truth <- self$calibration_model()

      invisible(self)

    },

    #' Save Data to User-defined positions
    #'
    #' @param .position_ List containing the name(s) of the name slot(s) to
    #' house the data to be stored into the R6 object.
    #' @param .data_ Object containing what to be saved into the R6 object.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    set_data = function(
    .position_,
    .data_) {
      purrr::pluck( .x = self$data, !!!.position_) <- .data_

      invisible(self)

    },

    #' Save to position if exists
    #'
    #' @param .position_ List containing the name(s) of the name slot(s) to
    #' house the data to be stored into the R6 object.
    #' @param .data_ Object containing what to be saved into the R6 object.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    set_position = function(
    .position_,
    .data_) {
      if(.position_ %in% names(self))
        self[[.position_]] <-.data_
      else
        warning(
          paste(.position_, "does not exist!")
        )

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
    #' @param .weighted Logical for whether the SSE was to be weighted,
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
    #' @param .n_samples Number of Starting sets (gausses) to use.
    #' @param .max_iterations Maximum number of algorithm iterations.
    #' @param temp SANN algorithm tuning parameter.
    #' @param trace Non-negative integer. If positive, tracing information on
    #' the progress of the optimization is produced. Higher values may produce
    #' more tracing information.
    #' @param .calibration_method The calibration process.
    #' @param .sample_method The method used to sample from the prior
    #' distribution.
    #' @param .maximise Logical for whether the function is to maximise.
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
                                   trace = NULL,
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
        trace = trace,
        .maximise = .maximise)
    },

    #' @description
    #' Calibrate the model using one or more calibration method.
    #'
    #' @param .b_methods Bayesian calibration method(s)
    #' @param .n_resample Desired number of draws from the posterior
    #' @param .IMIS_iterations Maximum number of IMIS iterations
    #' @param .IMIS_sample Positive integer for the IMIS sample size at each
    #' iteration.
    #' @param .MCMC_burnIn Positive integer for the MCMC burn-in sample.
    #' @param .MCMC_samples Positive integer for the total MCMC sample,
    #' including burn-in.
    #' @param .MCMC_thin Positive integer for the value by which MCMC results
    #' are to reduced. .MCMC_thin defines the number of samples from which the
    #' first will be retained while the rest are discarded.
    #' @param .MCMC_rerun Logical for whether to re-run MCMC using the proposal
    #' distribution covariance matrix from the first run.
    #' @param .diag_ Logical for whether to print diagnostics.
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
                                   .IMIS_sample = 100,
                                   .MCMC_burnIn = 10000,
                                   .MCMC_samples = 50000,
                                   .MCMC_thin = 5,
                                   .MCMC_rerun = TRUE,
                                   .diag_ = FALSE) {
      private$calibrateR_bayesian_(
        .b_methods = .b_methods,
        .n_resample = .n_resample,
        .IMIS_iterations = .IMIS_iterations,
        .IMIS_sample = .IMIS_sample,
        .MCMC_burnIn = .MCMC_burnIn,
        .MCMC_samples = .MCMC_samples,
        .MCMC_thin = .MCMC_thin,
        .MCMC_rerun = .MCMC_rerun,
        .diag_ = .diag_)
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
      # private$draw_pair_correlations()
      # if(isTRUE(print_pair_correlations))
      #   private$print_pair_correlations()
      private$draw_targets()

    },

    #' @description
    #' Plot Goodness of fit function(s)
    #'
    #' @param .engine_ String naming the plotting engine, currently "plotly".
    #' @param .blank_contour_ Logical for whether to only plot blank or empty GOF
    #' contour plots.
    #' @param .gof_ Goodness of fit (GOF) measure - fitness function. Either
    #' "LLK" or "SEE" for the log-likelihood and sum-of-squared-errors GOF,
    #' respectively.
    #' @param .percent_sampled_ .percent_sampled_ The fraction of LHS, RGS, or
    #' FGS samples to select.
    #' @param .n_samples_ Number of Grid samples to plot log likelihood
    #' @param .true_points_ Logical for whether to add "True set" to plots.
    #' @param .greys_ Logical for whether to use a Grey scale in the plot.
    #' @param .scale_ The colour bar colour-scale. Available options are Greys,
    #' YlGnBu, Greens, YlOrRd, Bluered, RdBu, Reds, Blues, Picnic, Rainbow,
    #' Portland, Jet, Hot, Blackbody, Earth, Electric, Viridis, Cividis.
    #' @param .coloring_ Which contouring is required (default fill) and options
    #' are "fill" | "heatmap" | "lines" | "none"
    #' @param .legend_ Logical for whether to show a legend (default is FALSE).
    #' This parameter also controls text labels in the opposite way.
    #' @param .zoom_ Logical (default FALSE) for whether to limit the resulting
    #' plot to the min() and max() of the two dimensional contour plots.
    #' @param .x_axis_lb_ Lower bound of the plot's x axis.
    #' @param .x_axis_ub_ Upper bound of the plot's x axis.
    #' @param .y_axis_lb_ Lower bound of the plot's y axis.
    #' @param .y_axis_ub_ Upper bound of the plot's y axis.
    #' @param .save_ Logical for whether to save plots.
    #' @param .saving_path_ String defining the path for where to save the
    #' plots.
    #' @param .saving_image_dir_ String defining the sub-folder in the path
    #' where to save the plots.
    #' @param .saving_x_params_ Integer or vector of integers for the rank(s) of
    #' @param .saving_image_scale_ Positive integer dictating the scale at which
    #' the image is saved. Default value is 5 for images with width of 1/3 an A4
    #' paper.
    #' @param .saving_image_width_ Positive integer setting the width of the
    #' saved version of the plot. The default (1,000 px) is appropriate for an
    #' image 1/3 of the width of A4 sheet.
    #' @param .saving_image_height_ Positive integer setting the height of the
    #' saved version of the plot. The default (600 px) is appropriate for an
    #' image 1/3 of the width of A4 sheet.    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    draw_GOF_measure = function(.engine_ = "plotly",
                                .blank_contour_ = TRUE,
                                .gof_ = "LLK",
                                .percent_sampled_ = 10,
                                .n_samples_ = 1e4,
                                .true_points_ = FALSE,
                                .greys_ = FALSE,
                                .scale_ = NULL,
                                .coloring_ = "fill",
                                .legend_ = FALSE,
                                .zoom_ = FALSE,
                                .x_axis_lb_ = NULL,
                                .x_axis_ub_ = NULL,
                                .y_axis_lb_ = NULL,
                                .y_axis_ub_ = NULL,
                                .save_ = FALSE,
                                .saving_path_ = here::here(),
                                .saving_image_dir_ = "/images/GOFs/",
                                .saving_x_params_ = 1,
                                .saving_image_scale_ = 2,
                                .saving_image_width_ = 1000,
                                .saving_image_height_ = 600) {
      ## Sanity check (stop if .gof not recognised):----
      stopifnot(".gof_ value is not supported by the function" =
                  all(.gof_ %in% c('LLK', 'SSE')))
      ## Sample values for the fitness plot:----
      if(is.null(self$GOF_measure_plot[["Samples"]]))
        self$GOF_measure_plot[["Samples"]] <- calibR::sample_prior_FGS_(
          .n_samples = .n_samples_,
          .l_params = self$calibration_parameters)
      ## Estimate GOF measure:----
      self$GOF_measure_plot[["Results"]] <- purrr::map(
        .x = .gof_ %>%
          `names<-`(.gof_),
        .f = function(.gof_name_) {
          if(is.null(self$GOF_measure_plot[["Results"]][[.gof_name_]])) {
            if(.gof_name_ == "LLK"){
              calibR::LLK_GOF(
                .samples = self$GOF_measure_plot$Samples,
                .sample_method = "FGS",
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .l_targets = self$calibration_targets,
                .maximise = TRUE)
            } else {
              calibR::wSSE_GOF(
                .samples = self$GOF_measure_plot$Samples,
                .sample_method = "FGS",
                .func = self$calibration_model,
                .args = self$calibration_model_args,
                .l_targets = self$calibration_targets,
                .maximise = FALSE)
            }
          } else {
            self$GOF_measure_plot[["Results"]][[.gof_name_]]
          }
        }
      )

      ## Plot the fitness function:----
      private$plot_GOF_measure(
        .blank_contour_ = .blank_contour_,
        .engine_ = .engine_,
        .gof_ = .gof_,
        .percent_sampled_= .percent_sampled_,
        .true_points_ = .true_points_,
        .greys_ = .greys_,
        .scale_ = .scale_,
        .coloring_ = .coloring_,
        .legend_ = .legend_,
        .zoom_ = .zoom_,
        .x_axis_lb_ = .x_axis_lb_,
        .x_axis_ub_ = .x_axis_ub_,
        .y_axis_lb_ = .y_axis_lb_,
        .y_axis_ub_ = .y_axis_ub_)

      ## Save plots:----
      if(.save_) {
        ### Prepare image saving path:----
        image_saving_path <- glue::glue(
          "{.saving_path_}{.saving_image_dir_}")
        #### Create the directory if missing:----
        if(!dir.exists(image_saving_path))
          dir.create(
            path = image_saving_path,
            recursive = TRUE)

        ### Walk through created plots and save them in image_saving_path:----
        purrr::walk(
          #### Walk through each group of plots:----
          .x = self$plots$GOF_plots %>%
            names(.),
          .f = function(.calib_category_) {
            ##### Walk through each GOF measure:----
            purrr::walk(
              .x = self$plots$GOF_plots[[.calib_category_]] %>%
                names(.),
              .f = function(.calib_gof_) {
                ##### Walk through each calibration method:----
                purrr::walk(
                  .x = self$plots$GOF_plots[[.calib_category_]][[.calib_gof_]] %>%
                    names(.),
                  .f = function(.calib_method_) {
                    ###### Pick the first parameter or user-defined as the x-axis:----
                    x_params_ <- self$calibration_parameters$
                      v_params_names[.saving_x_params_]
                    names(x_params_) <- x_params_
                    ####### Loop through x-axis names:----
                    purrr::walk(
                      .x = x_params_,
                      .f = function(.x_param_) {
                        ######## Exclude the x-axis param to get the y-axis params:----
                        other_params_names <- self$calibration_parameters$
                          v_params_names[-which(self$calibration_parameters$
                                                  v_params_names == .x_param_)]
                        ######### Loop through y-axis names:----
                        purrr::walk(
                          .x = other_params_names,
                          .f = function(.y_param_) {
                            if(.calib_category_ == "blank") {
                              #### Give the plot a name to be saved:----
                              image_name = glue::glue(
                                "{.calib_gof_}_blank.jpeg")
                              if(.zoom_)
                                image_name <- glue::glue(
                                  "{.calib_gof_}_blank_z.jpeg")
                              if(.engine_ == "plotly") {
                                #### Call reticulate to load python package for "plotly":----
                                reticulate::py_run_string("import sys")
                                #### Save the "plotly" generated plot:----
                                plotly::save_image(
                                  p = self$plots$
                                    GOF_plots[[.calib_category_]][[.calib_gof_]][[.x_param_]][[.y_param_]],
                                  file = glue::glue(
                                    "{image_saving_path}{image_name}"),
                                  scale = .saving_image_scale_,
                                  width = .saving_image_width_,
                                  height = .saving_image_height_)
                              }
                            } else {
                              image_name <- glue::glue(
                                "{.calib_gof_}_{.calib_method_}.jpeg")
                              if(.zoom_)
                                image_name <- glue::glue(
                                  "{.calib_gof_}_{.calib_method_}_z.jpeg")
                              if(.engine_ == "plotly") {
                                #### Call reticulate to load python package for "plotly":----
                                reticulate::py_run_string("import sys")
                                #### Save the "plotly" generated plot:----
                                plotly::save_image(
                                  p = self$plots
                                  $GOF_plots[[.calib_category_]][[.calib_gof_]][[.calib_method_]][[.x_param_]][[.y_param_]],
                                  file = glue::glue(
                                    "{image_saving_path}{image_name}"),
                                  scale = .saving_image_scale_,
                                  width = .saving_image_width_,
                                  height = .saving_image_height_)
                              }
                            }
                          })
                      })
                  })
              })
          })
      }

      invisible(self)
    },

    #' @description
    #' Plot Targets (with or without simulated targets)
    #'
    #' @param .engine_ String naming the plotting engine, currently "ggplot2".
    #' @param .sim_targets_ Logical (default FALSE) for whether generate then
    #' plot simulated targets. The generation of simulated targets requires
    #' the package to run the model using the sampled or identified PSA values.
    #' @param .calibration_methods_ String vector naming the calibration methods
    #' for which simulated targets are to be generated. Options is either "all"
    #' (the default) or any of c("random", "directed", "bayesian").
    #' @param .legend_pos_ String declaring the preferred position for the
    #' legend. Default is "bottom".
    #' @param .PSA_samples_ Integer defining the maximum number of PSA values to
    #' use in plotting/generating the simulated targets.
    #' @param .PSA_unCalib_values_ Tibble/table/dataframe containing the PSA
    #' draws for the parameters that are not calibrated in the object
    #' @param .save_ Logical for whether to save plots.
    #' @param .saving_path_ String defining the path for where to save the
    #' plots.
    #' @param .saving_image_dir_ String defining the sub-folder in the path
    #' where to save the plots.
    #' @param .saving_image_scale_ Positive integer dictating the scale at which
    #' the image is saved. Default value is 5 for images with width of 1/3 an A4
    #' paper.
    #' @param .saving_image_width_ Positive integer setting the width of the
    #' saved version of the plot. The default (1,000 px) is appropriate for an
    #' image 1/3 of the width of A4 sheet.
    #' @param .saving_image_height_ Positive integer setting the height of the
    #' saved version of the plot. The default (600 px) is appropriate for an
    #' image 1/3 of the width of A4 sheet.
    #' @param .saving_image_units_ A string (default "px") defining the units
    #' in which the width and height are provided.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    draw_targets_plots = function(.engine_ = "ggplot2",
                                  .sim_targets_ = FALSE,
                                  .calibration_methods_ = "all",
                                  .legend_pos_ = "none",
                                  .PSA_samples_ = NULL,
                                  .PSA_unCalib_values_ = NULL,
                                  .save_ = FALSE,
                                  .saving_path_ = here::here(),
                                  .saving_image_dir_ = "/images/Targets/",
                                  .saving_image_scale_ = 2,
                                  .saving_image_width_ = 1000,
                                  .saving_image_height_ = 600,
                                  .saving_image_units_ = "px") {
      ## Sanity checks (stop if .calibration_methods_ not recognised):----
      stopifnot("one or more .calibration_methods_ are not supported by the
                function" =
                  all(.calibration_methods_ %in%
                  c("all", "random", "directed", "bayesian")))
      if("all" %in% .calibration_methods_)
        .calibration_methods_ <- c("random", "directed", "bayesian")
      ## Call private function:----:
      private$plot_targets(
        .engine_ = .engine_,
        .sim_targets_ = .sim_targets_,
        .calibration_methods_ = .calibration_methods_,
        .legend_pos_ = .legend_pos_,
        .PSA_samples_ = .PSA_samples_,
        .PSA_unCalib_values_ = .PSA_unCalib_values_)

      ## Save plots:----
      if(.save_) {
        ### Prepare image saving path:----
        image_saving_path <- glue::glue(
          "{.saving_path_}{.saving_image_dir_}")
        #### Create the directory if missing:----
        if(!dir.exists(image_saving_path))
          dir.create(
            path = image_saving_path,
            recursive = TRUE)

        ### Walk through created plots and save them in image_saving_path:----
        purrr::walk(
          #### Walk through each group of plots:----
          .x = self$plots$targets %>%
            names(.),
          .f = function(.calib_category_) {
            ##### Walk through each calibration method:----
            purrr::walk(
              .x = self$plots$targets[[.calib_category_]] %>%
                names(.),
              .f = function(.calib_method_) {
                ###### Loop through target names:----
                purrr::walk(
                  .x = self$calibration_targets$v_targets_names,
                  .f = function(.target_) {
                    if(.calib_category_ == "blank") {
                      #### Give the plot a name to be saved:----
                      image_name = glue::glue("{.target_}_blank.jpeg")
                      if(.engine_ == "ggplot2") {
                        #### Save the "ggplot2" generated plot:----
                        ggplot2::ggsave(
                          filename = glue::glue("{image_saving_path}{image_name}"),
                          plot = self$plots$
                            targets[[.calib_category_]][[.target_]],
                          scale = .saving_image_scale_, #2.5
                          width = .saving_image_width_,
                          height = .saving_image_height_,
                          units = .saving_image_units_)
                      }
                    } else {
                      image_name = glue::glue("{.target_}_{.calib_method_}.jpeg")
                      if(.engine_ == "ggplot2") {
                        #### Save the "ggplot2" generated plot:----
                        ggplot2::ggsave(
                          filename = glue::glue("{image_saving_path}{image_name}"),
                          plot = self$plots$
                            targets[[.calib_category_]][[.calib_method_]][[.target_]],
                          scale = .saving_image_scale_, #2.5
                          width = .saving_image_width_, #1000
                          height = .saving_image_height_, #600
                          units = .saving_image_units_) #"px"
                      }
                    }
                  })
              })
          })
      }

      invisible(self)
    },

    #' @description
    #' Plot prior and posterior distributions
    #'
    #' @param .engine_ String naming plotting package currently only supports
    #' "ggplot2".
    #' @param .bins_ Numeric specifying the number of bins in the histograms.
    #' @param .legend_pos_ String (default bottom) setting legend position.
    #' @param .log_scaled_ Logical for whether to use log scale in the x axis.
    #' @param .save_ Logical for whether to save plots.
    #' @param .saving_path_ String defining the path for where to save the
    #' plots.
    #' @param .saving_image_dir_ String defining the sub-folder in the path
    #' where to save the plots.
    #' @param .saving_image_scale_ Positive integer dictating the scale at which
    #' the image is saved. Default value is 5 for images with width of 1/3 an A4
    #' paper.
    #' @param .saving_image_width_ Positive integer setting the width of the
    #' saved version of the plot. The default (1,000 px) is appropriate for an
    #' image 1/3 of the width of A4 sheet.
    #' @param .saving_image_height_ Positive integer setting the height of the
    #' saved version of the plot. The default (600 px) is appropriate for an
    #' image 1/3 of the width of A4 sheet.
    #' @param .saving_image_units_ A string (default "px") defining the units
    #' in which the width and height are provided.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    draw_distributions_plots = function(.engine_ = "ggplot2",
                                        .bins_ = 20,
                                        .legend_pos_ = "none",
                                        .log_scaled_ = FALSE,
                                        .save_ = FALSE,
                                        .saving_path_ = here::here(),
                                        .saving_image_dir_ = "/images/Prior-posterior/",
                                        .saving_image_scale_ = 2,
                                        .saving_image_width_ = 1000,
                                        .saving_image_height_ = 600,
                                        .saving_image_units_ = "px") {
      ## Invoke the private plotting function:----
      private$plot_distributions(
        .engine_ = .engine_,
        .bins_ = .bins_,
        .legend_pos_ = .legend_pos_,
        .log_scaled_ = .log_scaled_
      )

      ## Save plots:----
      if(.save_) {
        ### Prepare image saving path:----
        image_saving_path <- glue::glue(
          "{.saving_path_}{.saving_image_dir_}")
        #### Create the directory if missing:----
        if(!dir.exists(image_saving_path))
          dir.create(
            path = image_saving_path,
            recursive = TRUE)

        ### Walk through created plots and save them in image_saving_path:----
        purrr::walk(
          #### Walk through each Bayesian calibration method:----
          .x = self$plots$distributions %>%
            names(.),
          .f = function(.calib_method_) {
            ##### Walk through each parameter:----
            purrr::walk(
              .x = self$plots$distributions[[.calib_method_]] %>%
                names(.),
              .f = function(.param_) {
                image_name = glue::glue("PriPost_{.calib_method_}_{.param_}.jpeg")
                if(.engine_ == "ggplot2") {
                  #### Save the "ggplot2" generated plot:----
                  ggplot2::ggsave(
                    filename = glue::glue("{image_saving_path}{image_name}"),
                    plot = self$plots$
                      distributions[[.calib_method_]][[.param_]],
                    scale = .saving_image_scale_, #2.5
                    width = .saving_image_width_, #1000
                    height = .saving_image_height_, #600
                    units = .saving_image_units_) #"px"
                }
              })
          })
      }

      invisible(self)
    },

    #' @description
    #' Draw PSA summary tables:----
    #'
    #' @param .label_effects_ Character indicating the name of the column
    #' containing the mean values of the effects. Default is `"QALYs"`.
    #' @param .label_costs_ Character indicating the name of the column
    #' containing the mean values of the costs. Default is `"Costs"`.
    #' @param .wtp_key_values_ Numeric vector specifying the willingness-to-pay
    #' (WTP) values to be used in estimating the NMB and probability of the
    #' optimal choice being cost-effective. Default is `c(20000, 30000)`.
    #' @param .highlight_optimal_choices_ Logical for whether to report NMB, PCE
    #' or EVPI for the optimal choice. Default is `TRUE`. If `FALSE`, the values
    #' of NMB, PCE or EVPI will be reported for all options.
    #' @param .currency_symbol_ Character scalar representing the Hex code of
    #' the currency symbol to use in labeling relevant results. Default is
    #' `"\u00A3"` for Sterling (GBP). Use `"\u20AC"`for Euros or `"\u0024"` for
    #' US Dollars.
    #' @param .output_type_ Character scalar that takes one of three options:
    #' `"html"`, `"latex"` or `"dataframe"`. Default is `"dataframe"`. If
    #' `"html"` or `"latex"` were passed to this argument, the function calls
    #' \link{generate_gt_table} internally to generate the required output. The
    #' `"html"` is suitable for shiny application; whereas, `"latex"`is suitable
    #' for PDF document.
    #' @param .full_output_format_ Character scalar taking one of two options:
    #' `"wide"` or `"long"` specifying whether the generated results table would
    #' be in a wide or long format, respectively. Default is `"long"`.
    #' @param .add_simulated_truth_ Logical scalar for whether to add results
    #' from the Simulated Truth PSA data to the generated results tables.
    #' Default is `TRUE`.
    #' @param .truth_PSA_output_list_path_ Character scalar specifying the path
    #' where the list containing the Simulated Truth PSA data. The path is
    #' expected to lead to a `.RDS` file representing the list containing the
    #' PSA data. This list is expected to contain two dataframes named "e" or
    #' "effects" and "c" or "costs", containing the effects and costs PSA data,
    #' respectively; and a character vector containing the names of the
    #' interventions. This argument is ignored if `.add_simulated_truth_` is set
    #' to `FALSE`.
    #' @param .truth_PSA_output_list_ List containing the Simulated Truth PSA
    #' data. This list is expected to contain two dataframes named "e" or
    #' "effects" and "c" or "costs", containing the effects and costs PSA data,
    #' respectively; and a character vector containing the names of the
    #' interventions. This argument is ignored if `.add_simulated_truth_` is set
    #' to `FALSE` or if `.truth_PSA_output_list_path_` is set to `NULL`.
    #' @param .generate_partial_cea_table_ Logical for whether to generate CEA
    #' tables with fewer results. Default is `TRUE`.
    #' @param .partial_cea_table_groups_ Character vector specifying what CEA
    #' measures to report on the tables with fewer results. Default is
    #' `c("NMB", "PCE", "EVPI")`, but it can take any or all of these values.
    #' @param .generate_relative_values_ Logical for whether the produced CEA
    #' results table should be relative to the Simulated Truth or not. Default
    #' is `TRUE`.
    #' @param .relative_values_data_ Character vector specifying what CEA
    #' measures to report on the tables with fewer results. Default is
    #' `c("NMB")`, but it can take any or all of`c("NMB", "PCE", "EVPI")`.
    #' @param .save_ Logical for whether to save tables data.
    #' @param .saving_path_ String scalar defining the path for where to save
    #' the tables data.
    #' @param .saving_data_dir_ String scalar defining the sub-folder in the
    #' path where to save the tables data.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    draw_CEA_results_tables = function(.label_effects_ = "QALYs",
                                       .label_costs_ = "Costs",
                                       .wtp_key_values_ = c(20000, 30000),
                                       .highlight_optimal_choices_ = FALSE,
                                       .currency_symbol_ = "\u00A3",
                                       .output_type_ = "html",
                                       .full_output_format_ = "long",
                                       .add_simulated_truth_ = TRUE,
                                       .truth_PSA_output_list_path_ = "../5. Presentations/data/CRS_true_PSA.rds",
                                       .truth_PSA_output_list_ = NULL,
                                       .generate_partial_cea_table_ = TRUE,
                                       .partial_cea_table_groups_ = c("NMB", "PCE", "EVPI"),
                                       .generate_relative_values_ = TRUE,
                                       .relative_values_data_ = c("NMB"),
                                       .save_ = FALSE,
                                       .saving_path_ = here::here(),
                                       .saving_data_dir_ = "/data/PSA tables/") {

      ## Generate CEA results tables:----
      private$generate_cea_tables(
        .label_effects_ = .label_effects_,
        .label_costs_ = .label_costs_,
        .wtp_key_values_ = .wtp_key_values_,
        .highlight_optimal_choices_ = .highlight_optimal_choices_,
        .currency_symbol_ = .currency_symbol_,
        .output_type_ = .output_type_,
        .full_output_format_ = .full_output_format_,
        .add_simulated_truth_ = .add_simulated_truth_,
        .truth_PSA_output_list_path_ = .truth_PSA_output_list_path_,
        .truth_PSA_output_list_ = .truth_PSA_output_list_,
        .generate_partial_cea_table_ = .generate_partial_cea_table_,
        .partial_cea_table_groups_ = .partial_cea_table_groups_,
        .generate_relative_values_ = .generate_relative_values_,
        .relative_values_data_ = .relative_values_data_
      )

      ## Save plots:----
      if(.save_) {
        ### Prepare data saving path:----
        data_saving_path <- glue::glue(
          "{.saving_path_}{.saving_data_dir_}")
        #### Create the directory if missing:----
        if(!dir.exists(data_saving_path))
          dir.create(
            path = data_saving_path,
            recursive = TRUE)

        ### Walk through created tables and save them in data_saving_path:----
        purrr::walk(
          .x = self$CEA_results_tables %>%
            names(.),
          .f = function(.group_) {
            purrr::walk(
              .x = self$CEA_results_tables[[.group_]] %>%
                names(.),
              .f = function(.calib_category_) {
                if(.calib_category_ == "True") {
                  table_name <- glue::glue(
                    "{.group_}_truth.rds")
                  saveRDS(
                    object = self$
                      CEA_results_tables[[.group_]][[.calib_category_]],
                    file = glue::glue(
                      "{data_saving_path}{table_name}"))
                } else if(.group_ == "Combined") {
                  table_name <- glue::glue(
                    "{.group_}_{.calib_category_}.rds")
                  saveRDS(
                    object = self$
                      CEA_results_tables[[.group_]][[.calib_category_]],
                    file = glue::glue(
                      "{data_saving_path}{table_name}"))
                } else {
                  purrr::walk(
                    .x = self$CEA_results_tables[[.group_]][[.calib_category_]] %>%
                      names(.),
                    .f = function(.calib_method_) {
                      table_name <- glue::glue(
                        "{.group_}_{.calib_category_}_{.calib_method_}.rds")
                      saveRDS(
                        object = self$
                          CEA_results_tables[[.group_]][[.calib_category_]][[.calib_method_]],
                        file = glue::glue(
                          "{data_saving_path}{table_name}"))
                    }
                    )

                }
              }
              )
          }
          )
      }
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
          calibR::sample_prior_FGS_(...)
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
        cat(paste("Running RGS...", Sys.time(), "\n"))
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
        cat(paste("Running FGS...", Sys.time(), "\n"))
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
        cat(paste("Running LHS...", Sys.time(), "\n"))
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
    trace,
    .maximise = TRUE) {
      if("RGS" %in% .sample_method) {
        #### Nelder-Mead:----
        if("NM" %in% .calibration_method) {
          cat(paste("Running NM...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["NM_RGS"]] <- self$prior_samples[["RGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["NM_LLK_RGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = "LLK",
              .samples = initial_values[["NM_RGS"]],
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
              .samples = initial_values[["NM_RGS"]],
              .s_method = "NM",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("NM_", .gof, "_RGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["NM_RGS"]],
              .s_method = "NM",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### BFGS:----
        if("BFGS" %in% .calibration_method) {
          cat(paste("Running BFGS...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["BFGS_RGS"]] <- self$prior_samples[["RGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["BFGS_LLK_RGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["BFGS_RGS"]],
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
              .samples = initial_values[["BFGS_RGS"]],
              .s_method = 'BFGS',
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("BFGS_", .gof, "_RGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["BFGS_RGS"]],
              .s_method = "BFGS",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### SANN:----
        if("SANN" %in% .calibration_method) {
          cat(paste("Running SANN...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["SANN_RGS"]] <- self$prior_samples[["RGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["SANN_LLK_RGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["SANN_RGS"]],
              .s_method = 'SANN',
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
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
              .samples = initial_values[["SANN_RGS"]],
              .s_method = 'SANN',
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("SANN_", .gof, "_RGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["SANN_RGS"]],
              .s_method = "SANN",
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### GA:----
        if("GA" %in% .calibration_method) {
          cat(paste("Running GA...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["GA_RGS"]] <- self$prior_samples[["RGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["GA_LLK_RGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["GA_RGS"]],
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
              .samples = initial_values[["GA_RGS"]],
              .s_method = 'GA',
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("GA_", .gof, "_RGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["GA_RGS"]],
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
          cat(paste("Running NM...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["NM_FGS"]] <- self$prior_samples[["FGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["NM_LLK_FGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = "LLK",
              .samples = initial_values[["NM_FGS"]],
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
              .samples = initial_values[["NM_FGS"]],
              .s_method = "NM",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("NM_", .gof, "_FGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["NM_FGS"]],
              .s_method = "NM",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### BFGS:----
        if("BFGS" %in% .calibration_method) {
          cat(paste("Running BFGS...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["BFGS_FGS"]] <- self$prior_samples[["FGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["BFGS_LLK_FGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["BFGS_FGS"]],
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
              .samples = initial_values[["BFGS_FGS"]],
              .s_method = 'BFGS',
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("BFGS_", .gof, "_FGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["BFGS_FGS"]],
              .s_method = "BFGS",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### SANN:----
        if("SANN" %in% .calibration_method) {
          cat(paste("Running SANN...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["SANN_FGS"]] <- self$prior_samples[["FGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["SANN_LLK_FGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["SANN_FGS"]],
              .s_method = 'SANN',
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
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
              .samples = initial_values[["SANN_FGS"]],
              .s_method = 'SANN',
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("SANN_", .gof, "_FGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["SANN_FGS"]],
              .s_method = "SANN",
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### GA:----
        if("GA" %in% .calibration_method) {
          cat(paste("Running GA...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["GA_FGS"]] <- self$prior_samples[["FGS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["GA_LLK_FGS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["GA_FGS"]],
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
              .samples = initial_values[["GA_FGS"]],
              .s_method = 'GA',
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("GA_", .gof, "_FGS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["GA_FGS"]],
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
          cat(paste("Running NM...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["NM_LHS"]] <- self$prior_samples[["LHS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["NM_LLK_LHS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = "LLK",
              .samples = initial_values[["NM_LHS"]],
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
              .samples = initial_values[["NM_LHS"]],
              .s_method = "NM",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("NM_", .gof, "_LHS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["NM_LHS"]],
              .s_method = "NM",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### BFGS:----
        if("BFGS" %in% .calibration_method) {
          cat(paste("Running BFGS...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["BFGS_LHS"]] <- self$prior_samples[["LHS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["BFGS_LLK_LHS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["BFGS_LHS"]],
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
              .samples = initial_values[["BFGS_LHS"]],
              .s_method = 'BFGS',
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("BFGS_", .gof, "_LHS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["BFGS_LHS"]],
              .s_method = "BFGS",
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### SANN:----
        if("SANN" %in% .calibration_method) {
          cat(paste("Running SANN...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["SANN_LHS"]] <- self$prior_samples[["LHS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["SANN_LLK_LHS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["SANN_LHS"]],
              .s_method = 'SANN',
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
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
              .samples = initial_values[["SANN_LHS"]],
              .s_method = 'SANN',
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("SANN_", .gof, "_LHS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["SANN_LHS"]],
              .s_method = "SANN",
              maxit = .max_iterations,
              temp = temp,
              trace = trace,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
        }
        #### GA:----
        if("GA" %in% .calibration_method) {
          cat(paste("Running GA...", Sys.time(), "\n"))
          ##### Save initial values:----
          if(!exists("initial_values"))
            initial_values <- list()
          initial_values[["GA_LHS"]] <- self$prior_samples[["LHS"]] %>%
            dplyr::slice_sample(n = .n_samples)
          ##### LLK:----
          if("LLK" %in% .gof)
            self$calibration_results$directed[["GA_LLK_LHS"]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = 'LLK',
              .samples = initial_values[["GA_LHS"]],
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
              .samples = initial_values[["GA_LHS"]],
              .s_method = 'GA',
              maxit = .max_iterations,
              .maximise = .maximise,
              .l_params = self$calibration_parameters,
              .l_targets = self$calibration_targets
            )
          ##### others:----
          if(!any(.gof %in% c("LLK", "SSE")))
            self$calibration_results$
            directed[[paste0("GA_", .gof, "_LHS")]] <-
            calibR::calibrateModel_directed(
              .func = self$calibration_model,
              .args = self$calibration_model_args,
              .gof = .gof,
              .gof_func = .gof_func,
              .samples = initial_values[["GA_LHS"]],
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
                                    .n_resample = 1000,
                                    .IMIS_sample = 1000,
                                    .IMIS_iterations = 30,
                                    .MCMC_burnIn = 10000,
                                    .MCMC_samples = 50000,
                                    .MCMC_thin = 5,
                                    .MCMC_rerun = TRUE,
                                    .diag_ = FALSE) {
      #### SIR:----
      if('SIR' %in% .b_methods) {
        cat(paste("Running SIR...", Sys.time(), "\n"))
        ##### Get samples:----
        samples_ <- if(!is.null(self$prior_samples$LHS)) {
          self$prior_samples$LHS
        } else {
          self$prior_samples$LHS <- calibR::sample_prior_LHS(
            .n_samples = .n_resample,
            .l_params = self$calibration_parameters)
          self$prior_samples$LHS
        }
        ##### Call private function:----
        self$calibration_results$bayesian[["SIR"]] <-
          calibR::calibrateModel_beyesian(
            .b_method = 'SIR',
            .func = self$calibration_model,
            .args = self$calibration_model_args,
            .n_resample = .n_resample,
            .samples = samples_,
            .l_params = self$calibration_parameters,
            .l_targets = self$calibration_targets)
      }
      #### IMIS:----
      if('IMIS' %in% .b_methods) {
        cat(paste("Running IMIS...", Sys.time(), "\n"))
        self$calibration_results$bayesian[["IMIS"]] <-
          calibR::calibrateModel_beyesian(
            .b_method = 'IMIS',
            .func = self$calibration_model,
            .args = self$calibration_model_args,
            .transform = self$transform_parameters,
            .n_resample = .n_resample,
            .IMIS_iterations = .IMIS_iterations,
            .IMIS_sample = .IMIS_sample,
            .l_params = self$calibration_parameters,
            .l_targets = self$calibration_targets)
      }
      #### MCMC:----
      if('MCMC' %in% .b_methods) {
        cat(paste("Running MCMC...", Sys.time(), "\n"))
        self$calibration_results$bayesian[["MCMC"]] <-
          calibR::calibrateModel_beyesian(
            .b_method = 'MCMC',
            .func = self$calibration_model,
            .args = self$calibration_model_args,
            .transform = self$transform_parameters,
            .n_resample = .n_resample,
            .l_params = self$calibration_parameters,
            .l_targets = self$calibration_targets,
            .MCMC_burnIn = .MCMC_burnIn,
            .MCMC_samples = .MCMC_samples,
            .MCMC_thin = .MCMC_thin,
            .MCMC_rerun = .MCMC_rerun,
            .diag_ = .diag_)
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
        if(!is.null(self$transform_parameters) & self$transform_parameters) {
          calibR::run_PSA(
            .func_ = self$calibration_model,
            .PSA_calib_values_ = c(self$PSA_samples$random,
                                   self$PSA_samples$directed,
                                   self$PSA_samples$bayesian),
            .args_ = c(self$calibration_model_args,
                       "calibrate_" = FALSE,
                       "transform_" = self$transform_parameters),
            .PSA_unCalib_values_ = .PSA_unCalib_values_)
        } else {
          calibR::run_PSA(
            .func_ = self$calibration_model,
            .PSA_calib_values_ = c(self$PSA_samples$random,
                                   self$PSA_samples$directed,
                                   self$PSA_samples$bayesian),
            .args_ = c(self$calibration_model_args,
                       "calibrate_" = FALSE),
            .PSA_unCalib_values_ = .PSA_unCalib_values_)
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
    ### Prior-posterior plots:----
    # Create combined prior and posterior line plots
    #
    # @param data_ Data set containing prior and posterior data
    # @param ggplot_ Logical, \code{TRUE} to generate ggplot2 otherwise
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
          # If True set are known
          if(!is.null(self$calibration_parameters$
                      v_params_true_values[[.parameter_]])) {
            plot_ <- plot_ +
              ggplot2::geom_vline(
                xintercept = self$calibration_parameters$
                  v_params_true_values[[.parameter_]],
                show.legend = TRUE) +
              ggplot2::labs(
                caption = paste0(
                  "The black vertical line represents \"",
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
    # @param ggplot_ Logical, \code{TRUE} to generate ggplot2 otherwise
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
          # If True set are known
          if(!is.null(self$calibration_parameters$
                      v_params_true_values[[.parameter_]])) {
            plot_ <- plot_ +
              ggplot2::geom_vline(
                xintercept = self$calibration_parameters$
                  v_params_true_values[[.parameter_]],
                show.legend = TRUE) +
              ggplot2::labs(
                caption = paste0(
                  "The black vertical line represents \"",
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
    # @param ggplot_ Logical, \code{TRUE} to generate ggplot2 otherwise
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
          # If True set are known
          if(!is.null(self$calibration_parameters$
                      v_params_true_values[[.parameter_]])) {
            plot_ <- plot_ +
              ggplot2::geom_vline(
                xintercept = self$calibration_parameters$
                  v_params_true_values[[.parameter_]],
                show.legend = TRUE) +
              ggplot2::labs(
                caption = paste0(
                  "The black vertical line represents \"",
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
    ### Fitness function plots:----
    # Plot goodness-of-fitness (GOF) contour
    #
    # @param .engine_ String specifying the plotting engine, currently supports
    # "plotly".
    # @param .blank_contour_ Logical for whether to only plot blank or empty GOF contour
    # plots.
    # @param .prior_samples_  Numeric (integer) setting the number of prior samples
    # to be added to the plot.
    # @param .gof_ String identifying the name of the GOF measure used. The
    # function currently supports "LLK" or "SSE" for log-likelihood and
    # Sum-of-Squared-Errors fitness functions, respectively.
    # @param .percent_sampled_ Numeric (double) specifying the proportion of
    # of sets to identify as good-fitting sets by the "random" (undirected or
    # unguided) non-Bayesian calibration methods.
    # @param .true_points_ Logical for whether to show "True values" on the plot.
    # @param .greys_ Logical for whether to use the "Greys" colour-scale. The
    # .scale_ parameter overrides .greys_ if it was not NULL.
    # @param .scale_ String specifying colour-scale applied to the "ploty" contour.
    # The options are "Blackbody", "Bluered", "Blues", "Cividis", "Earth",
    # "Electric", "Greens", "Greys", "Hot", "Jet", "Picnic", "Portland", "Rainbow",
    # "RdBu", "Reds", "Viridis" (default), "YlGnBu", and "YlOrRd".
    # @param .coloring_ String specifying where the colour-scale set by the .scale_
    # parameter is applied on the "plotly" contour. The options are "fill",
    # "heatmap", "none", and "lines". The "fill" option (default) paints the colour
    # scales over the layers of the contour; the "heatmap" option employs a heatmap
    # gradient colouring between each contour level; the "none" option paints no
    # colours on the contour layers and uses a single colour with the contour
    # lines; and the "lines" option applies the colour-scale on the contour lines.
    # @param .legend_ Logical for whether to show the "plotly" contour colour-bar
    # legend.
    # @param .zoom_ Logical (default TRUE) for whether to zoom in to the identified
    # sets (best fitting sets, extrema, and posterior distributions centres).
    # @param .x_axis_lb_ Numeric (double) value specifying the lower bound of x-axis.
    # @param .x_axis_ub_ Numeric (double) value specifying the upper bound of x-axis.
    # @param .y_axis_lb_ Numeric (double) value specifying the lower bound of y-axis.
    # @param .y_axis_ub_ Numeric (double) value specifying the upper bound of y-axis.
    #
    # @return
    #
    # @examples
    # \dontrun{
    # }
    plot_GOF_measure = function(.engine_ = "plotly",
                                .blank_contour_ = TRUE,
                                .prior_samples_ = 1e3,
                                .gof_ = "LLK",
                                .percent_sampled_ = 10,
                                .true_points_ = FALSE,
                                .greys_ = FALSE,
                                .scale_ = NULL,
                                .coloring_ = "fill",
                                .legend_ = FALSE,
                                .zoom_ = FALSE,
                                .x_axis_lb_ = NULL,
                                .x_axis_ub_ = NULL,
                                .y_axis_lb_ = NULL,
                                .y_axis_ub_ = NULL) {
      #### Remove extreme values or plotly will trip:----
      self$GOF_measure_plot$Results <- self$GOF_measure_plot$Results %>%
        purrr::map(
          .x = .,
          .f = function(.gof_data) {
            .gof_data %>%
              as.data.frame() %>%
              dplyr::filter(!is.na(Overall_fit)) %>%
              dplyr::filter(Overall_fit != Inf) %>%
              dplyr::filter(Overall_fit != -Inf)
          })

      #### Generate plots:----
      self$plots$GOF_plots <- calibR::plot_fitness_function(
        .engine_ = .engine_,
        .l_params_ = self$calibration_parameters,
        .l_gof_values_ = self$GOF_measure_plot,
        .l_calibration_results_ = self$calibration_results,
        .l_PSA_samples_ = self$PSA_samples,
        .t_prior_samples_ = self$prior_samples$LHS,
        .prior_samples_ = .prior_samples_,
        .gof_ = .gof_,
        .blank_ = .blank_contour_,
        .percent_sampled_ = .percent_sampled_,
        .true_points_ = .true_points_,
        .greys_ = .greys_,
        .scale_ = .scale_,
        .coloring_ = .coloring_,
        .legend_ = .legend_,
        .zoom_ = .zoom_,
        .x_axis_lb_ = .x_axis_lb_,
        .x_axis_ub_ = .x_axis_ub_,
        .y_axis_lb_ = .y_axis_lb_,
        .y_axis_ub_ = .y_axis_ub_)

    },
    ### Target plots:----
    # Plot observed and simulated targets
    #
    # @param .engine_ String naming the plotting engine, currently "ggplot2".
    # @param .sim_targets_ List containing simulated targets.
    # @param .calibration_methods_ String or vector of strings specifying the
    # names of the calibration methods for which the function would have to
    # generate the simulated targets and plot them.
    # @param .legend_pos_ String specifying the location where the plot's
    # legend should be located.
    # @param .PSA_samples_ Integer defining the number of PSA samples to be
    # drawn, evaluated and the resulting targets plotted.
    # @param .PSA_unCalib_values_ Dataset or tibble containing with columns
    # containing data for the un-calibrated parameter.
    #
    # @return
    #
    # @examples
    # \dontrun{
    # }
    plot_targets = function(.engine_ = "ggplot2",
                             .sim_targets_ = FALSE,
                             .calibration_methods_ = c("random", "directed",
                                                       "bayesian"),
                             .legend_pos_ = "bottom",
                             .PSA_samples_ = NULL,
                             .PSA_unCalib_values_) {
      #### Plot observed targets:----
      self$plots$targets <- calibR::plot_targets(
        .engine_ = .engine_,
        .l_targets_ = self$calibration_targets,
        .simulated_targets_ = self$simulated_targets,
        .sim_targets_ = .sim_targets_,
        .legend_pos_ = .legend_pos_)

      #### Plot observed and simulated targets:----
      if(.sim_targets_) {
        ##### Sample PSA values if unavailable:----
        if(is.null(self$PSA_samples)) {
          self$PSA_samples <- self$sample_PSA_values(
            .calibration_methods = .calibration_methods_,
            .PSA_samples = .PSA_samples_)
        }
        ##### Get simulated targets from PSA samples:----
        if(is.null(self$simulated_targets) &
           !is.null(self$PSA_samples)) {
          names(.calibration_methods_) <- .calibration_methods_
          ###### Loop through methods:----
          self$simulated_targets <- purrr::map(
            .x = .calibration_methods_,
            .f = function(.calib_method) {
              # if the model supports parameter transformation:
              if(!is.null(self$transform_parameters) & self$transform_parameters) {
                calibR::run_PSA(
                  .func_ = self$calibration_model,
                  .PSA_calib_values_ = self$PSA_samples[[.calib_method]],
                  .args_ = c(self$calibration_model_args,
                             "calibrate_" = TRUE,
                             "transform_" = self$transform_parameters),
                  .PSA_unCalib_values_ = .PSA_unCalib_values_)
              } else {
                calibR::run_PSA(
                  .func_ = self$calibration_model,
                  .PSA_calib_values_ = self$PSA_samples[[.calib_method]],
                  .args_ = c(self$calibration_model_args,
                             "calibrate_" = TRUE),
                  .PSA_unCalib_values_ = .PSA_unCalib_values_)
              }
            })
        }
        ##### Plot observed and simulated targets:----
        self$plots$targets <- calibR::plot_targets(
          .engine_ = .engine_,
          .l_targets_ = self$calibration_targets,
          .simulated_targets_ = self$simulated_targets,
          .sim_targets_ = .sim_targets_,
          .legend_pos_ = .legend_pos_)
      }
    },

    ### Prior posterior plots:----
    # Plot posterior and prior density and histogram plots
    #
    # @param .engine_ String naming the plotting engine, currently "ggplot2".
    # @param .bins_ Numeric specifying the number of bins in the histograms.
    # @param .legend_pos_ String defining the location of the legend position
    # default (bottom).
    # @param .log_scaled_ Logical for whether to present the x-axis using the
    # log scale.
    #
    # @return
    #
    # @examples
    # \dontrun{
    # }
    plot_distributions = function(
    .engine_ = "ggplot2",
    .bins_ = 20,
    .legend_pos_ = "bottom",
    .log_scaled_ = FALSE) {

      #### Create plots list:----
      self$plots$distributions <- calibR::plot_pri_post_distributions(
        .engine_ = .engine_,
        .l_PSA_samples_ = self$PSA_samples,
        .l_params_ = self$calibration_parameters,
        .l_calibration_results_ = self$calibration_results,
        .t_prior_samples_ = self$prior_samples$LHS,
        .transform_ = self$transform_parameters,
        .bins_ = .bins_,
        .legend_pos_ = .legend_pos_,
        .log_scaled_ = .log_scaled_)

    },
    ## Tables:----
    ### CEA tables:----
    generate_cea_tables = function(
    .label_effects_ = "QALYs",
    .label_costs_ = "Costs",
    .wtp_key_values_ = c(20000, 30000),
    .highlight_optimal_choices_ = FALSE,
    .currency_symbol_ = "\u00A3",
    .output_type_ = "html",
    .full_output_format_ = "long",
    .add_simulated_truth_ = TRUE,
    .truth_PSA_output_list_path_ = "../5. Presentations/data/CRS_true_PSA.rds",
    .truth_PSA_output_list_ = NULL,
    .generate_partial_cea_table_ = TRUE,
    .partial_cea_table_groups_ = c("NMB", "PCE", "EVPI"),
    .generate_relative_values_ = TRUE,
    .relative_values_data_ = c("NMB")) {

      # save CEA results tables:----
      self$CEA_results_tables <- calibR::generate_cea_results_tables(
        .ls_calibration_results_ = self$calibration_results,
        .ls_PSA_results_ = self$PSA_results,
        .model_interventions_ = self$model_interventions$v_interv_names,
        .model_outcomes_ = self$model_interventions$v_interv_outcomes,
        .label_effects_ = .label_effects_,
        .label_costs_ = .label_costs_,
        .wtp_key_values_ = .wtp_key_values_,
        .highlight_optimal_choices_ = .highlight_optimal_choices_,
        .currency_symbol_ = .currency_symbol_,
        .output_type_ = .output_type_,
        .full_output_format_ = .full_output_format_,
        .add_simulated_truth_ = .add_simulated_truth_,
        .truth_PSA_output_list_path_ = .truth_PSA_output_list_path_,
        .truth_PSA_output_list_ = .truth_PSA_output_list_,
        .generate_partial_cea_table_ = .generate_partial_cea_table_,
        .partial_cea_table_groups_ = .partial_cea_table_groups_,
        .generate_relative_values_ = .generate_relative_values_,
        .relative_values_data_ = .relative_values_data_
      )
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
              as.data.frame() %>%
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
