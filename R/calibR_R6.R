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
    #' @field PSA_results PSA results
    PSA_results = NULL,
    #' @field PSA_summary PSA results' summary
    PSA_summary = NULL,
    #' @field PSA_summary_tables PSA results' summary tables
    PSA_summary_tables = NULL,
    #' @field simulated_targets PSA results
    simulated_targets = NULL,
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
    #'
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
                                .saving_image_scale_ = 5) {
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
                                  scale = .saving_image_scale_)
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
                                  scale = .saving_image_scale_)
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
                                  .legend_pos_ = "bottom",
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
      private$plot_targets_(
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
                                        .legend_pos_ = "bottom",
                                        .log_scaled_ = FALSE,
                                        .save_ = FALSE,
                                        .saving_path_ = here::here(),
                                        .saving_image_dir_ = "/images/Prior-posterior/",
                                        .saving_image_scale_ = 2,
                                        .saving_image_width_ = 1000,
                                        .saving_image_height_ = 700,
                                        .saving_image_units_ = "px") {
      ## Invoke the private plotting function:----
      private$plot_distributions(
        .engine_ = .engine_,
        .bins_ = .bins_,
        .legend_pos_ = .legend_pos_,
        .log_scaled_ = .log_scaled_)

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
    #' @param .save_ Logical for whether to save tables data.
    #' @param .saving_path_ String defining the path for where to save the
    #' tables data.
    #' @param .saving_data_dir_ String defining the sub-folder in the path
    #' where to save the tables data.
    #'
    #' @return
    #' @export
    #'
    #' @examples
    #' \dontrun{
    #' }
    draw_PSA_summary_tables = function(.save_ = FALSE,
                                       .saving_path_ = here::here(),
                                       .saving_data_dir_ = "/data/PSA tables/") {
      ## Generate PSA summary tables:----
      private$generate_PSA_summary_tables()

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
          .x = self$PSA_summary_tables %>%
            names(.),
          .f = function(.group_) {
            purrr::walk(
              .x = self$PSA_summary_tables[[.group_]] %>%
                names(.),
              .f = function(.calib_category_) {
                    if(.calib_category_ == "True") {
                      table_name <- glue::glue(
                        "{.group_}_truth.rds")
                      saveRDS(
                        object = self$
                          PSA_summary_tables[[.group_]][[.calib_category_]],
                        file = glue::glue(
                          "{data_saving_path}{table_name}"))
                    } else if(.group_ == "Combined") {
                      table_name <- glue::glue(
                        "{.group_}_{.calib_category_}.rds")
                      saveRDS(
                        object = self$
                          PSA_summary_tables[[.group_]][[.calib_category_]],
                        file = glue::glue(
                          "{data_saving_path}{table_name}"))
                    } else {
                      purrr::walk(
                        .x = self$PSA_summary_tables[[.group_]][[.calib_category_]] %>%
                          names(.),
                        .f = function(.calib_method_) {
                          table_name <- glue::glue(
                            "{.group_}_{.calib_category_}_{.calib_method_}.rds")
                          saveRDS(
                            object = self$
                              PSA_summary_tables[[.group_]][[.calib_category_]][[.calib_method_]],
                            file = glue::glue(
                              "{data_saving_path}{table_name}"))
                        })

                    }
              })
          })
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
    ### Bayesian helper functions:----
    #### Sample prior:----
    # Use Random Grid Sampling (RGS) to sample from prior distribution
    # This (_) version of the function outputs a vector of values if .n_samples = 1
    #
    # @param .l_params A list that contains a vector of parameter names,
    # distributions and distributions' arguments.
    # @param .n_samples An integer specifying the number of samples to be
    # generated.
    # @param ... additional arguments, for example: .seed_no to set a seed
    # number.
    #
    # @return A table with each parameter RGS samples in a separate column
    #
    # @examples
    # \dontrun{
    # v_params_names <- c("p_Mets", "p_DieMets")
    # v_params_dists <- c("unif", "unif")
    # args <- list(list(min = 0.04, max = 0.16),
    #              list(min = 0.04, max = 0.12))
    # l_params <- list('v_params_names' = v_params_names,
    #                  'v_params_dists' = v_params_dists,
    #                  'args' = args)
    #
    # sample_prior_RGS_(.l_params = l_params,
    #                   .n_samples = 1)
    # }
    sample_prior_RGS_ = function(
    .n_samples = 1,
    .l_params = self$calibration_parameters,
    ...) {
      # Grab additional arguments:
      dots = list(...)
      if(!is.null(dots[['.ssed_no']]))
        set.seed(dots[['.ssed_no']])
      # Define inputs list:
      l_rgs <- list(.l_params[['v_params_names']],
                    paste0('r', .l_params[['v_params_dists']]),
                    .l_params[['args']],
                    .l_params[['v_params_dists']])
      # Make sure parameter names are in a named vector:
      names(l_rgs[[1]]) <- l_rgs[[1]]
      # Map over parameters and sample values accordingly:
      if(.n_samples == 1){
        vec_rgs_samp <- purrr::pmap_dbl(
          .l = l_rgs,
          .f = function(.name, .func, .arg, .dist) {
            assign(.name,
                   purrr::exec(.func,
                               .n_samples,
                               !!!.arg)
            )
          }
        )

        return(vec_rgs_samp)
      } else {
        tbl_rgs_samp <- purrr::pmap_dfc(
          .l = l_rgs,
          .f = function(.name, .func, .arg, .dist) {
            assign(.name,
                   purrr::exec(.func,
                               .n_samples,
                               !!!.arg)
            )
          }
        )

        return(tbl_rgs_samp)
      }
    },

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
    # @param .transform Logical for whether to back-transform parameters to
    # their original scale.
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
    # @param .MCMC_burnIn the number of samples before starting to retain samples
    # @param .MCMC_samples the total number of samples the MCMC algorithm should
    # generate including the burn-in sample including the .MCMC_burnIn. This value
    # should not be equal to or less than .MCMC_burnIn.
    # @param .MCMC_thin the value used to thin the resulting chain
    # @param .MCMC_rerun use the proposal distribution covariance matrix from the
    # first run to re-run the MCMC chain.
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
    .IMIS_iterations = 30,
    .MCMC_burnIn = 10000,
    .MCMC_samples = 50000,
    .MCMC_thin = 5,
    .MCMC_rerun = TRUE) {
      # Ensure that .b_method is supported by the function:
      stopifnot(".b_method is supported by the function" =
                  any(.b_method %in% c('SIR', 'IMIS', 'MCMC')))

      if(.b_method == 'IMIS' & is.null(.IMIS_sample))
        .IMIS_sample <- 1000

      # If user set a .MCMC_burnIn equal to or more than .MCMC_samples:
      if(.b_method == 'MCMC' & !is.null(.MCMC_samples) & !is.null(.MCMC_burnIn)) {
        # ensure we can thin the chain as needed:
        if(.MCMC_samples < .n_resample * .MCMC_thin)
          .MCMC_samples <- .n_resample * .MCMC_thin
        # make sure samples are more than burn-in to accommodate it:
        if(.MCMC_burnIn >= .MCMC_samples)
          .MCMC_samples <- .MCMC_burnIn * 2
      }

      # SIR:
      if(.b_method == 'SIR') {
        cat(paste("Running SIR...", Sys.time(), "\n"))
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
        cat(paste("Running IMIS...", Sys.time(), "\n"))
        ## Run IMIS:
        fit_IMIS <- calibR::IMIS_(
          B = .IMIS_sample, # incremental sample size at each iteration
          B.re = .n_resample, # desired posterior sample size
          number_k = .IMIS_iterations, # maximum iterations
          D = 1, # use optimizer >= 1, do not use = 0.
          sample.prior = private$sample_prior_IMIS,
          prior = private$calculate_prior,
          priors = private$calculate_priors,
          likelihood = private$calculate_likelihood)
        ## Calculate log-likelihood (overall fit) and posterior probability:
        m_calib_res <- fit_IMIS$resample
        Overall_fit <- private$log_likelihood(
          .samples = m_calib_res,
          .func = .func,
          .args = .args,
          .l_targets = .l_targets)
        Posterior_prob <- private$calculate_posteriors(
          .samples = m_calib_res, .func = .func, .args = .args,
          .l_targets = .l_targets, .l_params = .l_params)
        ## Group results into one object:
        IMIS_results <- m_calib_res %>%
          dplyr::as_tibble(
            ~ vctrs::vec_as_names(...,
                                  repair = "unique",
                                  quiet = TRUE)) %>%
          dplyr::mutate(
            "Overall_fit" = Overall_fit,
            "Posterior_prob" = Posterior_prob / sum(Posterior_prob)) %>%
          dplyr::arrange(dplyr::desc(Overall_fit))
        ## Assign column names in the IMIS stats object:
        stats <- fit_IMIS$stat %>%
          dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                                 repair = "unique",
                                                 quiet = TRUE)) %>%
          `colnames<-`(c("MargLike", "UniquePoint", "MaxWeight", "ESS",
                         "ImpWt", "ImpWtVar"))

        return(list('Results' = IMIS_results, 'Method' = "IMIS",
                    'Fit'  = fit_IMIS, 'Stats' = stats))

      } else { # MCMC MH algorithm
        cat(paste("Running MCMC...", Sys.time(), "\n"))
        ## Sample a random set of parameters as a starting point for the chain:
        guess <- private$sample_prior_RGS_(
          .n_samples = 1,
          .l_params = .l_params)
        if(.diag_)
          cat(paste0(guess, "\n"))
        ## Run the Metropolis-Hastings algorithm
        fit_MCMC <- MHadaptive::Metro_Hastings(
          li_func = private$log_posterior,
          pars = guess,
          par_names = .l_params[["v_params_names"]],
          iterations = .MCMC_samples,
          burn_in = .MCMC_burnIn,
          .func = .func,
          .l_targets = .l_targets,
          .l_params = .l_params,
          .args = .args,
          .transform = .transform)
        if(.MCMC_rerun){
          cat(paste("Re-running MCMC...", Sys.time(), "\n"))
          guess <- private$sample_prior_RGS_(
            .n_samples = 1,
            .l_params = .l_params)
          if(.diag_)
            cat(paste0(guess, "\n"))
          fit_MCMC <- MHadaptive::Metro_Hastings(
            li_func = private$log_posterior,
            pars = guess,
            prop_sigma = fit_MCMC$prop_sigma,
            par_names = .l_params[["v_params_names"]],
            iterations = .MCMC_samples,
            burn_in = .MCMC_burnIn,
            .func = .func,
            .l_targets = .l_targets,
            .l_params = .l_params,
            .args = .args,
            .transform = .transform)}
        ## Estimate 95% credible interval:
        cred_int_95 <- MHadaptive::BCI(
          mcmc_object = fit_MCMC,
          interval = c(0.025, 0.975))
        ## Thin the chain if needed:
        if(!is.null(.MCMC_thin))
          fit_MCMC <- MHadaptive::mcmc_thin(
            mcmc_object = fit_MCMC,
            thin = .MCMC_thin)
        ## Calculate log-likelihood (overall fit) and posterior probability:
        m_calib_res <- fit_MCMC$trace
        colnames(m_calib_res) <- .l_params[["v_params_names"]]
        Overall_fit <- private$log_likelihood(
          .samples = m_calib_res, .func = .func, .args = .args,
          .l_targets = .l_targets)
        Posterior_prob <- private$calculate_posteriors(
          .samples = m_calib_res, .func = .func, .args = .args,
          .l_targets = .l_targets, .l_params = .l_params)
        ## Group results into one object:
        MCMC_results <- m_calib_res %>%
          dplyr::as_tibble(
            ~ vctrs::vec_as_names(...,
                                  repair = "unique",
                                  quiet = TRUE)) %>%
          dplyr::mutate(
            "Overall_fit" = Overall_fit,
            "Posterior_prob" = Posterior_prob / sum(Posterior_prob)) %>%
          dplyr::arrange(dplyr::desc(Overall_fit)) %>%
          # randomly sample from the posterior, if required sample smaller than draws:
          {if(.n_resample < nrow(.)){
            dplyr::slice_sample(.data = ., n = .n_resample)
          } else {
            .
          }}

        ## Assign column names to the MCMC proposal distribution covariance matrix:
        prop_sigma <- fit_MCMC$prop_sigma %>%
          dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                                 repair = "unique",
                                                 quiet = TRUE)) %>%
          `colnames<-`(.l_params[["v_params_names"]])

        return(list('Results' = MCMC_results, 'Method' = "MCMC",
                    'Cred_int_95' = cred_int_95, 'Fit'  = fit_MCMC,
                    'Propos_dist_Cov_matrix' = prop_sigma))
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
      # If True set are known
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
      # If True set are known
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
              dplyr::as_tibble() %>%
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
    plot_targets_ = function(.engine_ = "ggplot2",
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
    plot_distributions = function(.engine_ = "ggplot2",
                                  .bins_ = 20,
                                  .legend_pos_ = "bottom",
                                  .log_scaled_ = FALSE) {
      #### Create plots list:----
      self$plots$distributions <- plot_pri_post_distributions(
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
    ### PSA tables:----
    generate_PSA_summary_tables = function(.true_CE_object_path_ = "../../2. Confirmation Review/CR_data/Chap_3/data/CRS_true_PSA.rds",
                                           .subset_CE_table_ = c("NetBenefit",
                                                                "ProbabilityCE",
                                                                "EVPI")) {
      ### Read true CE data:----
      true_CE_object <- readRDS(
        file = .true_CE_object_path_)
      ### Create ShinyPSA's simulated truth object:----
      self$PSA_summary[["True"]] <- ShinyPSA::summarise_PSA_(
        .effs = true_CE_object[["e"]],
        .costs = true_CE_object[["c"]],
        .params = true_CE_object[["p"]],
        .interventions = true_CE_object[["treats"]],
        .plot = FALSE)

      ### Extract PSA results per outcome (costs/QALYs) for post processing:----
      #### Loop through PSA results of each calibration method:----
      PSA_costs_effects <-
        purrr::map(
          .x = self$PSA_results,
          .f = function(.res_) {
            ##### Loop through PSA results of each calibration method:----
            c(purrr::map(
              .x = self$model_interventions$v_interv_outcomes,
              .f = function(.outcome_) {
                ##### Loop through PSA outcomes (costs and effects) to use in PSA:----
                conseq_df <- .res_ %>%
                  ##### Select all costs or effects column:----
                dplyr::select(dplyr::contains(.outcome_)) %>%
                  ##### Remove outcome portion in the column name:----
                dplyr::rename_with(.fn = function(.x) {
                  stringr::str_remove(
                    string = .x,
                    pattern = paste0(".", .outcome_))},
                  .cols = dplyr::everything())
              }),
              "Calibration_data" = list(
                .res_ %>%
                  dplyr::select(
                    -dplyr::contains(self$model_interventions$v_interv_outcomes))),
              "Interventions" = list(self$model_interventions$v_interv_names))
          })

      #### Generate ShinyPSA objects from corresponding calibration results:----
      self$PSA_summary[["ShinyPSA"]] <- purrr::map(
        # to group PSA summary objects by calibration methods category
        .x = self$calibration_results %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          purrr::map(
            # grab the methods names from the calibration results outputs:
            .x = self$calibration_results[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method) {
              ####### Loop through each calibration method:----
              ShinyPSA::summarise_PSA_(
                .effs = PSA_costs_effects[[.calib_method]][["effects"]],
                .costs = PSA_costs_effects[[.calib_method]][["costs"]],
                .params = PSA_costs_effects[[.calib_method]][["params"]],
                .interventions = PSA_costs_effects[[.calib_method]][["Interventions"]],
                .plot = FALSE)
            })
        })

      #### Generate summary tables from the PSA Results:----
      ##### Full printable tables:----
      ###### Calibration methods tables:----
      self$PSA_summary_tables[["Full"]] <- purrr::map(
        .x = self$PSA_summary$ShinyPSA %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          purrr::map(
            .x = self$PSA_summary$ShinyPSA[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method_shinyPSA_) {
              ####### Loop through each calibration methods' shinyPSA object:----
              ShinyPSA::draw_summary_table_(
                .PSA_data = self$PSA_summary$
                  ShinyPSA[[.calib_category_]][[.calib_method_shinyPSA_]],
                .long_ = TRUE,
                .beautify_ = TRUE,
                .latex_ = TRUE,
                .latex_title_ = paste("Calibration methods PSA results", "-",
                                      PSA_costs_effects[[.calib_method_shinyPSA_]][["Calibration_data"]]$
                                        Label[[1]]),
                .latex_subtitle_ = "The table shows all generated results",
                .latex_code_ = FALSE,
                .footnotes_sourcenotes_ = TRUE,
                .all_sourcenotes_ = FALSE,
                .dominance_footnote_ = FALSE,
                .subset_tab_ = FALSE)
            })
        })

      ####### Simulated truth table:----
      self$PSA_summary_tables$Full[["True"]] <- ShinyPSA::draw_summary_table_(
        .PSA_data = self$PSA_summary$True,
        .long_ = TRUE,
        .beautify_ = TRUE,
        .latex_ = TRUE,
        .latex_title_ = "Simulated truth PSA results",
        .latex_subtitle_ = "The table shows all generated results",
        .latex_code_ = FALSE,
        .footnotes_sourcenotes_ = TRUE,
        .all_sourcenotes_ = FALSE,
        .dominance_footnote_ = FALSE,
        .subset_tab_ = FALSE)

      ####### Partial printable tables:----
      ######## Calibration methods tables:----
      self$PSA_summary_tables[["Partials"]] <- purrr::map(
        .x = self$PSA_summary$ShinyPSA %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          purrr::map(
            .x = self$PSA_summary$ShinyPSA[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method_shinyPSA_) {
              ####### Loop through each calibration methods' shinyPSA object:----
              ShinyPSA::draw_summary_table_(
                .PSA_data = self$PSA_summary$
                  ShinyPSA[[.calib_category_]][[.calib_method_shinyPSA_]],
                .long_ = TRUE,
                .beautify_ = TRUE,
                .latex_ = TRUE,
                .latex_title_ = paste("Calibration methods PSA results", "-",
                                      PSA_costs_effects[[.calib_method_shinyPSA_]][["Calibration_data"]]$
                                        Label[[1]]),
                .latex_subtitle_ = "The table shows a subset of the generated results.",
                .latex_code_ = FALSE,
                .dominance_footnote_ = FALSE,
                .footnotes_sourcenotes_ = TRUE,
                .all_sourcenotes_ = FALSE,
                .subset_tab_ = TRUE,
                .subset_group_ = .subset_CE_table_)
            })
        })
      ######## Simulated truth table:----
      self$PSA_summary_tables$Partials[["True"]] <- ShinyPSA::draw_summary_table_(
        .PSA_data = self$PSA_summary$True,
        .long_ = TRUE,
        .beautify_ = TRUE,
        .latex_ = TRUE,
        .latex_title_ = "Simulated truth PSA results",
        .latex_subtitle_ = "The table shows a subset of the generated results.",
        .latex_code_ = FALSE,
        .dominance_footnote_ = FALSE,
        .footnotes_sourcenotes_ = TRUE,
        .all_sourcenotes_ = FALSE,
        .subset_tab_ = TRUE,
        .subset_group_ = .subset_CE_table_)

      ####### Combined printable table:----
      ######## Partial un-printable calibration methods table:----
      PSA_summary_combo_tab_calibs <- purrr::map_df(
        .x = self$PSA_summary$ShinyPSA %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          purrr::map_df(
            .x = self$PSA_summary$ShinyPSA[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method_shinyPSA_) {
              ####### Loop through each calibration methods' shinyPSA object:----
              ShinyPSA::draw_summary_table_(
                .PSA_data = self$PSA_summary$
                  ShinyPSA[[.calib_category_]][[.calib_method_shinyPSA_]],
                .long_ = TRUE,
                .beautify_ = FALSE,
                .latex_ = FALSE,
                .footnotes_sourcenotes_ = FALSE,
                .all_sourcenotes_ = FALSE,
                .dominance_footnote_ = FALSE,
                .subset_tab_ = TRUE,
                .subset_group_ = .subset_CE_table_) %>%
                dplyr::mutate(
                  Method = PSA_costs_effects[[.calib_method_shinyPSA_]][["Calibration_data"]]$
                    Label[[1]])
            })
        })
      ######## Partial un-printable truth table:----
      PSA_summary_combo_tab_truth <- ShinyPSA::draw_summary_table_(
        .PSA_data = self$PSA_summary$True,
        .long_ = TRUE,
        .beautify_ = FALSE,
        .latex_ = FALSE,
        .footnotes_sourcenotes_ = FALSE,
        .all_sourcenotes_ = FALSE,
        .dominance_footnote_ = FALSE,
        .subset_tab_ = TRUE,
        .subset_group_ = .subset_CE_table_) %>%
        dplyr::mutate(
          Method = "Simulated truth")
      ####### Beautified tables:----
      ######## Absolute values:----
      ######### Apply clearer calibration methods' names and generate table:----
      self$PSA_summary_tables[["Combined"]][["Absolute"]] <- dplyr::bind_rows(
        PSA_summary_combo_tab_truth,
        PSA_summary_combo_tab_calibs)  %>%
        dplyr::mutate(
          Method = dplyr::case_when(
            Method == "log_likelihood_RGS" ~ "RGS (LLK):",
            Method == "wSumSquareError_RGS" ~ "RGS (SSE):",
            Method == "log_likelihood_FGS" ~ "FGS (LLK):",
            Method == "wSumSquareError_FGS" ~ "FGS (SSE):",
            Method == "log_likelihood_LHS" ~ "LHS (LLK):",
            Method == "wSumSquareError_LHS" ~ "LHS (SSE):",
            Method == "NM_LLK_0" ~ "NM (LLK):",
            Method == "NM_SSE_0" ~ "NM (SSE):",
            Method == "NM_LLK_1" ~ "NM (LLK - unconverged):",
            Method == "NM_SSE_1" ~ "NM (SSE - unconverged):",
            Method == "BFGS_LLK_0" ~ "BFGS (LLK):",
            Method == "BFGS_SSE_0" ~ "BFGS (SSE):",
            Method == "BFGS_LLK_1" ~ "BFGS (LLK - unconverged):",
            Method == "BFGS_SSE_1" ~ "BFGS (SSE - unconverged):",
            Method == "SANN_LLK_" ~ "SANN (LLK):",
            Method == "SANN_SSE_" ~ "SANN (SSE):",
            TRUE ~ Method)) %>%
        dplyr::mutate(
          Ranking = dplyr::case_when(
            Method == "Simulated truth" ~ 0,
            Method == "MCMC" ~ 1,
            Method == "SIR" ~ 2,
            Method == "IMIS" ~ 3,
            Method %in% c("FGS (LLK):", "FGS (SSE):") ~ 4,
            Method %in% c("RGS (LLK):", "RGS (SSE):") ~ 5,
            Method %in% c("LHS (LLK):", "LHS (SSE):") ~ 6,
            Method %in% c("BFGS (LLK):", "BFGS (SSE):", "BFGS (LLK - unconverged):",
                          "BFGS (SSE - unconverged):") ~ 7,
            Method %in% c("NM (LLK):", "NM (SSE):", "NM (LLK - unconverged):",
                          "NM (SSE - unconverged):") ~ 8,
            Method %in% c("SANN (LLK):", "SANN (SSE):") ~ 9)) %>%
        dplyr::arrange(Ranking) %>%
        dplyr::select(-Ranking) %>%
        dplyr::group_by(Method, RowGroup_) %>%
        gt::gt() %>%
        gt::tab_style(
          style = gt::cell_text(
            weight = "bold"),
          locations = list(
            gt::cells_column_labels(),
            gt::cells_row_groups())) %>%
        gt::tab_options(
          row_group.as_column = TRUE) %>%
        gt::tab_footnote(
          data = .,
          footnote = gt::md(
            "_The ICER threshold values used in computing the results are
      preceeded by the \"@\" symbol in the corresponding rows._"),
          locations = gt::cells_row_groups(
            groups = gt::contains(c(
              glue::glue("Expected Value of Perfect Information (£)"),
              glue::glue("Net Benefit (£)"),
              "Probability Cost-Effective"))))

      ######## Relative values:----
      PSA_summary_combo_tab_calibs_rel <- purrr::map_dfc(
        .x = self$model_interventions$v_interv_names,
        .f = function(.interv_) {
          ######### Loop over each intervention:----
          PSA_summary_combo_tab_calibs %>%
            ######### Filter Net Benefit to estimate relative values:----
          dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
            ######### Make sure relative values are estimated correctly per method:----
          dplyr::group_by(Method) %>%
            ######### Estimate absolute difference between calibrations and truth:----
          dplyr::mutate(
            ######### Truth and calibration results are estimated by intervention:----
            dplyr::across(
              .cols = .interv_,
              .fns = function(.x) {
                ######### Remove "£" and "," from values to compute differences:----
                x_ = gsub(
                  pattern = "£",
                  replacement = "",
                  x = .x)
                x_ = gsub(
                  pattern = ",",
                  replacement = "",
                  x = x_)
                ######### Convert characters/strings to numeric:----
                x_ = as.numeric(x_)
                ######### Next, grab simulated truth values for same intervention:----
                y_ = PSA_summary_combo_tab_truth %>%
                  ######### Keep Net Benefit values:----
                dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
                  ######### Mutate the values of the same intervention above:----
                dplyr::mutate(
                  dplyr::across(
                    .cols = .interv_,
                    .fns = function(.x) {
                      ######### Remove the "£" and "," to run calculations:----
                      x_ = gsub(
                        pattern = "£",
                        replacement = "",
                        x = .x)
                      x_ = gsub(
                        pattern = ",",
                        replacement = "",
                        x = x_)
                      ######### Convert the values to numeric for calculations:----
                      x_ = as.numeric(x_)})) %>%
                  ######### Extract the values to estimate abs difference from truth:----
                dplyr::pull(.data[[.interv_]])
                ######### Estimate absolute difference:----
                x_ = abs(x_ - y_)
                x_
              })) %>%
            ######### Remove grouping now that abs differences were obtained:----
          dplyr::ungroup() %>%
            dplyr::mutate(
              dplyr::across(
                .cols = .interv_,
                .fns = function(.x) {
                  ######### Re-apply the earlier formatting:----
                  scales::dollar(
                    x = .x,
                    prefix = "£")
                })) %>%
            ######### If this was the first intervention in intervention's list:----
          {if (.interv_ == self$model_interventions$v_interv_names[1]) {
            ######### Append the auxiliary columns from original data table:----
            dplyr::select(
              .data = .,
              colnames(PSA_summary_combo_tab_calibs)[
                which(
                  !colnames(PSA_summary_combo_tab_calibs) %in%
                    names(self$model_interventions$v_interv_names)[
                      which(names(self$model_interventions$v_interv_names) !=
                              .interv_)])])
          } else {
            dplyr::select(
              .data = .,
              .interv_)
          }}
        }) %>%
        ######### Bind the rows of the other reported results:----
      dplyr::bind_rows(
        PSA_summary_combo_tab_calibs %>%
          dplyr::filter(RowGroup_ != "Net Benefit (£)"),
        .) %>%
        ######### Ensure they are ranked as needed:----
      dplyr::mutate(
        Ranking = dplyr::case_when(
          RowGroup_ == "Net Benefit (£)" ~ 1,
          RowGroup_ == "Probability Cost-Effective" ~ 2,
          RowGroup_ == "Expected Value of Perfect Information (£)" ~ 3,
          TRUE ~ NA_real_)) %>%
        dplyr::arrange(Method, Ranking) %>%
        dplyr::select(-Ranking) %>%
        ######### Bind the simulated truth results to the top of the table:----
      dplyr::bind_rows(
        PSA_summary_combo_tab_truth,
        .)
      ######### Apply clearer calibration methods' names:----
      PSA_summary_combo_tab_calibs_rel <- PSA_summary_combo_tab_calibs_rel %>%
        dplyr::mutate(
          Method = dplyr::case_when(
            Method == "log_likelihood_RGS" ~ "RGS (LLK):",
            Method == "wSumSquareError_RGS" ~ "RGS (SSE):",
            Method == "log_likelihood_FGS" ~ "FGS (LLK):",
            Method == "wSumSquareError_FGS" ~ "FGS (SSE):",
            Method == "log_likelihood_LHS" ~ "LHS (LLK):",
            Method == "wSumSquareError_LHS" ~ "LHS (SSE):",
            Method == "NM_LLK_0" ~ "NM (LLK):",
            Method == "NM_SSE_0" ~ "NM (SSE):",
            Method == "NM_LLK_1" ~ "NM (LLK - unconverged):",
            Method == "NM_SSE_1" ~ "NM (SSE - unconverged):",
            Method == "BFGS_LLK_0" ~ "BFGS (LLK):",
            Method == "BFGS_SSE_0" ~ "BFGS (SSE):",
            Method == "BFGS_LLK_1" ~ "BFGS (LLK - unconverged):",
            Method == "BFGS_SSE_1" ~ "BFGS (SSE - unconverged):",
            Method == "SANN_LLK_" ~ "SANN (LLK):",
            Method == "SANN_SSE_" ~ "SANN (SSE):",
            TRUE ~ Method)) %>%
        dplyr::mutate(
          Ranking = dplyr::case_when(
            Method == "Simulated truth" ~ 0,
            Method == "MCMC" ~ 1,
            Method == "SIR" ~ 2,
            Method == "IMIS" ~ 3,
            Method %in% c("FGS (LLK):", "FGS (SSE):") ~ 4,
            Method %in% c("RGS (LLK):", "RGS (SSE):") ~ 5,
            Method %in% c("LHS (LLK):", "LHS (SSE):") ~ 6,
            Method %in% c("BFGS (LLK):", "BFGS (SSE):", "BFGS (LLK - unconverged):",
                          "BFGS (SSE - unconverged):") ~ 7,
            Method %in% c("NM (LLK):", "NM (SSE):", "NM (LLK - unconverged):",
                          "NM (SSE - unconverged):") ~ 8,
            Method %in% c("SANN (LLK):", "SANN (SSE):") ~ 9)) %>%
        dplyr::arrange(Ranking) %>%
        dplyr::select(-Ranking) %>%
        dplyr::group_by(Method, RowGroup_)

      ######### Locate footnotes locations:----
      relative_vals_footnote <- PSA_summary_combo_tab_calibs_rel %>%
        dplyr::filter(Method != "Simulated truth") %>%
        dplyr::pull(Method) %>%
        unique(.) %>%
        paste(., "-", "Net Benefit (£)")

      ######### Generate beautified tables:----
      self$PSA_summary_tables[["Combined"]][["Relative"]] <-
        PSA_summary_combo_tab_calibs_rel %>%
        gt::gt() %>%
        gt::tab_style(
          style = gt::cell_text(
            weight = "bold"),
          locations = list(
            gt::cells_column_labels(),
            gt::cells_row_groups())) %>%
        gt::tab_options(
          row_group.as_column = TRUE) %>%
        gt::tab_footnote(
          data = .,
          footnote = gt::md(
            "_The ICER threshold values used in computing the results are
      preceeded by the \"@\" symbol in the corresponding rows._"),
          locations = gt::cells_row_groups(
            groups = gt::contains(c(
              glue::glue("Expected Value of Perfect Information (£)"),
              glue::glue("Net Benefit (£)"),
              "Probability Cost-Effective")))) %>%
        gt::tab_footnote(
          data = .,
          footnote = gt::md(
            "_Absolute values_"),
          locations = gt::cells_row_groups(
            groups = gt::contains("True - Net Benefit"))) %>%
        gt::tab_footnote(
          data = .,
          footnote = gt::md(
            "_Relative to true 'Net Benefit' values_"),
          locations = gt::cells_row_groups(
            groups = relative_vals_footnote))
      ####### Combined group-level printable table:----
      ######## Partial group-level un-printable calibration methods table:----
      PSA_summary_combo_tab_calibs_grp <- purrr::map(
        .x = self$PSA_summary$ShinyPSA %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          purrr::map_df(
            .x = self$PSA_summary$ShinyPSA[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method_shinyPSA_) {
              ####### Loop through each calibration methods' shinyPSA object:----
              ShinyPSA::draw_summary_table_(
                .PSA_data = self$PSA_summary$
                  ShinyPSA[[.calib_category_]][[.calib_method_shinyPSA_]],
                .long_ = TRUE,
                .beautify_ = FALSE,
                .latex_ = FALSE,
                .footnotes_sourcenotes_ = FALSE,
                .all_sourcenotes_ = FALSE,
                .dominance_footnote_ = FALSE,
                .subset_tab_ = TRUE,
                .subset_group_ = .subset_CE_table_) %>%
                dplyr::mutate(
                  Method = PSA_costs_effects[[.calib_method_shinyPSA_]][["Calibration_data"]]$
                    Label[[1]])
            })
        })
      ####### Beautified tables:----
      ######## Absolute per group values:----
      ######### Apply clearer calibration methods' names and generate table:----
      self$PSA_summary_tables[["Comb_grp"]][["Absolute"]] <- purrr::map(
        .x = PSA_summary_combo_tab_calibs_grp %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          dplyr::bind_rows(
            PSA_summary_combo_tab_truth,
            PSA_summary_combo_tab_calibs_grp[[.calib_category_]])  %>%
            dplyr::mutate(
              Method = dplyr::case_when(
                Method == "log_likelihood_RGS" ~ "RGS (LLK):",
                Method == "wSumSquareError_RGS" ~ "RGS (SSE):",
                Method == "log_likelihood_FGS" ~ "FGS (LLK):",
                Method == "wSumSquareError_FGS" ~ "FGS (SSE):",
                Method == "log_likelihood_LHS" ~ "LHS (LLK):",
                Method == "wSumSquareError_LHS" ~ "LHS (SSE):",
                Method == "NM_LLK_0" ~ "NM (LLK):",
                Method == "NM_SSE_0" ~ "NM (SSE):",
                Method == "NM_LLK_1" ~ "NM (LLK - unconverged):",
                Method == "NM_SSE_1" ~ "NM (SSE - unconverged):",
                Method == "BFGS_LLK_0" ~ "BFGS (LLK):",
                Method == "BFGS_SSE_0" ~ "BFGS (SSE):",
                Method == "BFGS_LLK_1" ~ "BFGS (LLK - unconverged):",
                Method == "BFGS_SSE_1" ~ "BFGS (SSE - unconverged):",
                Method == "SANN_LLK_" ~ "SANN (LLK):",
                Method == "SANN_SSE_" ~ "SANN (SSE):",
                TRUE ~ Method)) %>%
            dplyr::mutate(
              Ranking = dplyr::case_when(
                Method == "Simulated truth" ~ 0,
                Method == "MCMC" ~ 1,
                Method == "SIR" ~ 2,
                Method == "IMIS" ~ 3,
                Method %in% c("FGS (LLK):", "FGS (SSE):") ~ 4,
                Method %in% c("RGS (LLK):", "RGS (SSE):") ~ 5,
                Method %in% c("LHS (LLK):", "LHS (SSE):") ~ 6,
                Method %in% c("BFGS (LLK):", "BFGS (SSE):", "BFGS (LLK - unconverged):",
                              "BFGS (SSE - unconverged):") ~ 7,
                Method %in% c("NM (LLK):", "NM (SSE):", "NM (LLK - unconverged):",
                              "NM (SSE - unconverged):") ~ 8,
                Method %in% c("SANN (LLK):", "SANN (SSE):") ~ 9)) %>%
            dplyr::arrange(Ranking) %>%
            dplyr::select(-Ranking) %>%
            dplyr::group_by(Method, RowGroup_) %>%
            gt::gt() %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold"),
              locations = list(
                gt::cells_column_labels(),
                gt::cells_row_groups())) %>%
            gt::tab_options(
              row_group.as_column = TRUE) %>%
            gt::tab_footnote(
              data = .,
              footnote = gt::md(
                "_The ICER threshold values used in computing the results are
      preceeded by the \"@\" symbol in the corresponding rows._"),
              locations = gt::cells_row_groups(
                groups = gt::contains(c(
                  glue::glue("Expected Value of Perfect Information (£)"),
                  glue::glue("Net Benefit (£)"),
                  "Probability Cost-Effective"))))
        })
      ######## Relative values:----
      self$PSA_summary_tables[["Comb_grp"]][["Relative"]] <- purrr::map(
        .x = PSA_summary_combo_tab_calibs_grp %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          PSA_summary_combo_tab_calibs_rel_grp <- purrr::map_dfc(
            .x = self$model_interventions$v_interv_names,
            .f = function(.interv_) {
              ######### Loop over each intervention:----
              PSA_summary_combo_tab_calibs_grp[[.calib_category_]] %>%
                ######### Filter Net Benefit to estimate relative values:----
              dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
                ######### Make sure relative values are estimated correctly per method:----
              dplyr::group_by(Method) %>%
                ######### Estimate absolute difference between calibrations and truth:----
              dplyr::mutate(
                ######### Truth and calibration results are estimated by intervention:----
                dplyr::across(
                  .cols = .interv_,
                  .fns = function(.x) {
                    ######### Remove "£" and "," from values to compute differences:----
                    x_ = gsub(
                      pattern = "£",
                      replacement = "",
                      x = .x)
                    x_ = gsub(
                      pattern = ",",
                      replacement = "",
                      x = x_)
                    ######### Convert characters/strings to numeric:----
                    x_ = as.numeric(x_)
                    ######### Next, grab simulated truth values for same intervention:----
                    y_ = PSA_summary_combo_tab_truth %>%
                      ######### Keep Net Benefit values:----
                    dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
                      ######### Mutate the values of the same intervention above:----
                    dplyr::mutate(
                      dplyr::across(
                        .cols = .interv_,
                        .fns = function(.x) {
                          ######### Remove the "£" and "," to run calculations:----
                          x_ = gsub(
                            pattern = "£",
                            replacement = "",
                            x = .x)
                          x_ = gsub(
                            pattern = ",",
                            replacement = "",
                            x = x_)
                          ######### Convert the values to numeric for calculations:----
                          x_ = as.numeric(x_)})) %>%
                      ######### Extract the values to estimate abs difference from truth:----
                    dplyr::pull(.data[[.interv_]])
                    ######### Estimate absolute difference:----
                    x_ = abs(x_ - y_)
                    x_
                  })) %>%
                ######### Remove grouping now that abs differences were obtained:----
              dplyr::ungroup() %>%
                dplyr::mutate(
                  dplyr::across(
                    .cols = .interv_,
                    .fns = function(.x) {
                      ######### Re-apply the earlier formatting:----
                      scales::dollar(
                        x = .x,
                        prefix = "£")
                    })) %>%
                ######### If this was the first intervention in intervention's list:----
              {if (.interv_ == self$model_interventions$v_interv_names[1]) {
                ######### Append the auxiliary columns from original data table:----
                dplyr::select(
                  .data = .,
                  colnames(PSA_summary_combo_tab_calibs_grp[[.calib_category_]])[
                    which(
                      !colnames(PSA_summary_combo_tab_calibs_grp[[.calib_category_]]) %in%
                        names(self$model_interventions$v_interv_names)[
                          which(names(self$model_interventions$v_interv_names) !=
                                  .interv_)])])
              } else {
                dplyr::select(
                  .data = .,
                  .interv_)
              }}
            }) %>%
            ######### Bind the rows of the other reported results:----
          dplyr::bind_rows(
            PSA_summary_combo_tab_calibs_grp[[.calib_category_]] %>%
              dplyr::filter(RowGroup_ != "Net Benefit (£)"),
            .) %>%
            ######### Ensure they are ranked as needed:----
          dplyr::mutate(
            Ranking = dplyr::case_when(
              RowGroup_ == "Net Benefit (£)" ~ 1,
              RowGroup_ == "Probability Cost-Effective" ~ 2,
              RowGroup_ == "Expected Value of Perfect Information (£)" ~ 3,
              TRUE ~ NA_real_)) %>%
            dplyr::arrange(Method, Ranking) %>%
            dplyr::select(-Ranking)
          PSA_summary_combo_tab_calibs_rel_grp <- PSA_summary_combo_tab_calibs_rel_grp %>%
            ######### Bind the simulated truth results to the top of the table:----
          dplyr::bind_rows(
            PSA_summary_combo_tab_truth,
            .) %>%
            ######### Apply clearer calibration methods' names:----
          dplyr::mutate(
            Method = dplyr::case_when(
              Method == "log_likelihood_RGS" ~ "RGS (LLK):",
              Method == "wSumSquareError_RGS" ~ "RGS (SSE):",
              Method == "log_likelihood_FGS" ~ "FGS (LLK):",
              Method == "wSumSquareError_FGS" ~ "FGS (SSE):",
              Method == "log_likelihood_LHS" ~ "LHS (LLK):",
              Method == "wSumSquareError_LHS" ~ "LHS (SSE):",
              Method == "NM_LLK_0" ~ "NM (LLK):",
              Method == "NM_SSE_0" ~ "NM (SSE):",
              Method == "NM_LLK_1" ~ "NM (LLK - unconverged):",
              Method == "NM_SSE_1" ~ "NM (SSE - unconverged):",
              Method == "BFGS_LLK_0" ~ "BFGS (LLK):",
              Method == "BFGS_SSE_0" ~ "BFGS (SSE):",
              Method == "BFGS_LLK_1" ~ "BFGS (LLK - unconverged):",
              Method == "BFGS_SSE_1" ~ "BFGS (SSE - unconverged):",
              Method == "SANN_LLK_" ~ "SANN (LLK):",
              Method == "SANN_SSE_" ~ "SANN (SSE):",
              TRUE ~ Method)) %>%
            dplyr::mutate(
              Ranking = dplyr::case_when(
                Method == "Simulated truth" ~ 0,
                Method == "MCMC" ~ 1,
                Method == "SIR" ~ 2,
                Method == "IMIS" ~ 3,
                Method %in% c("FGS (LLK):", "FGS (SSE):") ~ 4,
                Method %in% c("RGS (LLK):", "RGS (SSE):") ~ 5,
                Method %in% c("LHS (LLK):", "LHS (SSE):") ~ 6,
                Method %in% c("BFGS (LLK):", "BFGS (SSE):", "BFGS (LLK - unconverged):",
                              "BFGS (SSE - unconverged):") ~ 7,
                Method %in% c("NM (LLK):", "NM (SSE):", "NM (LLK - unconverged):",
                              "NM (SSE - unconverged):") ~ 8,
                Method %in% c("SANN (LLK):", "SANN (SSE):") ~ 9)) %>%
            dplyr::arrange(Ranking) %>%
            dplyr::select(-Ranking) %>%
            dplyr::group_by(Method, RowGroup_)

          ######### Locate footnotes locations:----
          relative_vals_footnote <- PSA_summary_combo_tab_calibs_rel_grp %>%
            dplyr::filter(Method != "Simulated truth") %>%
            dplyr::pull(Method) %>%
            unique(.) %>%
            paste(., "-", "Net Benefit (£)")

          ######### Generate beautified tables:----
          PSA_summary_combo_tab_calibs_rel_grp <- PSA_summary_combo_tab_calibs_rel_grp %>%
            gt::gt() %>%
            gt::tab_style(
              style = gt::cell_text(
                weight = "bold"),
              locations = list(
                gt::cells_column_labels(),
                gt::cells_row_groups())) %>%
            gt::tab_options(
              row_group.as_column = TRUE) %>%
            gt::tab_footnote(
              data = .,
              footnote = gt::md(
                "_The ICER threshold values used in computing the results are
      preceeded by the \"@\" symbol in the corresponding rows._"),
              locations = gt::cells_row_groups(
                groups = gt::contains(c(
                  glue::glue("Expected Value of Perfect Information (£)"),
                  glue::glue("Net Benefit (£)"),
                  "Probability Cost-Effective")))) %>%
            gt::tab_footnote(
              data = .,
              footnote = gt::md(
                "_Absolute values_"),
              locations = gt::cells_row_groups(
                groups = gt::contains("True - Net Benefit"))) %>%
            gt::tab_footnote(
              data = .,
              footnote = gt::md(
                "_Relative to true 'Net Benefit' values_"),
              locations = gt::cells_row_groups(
                groups = relative_vals_footnote))
        })
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
