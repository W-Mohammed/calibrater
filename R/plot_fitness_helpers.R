#' Draw fitness function(s) contour plots
#'
#' @param .engine_ String specifying the plotting engine, currently supports
#' "plotly".
#' @param .l_params_ List containing information about calibration parameters,
#' including parameters' names, distributions, and boundaries.
#' @param .l_gof_values_ List containing the goodness-of-fit (GOF) values
#' corresponding to specific parameter(s) configurations.
#' @param .l_calibration_results_ List containing calibration results for each
#' of the tested or employed calibration method.
#' @param .l_PSA_samples_ List containing Probabilistic Sensitivity Analysis
#' (PSA) values corresponding to each calibration method.
#' @param .t_prior_samples_ Dataset or tibble containing prior samples.
#' @param .prior_samples_ Numeric (integer) setting the number of prior samples
#' to be added to the plot.
#' @param .gof_ String identifying the name of the GOF measure used. The
#' function currently supports "LLK" or "SSE" for log-likelihood and
#' Sum-of-Squared-Errors fitness functions, respectively.
#' @param .blank_ Logical for whether to only plot blank or empty GOF contour
#' plots.
#' @param .percent_sampled_ Numeric (double) specifying the proportion of
#' of sets to identify as good-fitting sets by the "random" (undirected or
#' unguided) non-Bayesian calibration methods.
#' @param .true_points_ Logical for whether to show "True values" on the plot.
#' @param .greys_ Logical for whether to use the "Greys" colour-scale. The
#' .scale_ parameter overrides .greys_ if it was not NULL.
#' @param .scale_ String specifying colour-scale applied to the "ploty" contour.
#' The options are "Blackbody", "Bluered", "Blues", "Cividis", "Earth",
#' "Electric", "Greens", "Greys", "Hot", "Jet", "Picnic", "Portland", "Rainbow",
#' "RdBu", "Reds", "Viridis" (default), "YlGnBu", and "YlOrRd".
#' @param .coloring_ String specifying where the colour-scale set by the .scale_
#' parameter is applied on the "plotly" contour. The options are "fill",
#' "heatmap", "none", and "lines". The "fill" option (default) paints the colour
#' scales over the layers of the contour; the "heatmap" option employs a heatmap
#' gradient colouring between each contour level; the "none" option paints no
#' colours on the contour layers and uses a single colour with the contour
#' lines; and the "lines" option applies the colour-scale on the contour lines.
#' @param .legend_ Logical for whether to show the "plotly" contour colour-bar
#' legend.
#' @param .zoom_ Logical (default TRUE) for whether to zoom in to the identified
#' sets (best fitting sets, extrema, and posterior distributions centres).
#' @param .x_axis_lb_ Numeric (double) value specifying the lower bound of x-axis.
#' @param .x_axis_ub_ Numeric (double) value specifying the upper bound of x-axis.
#' @param .y_axis_lb_ Numeric (double) value specifying the lower bound of y-axis.
#' @param .y_axis_ub_ Numeric (double) value specifying the upper bound of y-axis.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' fitness_function_plots <- plot_fitness_function(
#'   .l_params_ = CR_CRS_2P2T$calibration_parameters,
#'   .l_gof_values_ = CR_CRS_2P2T$GOF_measure_plot,
#'   .l_calibration_results_ = CR_CRS_2P2T$calibration_results,
#'   .l_PSA_samples_ = CR_CRS_2P2T$PSA_samples,
#'   .t_prior_samples_ = CR_CRS_2P2T$prior_samples$LHS,
#'   .true_points_ = TRUE,
#'   .greys_ = FALSE,
#'   .scale_ = NULL,
#'   .coloring_ = "none",
#'   .blank_ = FALSE)
#' fitness_function_plots <- plot_fitness_function(
#'   .l_params_ = CR_CRS_2P1T$calibration_parameters,
#'   .l_gof_values_ = CR_CRS_2P1T$GOF_measure_plot,
#'   .l_calibration_results_ = CR_CRS_2P1T$calibration_results,
#'   .l_PSA_samples_ = CR_CRS_2P1T$PSA_samples,
#'   .t_prior_samples_ = CR_CRS_2P1T$prior_samples$LHS,
#'   .true_points_ = TRUE,
#'   .greys_ = FALSE,
#'   .scale_ = NULL,
#'   .coloring_ = "none",
#'   .blank_ = FALSE)
#' }
plot_fitness_function <- function(.engine_ = "plotly",
                                  .l_params_ = self$calibration_parameters,
                                  .l_gof_values_ = self$GOF_measure_plot,
                                  .l_calibration_results_ = self$calibration_results,
                                  .l_PSA_samples_ = self$PSA_samples,
                                  .t_prior_samples_ = self$prior_samples$LHS,
                                  .prior_samples_ = 1e3,
                                  .gof_ = "LLK",
                                  .blank_ = TRUE,
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
  ## For "plotly" plot:----
  if(.engine_ == "plotly") {
    blank_gof_plots_lst <- purrr::map(
      ### Loop over Goodness-of-fit functions:----
      .x = .gof_ %>%
        `names<-`(.gof_),
      .f = function(.gof_name_) {
        purrr::map(
          #### Loop over parameters, start by picking the one on the x-axis:----
          .x = .l_params_$v_params_names,
          .f = function(.param_x) {
            ##### Ignore the current x-param and use the others as y-params:----
            other_params_names <- .l_params_$v_params_names[
              -which(.l_params_$v_params_names == .param_x)]
            ##### Plots lists':----
            purrr::map(
              ###### Loop through the y-params creating a plot for each:----
              .x = other_params_names,
              .f = function(.param_y) {
                ####### Restrict plot to plausible parameter range:----
                x_y_axis_limits_ <- calibR:::fitness_contour_area(
                  .l_params_ = .l_params_,
                  .zoom_ = FALSE,
                  .param_x = .param_x,
                  .param_y = .param_y,
                  .x_axis_lb_ = .x_axis_lb_,
                  .x_axis_ub_ = .x_axis_ub_,
                  .y_axis_lb_ = .y_axis_lb_,
                  .y_axis_ub_ = .y_axis_ub_)
                ####### Create plot:----
                calibR:::plot_fitness_contour(
                  .l_gof_values_ = .l_gof_values_,
                  .l_params_ = .l_params_,
                  .gof_name_ = .gof_name_,
                  .title_ = glue::glue(
                    "<span style = 'color:blue;'><b>{.gof_name_}</b></span> contour plot."),
                  .param_x = .param_x,
                  .param_y = .param_y,
                  .scale_ = .scale_,
                  .greys_ = .greys_,
                  .legend_ = .legend_,
                  .coloring_ = .coloring_,
                  .true_points_ = .true_points_,
                  .legend_x_t_llk_ = "0.15",
                  .legend_x_t_sse_ = "0.15",
                  .legend_x_nt_llk_ = "0.25",
                  .legend_x_nt_sse_ = "0.25",
                  .x_axis_lb_ = x_y_axis_limits_[[".x_axis_lb_"]],
                  .x_axis_ub_ = x_y_axis_limits_[[".x_axis_ub_"]],
                  .y_axis_lb_ = x_y_axis_limits_[[".y_axis_lb_"]],
                  .y_axis_ub_ = x_y_axis_limits_[[".y_axis_ub_"]])
              })
          })
      })
    if(.blank_) {

      return(list(
        "blank" = blank_gof_plots_lst))
    } else {
      ###### Points shapes and colours:----
      symbols_ <- c(
        "Sampled sets" = "x-thin-open", #"x-dot",
        "Starting sets" = "x-thin-open", #"x-dot",
        "Prior samples" = "x-thin-open", #"x-dot",
        "Identified sets" = "circle-open",
        "Local extremas" = "circle-open",
        "Global extrema" = "circle-dot",
        "PSA draws" = "circle-open",
        "Posterior samples" = "circle-open",
        "Posterior centres" = "circle-dot")
      colors_ <- c(
        "Sampled sets" = "black",
        "Starting sets" = "black",
        "Prior samples" = "black",
        "Identified sets" = "red",
        "Local extremas" = "darkorange",
        "Global extrema" = "red",
        "PSA draws" = "pink",
        "Posterior samples" = "red",
        "Posterior centres" = "yellow")
      if(.true_points_) {
        symbols_ <- c(
          "Sampled sets" = "x-thin-open", #"x-dot",
          "Starting sets" = "x-thin-open", #"x-dot",
          "Prior samples" = "x-thin-open", #"x-dot",
          "Identified sets" = "circle-open",
          "Local extremas" = "circle-open",
          "Global extrema" = "circle-dot",
          "PSA draws" = "circle-open",
          "Posterior samples" = "circle-open",
          "Posterior centres" = "circle-dot",
          "True set" = "circle-dot")
        colors_ <- c(
          "Sampled sets" = "black",
          "Starting sets" = "black",
          "Prior samples" = "black",
          "Identified sets" = "red",
          "Local extremas" = "darkorange",
          "Global extrema" = "red",
          "PSA draws" = "pink",
          "Posterior samples" = "red",
          "Posterior centres" = "yellow",
          "True set" = "green")
      }
      ## Loop over the calibration methods categories:----
      calibration_gof_plots_lst <- purrr::map(
        .x = .l_calibration_results_ %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.l_calib_res_) {
          if(.l_calib_res_ == "bayesian") {
            ####### Ensure Priors samples exist:----
            if(is.null(.t_prior_samples_)) {
              assign(
                x = .t_prior_samples_,
                value = calibR::sample_prior_LHS(
                  .n_samples = .prior_samples_,
                  .l_params = .l_params_))
            }
          }
          #### Loop over goodness-of-fit (.gof_) measures:----
          purrr::map(
            .x = .gof_ %>%
              `names<-`(.gof_),
            .f = function(.gof_name_) {
              if(.l_calib_res_ %in% c("random", "directed")) {
                ####### Grab those files that are related to the .gof used:----
                calib_res_objs_names <- grep(
                  pattern = .gof_name_,
                  x = names(.l_calibration_results_[[.l_calib_res_]]),
                  value = TRUE)
              }
              ####### Loop through identified calibration results objects:----
              purrr::map(
                .x = if(.l_calib_res_ %in% c("random", "directed")) {
                  calib_res_objs_names %>%
                    `names<-`(., .)
                } else if(.l_calib_res_ == "bayesian") {
                  .l_PSA_samples_[[.l_calib_res_]] %>%
                    names(.) %>%
                    `names<-`(., .)
                },
                .f = function(.calib_res_) {
                  if(.l_calib_res_ == "random") {
                    ###### Sort calibration results:----
                    sorted_calib_res <- .l_calibration_results_[[.l_calib_res_]][[.calib_res_]] %>%
                      dplyr::arrange(
                        .data = .,
                        dplyr::desc(Overall_fit))

                    calib_res <- sorted_calib_res %>%
                      dplyr::mutate(
                        Points = "Sampled sets") %>%
                      dplyr::bind_rows(
                        sorted_calib_res %>%
                          dplyr::slice_head(
                            n = round(nrow(.)/.percent_sampled_)) %>%
                          dplyr::mutate(
                            Points = "Identified sets")) %>%
                      ###### Add True set:----
                    {if(.true_points_) {
                      dplyr::bind_rows(
                        .,
                        .l_params_$v_params_true_values) %>%
                        dplyr::mutate(Points = dplyr::case_when(
                          is.na(Points) ~ "True set",
                          TRUE ~ Points))
                    } else {
                      .}}
                    ###### Ensure colours and groups share same levels:----
                    calib_res <- calib_res %>%
                      {if(!.true_points_) {
                        dplyr::mutate(
                          .data = .,
                          Points = factor(
                            x = Points,
                            levels = c("Sampled sets",
                                       "Identified sets")))
                      } else {
                        dplyr::mutate(
                          .data = .,
                          Points = factor(
                            x = Points,
                            levels = c("Sampled sets",
                                       "Identified sets",
                                       "True set")))
                      }}

                    ####### Change colour if too many points in plot:----
                    colors_["Sampled sets"] <- ifelse(
                      calib_res %>%
                        dplyr::filter(
                          Points == "Sampled sets") %>%
                        nrow(.) > 100,
                      "grey",
                      colors_["Sampled sets"])
                  } else if(.l_calib_res_ == "directed") {
                    ###### Transpose the list to group outputs together:----
                    transposed_calib_res <- .l_calibration_results_[[.l_calib_res_]][[.calib_res_]] %>%
                      purrr::transpose()
                    ###### Extract "Starting sets":----
                    calib_res <- transposed_calib_res[["Guess"]] %>%
                      purrr::map_dfr(
                        .x = .,
                        .f = function(.x) {
                          .x}) %>%
                      dplyr::mutate(
                        Points = "Starting sets") %>%
                      ###### Join "Local extremas":----
                    dplyr::bind_rows(
                      ###### Extract identified extremas:----
                      transposed_calib_res[["Estimate"]] %>%
                        purrr::map_dfr(
                          .x = .,
                          .f = function(.x) {
                            .x
                          }) %>%
                        dplyr::mutate(
                          Points = "Local extremas") %>%
                        ###### Find "Global extrema":----
                      dplyr::mutate(
                        GOF = unlist(
                          transposed_calib_res[["GOF value"]])) %>%
                        dplyr::arrange(
                          dplyr::desc(GOF)) %>%
                        dplyr::mutate(
                          Points = dplyr::case_when(
                            dplyr::row_number() == 1 ~ "Global extrema",
                            TRUE ~ Points)
                        )) %>%
                      ###### Grab relevant PSA samples:----
                    {if(!is.null(.l_PSA_samples_[[.l_calib_res_]][[.calib_res_]])) {
                      dplyr::bind_rows(
                        .,
                        .l_PSA_samples_[[.l_calib_res_]][[.calib_res_]]$
                          PSA_calib_draws %>%
                          dplyr::filter(Plot_label == "PSA sets") %>%
                          dplyr::select(
                            dplyr::all_of(
                              .l_params_$v_params_names)) %>%
                          dplyr::mutate(
                            Points = "PSA draws")
                      )
                    } else {
                      .
                    }} %>%
                      ###### Add True set:----
                    {if(.true_points_) {
                      dplyr::bind_rows(
                        .,
                        .l_params_$v_params_true_values) %>%
                        dplyr::mutate(Points = dplyr::case_when(
                          is.na(Points) ~ "True set",
                          TRUE ~ Points))
                    } else {
                      .
                    }}

                    ###### Ensure colours and groups share same levels:----
                    calib_res <- calib_res %>%
                      {if(!.true_points_) {
                        dplyr::mutate(
                          .data = .,
                          Points = factor(
                            x = Points,
                            levels = c("Starting sets",
                                       "Global extrema",
                                       "Local extremas",
                                       "PSA draws")))
                      } else {
                        dplyr::mutate(
                          .data = .,
                          Points = factor(
                            x = Points,
                            levels = c("Starting sets",
                                       "Global extrema",
                                       "Local extremas",
                                       "PSA draws",
                                       "True set")))
                      }}

                    ####### Change colour if too many points in plot:----
                    colors_["Starting sets"] <- ifelse(
                      calib_res %>%
                        dplyr::filter(
                          Points == "Starting sets") %>%
                        nrow(.) > 100,
                      "grey",
                      colors_["Starting sets"])

                  } else if(.l_calib_res_ == "bayesian") {
                    calib_res <- .l_PSA_samples_[[.l_calib_res_]][[.calib_res_]]$
                      PSA_calib_draws %>%
                      dplyr::mutate(
                        Points = dplyr::case_when(
                          Plot_label %in% c("Maximum-a-posteriori",
                                            "Posterior mean") ~ "Posterior centres",
                          TRUE ~ "Posterior samples")) %>%
                      dplyr::bind_rows(
                        .,
                        .t_prior_samples_ %>%
                          dplyr::mutate(
                            Points = "Prior samples")) %>%
                      ###### Add True set:----
                    {if(.true_points_) {
                      dplyr::bind_rows(
                        .,
                        .l_params_$v_params_true_values) %>%
                        dplyr::mutate(Points = dplyr::case_when(
                          is.na(Points) ~ "True set",
                          TRUE ~ Points))
                    } else {
                      .
                    }}
                    ###### Ensure colours and groups share same levels:----
                    calib_res <- calib_res %>%
                      {if(calib_res %>%
                          dplyr::filter(
                            Points == "Posterior centres") %>%
                          nrow(.) > 0) {
                        {if(!.true_points_) {
                          dplyr::mutate(
                            .data = .,
                            Points = factor(
                              x = Points,
                              levels = c("Prior samples",
                                         "Posterior samples",
                                         "Posterior centres")))
                        } else {
                          dplyr::mutate(
                            .data = .,
                            Points = factor(
                              x = Points,
                              levels = c("Prior samples",
                                         "Posterior samples",
                                         "Posterior centres",
                                         "True set")))
                        }}
                      } else {
                        {if(!.true_points_) {
                          dplyr::mutate(
                            .data = .,
                            Points = factor(
                              x = Points,
                              levels = c("Prior samples",
                                         "Posterior samples")))
                        } else {
                          dplyr::mutate(
                            .data = .,
                            Points = factor(
                              x = Points,
                              levels = c("Prior samples",
                                         "Posterior samples",
                                         "True set")))
                        }}
                      }}

                    ####### Change colour if too many points in plot:----
                    colors_["Prior samples"] <- ifelse(
                      calib_res %>%
                        dplyr::filter(
                          Points == "Prior samples") %>%
                        nrow(.) > 100,
                      "grey",
                      colors_["Prior samples"])
                    ####### Assign colour and symbol for posterior centres:----
                    colors_["Posterior centres"] <- ifelse(
                      calib_res %>%
                        dplyr::filter(
                          Points == "Posterior centres") %>%
                        nrow(.) > 0,
                      colors_["Posterior centres"],
                      NULL)

                    symbols_["Posterior centres"] <- ifelse(
                      calib_res %>%
                        dplyr::filter(
                          Points == "Posterior centres") %>%
                        nrow(.) > 0,
                      symbols_["Posterior centres"],
                      NULL)

                  }

                  #### For zoomed in plots:----
                  zoom_calib_res <<- if(.zoom_) {
                    calib_res %>%
                      dplyr::filter(
                        !(Points %in% c("Sampled sets",
                                        "Starting sets",
                                        "Prior samples")))
                  }

                  ###### Add points to the plots:----
                  purrr::map(
                    .x = .l_params_$v_params_names,
                    .f = function(.param_x) {
                      ####### Prepare parameter names:----
                      other_params_names <- .l_params_$v_params_names[
                        -which(.l_params_$v_params_names == .param_x)]
                      ####### Plots list:----
                      plots_list_ <- purrr::map(
                        .x = other_params_names,
                        .f = function(.param_y) {
                          ####### Generate plot title:----
                          title_ <- calibR:::get_gof_contour_plot_title(
                            .gof_function_ = .gof_name_,
                            .calib_method_ = .calib_res_,
                            .colors_ = colors_,
                            .points_names = if(!.zoom_){
                              calib_res %>%
                                dplyr::pull(Points) %>%
                                as.character(.) %>%
                                unique(.)
                            } else {
                              zoom_calib_res %>%
                                dplyr::pull(Points) %>%
                                as.character(.) %>%
                                unique(.)
                            }
                          )
                          ####### Dynamically get axis coordinates to zoom in:----
                          x_y_axis_limits_ <- calibR:::fitness_contour_area(
                            .l_params_ = .l_params_,
                            .zoom_ = .zoom_,
                            .zoom_calib_res_ = zoom_calib_res,
                            .param_x = .param_x,
                            .param_y = .param_y,
                            .x_axis_lb_ = .x_axis_lb_,
                            .x_axis_ub_ = .x_axis_ub_,
                            .y_axis_lb_ = .y_axis_lb_,
                            .y_axis_ub_ = .y_axis_ub_)
                          ####### Create plot:----
                          calibR:::plot_fitness_contour(
                            .l_gof_values_ = .l_gof_values_,
                            .l_params_ = .l_params_,
                            .gof_name_ = .gof_name_,
                            .title_ = title_,
                            .param_x = .param_x,
                            .param_y = .param_y,
                            .scale_ = .scale_,
                            .greys_ = .greys_,
                            .legend_ = .legend_,
                            .coloring_ = .coloring_,
                            .true_points_ = FALSE,
                            .legend_x_t_llk_ =
                              if(.l_calib_res_ == "random") {
                                "0.10"
                              } else if (.l_calib_res_ == "directed") {
                                "0"
                              } else if (.l_calib_res_ == "bayesian") {
                                "0"
                              },
                            .legend_x_t_sse_ =
                              if(.l_calib_res_ == "random") {
                                "0.10"
                              } else if (.l_calib_res_ == "directed") {
                                "0"
                              } else if (.l_calib_res_ == "bayesian") {
                                "0"
                              },
                            .legend_x_nt_llk_ =
                              if(.l_calib_res_ == "random") {
                                "0.20"
                              } else if (.l_calib_res_ == "directed") {
                                "0.05"
                              } else if (.l_calib_res_ == "bayesian") {
                                "0.10"
                              },
                            .legend_x_nt_sse_ =
                              if(.l_calib_res_ == "random") {
                                "0.20"
                              } else if (.l_calib_res_ == "directed") {
                                "0.05"
                              } else if (.l_calib_res_ == "bayesian") {
                                "0.10"
                              },
                            .x_axis_lb_ = x_y_axis_limits_[[".x_axis_lb_"]],
                            .x_axis_ub_ = x_y_axis_limits_[[".x_axis_ub_"]],
                            .y_axis_lb_ = x_y_axis_limits_[[".y_axis_lb_"]],
                            .y_axis_ub_ = x_y_axis_limits_[[".y_axis_ub_"]]) %>%
                            ####### Add points to plot:----
                          plotly::add_trace(
                            p = .,
                            inherit = FALSE,
                            x = calib_res[[.param_x]],
                            y = calib_res[[.param_y]],
                            type = 'scatter',
                            mode = 'markers',
                            marker = list(
                              size = 5),
                            symbol = ~ calib_res[["Points"]],
                            color = ~ calib_res[["Points"]],
                            symbols = symbols_,
                            colors = colors_)
                        })
                    })
                })
            })
        })

      return(c(list(
        "blank" = blank_gof_plots_lst),
        calibration_gof_plots_lst))
    }
  }
}

#' Plot fitness function contour
#'
#' @param .l_gof_values_ List containing the goodness-of-fit (GOF) values
#' corresponding to specific parameter(s) configurations.
#' @param .l_params_ List containing information about calibration parameters,
#' including parameters' names, distributions, and boundaries.
#' @param .gof_name_ String specifying the GOF function name.
#' @param .param_x String identifying the parameter plotted in the x-axis.
#' @param .param_y String identifying the parameter plotted in the y-axis.
#' @param .scale_ String specifying colour-scale applied to the "ploty" contour.
#' The options are "Blackbody", "Bluered", "Blues", "Cividis", "Earth",
#' "Electric", "Greens", "Greys", "Hot", "Jet", "Picnic", "Portland", "Rainbow",
#' "RdBu", "Reds", "Viridis" (default), "YlGnBu", and "YlOrRd".
#' @param .greys_ Logical for whether to use the "Greys" colour-scale. The
#' .scale_ parameter overrides .greys_ if it was not NULL.
#' @param .legend_ Logical for whether to show the "plotly" contour colour-bar
#' legend.
#' @param .coloring_ String specifying where the colour-scale set by the .scale_
#' parameter is applied on the "plotly" contour. The options are "fill",
#' "heatmap", "none", and "lines". The "fill" option (default) paints the colour
#' scales over the layers of the contour; the "heatmap" option employs a heatmap
#' gradient colouring between each contour level; the "none" option paints no
#' colours on the contour layers and uses a single colour with the contour
#' lines; and the "lines" option applies the colour-scale on the contour lines.
#' @param .true_points_ Logical for whether to show "True values" on the plot.
#' @param .legend_x_t_llk_ Numeric (double) value specifying the x-axis
#' relative location when "True values" are plotted and in "LLK" GOF plots.
#' @param .legend_x_t_sse_ Numeric (double) value specifying the x-axis
#' relative location when "True values" are plotted and in "SSE" GOF plots.
#' @param .legend_x_nt_llk_ Numeric (double) value specifying the x-axis
#' relative location when no "True values" are plotted and in "LLK" GOF plots.
#' @param .legend_x_nt_sse_ Numeric (double) value specifying the x-axis
#' relative location when no "True values" are plotted and in "SSE" GOF plots.
#' @param .x_axis_lb_ Numeric (double) value specifying the lower bound of x-axis.
#' @param .x_axis_ub_ Numeric (double) value specifying the upper bound of x-axis.
#' @param .y_axis_lb_ Numeric (double) value specifying the lower bound of y-axis.
#' @param .y_axis_ub_ Numeric (double) value specifying the upper bound of y-axis.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' }
plot_fitness_contour <- function(.l_gof_values_,
                                 .l_params_,
                                 .gof_name_,
                                 .title_ = "",
                                 .param_x,
                                 .param_y,
                                 .scale_,
                                 .greys_,
                                 .legend_,
                                 .coloring_,
                                 .true_points_,
                                 .legend_x_t_llk_ = "0.15",
                                 .legend_x_t_sse_ = "0.15",
                                 .legend_x_nt_llk_ = "0.25",
                                 .legend_x_nt_sse_ = "0.25",
                                 .x_axis_lb_,
                                 .x_axis_ub_,
                                 .y_axis_lb_,
                                 .y_axis_ub_) {
  ## Create plot:----
  plotly::plot_ly(
    name = ifelse(.gof_name_ == "LLK", "LLK", "SSE"),
    x = .l_gof_values_$Results[[.gof_name_]][[.param_x]],
    y = .l_gof_values_$Results[[.gof_name_]][[.param_y]],
    z = .l_gof_values_$Results[[.gof_name_]][["Overall_fit"]],
    ### Declare plot type:----
    type = "contour",
    ### Define background color for the fitness plot:----
    colorscale = if(is.null(.scale_) & .greys_){
      "Greys"
    } else if(is.null(.scale_) & !.greys_) {
      "Viridis"
    } else if(is.null(.scale_) & is.null(.greys_)){
      "Viridis"
    } else {
      .scale_
    },
    contours = list(
      ### Switch contour lines labels:----
      showlabels = ifelse(.legend_,
                          FALSE,
                          TRUE),
      coloring = .coloring_)) %>%
    ### Control legend position:----
  plotly::layout(
    showlegend = ifelse(!.legend_ & (.title_ == "" | is.na(.title_)),
                        TRUE,
                        FALSE),
    margin = list(
      t = 80,
      b = 80 # push x-axis upwards
    ),
    title = list(
      text = .title_,
      font = list(
        size = 25
      ),
      x = 0,
      y = 1,
      xanchor = "left",
      yanchor = "top",
      pad = list(
        b = 3,
        t = 30
      )
    ),
    legend = list(
      x = ifelse(.legend_,
                 "1.02",
                 # Adjust legend if true points are used:
                 ifelse(.true_points_,
                        # Adjust legend if LLK is used:
                        ifelse(.gof_name_ == "LLK",
                               .legend_x_t_llk_, # "0.15"
                               .legend_x_t_sse_), # "0.15"
                        ifelse(.gof_name_ == "LLK",
                               .legend_x_nt_llk_, # "0.25"
                               .legend_x_nt_sse_))), # "0.25"
      y = ifelse(.legend_,
                 "1",
                 "-0.15"),
      orientation = ifelse(.legend_,
                           'v',
                           'h')),
    xaxis = list(
      title = list(
        text = .l_params_$v_params_labels[[.param_x]],
        font = list(
          size = 20
        )
      ),
      range = list(
        .x_axis_lb_,
        .x_axis_ub_),
      showline = TRUE,
      linewidth = 1,
      linecolor = "grey",
      mirror = TRUE), # creates a box
    yaxis = list(
      title = list(
        text = .l_params_$v_params_labels[[.param_y]],
        font = list(
          size = 20
        )
      ),
      range = list(
        .y_axis_lb_,
        .y_axis_ub_),
      showline = TRUE,
      linewidth = 1,
      linecolor = "grey",
      mirror = TRUE)) %>% # creates a box
    {if(.legend_) {
      plotly::colorbar(
        p = .,
        title = ifelse(.gof_name_ == "LLK",
                       "LLK",
                       "SSE"))
    } else {
      # plotly::hide_legend(p = .) %>%
      plotly::hide_colorbar(p = .)
    }} %>%
    {if(.true_points_) {
      plotly::add_trace(
        name = "True values",
        p = .,
        inherit = FALSE,
        x = .l_params_$v_params_true_values[[.param_x]],
        y = .l_params_$v_params_true_values[[.param_y]],
        type = 'scatter',
        mode = 'markers',
        marker = list(
          size = 5,
          color = "green",
          symbol = "x")
      )
    } else {
      .
    }}
}

#' Get the minimum and maximum values for the x and y axis
#'
#' @param .l_params_ List containing information about calibration parameters,
#' including parameters' names, distributions, and boundaries.
#' @param .zoom_ Logical (default TRUE) for whether to zoom in to the identified
#' sets (best fitting sets, extrema, and posterior distributions centres).
#' @param .zoom_calib_res_ Dataset/tibble containing all relevant points but
#' sampled prior values or initial guesses.
#' @param .param_x String identifying the parameter plotted in the x-axis.
#' @param .param_y String identifying the parameter plotted in the y-axis.
#' @param .x_axis_lb_ Numeric (double) value specifying the lower bound of x-axis.
#' @param .x_axis_ub_ Numeric (double) value specifying the upper bound of x-axis.
#' @param .y_axis_lb_ Numeric (double) value specifying the lower bound of y-axis.
#' @param .y_axis_ub_ Numeric (double) value specifying the upper bound of y-axis.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' }
fitness_contour_area <- function(.l_params_,
                                 .zoom_,
                                 .zoom_calib_res_,
                                 .param_x,
                                 .param_y,
                                 .x_axis_lb_,
                                 .x_axis_ub_,
                                 .y_axis_lb_,
                                 .y_axis_ub_) {
  outputs_ <- list()
  outputs_[[".x_axis_lb_"]] <- if(is.null(.x_axis_lb_)) {
    if(.zoom_) {
      tmp <- min(.zoom_calib_res_[[.param_x]]) -
        (min(.zoom_calib_res_[[.param_x]]) * 0.05)
      tmp <- ifelse(
        tmp < .l_params_$Xargs[[.param_x]]$min,
        .l_params_$Xargs[[.param_x]]$min,
        tmp)
      tmp
    } else {
      .l_params_$Xargs[[.param_x]]$min
    }
  } else {
    .x_axis_lb_
  }
  outputs_[[".x_axis_ub_"]] <- if(is.null(.x_axis_ub_)) {
    if(.zoom_) {
      tmp <- max(.zoom_calib_res_[[.param_x]]) +
        (max(.zoom_calib_res_[[.param_x]]) * 0.05)
      tmp <- ifelse(
        tmp > .l_params_$Xargs[[.param_x]]$max,
        .l_params_$Xargs[[.param_x]]$max,
        tmp)
      tmp
    } else {
      .l_params_$Xargs[[.param_x]]$max
    }
  } else {
    .x_axis_ub_
  }
  outputs_[[".y_axis_lb_"]] <- if(is.null(.y_axis_lb_)) {
    if(.zoom_) {
      tmp <- min(.zoom_calib_res_[[.param_y]]) -
        (min(.zoom_calib_res_[[.param_y]]) * 0.05)
      tmp <- ifelse(
        tmp < .l_params_$Xargs[[.param_y]]$min,
        .l_params_$Xargs[[.param_y]]$min,
        tmp)
      tmp
    } else {
      .l_params_$Xargs[[.param_y]]$min
    }
  } else {
    .y_axis_lb_
  }
  outputs_[[".y_axis_ub_"]] <- if(is.null(.y_axis_ub_)) {
    if(.zoom_) {
      tmp <- max(.zoom_calib_res_[[.param_y]]) +
        (max(.zoom_calib_res_[[.param_y]]) * 0.05)
      tmp <- ifelse(
        tmp > .l_params_$Xargs[[.param_y]]$max,
        .l_params_$Xargs[[.param_y]]$max,
        tmp)
      tmp
    } else {
      .l_params_$Xargs[[.param_y]]$max
    }
  } else {
    .y_axis_ub_
  }

  return(outputs_)
}

#' Create contour plot title based on the calibration method and its outputs
#'
#' @param .gof_function_ String specifying the name of the goodness-of-fit (GOF)
#' measure used in generating the contour.
#' @param .calib_method_ String specifying the name of the calibration method used
#' to generate the points displayed over the contour plot.
#' @param .colors_ String vector naming the colours of the each of displayed
#' points.
#' @param .points_names String vector labelling the points displayed on the
#' contour plot.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' get_gof_contour_plot_title(
#'  .gof_function_ = "LLK",
#'  .calib_method_ = "IMIS",
#'  .colors_ = c(`Sampled sets` = "black",
#'              `Starting sets` = "black",
#'              `Prior samples` = "grey",
#'              `Identified sets` = "red",
#'              `Local extremas` = "darkorange",
#'              `Global extrema` = "red",
#'              `PSA draws` = "pink",
#'              `Posterior samples` = "red",
#'              `Posterior centres` = "yellow"),
#'  .points_names = c("Posterior centres",
#'                    "Posterior samples",
#'                    "Prior samples",
#'                    "True set")
#'  )
#' }
get_gof_contour_plot_title <- function(.gof_function_,
                                       .calib_method_,
                                       .colors_,
                                       .points_names) {

  .colors_ <- .colors_[names(.colors_) %in% .points_names]
  names(.points_names) <- .points_names

  title_ <- paste0(
    glue::glue("<span style = 'color:blue;'><b>{.gof_function_}</b></span> contour plot showing <span style = 'color:black;'><b>{.calib_method_}</b> </span>"),
    paste0(
      purrr::map_chr(
        .x = .points_names[1:(length(.points_names) - 1)],
        .f = function(.point_name) {
          if(.point_name == .points_names[1]){
            if(length(.points_names) == 2){
              glue::glue("<span style = 'color:{.colors_[.point_name]};'><b>{.points_names[.point_name]}</b></span> and ")
            } else {
              glue::glue("<span style = 'color:{.colors_[.point_name]};'><b>{.points_names[.point_name]}</b></span>, ")
            }
          } else {
            if(length(.points_names) == 2)
              glue::glue("<span style = 'color:{.colors_[.point_name]};'><b>{.points_names[.point_name]}</b></span>, ")
            else {
              if(.point_name == .points_names[2])
                glue::glue("<span style = 'color:{.colors_[.point_name]};'><b>{.points_names[.point_name]}</b></span>,<br>")
              else
                glue::glue("<span style = 'color:{.colors_[.point_name]};'><b>{.points_names[.point_name]}</b></span>, ")
            }
          }
        }
      ),
      collapse = ""
    ),
    glue::glue("and <span style = 'color:{.colors_[length(.colors_)]};'><b>{.points_names[length(.points_names)]}</b></span>")
  )

  return(title_)
}
