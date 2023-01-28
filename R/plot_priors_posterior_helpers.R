#' Plot prior-posterior distributions
#'
#' @param .engine_ String specifying the plotting engine. Currently supports
#' "ggplot2". Also, some of the code is written for the "triliscope" engine.
#' @param .l_PSA_samples_ List containing PSA samples - from which the function
#' uses Bayesian calibration results.
#' @param .l_params_ List containing information about calibration parameters,
#' including parameters' names, distributions, and boundaries.
#' @param .t_prior_samples_ Dataset or tibble containing prior samples.
#' @param .l_calibration_results_ List containing the results from the
#' calibration methods.
#' @param .transform_ Logical for whether the model is set to handle parameters
#' on a transformed scale.
#' @param .bins_ Numeric specifying the number of bins in the histograms.
#' @param .legend_pos_ String defining the location of the legend position
#' default (bottom).
#' @param .log_scaled_ Logical for whether to present the x-axis using the
#' log scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' pri_post_plots <- plot_pri_post_distributions(
#'   .engine_ = "ggplot2",
#'   .l_PSA_samples_ = CR_CRS_2P2T$PSA_samples,
#'   .l_params_ = CR_CRS_2P2T$calibration_parameters,
#'   .l_calibration_results_ = CR_CRS_2P2T$calibration_results,
#'   .t_prior_samples_ = CR_CRS_2P2T$prior_samples[["LHS"]],
#'   .transform_ = CR_CRS_2P2T$transform_parameters,
#'   .bins_ = 20,
#'   .legend_pos_ = "bottom",
#'   .log_scaled_ = FALSE)
#' }
plot_pri_post_distributions = function(.engine_ = "ggplot2",
                                       .l_PSA_samples_ = self$PSA_samples,
                                       .l_params_ = self$calibration_parameters,
                                       .l_calibration_results_ = self$calibration_results,
                                       .t_prior_samples_ = self$prior_samples[["LHS"]],
                                       .transform_ = self$transform_parameters,
                                       .bins_ = 100,
                                       .legend_pos_ = "bottom",
                                       .log_scaled_ = FALSE) {
  #### Grab Bayesian data PSA samples:----
  data_ <- .l_PSA_samples_$bayesian %>%
    purrr::transpose() %>%
    .[['PSA_calib_draws']]

  #### Join Prior data:----
  data_ <- if(.transform_) {
    purrr::map(
      .x = data_,
      .f = function(.data_) {
        .data_ %>%
          dplyr::bind_rows(
            .t_prior_samples_ %>%
              dplyr::mutate(Label = 'Prior') %>%
              calibR::backTransform(
                .t_data_ = .,
                .l_params_ = .l_params_)
          )
      })
  } else {
    purrr::map(
      .x = names(data_) %>%
        `names<-`(names(data_)),
      .f = function(.data_) {
        data_[[.data_]] %>%
          dplyr::bind_rows(
            .,
            .t_prior_samples_ %>%
              dplyr::mutate(
                Label = "Prior",
                Plot_label = "Prior samples")
          )
      })
  }

  #### Prepare data for triliscope plot:----
  if(.engine_ == "triliscope") {
    data2_ <- purrr::map(
      ##### Loop through each Bayesian method:----
      .x = names(data_) %>%
        `names<-`(names(data_)),
      .f = function(.data_) {
        data_[[.data_]] %>%
          tidyr::pivot_longer(
            cols = .l_params_$v_params_names,
            names_to = "Parameter",
            values_to = "Distribution draws")
      })
  ##### Add True set (if known):----
    if(!is.null(.l_params_$v_params_true_values)) {
      data2_ <- purrr::map(
        ###### Loop through each Bayesian method:----
        .x = names(data_) %>%
          `names<-`(names(data_)),
        .f = function(.data_) {
          data2_[[.data_]] %>%
            dplyr::bind_rows(
              .l_params_$
                v_params_true_values %>%
                dplyr::as_tibble(rownames = "Parameter") %>%
                dplyr::rename(`Distribution draws` = value) %>%
                dplyr::mutate(
                  Label = "True",
                  Plot_label = "Prior samples"))
        })
    }
  }

  #### Create plots list:----
  prior_posterior_distributions_list <- purrr::map(
    ##### Loop through each Bayesian method:----
    .x = names(data_) %>%
      `names<-`(names(data_)),
    .f = function(.data_) {
      ###### Loop through calibration parameters:----
      plot_ <- purrr::map(
        .x = .l_params_$v_params_names,
        .f = function(.parameter_) {
          plot_ <-
            ggplot2::ggplot() +
            ggplot2::geom_histogram(
              data = data_[[.data_]] %>%
                dplyr::filter(Label %in% "Prior") %>%
                dplyr::rename(Method = Label),
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ggplot2::after_stat(count) / max(ggplot2::after_stat(count)),
                fill = Method,
                colour = Method,
                alpha = Method),
              bins = .bins_,
              alpha = 0.2) +
            ggplot2::geom_density(
              data = data_[[.data_]] %>%
                dplyr::filter(Label %in% "Prior") %>%
                dplyr::rename(Method = Label),
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ggplot2::after_stat(scaled),
                fill = Method,
                colour = Method,
                alpha = Method)) +
            ggplot2::geom_histogram(
              data = data_[[.data_]] %>%
                dplyr::filter(!Label %in% "Prior") %>%
                dplyr::mutate(Label = "Posterior") %>%
                dplyr::rename(Method = Label),
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ggplot2::after_stat(count) / max(ggplot2::after_stat(count)),
                fill = Method,
                colour = Method,
                alpha = Method),
              bins = .bins_,
              alpha = 0.5) +
            ggplot2::geom_density(
              data = data_[[.data_]] %>%
                dplyr::filter(!Label %in% "Prior") %>%
                dplyr::mutate(Label = "Posterior") %>%
                dplyr::rename(Method = Label),
              ggplot2::aes(
                x = .data[[.parameter_]],
                y = ggplot2::after_stat(scaled),
                fill = Method,
                colour = Method,
                alpha = Method)) +
            ggplot2::theme(
              panel.border = ggplot2::element_rect(
                colour = 'black',
                fill = NA),
              plot.title.position = "plot",
              plot.title = ggtext::element_markdown(
                # family = "Source Sans Pro"
                # size = 11,
                # lineheight = 1.2
                ),
              plot.subtitle = ggplot2::element_text(
                face = "italic"),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              legend.position = .legend_pos_,
              legend.title = ggplot2::element_blank(),
              # Control legend text alignment:0 left (default), 1 right
              legend.text.align = 0,
              # Remove background and box around the legend:
              legend.background = ggplot2::element_rect(
                fill = NA,
                color = NA),
              legend.margin = ggplot2::margin(c(-10, 0, 0, 0)),
              axis.title.y = ggplot2::element_blank(),
              axis.title.x = ggplot2::element_blank())

          ####### Colour, scale and fill values:----
          color_scale <- c(
            "Prior" = "#0000FF",
            "Posterior" = "#FF0000")

          fill_scale <- c(
            "Prior" = "lightblue",
            "Posterior" = "pink")

          alpha_scale <- c(
            "Prior" = 0.7,
            "Posterior" = 0.7)

          plot_ <- plot_ +
            ggplot2::scale_fill_manual(
              name = "Distribution",
              values = fill_scale) +
            ggplot2::scale_color_manual(
              name = "Distribution",
              values = color_scale) +
            ggplot2::scale_alpha_manual(
              name = "Distribution",
              values = alpha_scale)

          ####### Get effective sample size (ESS):----
          ESS_ <- calibR::effective_sample_size(
            bayes_calib_output_list = .l_calibration_results_$bayesian[[.data_]])
          ESS_ <- round(ESS_)

          ####### Log scale plots:----
          if(.log_scaled_) {
            plot_ <- plot_ +
              ggplot2::scale_x_log10() +
              ggplot2::labs(
                caption = "x-axis on logarithmic scale")
          }

          ####### Add True set, if known:----
          if(!is.null(.l_params_$v_params_true_values[[.parameter_]])) {
            plot_ <- plot_ +
              ggplot2::geom_vline(
                xintercept = .l_params_$v_params_true_values[[.parameter_]],
                colour = "green",
                show.legend = TRUE) +
              ggplot2::labs(
                title = glue::glue(# font-family:Source Sans Pro;
                  "<span style = 'font-size:10pt;'>_{.l_params_$
                  v_params_labels[[.parameter_]]}_<span style =
                  'color:#00FF00;'> **true**</span> value, <span style =
                  'color:{color_scale[\"Prior\"]};'>**prior**</span>
                  distribution <br>and the **{.data_}** <span style = 'color:
                  {color_scale[\"Posterior\"]};'>**posterior**</span> distribution.
                  *ESS = **{ESS_}***.</span>"
                )
              )
          } else {
            plot_ <- plot_ +
              ggplot2::labs(
                title = glue::glue(# font-family:Source Sans Pro;
                  "<span style = 'font-size:10pt;'>_{.l_params_$
                  v_params_labels[[.parameter_]]}_ <span style =
                  'color:{color_scale[\"Prior\"]};'>**prior**</span>
                  distribution <br>and the **{.data_}** <span style = 'color:
                  {color_scale[\"Posterior\"]};'>**posterior**</span> distribution.
                  *ESS = **{ESS_}***.</span>"
                )
              )
          }
        })
    })

  return(prior_posterior_distributions_list)
}
