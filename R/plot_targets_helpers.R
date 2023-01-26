#' Plot observed and simulated targets
#'
#' @param .engine_ String naming the plotting engine, currently "ggplot2".
#' @param .l_targets_ List containing calibration (observed) targets
#' information.
#' @param .simulated_targets_ List containing simulated targets.
#' @param .sim_targets_ Logical (default FALSE) for whether to generate
#' plots for the simulated targets.
#' @param .legend_pos_ String defining the location of the legend position
#' default (bottom).
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' targets_plots <- plot_targets(
#'   .l_targets_ = CR_CRS_2P2T$calibration_targets,
#'   .simulated_targets_ = CR_CRS_2P2T$simulated_targets,
#'   .sim_targets_ = TRUE)
#' }
plot_targets <- function(.engine_ = "ggplot2",
                         .l_targets_ = self$calibration_targets,
                         .simulated_targets_ = self$simulated_targets,
                         .sim_targets_ = FALSE,
                         .legend_pos_ = "bottom") {
  ## For "ggplot2" plots:----
  if(.engine_ == "ggplot2") {
    ### Observed targets' plots:----
    observed_targets_lst <- purrr::map(
      #### Loop over all targets:----
      .x = .l_targets_$v_targets_names,
      .f = function(.target_ = .x) {
        ##### Create line plots:----
        .l_targets_[[.target_]] %>%
          ggplot2::ggplot(
            data = .,
            ggplot2::aes(
              x = .data[[.l_targets_$v_targets_axis[[.target_]]$x]],
              y = .data[[.l_targets_$v_targets_axis[[.target_]]$y]])) +
          ##### Add 95% CI to the line plots:----
        ggplot2::geom_errorbar(
          ggplot2::aes(
            ymin = lb,
            ymax = ub)) +
          ##### Add 95% CI to the line plots:----
        ggplot2::geom_point() +
          ##### Apply theme and labels:----
        ggplot2::theme(
          panel.border = ggplot2::element_rect(
            fill = NA,
            color = 'black')) +
          ggplot2::labs(
            x = .l_targets_$v_targets_axis_labels[[.target_]]$x,
            y = .l_targets_$v_targets_axis_labels[[.target_]]$y)
      })
    ### Simulated targets' plots' displayed over observed ones:----
    if(.sim_targets_) {
      ###### Loop through calibration methods categories:----
      simulated_targets_lst <- purrr::map(
        .x = .simulated_targets_ %>%
          names(.) %>%
          `names<-`(., .),
        .f = function(.calib_category_) {
          ####### Loop through calibration methods:----
          purrr::map(
            .x = .simulated_targets_[[.calib_category_]] %>%
              names(.) %>%
              `names<-`(., .),
            .f = function(.calib_method_) {
              ######## Loop through calibration targets:----
              purrr::map(
                .x = .l_targets_$v_targets_names,
                .f = function(.target_) {
                  ######### Grab axis names from targets list:----
                  x_axis_name_ <- .l_targets_$v_targets_axis[[.target_]]$x
                  y_axis_name_ <- .l_targets_$v_targets_axis[[.target_]]$y
                  ######### Prepare plotting data:----
                  plotting_df <-
                    .simulated_targets_[[.calib_category_]][[.calib_method_]] %>%
                    ######## Select one target at a time:----
                  dplyr::select(dplyr::contains(.target_)) %>%
                    t() %>%
                    dplyr::as_tibble() %>%
                    ######## Name columns as numbers for grouping:----
                  `names<-`(paste0(1:ncol(.))) %>%
                    ######## Generate x axis name:----
                  dplyr::mutate(
                    {{x_axis_name_}} := 2:(nrow(.) + 1)) %>%
                    tidyr::pivot_longer(
                      cols = -{{x_axis_name_}},
                      names_to = "id",
                      values_to = "value") %>%
                    dplyr::select(id, {{x_axis_name_}}, value) %>%
                    dplyr::mutate(
                      id = as.numeric(id)) %>%
                    dplyr::left_join(
                      x = .,
                      y = .simulated_targets_[[.calib_category_]][[.calib_method_]] %>%
                        dplyr::select(
                          !dplyr::contains(
                            .l_targets_$v_targets_names)) %>%
                        dplyr::mutate(
                          id = dplyr::row_number()),
                      by = "id")

                  ######## Prepare lines' colours, sizes and opacity:-----
                  expected_labels <- c(
                    'PSA sets' = "PSA sets",
                    'Distribution samples' = "Distribution samples",
                    'Credible interval - LB' = "Credible interval - LB",
                    'Credible interval - UB' = "Credible interval - UB",
                    'Posterior mean' = "Posterior mean",
                    'Identified set' = "Identified set",
                    'Maximum-a-posteriori' = "Maximum-a-posteriori")

                  color_options <- c(
                    'PSA sets' = "skyblue",
                    'Distribution samples' = "skyblue",
                    'Credible interval - LB' = "red",
                    'Credible interval - UB' = "brown",
                    'Posterior mean' = "darkgreen",
                    'Identified set' = "green",
                    'Maximum-a-posteriori' = "green")

                  alpha_options <- c(
                    'PSA sets' = 0.4,
                    'Distribution samples' = 0.4,
                    'Credible interval - LB' = 0.8,
                    'Credible interval - UB' = 0.8,
                    'Posterior mean' = 1,
                    'Identified set' = 1,
                    'Maximum-a-posteriori' = 1)

                  size_options <- c(
                    'PSA sets' = 0.6,
                    'Distribution samples' = 0.6,
                    'Credible interval - LB' = 1,
                    'Credible interval - UB' = 1,
                    'Posterior mean' = 1,
                    'Identified set' = 1,
                    'Maximum-a-posteriori' = 1)

                  scale_names <- plotting_df %>%
                    dplyr::pull(Plot_label) %>%
                    unique()

                  scale_names <- expected_labels[expected_labels %in%
                                                   scale_names]
                  scale_colors <- color_options[scale_names]
                  scale_alphas <- alpha_options[scale_names]
                  scale_sizes <- size_options[scale_names]

                  ######## Reorder rows in dataset for plotting:-----
                  plotting_df <- plotting_df %>%
                    dplyr::mutate(
                      ranking = dplyr::case_when(
                        Plot_label %in% c(
                          "Identified set",
                          "Maximum-a-posteriori") ~ 4,
                        Plot_label %in% c(
                          'Posterior mean') ~ 3,
                        Plot_label %in% c(
                          "Credible interval - LB",
                          "Credible interval - UB") ~ 2,
                        Plot_label %in% c(
                          "PSA sets",
                          "Distribution samples") ~ 1)) %>%
                    dplyr::arrange(ranking) %>%
                    dplyr::select(-ranking)

                  ######## More transparent if many PSA values:-----
                  alpha_options["PSA sets"] <- ifelse(
                    nrow(plotting_df %>%
                           dplyr::filter(Plot_label == "PSA sets")) > 1e3,
                    0.2,
                    alpha_options["PSA sets"])
                  alpha_options["Distribution samples"] <- ifelse(
                    nrow(plotting_df %>%
                           dplyr::filter(Plot_label == "PSA sets")) > 1e3,
                    0.2,
                    alpha_options["Distribution samples"])

                  ##### Re-arrange and re-level the Plot_label variable:----
                  plotting_df <- purrr::map_dfr(
                    .x = scale_names,
                    .f = function(.label_) {
                      plotting_df %>%
                        dplyr::filter(Plot_label == .label_)
                    }) %>%
                    dplyr::mutate(
                      Plot_label = factor(
                        x = Plot_label,
                        levels = scale_names))

                  ##### Generate simulated targets' plots:----
                  plot_lists <- {observed_targets_lst[[.target_]] +
                      ######## Add lines to target plot:-----
                    ggplot2::geom_line(
                      inherit.aes = FALSE,
                      data = plotting_df,
                      ggplot2::aes(
                        x = .data[[x_axis_name_]],
                        y = .data[[y_axis_name_]],
                        group = id,
                        color = Plot_label,
                        alpha = Plot_label,
                        size = Plot_label)) +
                      ggplot2::scale_color_manual(
                        limits = scale_names,
                        values = scale_colors) +
                      ggplot2::scale_alpha_manual(
                        limits = scale_names,
                        values = scale_alphas) +
                      ggplot2::scale_size_manual(
                        limits = scale_names,
                        values = scale_sizes) +
                      ggplot2::guides(
                        # Increase the size of the colour area in the legend:
                        color = ggplot2::guide_legend(
                          ncol = 3,
                          override.aes = list(
                            size = 2,
                            alpha = 2,
                            stroke = 2))) +
                      ggplot2::theme(
                        # Start title from near the margin
                        plot.title.position = "plot",
                        legend.position = .legend_pos_,
                        legend.title = ggplot2::element_blank(),
                        # Control legend text alignment:
                        legend.text.align = 0, # 0 left (default), 1 right
                        # Remove background and box around the legend:
                        legend.background = ggplot2::element_rect(
                          fill = NA,
                          color = NA),
                        # spacing between legend items:
                        legend.spacing = ggplot2::unit(0, "cm"),
                        # bring legends closer:
                        legend.spacing.y = ggplot2::unit(-0.195, "cm"),
                        # remove legend padding:
                        # legend.box.margin = ggplot2::margin(c(0, 0, 0, 0)),
                        legend.margin = ggplot2::margin(c(0, 0, 0, 0)),
                        # Add a box around the keys:
                        legend.key = ggplot2::element_rect(
                          fill = "white",
                          colour = "grey"),
                        legend.key.size = ggplot2::unit(0.35, "cm"),
                        # Add a border and space around the plot:
                        panel.border = ggplot2::element_rect(
                          colour = 'black',
                          fill = NA)
                      )}
                })
            })
        })

      return(c(
        list(
          "blank" = observed_targets_lst),
        simulated_targets_lst))
    }
    return(
      list(
        "blank" = observed_targets_lst))
  }
}
