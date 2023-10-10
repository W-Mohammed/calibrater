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
                         .legend_pos_ = "none") {
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
                  ########## Ensure proper levels arrangement:----
                  expected_labels <- c(
                    'PSA sets' = "PSA sets",
                    'Distribution samples' = "Distribution samples",
                    'Credible interval - LB' = "Credible interval - LB",
                    'Credible interval - UB' = "Credible interval - UB",
                    'Posterior mean' = "Posterior mean",
                    'Identified set' = "Identified set",
                    'Maximum-a-posteriori' = "Maximum-a-posteriori")

                  plotting_df <-
                    .simulated_targets_[[.calib_category_]][[.calib_method_]]

                  scale_names <- plotting_df %>%
                    dplyr::pull(Plot_label) %>%
                    unique()

                  scale_names <- expected_labels[expected_labels %in%
                                                   scale_names]
                  ########## Re-sort rows to get display lines correctly:----
                  # the id variable introduced later and controlling the lines
                  # produced by geom_line() needs to be in the correct order
                  plotting_df <- purrr::map_dfr(
                    .x = scale_names,
                    .f = function(.label_) {
                      plotting_df %>%
                        dplyr::filter(Plot_label == .label_)
                    })

                  ########## Reshape tibble for plotting:----
                  plotting_df <- plotting_df %>%
                    ######## Select one target at a time:----
                  dplyr::select(
                    dplyr::contains(.target_)) %>%
                    t() %>%
                    as.data.frame() %>%
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
                      y = plotting_df %>%
                        dplyr::select(
                          !dplyr::contains(
                            .l_targets_$v_targets_names)) %>%
                        dplyr::mutate(
                          id = dplyr::row_number()),
                      by = "id")

                  ######## Prepare lines' colours, sizes and opacity:-----
                  color_options <- c(
                    'PSA sets' = "#0072B2",
                    'Distribution samples' = "#0072B2",
                    'Credible interval - LB' = "#FFC300",
                    'Credible interval - UB' = "#FFC300",
                    'Posterior mean' = "#35D220",
                    'Identified set' = "#FF0000",
                    'Maximum-a-posteriori' = "#FF0000")

                  alpha_options <- c(
                    'PSA sets' = 0.3,
                    'Distribution samples' = 0.3,
                    'Credible interval - LB' = 1,
                    'Credible interval - UB' = 1,
                    'Posterior mean' = 1,
                    'Identified set' = 1,
                    'Maximum-a-posteriori' = 1)

                  # size_options <- c(
                  #   'PSA sets' = 0.6,
                  #   'Distribution samples' = 0.6,
                  #   'Credible interval - LB' = 1,
                  #   'Credible interval - UB' = 1,
                  #   'Posterior mean' = 1,
                  #   'Identified set' = 1,
                  #   'Maximum-a-posteriori' = 1)

                  scale_colors <- color_options[scale_names]
                  scale_alphas <- alpha_options[scale_names]
                  # scale_sizes <- size_options[scale_names]

                  ######## More transparent if many PSA values:-----
                  alpha_options["PSA sets"] <- ifelse(
                    nrow(
                      plotting_df %>%
                        dplyr::filter(
                          Plot_label == "PSA sets")) > 1e3,
                    0.2,
                    alpha_options["PSA sets"])
                  alpha_options["Distribution samples"] <- ifelse(
                    nrow(
                      plotting_df %>%
                        dplyr::filter(
                          Plot_label == "Distribution samples")) > 1e3,
                    0.2,
                    alpha_options["Distribution samples"])

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
                        # size = Plot_label,
                        alpha = Plot_label)) +
                      ggplot2::scale_color_manual(
                        limits = scale_names,
                        values = scale_colors) +
                      ggplot2::scale_alpha_manual(
                        limits = scale_names,
                        values = scale_alphas) +
                      # ggplot2::scale_size_manual(
                      #   limits = scale_names,
                      #   values = scale_sizes) +
                      ggplot2::guides(
                        # Increase the size of the colour area in the legend:
                        color = ggplot2::guide_legend(
                          ncol = 3,
                          override.aes = list(
                            size = 2,
                            alpha = 2,
                            stroke = 2))) +
                      ggplot2::labs(
                        title = calibR:::get_target_plot_title(
                          .scale_names_ = scale_names,
                          .scale_colors_ = scale_colors,
                          .target_ = .l_targets_$v_targets_labels[[.target_]],
                          .method_ = .calib_method_)) +
                      ggplot2::theme(
                        # Start title from near the margin
                        plot.title.position = "plot",
                        plot.title = ggtext::element_textbox_simple(
                          lineheight = 1,
                          padding = ggplot2::margin(0, 0, 5, 0)
                        ),
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

#' Create plot title based on the calibration method and its outputs
#'
#' @param .scale_names_ String vector specifying the levels or names of line
#' groups in the plot.
#' @param .scale_colors_ String vector identifying the colours hex codes for
#' each of the groups in the plot.
#' @param .target_ String naming the calibration target.
#' @param .method_ String naming the calibration method that generate the
#' results in the plot.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' }
get_target_plot_title <- function(.scale_names_ = scale_names,
                                  .scale_colors_ = scale_colors,
                                  .target_ = .target_,
                                  .method_ = .calib_method_) {

  ## Some cleaning:----
  .scale_names_ <- .scale_names_[!names(.scale_names_) == 'Credible interval - LB']
  .scale_colors_ <- .scale_colors_[!names(.scale_colors_) == 'Credible interval - LB']
  if(!is.na(.scale_names_['Credible interval - UB']))
    .scale_names_['Credible interval - UB'] <- "95% credible interval"

  if(!is.na(.scale_names_['Distribution samples']))
    .scale_names_['Distribution samples'] <- "samples"

  if(!is.na(.scale_names_['PSA sets']))
    .scale_names_['PSA sets'] <- "PSA samples"

  if(!is.na(.scale_names_['Identified set']))
    .scale_names_['Identified set'] <- "identified set(s)"

  if(!is.na(.scale_names_['Posterior mean']))
    .scale_names_['Posterior mean'] <- "mean"

  if(!is.na(.scale_names_['Maximum-a-posteriori']))
    .scale_names_['Maximum-a-posteriori'] <- "mode"

  .method_ <- calibR:::get_clean_method_name(.method_ = .method_)

  .title_ <- paste0(
    glue::glue(
      "<span style = 'color:black;'>**Observed**</span> *{.target_}* with
      <span style = 'color:black;'>**95% confidence interval**</span> & {.method_} *{.target_}*
      simulated using "),
    if(.method_ %in% c("MCMC", "SIR", "IMIS")) {
      "posterior "
    } else {
      ""
    },
    paste0(
      purrr::map_chr(
        .x = names(
          .scale_names_[1:(length(.scale_names_) - 1)]),
        .f = function(.scale_name_) {
          if(.scale_name_ == names(.scale_names_[1])){
            if(length(.scale_names_) == 2){
              glue::glue(
                "<span style = 'color:{.scale_colors_[.scale_name_]};
                '>**{.scale_names_[.scale_name_]}**</span> & ")
            } else {
              glue::glue(
                "<span style = 'color:{.scale_colors_[.scale_name_]};
                '>**{.scale_names_[.scale_name_]}**</span>, ")
            }
          } else {
            glue::glue(
              "<span style = 'color:{.scale_colors_[.scale_name_]};
              '>**{.scale_names_[.scale_name_]}**</span>, ")
          }
        }
      ),
      collapse = ""),
    if(length(.scale_names_) == 2){
      glue::glue(
        "<span style = 'color:{.scale_colors_[length(.scale_names_)]};
        '>**{.scale_names_[length(.scale_names_)]}**.</span>"
      )
    } else {
      glue::glue(
        "& <span style = 'color:{.scale_colors_[length(.scale_names_)]};
        '>**{.scale_names_[length(.scale_names_)]}**.</span>"
      )
    }
  )

  .title_ <- glue::glue(
    "<span style = 'font-size:13pt; color:#383838;'>{.title_}</span>"
  )

  return(.title_)
}

#' Get cleaner calibration methods names
#'
#' @param .method_ String naming the calibration method name to be cleaned.
#'
#' @return
#'
#' @examples
#' \dontrun{
#' }
get_clean_method_name = function(.method_) {
  df_methods_dictionary <- data.frame(
    value = c("LLK_RGS",
              "log_likelihood_RGS",
              "SSE_RGS",
              "wSumSquareError_RGS",
              "LLK_FGS",
              "log_likelihood_FGS",
              "SSE_FGS",
              "wSumSquareError_FGS",
              "LLK_LHS",
              "log_likelihood_LHS",
              "SSE_LHS",
              "wSumSquareError_LHS",
              "NM_LLK_0",
              "NM_LLK_RGS",
              "NM_SSE_0",
              "NM_SSE_RGS",
              "NM_LLK_1",
              "NM_SSE_1",
              "BFGS_LLK_0",
              "BFGS_LLK_RGS",
              "BFGS_SSE_0",
              "BFGS_SSE_RGS",
              "BFGS_LLK_1",
              "BFGS_SSE_1",
              "SANN_LLK_",
              "SANN_LLK_RGS",
              "SANN_SSE_",
              "SANN_SSE_RGS"),
    label = c("RGS-LLK",
              "RGS-LLK",
              "RGS-SSE",
              "RGS-SSE",
              "FGS-LLK",
              "FGS-LLK",
              "FGS-SSE",
              "FGS-SSE",
              "LHS-LLK",
              "LHS-LLK",
              "LHS-SSE",
              "LHS-SSE",
              "NM-LLK",
              "NM-LLK",
              "NM-SSE",
              "NM-SSE",
              "NM-LLK-unconverged",
              "NM-SSE-unconverged",
              "GRG-LLK",
              "GRG-LLK",
              "GRG-SSE",
              "GRG-SSE",
              "GRG-LLK-unconverged",
              "GRG-SSE-unconverged",
              "SANN-LLK",
              "SANN-LLK",
              "SANN-SSE",
              "SANN-SSE")
  )

  method_label <- df_methods_dictionary[
    df_methods_dictionary$value == .method_,
    "label"
  ]

  if(length(method_label) == 0)
    method_label <- .method_

  return(method_label)
}
