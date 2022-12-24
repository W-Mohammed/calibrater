plot_algorithm_points <- function(
    .calibration_results = CR_CRS_2P1T[["calibration_results"]],
    .gof_plots = self$plots$GOF_plots$blank) {

  self$plots$GOF_plots$directed <- .calibration_results$directed %>%
    purrr::map(
      .x = .,
      .f = function(.calib_res_algorithm) {
        ## Transpose the list to group outputs together:----
        transposed_calib_res <- .calib_res_algorithm %>%
          purrr::transpose()
        ## Extract "Starting values":----
        calib_res <- transposed_calib_res[["Guess"]] %>%
          purrr::map_dfr(
            .x = .,
            .f = function(.x) {
              .x}) %>%
          dplyr::mutate(Points = "Starting values") %>%
          ## Join "Identified values":----
        dplyr::bind_rows(
          ## Extract identified values:----
          transposed_calib_res[["Estimate"]] %>%
            purrr::map_dfr(
              .x = .,
              .f = function(.x) {
                .x}) %>%
            dplyr::mutate(Points = "Identified values"))
        ## Add points to the plots:----
        purrr::map(
          .x = self$calibration_parameters$v_params_names,
          .f = function(.param_x) {
            ##### Prepare parameter names:----
            other_params_names <- self$calibration_parameters$
              v_params_names[-which(self$calibration_parameters$
                                      v_params_names == .param_x)]
            ##### GOF Plots list:----
            plots_list_ <- purrr::map(
              .x = other_params_names,
              .f = function(.param_y) {
                .gof_plots[[.param_x]][[.param_y]] %>%
                  plotly::add_trace(
                    p = .,
                    inherit = FALSE,
                    x = calib_res[[.param_x]],
                    y = calib_res[[.param_y]],
                    type = 'scatter',
                    mode = 'markers',
                    marker = list(size = 5),
                    symbol = ~ calib_res[["Points"]],
                    symbols = c("triangle-up", "circle-open"))
              })
            })
      })
}
