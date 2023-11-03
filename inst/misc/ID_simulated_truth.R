set.seed(1)
# ID_data_3t0 <- ID_data_3t
# # Make sure we are sampling from the log-normal distribution:
# ID_data_3t0$l_params$args <- ID_data_3t0$l_params$true_args
# ID_data_3t0$l_params$v_params_dists <- ID_data_3t0$l_params$v_true_params_dists
# ID_data_3t0$l_params$args <- ID_data_3t0$l_params$args %>%
#   purrr::map(
#     .f = function(param_) {
#       if(any(c("sdlog", "meanlog") %in% names(param_))) {
#         param_[["sdlog"]] <- param_[["sdlog"]]/100
#         param_[["meanlog"]] <- param_[["meanlog"]]
#         param_
#       } else {
#         param_
#       }
#     }
#   )
# PSA_samples <- sample_prior_RGS_(
#   .l_params = ID_data_3t0$l_params,
#   .n_samples = 1e4
# )
# .args_ = c("calibrate_" = FALSE, "transform_" = FALSE)
ID_data_3t0 <- ID_data_3t
ID_data_3t0$l_params$args <- ID_data_3t$l_params$args %>%
  purrr::map(
    .f = function(param_) {
      param_[["sd"]] <- param_[["sd"]]/100
      param_[["mean"]] <- param_[["mean"]]
      param_
    }
  )
PSA_samples <- sample_prior_RGS_(
  .l_params = ID_data_3t0$l_params,
  .n_samples = 1e4
)
.func_ <- calibR::infectious_disease_stm
l_PSA_results <- purrr::pmap(
  .l = PSA_samples,
  .f = function(...) {
    params_ <- list(...)
    .args_ <- list(
      "calibrate_" = FALSE,
      "transform_" = TRUE,
      ".v_params_" = params_)
    purrr::exec(
      .fn = .func_,
      !!!.args_
    ) %>%
      unlist()
  },
  .progress = list(
    type = "iterator",
    format = "Running PSA {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE
  )
)
l_PSA_outputs <- dplyr::bind_rows(l_PSA_results) %>%
  {
    df_ = .
    purrr::map(
      .x = ID_data_3t0$l_intervs$v_interv_outcomes %>%
        `names<-`(c("c", "e")),
      .f = function(outcome) {
        remove_pattern = paste(
          outcome,
          paste0(
            ".",
            outcome
          ),
          sep = "|"
        )
        dplyr::select(
          .data = .,
          dplyr::all_of(
            grep(
              pattern = outcome,
              x = colnames(.),
              value = TRUE
            )
          )
        ) %>%
          dplyr::rename_with(
            .fn = function(x_) {
              gsub(
                pattern = remove_pattern,
                replacement = "",
                x = x_
              )
            }
          )
      }
    )
  }
l_PSA_outputs <- c(
  l_PSA_outputs,
  list(
    "p" = PSA_samples,
    "treats" = ID_data_3t0$l_intervs$v_interv_names
  )
)
saveRDS(l_PSA_outputs, file = "../4. Post CR/VOI/ID_data/ID_true_PSA.rds")
