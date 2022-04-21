#' Merge calibration outputs and sample PSA values
#'
#' @param .l_calib_res_lists A list of calibration results lists. The
#' function will use the outputs of calibration to sample PSA values or
#' tabulate them appropriately.
#' @param .search_method Class of calibration method. Currently supports
#' "Directed", "Random" and "Bayesian".
#' @param .PSA_samples The number of PSA samples to sample or tabulate.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .transform_ Logical, if TRUE the back transformation functions
#' in the .l_params list will be used to transform the parameters to their
#' original scale.
#'
#' @return
#' @export
#'
#' @examples
#' l_optim_lists <- list(GA_optimise_lLLK, GA_optimise_wSSE,
#'                       GB_optimise_lLLK, GB_optimise_wSSE,
#'                       NM_optimise_lLLK, NM_optimise_wSSE,
#'                       SA_optimise_lLLK, SA_optimise_wSSE)
#'
#' calibrated_values <- PSA_calib_values(.l_calib_res_lists = l_optim_lists)
#'
PSA_calib_values <- function(.l_calib_res_lists = l_optim_lists,
                             .search_method = 'Directed',
                             .PSA_samples = 10000, .l_params = NULL,
                             .transform_ = FALSE) {
  # Stop the .search_method is not supported by this function:
  stopifnot(".search_method is supported by the function" =
              any(.search_method %in% c('Directed', 'Random', 'Bayesian')))
  # Apply appropriate extraction method:
  if(.search_method == 'Directed') {
    # Collect lower and upper bounds for tmvtnorm::rtmvnorm():
    lb <- map_dbl(.x = .l_params$Xargs, .f = function(.x) .x$min)
    ub <- map_dbl(.x = .l_params$Xargs, .f = function(.x) .x$max)
    results <- map(
      .x = .l_calib_res_lists,
      .f = function(.list_ = .x) {
        Sigma_ <- round(.list_[[1]][["Sigma"]] %>%
                           `dimnames<-`(NULL), 8)
        # Can not sample PSA values if any cov-mat (Sigma) values < 0 | NA:
        if(!is.null(Sigma_) &
           !any(is.na(Sigma_)) &
           if(!is.null(Sigma_)) {
             if(!any(is.na(Sigma_))) {
               if(length(Sigma_ > 0)) {
                 if(matrixcalc::is.symmetric.matrix(Sigma_)){
                   matrixcalc::is.positive.definite(Sigma_)
                 } else FALSE
               } else FALSE
             } else FALSE
           } else FALSE) {
          # Sample from a multivariate normal distribution:
          PSA_calib_draws <- tmvtnorm::rtmvnorm(
            n = .PSA_samples,
            mean = .list_[[1]][["Estimate"]],
            sigma = Sigma_,
            lower = lb,
            upper = ub,
            algorithm = "gibbs",
            burn.in.samples = 10000) %>%
            as_tibble(~ vctrs::vec_as_names(...,
                                            repair = "unique",
                                            quiet = TRUE)) %>%
            `colnames<-`(.list_[[1]][["Params"]]) %>%
            dplyr::bind_rows(
              .list_[[1]][["Estimate"]] %>%
                t() %>%
                as_tibble(~ vctrs::vec_as_names(...,
                                                repair = "unique",
                                                quiet = TRUE)) %>%
                `colnames<-`(.list_[[1]][["Params"]])) %>%
            mutate(Label =
                     .list_[[1]][["Calibration method"]],
                   Overall_fit =
                     round(.list_[[1]][["GOF value"]], 2)) %>%
            select(Label, Overall_fit, everything())
          # Back transform sampled parameters if any:
          if(.transform_) {
            PSA_calib_draws <- PSA_calib_draws %>%
              backTransform(.t_data_ = ., .l_params_ = .l_params)
          }
          # Prepare outputs list:
          list(
            'Calib_results' = .list_[[1]],
            'PSA_calib_draws' = PSA_calib_draws
          )
        } else {
          # Retain the point estimates if sigma is NA or not +ve definite:
          PSA_calib_draws <- .list_[[1]][["Estimate"]] %>%
            t() %>%
            as_tibble(~ vctrs::vec_as_names(...,
                                            repair = "unique",
                                            quiet = TRUE)) %>%
            `colnames<-`(.list_[[1]][["Params"]]) %>%
            mutate(Label =
                     .list_[[1]][["Calibration method"]],
                   Overall_fit =
                     round(.list_[[1]][["GOF value"]], 2)) %>%
            select(Label, Overall_fit, everything())
          # Back transform sampled parameters if any:
          if(.transform_) {
            PSA_calib_draws <- PSA_calib_draws %>%
              backTransform(.t_data_ = ., .l_params_ = .l_params)
          }
          # Prepare outputs list:
          list(
            'Calib_results' = .list_[[1]],
            'PSA_calib_draws' = PSA_calib_draws
          )
        }
      })
  } else if(.search_method == 'Random') {
    if(nrow(.l_calib_res_lists[[1]]) < .PSA_samples)
      stop(paste("Please sample at least", .PSA_samples,
                 "samples, and assess their goodness-of-fit."))
    results <- map(
      .x = .l_calib_res_lists,
      .f = function(.data_ = .x) {
        PSA_calib_draws = .data_ %>%
          slice(1:.PSA_samples) %>%
          select(Label, Overall_fit, everything())
        # Back transform sampled parameters if any:
        if(.transform_) {
          PSA_calib_draws <- PSA_calib_draws %>%
            backTransform(.t_data_ = ., .l_params_ = .l_params)
        }
        # Prepare outputs list:
        list(
          'Calib_results' = .data_$Overall_fit,
          'PSA_calib_draws' = PSA_calib_draws
        )
      }
    )
  } else {
    results <- map(
      .x = .l_calib_res_lists,
      .f = function(.list_ = .x) {
        PSA_calib_draws <- .list_[["Results"]] %>%
          mutate(Label = .list_[["Method"]]) %>%
          select(-Posterior_prob) %>%
          select(Label, Overall_fit, everything())
        # Back transform sampled parameters if any:
        if(.transform_) {
          PSA_calib_draws <- PSA_calib_draws %>%
            backTransform(.t_data_ = ., .l_params_ = .l_params)
        }
        # Prepare outputs list:
        list(
          'Calib_results' = .list_[["Method"]],
          'Posterior_prob' = .list_[["Results"]]$Posterior_prob,
          'PSA_calib_draws' = PSA_calib_draws
        )
      }
    )
  }

  return(results)
}

#' Run Probabilistic Sensitivity Analysis (PSA) for a Decision Analytic
#' Models (DAM) using both calibrated and un-calibrated parameters.
#'
#' @param .func_ The Decision Analytic Models (DAM) that was calibrated and
#' for which Probabilistic Sensitivity Analysis (PSA) will be conducted.
#' @param .args_ Extra arguments passed to .func_, the DAM.
#' @param .PSA_calib_values_ PSA values sampled for the calibration
#' parameter(s).
#' @param .PSA_unCalib_values_ PSA values sampled for un-calibrated
#' (known) parameters(s).
#'
#' @return
#' @export
#'
#' @examples
run_PSA <- function(.func_, .args_, .PSA_calib_values_,
                    .PSA_unCalib_values_) {
  # Avoid issues when .func_ takes only the .PSA_calib_values_:
  if(!is.null(.PSA_unCalib_values_))
    l_PSA_Calib_params_ <- .PSA_calib_values_ %>%
      transpose() %>%
      .[['PSA_calib_draws']] %>%
      cbind(.PSA_nonCalib_values_)
  else
    l_PSA_Calib_params_ <- .PSA_calib_values_ %>%
      transpose() %>%
      .[['PSA_calib_draws']]

  # Run PSA using combined draws:
  l_PSA_results <- map(
    .x = l_PSA_Calib_params_,
    .f = function(method_draws) {
      PSA_runs <- pmap(
        .l = method_draws,
        .f = function(...) {
          params_ <- list(...)
          results <- exec(.fn = .func_,
                          params_[-c(1, 2)],
                          !!!.args_) %>%
            unlist()
        }
      )
      bind_rows(PSA_runs) %>%
        mutate(
          'Label' = method_draws %>%
            select(Label) %>%
            .[[1]],
          'Overall_fit' = method_draws %>%
            select(Overall_fit) %>%
            .[[1]]
        )
    }
  )

  return(l_PSA_results)
}
