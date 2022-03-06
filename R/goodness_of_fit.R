#' A function to estimate log likelihood (LLK) goodness-of-fit
#'
#' @param .func A function defining the model to be calibrated
#' @param .args A list of arguments to be passed to .func
#' @param .samples A table with sampled parameter values
#' @param .l_targets A list containing a vector of targets' names,
#' a vector of targets' distributions, and a table for each target that
#' contains the values (column name 'value') and standard errors (column
#' name 'sd') of the corresponding target.
#'
#' @return A table with proposed parameter sets and their corresponding log
#' likelihood values.
#' @export
#'
#' @examples
#' library(calibrater)
#' data("CRS_targets")
#' Surv <- CRS_targets$Surv
#' v_targets_names <- c("Surv")
#' v_targets_dists <- c('norm')
#' v_targets_weights <- c(1)
#' l_targets <- list('v_targets_names' = v_targets_names, 'Surv' = Surv,
#'                   'v_targets_dists' = v_targets_dists,
#'                   'v_targets_weights' = v_targets_weights)
#'
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#'
#' samples <- sample_prior_LHS(
#'           .l_params = list(v_params_names = v_params_names,
#'                            v_params_dists = v_params_dists,
#'                            args = args),
#'                        .n_samples = 10)
#'
#' GOF_llik <- log_likelihood(.func = CRS_markov, .samples = samples,
#'                           .l_targets = l_targets)
#'
log_likelihood <- function(.func, .args = list(NULL), .samples,
                           .l_targets) {
  # Run the model using each set of sampled parameters:
  model_results <- pmap(
    .l = .samples,
    .f = function(...) {
      dots <- list(...)
      exec(.func, dots, !!!.args)
    })
  # Define inputs list for the log likelihood function:
  l_llk <- list(.l_targets[['v_targets_names']],
                paste0('d', .l_targets[['v_targets_dists']]),
                .l_targets[['v_targets_dists']],
                .l_targets[['v_targets_weights']])
  # Estimate the log likelihood for each model output:
  summed_log_likelihood <- pmap(
    .l = l_llk,
    .f = function(.name, .func, .dist, .weight) {
      map_dbl(
        .x = model_results,
        .f = function(.mod_res) {
          if(.dist != 'lnorm') {
            sum( # Sum all values
              exec(.func,
                   .l_targets[[.name]]$value, # target's sd
                   .mod_res[[.name]], # mean value
                   .l_targets[[.name]]$se, # sd value (target's sd)
                   log = TRUE)
            ) * .weight # target weight
          } else {
            sum( # Sum all values
              exec(.func,
                   .l_targets[[.name]]$value, # target's mean
                   log(.mod_res[[.name]]) - (1/2) *
                     .l_targets[[.name]]$se^2, # mean value (model output)
                   .l_targets[[.name]]$se, # sd value (target's sd)
                   log = TRUE)
            ) * .weight # target weight
          }
        })
    })
  # Name lists correctly:
  names(summed_log_likelihood) <- .l_targets[['v_targets_names']]
  # Overall log likelihood (over all targets):
  overall_log_likelihood <- pmap_dbl(
    .l = summed_log_likelihood,
    .f = function(...) {
      dots = list(...) # grab all targets' llik for proposed parameter sets
      reduce(.x = dots, .f = `+`, .init = 1) # sum them together
    })
  # Prepare the output table:
  output <- .samples %>%
    mutate('Overall_fit' = overall_log_likelihood) %>%
    arrange(desc(Overall_fit))

  return(output)
}

#' A function to estimate weighted sum of squared errors (WSSE)
#' goodness-of-fit
#'
#' @param .func
#' @param .args
#' @param .samples
#' @param .l_targets
#'
#' @return
#' @export
#'
#' @examples
wSSE_GOF <- function(.func, .args = list(NULL), .samples,
                     .l_targets) {
  # Run the model using each set of sampled parameters:
  model_results <- pmap(
    .l = .samples,
    .f = function(...) {
      dots <- list(...)
      exec(.func, dots, !!!.args)
    })
  # Define inputs list for the log likelihood function:
  l_llk <- list(.l_targets[['v_targets_names']],
                paste0('d', .l_targets[['v_targets_dists']]),
                .l_targets[['v_targets_dists']],
                model_results)
  # Estimate the log likelihood for each model output:
  log_likelihood <- pmap_dfc(
    .l = l_llk,
    .f = function(.name, .func, .dist, .mod_res) {
      if(.dist != 'lnorm') {
        exec(.func,
             .l_targets[[.name]]$value,
             .mod_res[[.name]],
             .l_targets[[.name]]$se,
             log = TRUE)
      } else {
        exec(.func,
             .l_targets[[.name]]$value,
             log(.mod_res[[.name]]) - (1/2) * .l_targets[[.name]]$se^2,
             .l_targets[[.name]]$se,
             log = TRUE)
      }
    })
  # Prepare the output table:
  output <- .samples %>%
    mutate('Overall_fit' = colSums(log_likelihood)) %>%
    arrange(desc(Overall_fit))

  return(output)
}
