#' A function to estimate log likelihood (LLK) goodness-of-fit
#'
#' @param .samples A table or vector of sampled parameter values
#' @param .func A function defining the model to be calibrated
#' @param .args A list of arguments to be passed to .func
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param .maximise Logical for whether the output of the function is used
#' in a maximising optimisation function. Default is \code{TRUE}.
#' @param .optim Logical for whether the function is used by an
#' optimisation algorithm. Default is \code{FALSE}.
#' @param .seed_no Integer for a random number generator seed number.
#'
#' @return A table with proposed parameter sets and their corresponding
#' summed overall likelihood values sorted in descending order.
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
log_likelihood <- function(.samples, .func, .args = list(NULL),
                           .l_targets, .maximise = TRUE, .optim = FALSE,
                           .seed_no = 1) {
  set.seed(.seed_no)
  # Run the model using each set of sampled parameters:
  model_results <- pmap(
    .l = as.list(.samples),
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
      reduce(.x = dots, .f = `+`, .init = 0) # sum them together
    })
  # Amend output if optimisation function was a minimiser (flip signs):
  if(!.maximise)
    overall_log_likelihood <- -overall_log_likelihood

  # Return overall GoF if used in an optimisation function:
  if(.optim)
    return(overall_log_likelihood)

  # Prepare extensive output table if not used by an optimisation function:
  output <- .samples %>%
    as_tibble() %>% # when .samples is a vector
    mutate('Overall_fit' = overall_log_likelihood) %>%
    arrange(desc(Overall_fit))

  return(output)
}

#' A function to estimate weighted sum of squared errors (WSSE)
#' goodness-of-fit
#'
#' @param .samples A table or vector of sampled parameter values
#' @param .func A function defining the model to be calibrated
#' @param .args A list of arguments to be passed to .func
#' @param .weighted Logical for whether the SSR was to be weighted, default
#' is \code{TRUE}. The weight used by function is \code{1/(sd^2)}.
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights and a table for each target that contains the values
#' (column name 'value') and standard errors (column name 'sd') of the
#' corresponding target.
#' @param .maximise Logical for whether the output of the function is used
#' in a maximising optimisation function. Default is \code{TRUE}.
#' @param .optim Logical for whether the function is used by an
#' optimisation algorithm. Default is \code{FALSE}.
#' @param .seed_no Integer for a random number generator seed number.
#'
#' @return A table with proposed parameter sets and their corresponding
#' summed overall weighted sum of square values sorted in descending order.
#' @export
#'
#' @examples
#' data("CRS_targets")
#' Surv <- CRS_targets$Surv
#' v_targets_names <- c("Surv")
#' v_targets_weights <- c(1)
#' l_targets <- list('v_targets_names' = v_targets_names, 'Surv' = Surv,
#'                  'v_targets_weights' = v_targets_weights)
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#' samples <- sample_prior_LHS(
#'            .l_params = list(v_params_names = v_params_names,
#'                            v_params_dists = v_params_dists,
#'                            args = args),
#'            .n_samples = 10000)
#'
#' GOF_wsse <- wSSE_GOF(.func = CRS_markov, .samples = samples,
#'                      .l_targets = l_targets)
#'
wSSE_GOF <- function(.samples, .func, .args = list(NULL), .weighted = TRUE,
                     .l_targets, .maximise = TRUE, .optim = FALSE,
                     .seed_no = 1) {
  set.seed(.seed_no)
  # Run the model using each set of sampled parameters:
  model_results <- pmap(
    .l = as.list(.samples),
    .f = function(...) {
      dots <- list(...)
      exec(.func, dots, !!!.args)
    })
  # Define inputs list for the weighted sum of squared errors function:
  l_wsse <- list(.l_targets[['v_targets_names']],
                 .l_targets[['v_targets_weights']])
  # Estimate the weighted sum of squared errors for each model output:
  wsse <- pmap(
    .l = l_wsse,
    .f = function(.name, .weight) {
      map_dbl(
        .x = model_results,
        .f = function(.mod_res) {
          if(.weighted) {
            weights <- 1/(.l_targets[[.name]]$se^2)
            -sum(weights *
                   ((.l_targets[[.name]]$value - .mod_res[[.name]])^2)) *
              .weight
          } else {
            -sum((.l_targets[[.name]]$value - .mod_res[[.name]])^2) *
              .weight
          }
        })
    })
  # Overall log likelihood (over all targets):
  overall_weighted_sse <- pmap_dbl(
    .l = wsse,
    .f = function(...) {
      dots = list(...) # grab all targets' wsse for proposed parameter sets
      reduce(.x = dots, .f = `+`, .init = 0) # sum them together
    })
  # Amend output if optimisation function was a minimiser (flip signs):
  if(!.maximise)
    overall_weighted_sse <- -overall_weighted_sse

  # Return overall GoF if used in an optimisation function:
  if(.optim)
    return(overall_weighted_sse)

  # Prepare extensive output table if not used by an optimisation function:
  output <- .samples %>%
    as_tibble() %>% # when .samples is a vector
    mutate('Overall_fit' = overall_weighted_sse) %>%
    arrange(desc(Overall_fit))

  return(output)
}
