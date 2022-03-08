#' Summarise and/or create uncertainty estimates from the hessian matrix
#'
#' @param .params_name Character vector containing the names of the
#' parameters that were passed to the goodness-of-fit algorithm or will be
#' passed to .func.
#' @param .GoF_value Numeric goodness-of-fit value for the corresponding
#' parameters.
#' @param .GoF_method Character naming goodness-of-fit algorithm that
#' produced .GoF_value.
#' @param .par Parameter values to be used to generate \code{95% confidence
#' interval} and/or passed to .func to generate the .hessian matrix.
#' @param .func A function to be used to estimate the .hessian matrix.
#' @param .args A list of arguments passed to .func.
#' @param .hessian The hessian matrix.
#' @param .maximiser Logical for whether algorithm that created (or .func
#' which will create) the hessian matrix maximised the goodness-of-fit
#' function. Default is \code{TRUE}.
#'
#' @return A tibble with the best identified parameters, their 95%
#' confidence intervals and corresponding goodness-of-fit values.
#' @export
#'
#' @examples
summ_optim <- function(.params_name = v_params_names, .GoF_value,
                       .GoF_method, .par, .func = NULL, .args = NULL,
                       .hessian = NULL, .maximiser = TRUE) {
  # Stop if neither the .hessian matrix nor .func were supplied:
  stopifnot((!is.null(.hessian) | !is.null(.func)))
  # Approximate the .hessian matrix if not estimated by the optimisation function:
  if(is.null(.hessian)) {
    .hessian <- hessian(func = .func, x = .par, .args = .args,
                        .maximise = .maximiser)
  }

  # If .func was not maximising or .hessian resulted from a minimiser function:
  if(.maximiser) {
    .hessian <- -.hessian
  }

  # Estimate  Fisher Information Matrix (FIM):
  fisher_info <- solve(.hessian)

  # # Negative numbers don't have real square roots, correct if diag is < 0:
  # # If optim minimised GOF then we need negative hessian:
  # if(any(diag(fisher_info) < 0)) fisher_info = -fisher_info

  # Get the standard errors:
  prop_se <- sqrt(diag(fisher_info))

  # Calculate confidence interval:
  upper <- .par + (1.96 * prop_se)
  lower <- .par - (1.96 * prop_se)

  return(list(Params = .params_name, 'GoF value' = .GoF_value,
              'GoF algorithm' = .GoF_method, Estimate = .par,
              Lower = lower, Upper = upper))
}

#' Optimise model parameters
#'
#' @param .params_name Character vector containing the names of the
#' parameters that were passed to the goodness-of-fit algorithm.
#' @param .func A function defining the model to be optimised.
#' @param .args A list of arguments to be passed to .func.
#' @param .gof A goodness-of-fit function, default is log-likelihood.
#' @param .samples A table with sampled parameter values.
#' @param .method A Character, "Nelder-Mead", "BFGS", "SANN" or "GA", that
#' would identify the optimisation algorithm to be used.
#' @param .maximise Logical for whether algorithm that created (or .func
#' which will create) the hessian matrix maximised the goodness-of-fit
#' function. Default is \code{TRUE}.
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param .seed_no Integer for a random number generator seed number.
#' @param ... Arguments to be passed to the optimisation function.
#'
#' @return A list containing GOF values, identified parameters and
#' associated uncertainty. The returned list is sorted in descending order
#' based on the GOF values.
#' @export
#'
#' @examples
#' data("CRS_targets")
#' Surv <- CRS_targets$Surv
#' v_targets_names <- c("Surv")
#' v_targets_dists <- c('norm')
#' v_targets_weights <- c(1)
#' l_targets <-
#'   list('v_targets_names' = v_targets_names,
#'          'Surv' = Surv,
#'                 'v_targets_dists' = v_targets_dists,
#'                        'v_targets_weights' = v_targets_weights)
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#'
#' samples <- sample_prior_LHS(
#'   .l_params = list(v_params_names = v_params_names,
#'    v_params_dists = v_params_dists, args = args),
#'   .n_samples = 10000)
#'
#' NM_optimise_mod <- optimise_model(.l_params = list(
#'                     v_params_names = v_params_names,
#'                     v_params_dists = v_params_dists,
#'                     args = args),
#'                                   .func = CRS_markov,
#'                                   .args = NULL,
#'                                   .gof = log_likelihood,
#'                                   .samples = samples[1:10,],
#'                                   .method = 'Nelder-Mead',
#'                                   .maximise = TRUE,
#'                                   .l_targets = l_targets,
#'                                   maxit = 1000)
#'
#' GB_optimise_mod <- optimise_model(.l_params = list(
#'                     v_params_names = v_params_names,
#'                     v_params_dists = v_params_dists,
#'                     args = args),
#'                                   .func = CRS_markov,
#'                                   .args = NULL,
#'                                   .gof = log_likelihood,
#'                                   .samples = samples[1:10,],
#'                                   .method = 'BFGS',
#'                                   .maximise = TRUE,
#'                                   .l_targets = l_targets,
#'                                   maxit = 1000)
#'
#' SA_optimise_mod <- optimise_model(.l_params = list(
#'                     v_params_names = v_params_names,
#'                     v_params_dists = v_params_dists,
#'                     args = args),
#'                                   .func = CRS_markov,
#'                                   .args = NULL,
#'                                   .gof = log_likelihood,
#'                                   .samples = samples[1:10,],
#'                                   .method = 'SANN',
#'                                   .maximise = TRUE,
#'                                   .l_targets = l_targets,
#'                                   maxit = 1000,
#'                                   temp = 10,
#'                                   tmax = 10)
#'
optimise_model <- function(.l_params = l_params, .func, .args,
                           .gof = log_likelihood, .samples,
                           .method = 'Nelder-Mead', .maximise = TRUE,
                           .l_targets, .seed_no = 1, ...) {
  set.seed(.seed_no)
  # Ensure that .method is supported by the function:
  stopifnot(".method is supported by the function" =
              any(.method %in% c('Nelder-Mead', 'BFGS', 'SANN', 'GA')))
  # Get parameters' names:
  params_name <- .l_params[['v_params_names']]
  # Capture the arguments in the .dots:
  arguments <- list(...)
  # Apply appropriate method:
  if(any(.method %in% c('Nelder-Mead', 'BFGS', 'SANN'))) {
    # Map over sampled values:
    fits <- pmap(
      .l = .samples,
      .f = function(...) {
        # Grab a parameter set:
        params_set = c(...)
        # Run the optimisation function optim:
        fit <- optim(
          par = params_set,
          fn = .gof,
          method = .method,
          control = list( # control parameters
            fnscale = ifelse(.maximise, -1, # maximiser/minimiser
                             arguments[['fnscale']]),
            temp = arguments[['temp']], # SANN algorithm tuning
            tmax = arguments[['tmax']], # SANN algorithm tuning
            maxit = arguments[['maxit']]), # maximum iterations
          hessian = TRUE, # estimate hessian matrix
          .func = .func, # model to be optimised
          .args = .args, # arguments to be passed to the model
          .l_targets = .l_targets, # targets passed to .gof
          .maximise = TRUE, # .gof should maximise
          .optim = TRUE) # .gof reports gof value only
        # Summarise output produced by optim():
        fit_summary <- summ_optim(
          .params_name = params_name,
          .GoF_value = fit$value, # best GoF value estimated by optim()
          .GoF_method = .method,
          .par = fit$par, # best parameter set identified by optim()
          .func = .gof, # .gof function
          .hessian = fit$hessian) # hessian matrix estimated by optim()
      })
  } else {
    # Collect lower and upper bounds for DEoptim():
    lb = map_dbl(.x = .l_params$args, .f = function(.x) .x$min)
    ub = map_dbl(.x = .l_params$args, .f = function(.x) .x$max)
    # Map over sampled values:
    fits <- pmap(
      .l = .samples,
      .f = function(...) {
        # Run the optimisation function DEoptim:
        fit <- DEoptim::DEoptim(
          fn = .gof,
          lower = lb,
          upper = ub,
          control = DEoptim::DEoptim.control( # control parameters
            trace = FALSE), # printing a trace
          .func = .func, # model to be optimised
          .args = .args, # arguments to be passed to the model
          .l_targets = .l_targets, # targets passed to .gof
          .maximise = FALSE, # .gof should minimise
          .optim = TRUE) # .gof reports gof value only
        # Summarise output produced by DEoptim():
        fit_summary <- summ_optim(
          .params_name = params_name,
          .GoF_value = fit$optim$bestval, # best GoF value
          .GoF_method = .method,
          .par = fit$optim$bestmem, # best parameter set
          .func = .gof, # .gof function
          .maximiser = FALSE) # arguments passed to .gof
      })
  }

  # Sort items on the list based on overall fit:
  fits <- fits %>%
    rlist::list.sort(-`GoF value`)

  return(fits)
}
