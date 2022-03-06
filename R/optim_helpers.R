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
                       .GoF_method, .par, .func = NULL, .hessian = NULL,
                       .maximiser = TRUE) {
  # Stop if neither the .hessian matrix nor .func were supplied:
  stopifnot((!is.null(.hessian) | !is.null(.func)))
  # Approximate the .hessian matrix if not estimated by the optimisation function:
  if(is.null(.hessian)) {
    .hessian <- hessian(func = .func, x = .par)
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
#' @param .method A Character, "Nelder-Mead", "BFGS" or "SANN", that would
#' identify the optimisation algorithm to be used.
#' @param .maximise Logical for whether algorithm that created (or .func
#' which will create) the hessian matrix maximised the goodness-of-fit
#' function. Default is \code{TRUE}.
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param ... Arguments to be passed to the optimisation function.
#'
#' @return
#' @export
#'
#' @examples
optimise_model <- function(.params_name = v_params_names, .func, .args,
                           .gof = log_likelihood, .samples,
                           .method = 'Nelder-Mead', .maximise = TRUE,
                           .l_targets, ...) {
  # Capture the arguments in the .dots:
  arguments = list(...)
  # Map over sampled values:
  fits <- pmap(
    .l = .samples,
    .f = function(...) {
      params_set = c(...) # grab a parameter set
      # Run the optimisation function optim:
      fit <- optim(
        par = params_set,
        fn = .gof,
        method = .method,
        control = list(
          fnscale = ifelse(.maximise, -1, # maximiser/minimiser
                           arguments[['fnscale']]),
          temp = arguments[['temp']], # SANN algorithm tuning
          tmax = arguments[['tmax']], # SANN algorithm tuning
          maxit = arguments[['maxit']]), # maximum iterations
        hessian = TRUE, # estimate hessian matirx
        .func = .func, # model to be optimised
        .args = .args, # arguments to be passed to the model
        .l_targets = .l_targets, # targets passed to .gof
        .optim = TRUE) # .gof reports gof value only
      # Summarise output produced by optim():
      fit_summary <- summ_optim(
        .params_name = v_params_names,
        .GoF_value = fit$value, # best GoF value estimated by optim()
        .GoF_method = .method,
        .par = fit$par, # best parameter set identified by optim()
        .func = .func,
        .hessian = fit$hessian) # hessian matrix estimated by optim()
    })

  return(fits)
}









