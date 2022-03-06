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
#' @param .iter Integer used if
#' @param .maximiser Logical for whether algorithm that created (or .func
#' which will create) the hessian matrix maximised the goodness-of-fit
#' function. Default is \code{TRUE}.
#'
#' @return
#' @export
#'
#' @examples
summ_optim <- function(.params_name = v_params_names, .GoF_value,
                       .GoF_method, .par, .func = NULL, .hessian = NULL,
                       .iter = 1, .maximiser = TRUE) {
  # Stop if neither the .hessian matrix nor .func were supplied:
  stopifnot((!is.null(.hessian) | !is.null(.func)))
  # sigma.mat <- solve(-.hessian)
  # cor.mat <- cov2cor(sigma.mat)
  # sd.vec <- sqrt(diag(sigma.mat))
  # upper <- .par + 2 * sd.vec
  # lower <- .par - 2 * sd.vec
  # fisher_info <- solve(-.hessian)
  # prop_sigma <- sqrt(diag(fisher_info))
  # prop_sigma <- diag(prop_sigma)
  # upper<- .par + 1.96 * prop_sigma
  # lower<- .par - 1.96 * prop_sigma
  # Approximate the hessian matrix if not estimated by the package:
  if(is.null(.hessian)) {
    .hessian <- hessian(func = .func, x = .par)
  }

  # If .func was not maximising or .hessian resulted from a minimiser function:
  if(!.maximiser) {
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
  upper <- .par + 1.96 * prop_se
  lower <- .par - 1.96 * prop_se

  return(tibble(Params = .params_name, 'GoF value' = .GoF_value,
                'GoF algorithm' = .GoF_method, Estimate = .par,
                Lower = lower, Upper = upper, Iteration = .iter))
}
