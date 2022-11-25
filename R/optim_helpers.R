#' Summarise and/or create uncertainty estimates from the hessian matrix
#'
#' @param .params_name Character vector containing the names of the
#' parameters that were passed to the goodness-of-fit algorithm or will be
#' passed to .func.
#' @param .gof goodness-of-fit function used or to be used in the
#' optimisation.
#' @param .gof_name Character naming goodness-of-fit method that produced
#' samples
#' @param .gof_value Numeric goodness-of-fit value for the corresponding
#' parameters.
#' @param .par Parameter values to be used to generate \code{95% confidence
#' interval} and/or passed to .func to generate the .hessian matrix.
#' @param .s_method Character naming search algorithm that produced
#' .GoF_value.
#' @param .func A function passed to and to be optimised by .gof.
#' @param .args A list of arguments passed to .func.
#' @param .hessian The hessian matrix.
#' @param .maximiser Logical for whether algorithm that created (or .func
#' which will create) the hessian matrix maximised the goodness-of-fit
#' function. Default is \code{TRUE}.
#' @param .convergence Convergence label; 0 successful convergence
#' @param ... Extra arguments to be passed to .gof.
#'
#' @return A tibble with the best identified parameters, their 95%
#' confidence intervals and corresponding goodness-of-fit values.
#'
#' @examples
#' \dontrun{
#' }
summ_optim <- function(.params_name = v_params_names, .gof = NULL,
                       .gof_name, .gof_value, .s_method, .par,
                       .func = NULL, .args = NULL, .hessian = NULL,
                       .maximiser = TRUE, .convergence, ...) {
  if(.s_method == "Nelder-Mead") .s_method = 'NM'
  # Grab and assign additional arguments:
  dots <- list(...)
  if(!is.null(dots[['.l_targets']])) .l_targets <- dots[['.l_targets']]
  # Stop if neither the .hessian matrix nor .gof & .func were supplied:
  stopifnot((!is.null(.hessian) | (!is.null(.gof)) & !is.null(.func)))
  # Ensure .par object is named:
  if(!is.null(.par)) names(.par) <- .params_name
  # Approximate the .hessian if not estimated by the optimisation function:
  if(is.null(.hessian)) {
    .hessian <- tryCatch(
      expr = {
        temp <- numDeriv::hessian(func = .gof, x = .par, .func = .func,
                                  .args = .args, .l_targets = .l_targets,
                                  .maximise = .maximiser, .optim = TRUE,
                                  v_params_names = .params_name)
        # Name columns and rows:
        if(!is.null(temp))
          dimnames(temp) <- list(.params_name, .params_name)

        temp
      }, error = function(e) {
        message(paste0("\r", e))

        NULL
      })
  }

  # If .hessian is NULL:
  if(is.null(.hessian)) {
    return(list(Params = .params_name, Estimate = .par, Lower = NA,
                Upper = NA, 'GOF value' = .gof_value,
                'Calibration method' = paste0(.s_method, "_", .gof_name,
                                              "_", .convergence),
                'Sigma' = NA))
  }

  # If .func was not maximising or .hessian resulted from a minimiser:
  if(.maximiser) {
    .hessian <- -.hessian
  }

  # Estimate Fisher Information Matrix (FIM), also the covariance matrix:
  fisher_info <- tryCatch(
    # Avoid "Lapack routine dgesv: system is exactly singular: U[6,6] = 0"
    expr = {
      solve(.hessian)

    }, error = function(e) {
      message(paste0("\r", e))

      NULL
    }
  )
  # Exit function early if the inverse of .hessian is unattainable:
  if(is.null(fisher_info))
    return(list(Params = .params_name, Estimate = .par, Lower = NA,
                Upper = NA, 'GOF value' = .gof_value,
                'Calibration method' = paste0(.s_method, "_", .gof_name,
                                              "_", .convergence),
                'Sigma' = NA))

  # Continue if .hessian could be inversed:
  covr_mat <- fisher_info
  diag(covr_mat) <- 1

  # # Negative numbers don't have real square roots, correct if diag is < 0:
  # # If optim minimised GOF then we need negative hessian:
  # if(any(diag(fisher_info) < 0)) fisher_info = -fisher_info

  # Get the standard errors:
  prop_se <- sqrt(diag(fisher_info))

  # Calculate confidence interval:
  upper <- .par + (1.96 * prop_se)
  lower <- .par - (1.96 * prop_se)

  return(list(Params = .params_name, Estimate = .par, Lower = lower,
              Upper = upper, 'GOF value' = .gof_value,
              'Calibration method' = paste0(.s_method, "_", .gof_name,
                                            "_", .convergence),
              'Sigma' = covr_mat))
}

#' Calibrate model using Directed search algorithms (optimisation
#' functions)
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .func Function defining the model to be optimised.
#' @param .args List of arguments to be passed to .func.
#' @param .gof Name goodness-of-fit function, default is log-likelihood.
#' @param .gof_func Goodness-of-fit function, when one other than supported
#' is needed.
#' @param .samples A table with sampled parameter values.
#' @param .s_method A Character, "NM", "BFGS", "SANN" or "GA",
#' that would identify the optimisation algorithm to be used.
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
#' \dontrun{
#' library(calibR)
#' data("CRS_targets")
#' Surv <- CRS_targets$Surv
#' v_targets_names <- c("Surv", "Surv")
#' v_targets_weights <- c(0.5, 0.5)
#' v_targets_dists <- c("norm", "norm")
#' # v_targets_names <- c("Surv")
#' # v_targets_weights <- c(1)
#' l_targets <-
#'   list('v_targets_names' = v_targets_names,
#'        'Surv' = Surv,
#'        'v_targets_dists' = v_targets_dists,
#'        'v_targets_weights' = v_targets_weights)
#'
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#' l_params <- list('v_params_names' = v_params_names,
#'                  'v_params_dists' = v_params_dists,
#'                  'args' = args)
#'
#' set.seed(1)
#' samples <- sample_prior_LHS(.l_params = l_params,
#'                             .n_samples = 5)
#'
#' NM_optimise_wSSE <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = NULL,
#'   .gof = 'SSE',
#'   .samples = samples,
#'   .s_method = 'NM',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   maxit = 1000)
#'
#' GB_optimise_wSSE <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = NULL,
#'   .gof = 'SSE',
#'   .samples = samples,
#'   .s_method = 'BFGS',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   maxit = 1000)
#'
#' SA_optimise_wSSE <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = list(NULL),
#'   .gof = 'SSE',
#'   .samples = samples,
#'   .s_method = 'SANN',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   maxit = 1000,
#'   temp = 10,
#'   tmax = 10)
#'
#' GA_optimise_wSSE <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = list(NULL),
#'   .gof = 'SSE',
#'   .samples = samples,
#'   .s_method = 'GA',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   maxit = 1000)
#'
#' NM_optimise_lLLK <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = NULL,
#'   .gof = 'LLK',
#'   .samples = samples,
#'   .s_method = 'NM',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   maxit = 1000)
#'
#' GB_optimise_lLLK <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = NULL,
#'   .gof = 'LLK',
#'   .samples = samples,
#'   .s_method = 'BFGS',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   maxit = 1000)
#'
#' SA_optimise_lLLK <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = NULL,
#'   .gof = 'LLK',
#'   .samples = samples,
#'   .s_method = 'SANN',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   temp = 10,
#'   tmax = 10,
#'   maxit = 1000)
#'
#' GA_optimise_lLLK <- calibrateModel_directed(
#'   .l_params = l_params,
#'   .func = CRS_markov,
#'   .args = list(NULL),
#'   .gof = 'LLK',
#'   .samples = samples,
#'   .s_method = 'GA',
#'   .maximise = TRUE,
#'   .l_targets = l_targets,
#'   maxit = 1000)
#' }
calibrateModel_directed <- function(.l_params = l_params, .func, .args,
                                    .gof = 'LLK', .gof_func = NULL,
                                    .samples, .s_method = 'NM',
                                    .maximise = TRUE,
                                    .l_targets, .seed_no = 1, ...) {
  set.seed(.seed_no)
  # Ensure that .s_method is supported by the function:
  stopifnot(".s_method is supported by the function" =
              any(.s_method %in% c('NM', 'BFGS', 'SANN', 'GA')))
  # Get the .gof method:
  .gof_name <- .gof
  if(is.null(.gof_func)) {
    .gof <- switch(.gof,
                   LLK = calibR::LLK_GOF,
                   SSE = calibR::wSSE_GOF)
  } else {
    .gof <- .gof_func
    message(
      paste0("using the bespoke goodness-of-fit function: ",
             .gof_name))
  }

  # Get parameters' names:
  params_name <- .l_params[['v_params_names']]
  # Capture the arguments in the .dots:
  arguments <- list(...)
  # Apply appropriate method:
  if(any(.s_method %in% c('NM', 'BFGS', 'SANN'))) {
    # Map over sampled values:
    fits <- purrr::pmap(
      .l = .samples,
      .f = function(...) {
        # Grab a parameter set:
        params_set = c(...)
        # Run the optimisation function optim:
        fit <- tryCatch(
          # Make sure to return NULL if the algorithm fails:
          expr = {
            if(.s_method == 'NM') .s_method = "Nelder-Mead"
            stats::optim(
              par = params_set,
              fn = .gof,
              method = .s_method,
              control = list( # control parameters
                fnscale = ifelse(.maximise, -1, 1), # maximiser/minimiser
                temp = arguments[['temp']], # SANN algorithm tuning
                tmax = arguments[['tmax']], # SANN algorithm tuning
                maxit = arguments[['maxit']]), # maximum iterations
              hessian = TRUE, # estimate hessian matrix
              .func = .func, # model to be optimised
              .args = .args, # arguments to be passed to the model
              .l_targets = .l_targets, # targets passed to .gof
              .maximise = .maximise, # .gof should maximise
              .optim = TRUE, # .gof reports gof value only
              seed_no = .seed_no
            )
          }, error = function(e) {
            message(paste0("\r", e))

            NULL
          }
        )
        # Summarise output produced by optim():
        fit_summary <- tryCatch(
          # Make sure to return NULL if the algorithm fails:
          expr = {
            summ_optim(
              .params_name = params_name,
              .gof = .gof, # goodness-of-fit function used/to be used.
              .gof_value = fit$value, # best goodness-of-fit value
              .s_method = .s_method, # the name of the search method
              .par = fit$par, # best parameter set identified
              .func = .func, # the optimised function (decision model)
              .args = .args, # arguments passed to .func
              .hessian = fit$hessian, # hessian matrix estimated by optim()
              .convergence = fit$convergence, # 0 = successful
              .l_targets = .l_targets, # targets passed to .gof
              .gof_name = .gof_name, # name of the goodness-of-fit function
              .maximiser = .maximise # whether the GOF was a maximiser
            )
          }, error = function(e) {
            message(paste0("\r", e))

            NULL
          }
        )
      }
    )
  } else {
    # Collect lower and upper bounds for DEoptim():
    lb = purrr::map_dbl(.x = .l_params$Xargs, .f = function(.x) .x$min)
    ub = purrr::map_dbl(.x = .l_params$Xargs, .f = function(.x) .x$max)
    # Map over sampled values:
    fits <- purrr::pmap(
      .l = .samples,
      .f = function(...) {
        # Run the optimisation function DEoptim:
        fit <- tryCatch(
          # Make sure to return NULL if the algorithm fails:
          expr = {
            DEoptim::DEoptim(
              fn = .gof,
              lower = lb,
              upper = ub,
              control = DEoptim::DEoptim.control( # control parameters
                trace = FALSE), # printing a trace
              .func = .func, # model to be optimised
              .args = .args, # arguments to be passed to the model
              .l_targets = .l_targets, # targets passed to .gof
              .maximise = FALSE, # .gof should minimise
              .optim = TRUE, # .gof reports gof value only
              seed_no = .seed_no,
              # DEoptim() requires params names to be re-assigned in .gof:
              v_params_names = params_name
            )
          }, error = function(e) {
            message(paste0("\r", e))

            NULL
          }
        )
        # Summarise output produced by DEoptim():
        fit_summary <- tryCatch(
          # Make sure to return NULL if the algorithm fails:
          expr = {
            summ_optim(
              .params_name = params_name,
              .gof = .gof, # goodness-of-fit function used/to be used.
              .gof_value = -fit$optim$bestval, # best goodness-of-fit value
              .s_method = .s_method, #  name of the goodness-of-fit method
              .par = fit$optim$bestmem, # best parameter set identified
              .func = .func, # the optimised function (decision model)
              .args = .args, # arguments passed to .func
              .maximiser = FALSE, # GA is a minimiser
              .convergence = NULL, # 0 = successful
              .l_targets = .l_targets, # targets passed to .gof
              .gof_name = .gof_name # name of the goodness-of-fit function
            )
          }, error = function(e) {
            message(paste0("\r", e))

            NULL
          }
        )
      }
    )
  }

  # Sort items on the list based on overall fit:
  # tryCatch errors where `GOF value` does not exist
  fits <- tryCatch(
    expr = {
      # if maximise, sort outputs in descending order:
      if(.maximise) {
        fits %>%
          rlist::list.exclude(is.null(.)) %>%
          rlist::list.exclude(is.null(`GOF value`)) %>%
          rlist::list.exclude(is.na(`GOF value`)) %>%
          rlist::list.sort(-`GOF value`)
      } else {
        fits %>%
          rlist::list.exclude(is.null(.)) %>%
          rlist::list.exclude(is.null(`GOF value`)) %>%
          rlist::list.exclude(is.na(`GOF value`)) %>%
          rlist::list.sort(`GOF value`)
      }
    }, error = function(e) {
      message(paste0("\r", e))

      NULL
    }
  )

  return(fits)
}
