#' Use Latin Hypercube Sampling (LHS) to sample from prior for IMIS method
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .n_samples An integer specifying the number of samples to be
#' generated.
#' @param ... additional arguments, for example: .seed_no to set a seed
#' number.
#'
#' @return A table with each parameter LHS samples in a separate column
#' @export
#'
#' @examples
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#' l_params <- list('v_params_names' = v_params_names,
#'                  'v_params_dists' = v_params_dists,
#'                  'args' = args)
#'
#' sample_prior_LHS(.l_params = l_params,
#'                  .n_samples = 10)
#'
sample_prior_IMIS <- function(.n_samples, .l_params = .l_params_,
                              ...) {
  # Grab additional arguments:
  dots = list(...)
  if(!is.null(dots[['.ssed_no']]))
    set.seed(dots[['.ssed_no']])
  # Get the number of parameters:
  n_params <- length(.l_params[["v_params_names"]])
  # Get LHS samples:
  tbl_lhs_unit <- lhs::randomLHS(.n_samples, n_params) %>%
    as_tibble(~ vctrs::vec_as_names(...,
                                    repair = "unique",
                                    quiet = TRUE))
  # Define inputs list:
  l_lhs <- list(.l_params[['v_params_names']],
                paste0('q', .l_params[['v_params_dists']]),
                tbl_lhs_unit,
                .l_params[['args']],
                .l_params[['v_params_dists']])
  # Make sure parameter names are in a named vector:
  names(l_lhs[[1]]) <- l_lhs[[1]]
  # Map over parameters to scale up LHS samples to appropriate values:
  tbl_lhs_samp <- pmap_dfc(
    .l = l_lhs,
    .f = function(.name, .func, p, .arg, .dist) {
      assign(.name,
             exec(.func,
                  p,
                  !!!.arg)
      )
    })

  return(tbl_lhs_samp %>% as.matrix())
}

#' Calculate log prior
#'
#' @param .samples A vector/dataset containing sampled/proposed values.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#'
#' @return
#' @export
#'
#' @examples
log_prior <- function(.samples, .l_params) {
  v_params_names <- .l_params[['v_params_names']]
  names(.l_params[['v_params_names']]) <- v_params_names
  # Ensure .samples is of appropriate class and named properly:
  if(is.null(dim(.samples))) # If vector, change to matrix
    .samples <- t(.samples)
  if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
    .samples <- .samples %>%
    as_tibble(~ vctrs::vec_as_names(...,
                                    repair = "unique",
                                    quiet = TRUE)) %>%
    `colnames<-`(v_params_names)
  # Define inputs list for the pmap function:
  l_lprior <- list(.l_params[['v_params_names']],
                   paste0('d', .l_params[['v_params_dists']]),
                   .l_params[['v_params_dists']],
                   .l_params[['args']],
                   .samples)
  # Estimate the log prior:
  v_lprior <- rowSums(pmap_df(
    .l = l_lprior,
    .f = function(.name, .func, .dist, .arg, .param) {
      if(.dist != 'lnorm') {
        exec(.func, .param, !!!.arg, log = TRUE)
      } else {
        exec(.func, .param, log(.arg[['mean']]) - (1/2) *
               .arg[['sd']]^2,
             .arg[['sd']],
             log = TRUE)
      }
    }
  ))

  return(v_lprior)

}

#' Calculate prior
#'
#' @param .samples A vector/dataset containing sampled/proposed values.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#'
#' @return
#' @export
#'
#' @examples
calculate_prior <- function(.samples, .l_params = .l_params_) {

  v_prior <-  exp(log_prior(.samples = .samples, .l_params = .l_params))

  return(v_prior)
}

#' Calculate log likelihood (LLK)
#'
#' @param .samples A table or vector of sampled parameter values
#' @param .func A function defining the model to be calibrated
#' @param .args A list of arguments to be passed to .func
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param ... Extra arguments, e.g. seed number.
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
#' l_params = list(v_params_names = v_params_names,
#'                 v_params_dists = v_params_dists,
#'                 args = args)
#'
#' samples <- sample_prior_LHS(.l_params = l_params,
#'                             .n_samples = 10)
#'
#' l_lik <- log_likelihood(.func = CRS_markov, .args = NULL,
#'                         .samples = samples, .l_targets = l_targets)
#'
log_likelihood <- function(.samples, .func, .args, .l_targets, ...) {
  # Grab and assign additional arguments:
  dots <- list(...)
  if(!is.null(dots[['seed_no']]))
    set.seed(dots[['seed_no']])
  # Ensure .samples is of appropriate class and named properly:
  if(is.null(dim(.samples))) # If vector, change to matrix
    .samples <- t(.samples)
  if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
    .samples <- .samples %>%
      as_tibble(~ vctrs::vec_as_names(...,
                                      repair = "unique",
                                      quiet = TRUE))
  # Run the model using each set of sampled parameters:
  model_results <- pmap(
    .l = as.list(.samples),
    .f = function(...) {
      dots <- list(...)
      exec(.func, dots, !!!.args)
    })
  # Define inputs list for the pmap function:
  l_llik <- list(.l_targets[['v_targets_names']],
                 paste0('d', .l_targets[['v_targets_dists']]),
                 .l_targets[['v_targets_dists']],
                 .l_targets[['v_targets_weights']])
  # Estimate the overall log likelihood for each model output:
  overall_lliks <- map_dbl(
    .x = model_results,
    .f = function(.mod_res) {
      # Log likelihood (for each target):
      overall_llik <- pmap(
        .l = l_llik,
        .f = function(.name, .func, .dist, .weight) {
          if(.dist == 'norm') {
            sum( # Sum all values of that one target, if many
              exec(.func,
                   .l_targets[[.name]]$value, # target's sd
                   .mod_res[[.name]], # mean value
                   .l_targets[[.name]]$se, # sd value (target's sd)
                   log = TRUE)
            ) * .weight # target weight
          } else if(.dist == 'binom') {
            sum( # Sum all values of that one target, if many
              exec(.func,
                   prob = .mod_res[[.name]],
                   x = .l_targets[[.name]]$x,
                   size = .l_targets[[.name]]$size,
                   log = TRUE)
            ) * .weight # target weight
          } else if(.dist == 'lnorm') {
            sum( # Sum all values of that one target, if many
              exec(.func,
                   .l_targets[[.name]]$value, # target's mean
                   log(.mod_res[[.name]]) - (1/2) *
                     .l_targets[[.name]]$se^2, # mean value (model output)
                   .l_targets[[.name]]$se, # sd value (target's sd)
                   log = TRUE)
            ) * .weight # target weight
          }
        })
      # Overall log likelihood (for all targets):
      overall_llik <- overall_llik %>%
        reduce(`+`, .init = 0)
    })

  return(overall_lliks)
}

#' Calculate likelihood
#'
#' @param .samples A table or vector of sampled parameter values
#' @param .func A function defining the model to be calibrated
#' @param .args A list of arguments to be passed to .func
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param ... Extra arguments, e.g. seed number.
#'
#' @return
#' @export
#'
#' @examples
calculate_likelihood <- function(.samples, .func = .func_, .args = .args_,
                                 .l_targets = .l_targets_, ...) {

  v_likelihood <-  exp(log_likelihood(.samples = .samples,
                                      .func = .func,
                                      .args = .args,
                                      .l_targets = .l_targets,
                                      ...))

  return(v_likelihood)
}

#' Calculate log posterior
#'
#' @param .samples A table or vector of sampled parameter values
#' @param .func A function defining the model to be calibrated
#' @param .args A list of arguments to be passed to .func
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param ... Extra arguments, e.g. seed number.
#'
#' @return
#' @export
#'
#' @examples
log_posterior <- function(.samples, .func, .args, .l_targets, .l_params,
                          ...) {
  # calculate log prior:
  l_prior <- log_prior(.samples = .samples, .l_params = l_params)
  # calculate log likelihood:
  l_lilk <- log_likelihood(.samples = .samples, .func = .func,
                           .args = .args, .l_targets = .l_targets, ...)
  # calculate log posterior:
  l_posterior <- l_prior + l_lilk

  return(l_posterior)
}

#' Calculate posterior
#'
#' @param .samples A table or vector of sampled parameter values
#' @param .func A function defining the model to be calibrated
#' @param .args A list of arguments to be passed to .func
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param ... Extra arguments, e.g. seed number.
#'
#' @return
#' @export
#'
#' @examples
calculate_posterior <- function(.samples, .func = .func_, .args = .args_,
                                .l_targets = .l_targets_,
                                .l_params = .l_params_, ...) {
  # calculate the posterior:
  posterior <- exp(log_posterior(.samples = .samples, .func = .func,
                                 .args = .args, .l_targets = .l_targets,
                                 .l_params = .l_params, ...))

  return(posterior)
}

#' Title
#'
#' @param .b_method
#' @param .func
#' @param .args
#' @param .l_targets
#' @param .l_params
#' @param .samples
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
calibrateModel_beyesian <- function(.b_method = "SIR", .func, .args,
                                    .l_targets, .l_params, .samples,
                                    ...) {
  # Ensure that .b_method is supported by the function:
  stopifnot(".b_method is supported by the function" =
              any(.b_method %in% c('SIR', 'IMIS', 'MCMC')))

  # Grab additional arguments for some of the methods:
  dots <- list(...)
  .n_resample <- dots[['.n_resample']]
  if(any(.b_method %in% c('SIR', 'IMIS')) & is.null(.n_resample))
    .n_resample <- nrow(.samples)

  if(.b_method == 'IMIS' & is.null(dots[['IMIS_sample']]))
    IMIS_sample <- 1000

  # SIR:
  if(.b_method == 'SIR') {
    ## Calculate log-likelihood for each sample value:
    llik <- log_likelihood(.samples = .samples, .func = .func,
                           .args = .args, .l_targets = .l_targets)

    ## Calculate weights for the re-sample:
    # Note: subtracting off the maximum log-likelihood before
    # exponentiating helps avoid numerical under/overflow, which would
    # result in weights of Inf or 0.
    weight <- exp(llik - max(llik)) / sum(exp(llik - max(llik)))
    ## Re-sample from samples with wt as sampling weights:
    SIR_resample  <- sample.int(.n_resample, replace = TRUE, prob = weight)
    posterior_SIR <- .samples[SIR_resample, ]
    ## Combine log-likelihood & posterior probability of each sample:
    SIR_results <- cbind(posterior_SIR,
                         "Overall_fit" = llik[SIR_resample],
                         "Posterior_prob" = weight[SIR_resample]) %>%
      arrange(desc(Overall_fit))

    return(list('Results' = SIR_results, 'Method' = "SIR"))

  } else if(.b_method == 'IMIS') { # IMIS:
    ## Define three functions needed by IMIS:
    ### prior(x), likelihood(x), sample.prior(n)
    prior <<- calculate_prior
    likelihood <<- calculate_likelihood
    sample.prior <<- sample_prior_IMIS
    ## Define function inputs:
    .l_params_ <<- .l_params # prior/sample.prior
    .func_ <<- .func # calculate_likelihood
    .args_ <<- .args # calculate_likelihood
    .l_targets_ <<- .l_targets # calculate_likelihood
    ## Run IMIS:
    fit_IMIS <- IMIS::IMIS(
      B = IMIS_sample, # the incremental sample size at each IMIS iteration
      B.re = .n_resample, # the desired posterior sample size
      number_k = 10, # the maximum number of iterations in IMIS
      D = 0)

    ## Obtain draws from posterior:
    m_calib_res <- fit_IMIS$resample
    Overall_fit <- log_likelihood(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets)
    Posterior_prob <- calculate_posterior(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets, .l_params = .l_params)
    ## Calculate log-likelihood (overall fit) and posterior probability:
    IMIS_results <- m_calib_res %>%
      as_tibble(~ vctrs::vec_as_names(...,
                                      repair = "unique",
                                      quiet = TRUE)) %>%
      mutate(
        "Overall_fit" = Overall_fit,
        "Posterior_prob" = Posterior_prob) %>%
      arrange(desc(Overall_fit))
    ## Name column names IMIS stats object:
    stats <- fit_IMIS$stat %>%
      as_tibble(~ vctrs::vec_as_names(...,
                                      repair = "unique",
                                      quiet = TRUE)) %>%
      `colnames<-`(c("MargLike", "UniquePoint", "MaxWeight", "ESS",
                     "ImpWt", "ImpWtVar"))

    return(list('Results' = IMIS_results, 'Method' = "IMIS",
                'Fit'  = fit_IMIS, 'Stats' = stats))

  } else {

  }
}















