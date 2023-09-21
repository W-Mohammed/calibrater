#' Use Latin Hypercube Sampling (LHS) to sample from prior for IMIS method
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .n_samples An integer specifying the number of samples to be
#' generated.
#'
#' @return A table with each parameter LHS samples in a separate column
#' @export
#'
#' @examples
#' \dontrun{
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
#' }
sample_prior_IMIS <- function(.n_samples, .l_params = .l_params_) {
  # Get the number of parameters:
  n_params <- length(.l_params[["v_params_names"]])
  # Get LHS samples:
  tbl_lhs_unit <- lhs::randomLHS(.n_samples, n_params) %>%
    as.data.frame()

  # Define inputs list:
  l_lhs <- list(.l_params[['v_params_names']],
                paste0('q', .l_params[['v_params_dists']]),
                tbl_lhs_unit,
                .l_params[['args']],
                .l_params[['v_params_dists']])
  # Make sure parameter names are in a named vector:
  names(l_lhs[[1]]) <- l_lhs[[1]]
  # Map over parameters to scale up LHS samples to appropriate values:
  tbl_lhs_samp <- purrr::pmap_dfc(
    .l = l_lhs,
    .f = function(.name, .func, p, .arg, .dist) {
      assign(.name,
             purrr::exec(.func,
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
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
log_priors <- function(.samples, .l_params, .transform) {

  v_params_names <- .l_params[['v_params_names']]
  names(.l_params[['v_params_names']]) <- v_params_names
  # Ensure .samples is of appropriate class and named properly:
  if(is.null(dim(.samples))) # If vector, change to matrix
    .samples <- t(.samples)
  if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
    .samples <- .samples %>%
    as.data.frame() %>%
    `colnames<-`(v_params_names)
  # Get appropriate distributions' and distributions' parameters' objects:
  params_dists <- 'v_params_dists'
  params_args <- 'args'
  # Transform the sampled parameters back to their original scale:
  if(.transform) {
    .samples <- .samples %>%
      calibR::backTransform(.t_data_ = ., .l_params_ = .l_params)
    params_dists <- 'v_true_params_dists'
    params_args <- 'true_args'
  }
  # Define inputs list for the pmap function:
  l_lprior <- list(.l_params[['v_params_names']],
                   paste0('d', .l_params[[params_dists]]),
                   .l_params[[params_dists]],
                   .l_params[[params_args]],
                   .samples)
  # Estimate the log prior:
  v_lprior <- rowSums(purrr::pmap_df(
    .l = l_lprior,
    .f = function(.name, .func, .dist, .arg, .param) {
      purrr::exec(.func, .param, !!!.arg, log = TRUE)
    }
  ))

  return(v_lprior)

}

#' Calculate log prior (one set of parameters at a time)
#'
#' @param .samples A vector/dataset containing sampled/proposed values.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
log_prior <- function(.samples, .l_params, .transform) {

  v_params_names <- .l_params[['v_params_names']]
  names(.l_params[['v_params_names']]) <- v_params_names
  # Ensure .samples is of appropriate class and named properly:
  if(is.null(dim(.samples))) # If vector, change to matrix
    .samples <- t(.samples)
  if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
    .samples <- .samples %>%
    as.data.frame() %>%
    `colnames<-`(v_params_names)
  # Get appropriate distributions' and distributions' parameters' objects:
  params_dists <- 'v_params_dists'
  params_args <- 'args'
  # Transform the sampled parameters back to their original scale:
  if(.transform) {
    .samples <- .samples %>%
      calibR::backTransform(.t_data_ = ., .l_params_ = .l_params)
    params_dists <- 'v_true_params_dists'
    params_args <- 'true_args'
  }
  # Define inputs list for the pmap function:
  l_lprior <- list(.l_params[['v_params_names']],
                   paste0('d', .l_params[[params_dists]]),
                   .l_params[[params_dists]],
                   .l_params[[params_args]],
                   .samples)
  # Estimate the log prior:
  v_lprior <- sum(purrr::pmap_dbl(
    .l = l_lprior,
    .f = function(.name, .func, .dist, .arg, .param) {
      purrr::exec(.func, .param, !!!.arg, log = TRUE)
    }
  ))

  return(v_lprior)

}

#' Calculate priors
#'
#' @param .samples A vector/dataset containing sampled/proposed values.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
calculate_priors <- function(.samples, .l_params = .l_params_,
                             .transform = .transform_) {

  v_prior <-  exp(calibR::log_priors(.samples = .samples, .l_params = .l_params,
                                     .transform = .transform))

  return(v_prior)
}

#' Calculate prior (one set of parameters at a time)
#'
#' @param .samples A vector/dataset containing sampled/proposed values.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
calculate_prior <- function(.samples, .l_params = .l_params_,
                            .transform = .transform_) {

  v_prior <-  exp(calibR::log_prior(.samples = .samples, .l_params = .l_params,
                                    .transform = .transform))

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
#'
#' @return A table with proposed parameter sets and their corresponding
#' summed overall likelihood values sorted in descending order.
#' @export
#'
#' @examples
#' \dontrun{
#' library(calibR)
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
#' }
log_likelihood <- function(.samples, .func, .args, .l_targets) {
  # Ensure .samples is of appropriate class and named properly:
  if(is.null(dim(.samples))) # If vector, change to matrix
    .samples <- t(.samples)
  if(!any(class(.samples) %in% c("tbl_df", "tbl", "data.frame")))
    .samples <- .samples %>%
      as.data.frame()
  # Run the model using each set of sampled parameters:
  model_results <- purrr::pmap(
    .l = as.list(.samples),
    .f = function(...) {
      dots <- list(...)
      purrr::exec(.func, dots, !!!.args)
    })
  # Define inputs list for the pmap function:
  l_llik <- list(.l_targets[['v_targets_names']],
                 paste0('d', .l_targets[['v_targets_dists']]),
                 .l_targets[['v_targets_dists']],
                 .l_targets[['v_targets_weights']])

  # Estimate the overall log likelihood for each model output:
  overall_lliks <- purrr::map_dbl(
    .x = model_results,
    .f = function(.mod_res) {
      # Log likelihood (for each target):
      overall_llik <- purrr::pmap(
        .l = l_llik,
        .f = function(.name, .func, .dist, .weight) {
          tryCatch({
            if(.dist == 'norm') {
              sum( # Sum all values of that one target, if many
                purrr::exec(.func,
                            .l_targets[[.name]]$value, # target's sd
                            .mod_res[[.name]], # mean value
                            .l_targets[[.name]]$se, # sd value (target's sd)
                            log = TRUE)
              ) * .weight # target weight
            } else if(.dist == 'binom') {
              sum( # Sum all values of that one target, if many
                purrr::exec(.func,
                            prob = .mod_res[[.name]],
                            x = .l_targets[[.name]]$x,
                            size = .l_targets[[.name]]$size,
                            log = TRUE)
              ) * .weight # target weight
            } else if(.dist == 'lnorm') {
              sum( # Sum all values of that one target, if many
                purrr::exec(.func,
                            .l_targets[[.name]]$value, # target's mean
                            log(.mod_res[[.name]]) - (1/2) *
                              .l_targets[[.name]]$se^2, # mean value
                            .l_targets[[.name]]$se, # sd value (target's sd)
                            log = TRUE)
              ) * .weight # target weight
            }
          },
          error = function(e) -Inf
          )
        }
      )
      # Overall log likelihood (for all targets):
      overall_llik <- overall_llik %>%
        purrr::reduce(`+`, .init = 0)
    })

  # Set NaN values to -Inf to avoid other functions from crashing:
  overall_lliks[is.na(overall_lliks)] <- -Inf

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
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
calculate_likelihood <- function(.samples, .func = .func_, .args = .args_,
                                 .l_targets = .l_targets_) {

  v_likelihood <-  exp(calibR::log_likelihood(.samples = .samples,
                                              .func = .func,
                                              .args = .args,
                                              .l_targets = .l_targets))

  return(v_likelihood)
}

#' Calculate log posteriors
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
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
log_posteriors <- function(.samples, .func, .args, .l_targets, .l_params,
                           .transform) {
  # calculate log prior:
  l_prior <- calibR::log_priors(.samples = .samples, .l_params = .l_params,
                                .transform = .transform)
  # calculate log likelihood:
  l_lilk <- calibR::log_likelihood(.samples = .samples, .func = .func,
                                   .args = .args, .l_targets = .l_targets)
  # calculate log posterior:
  l_posterior <- l_prior + l_lilk

  return(l_posterior)
}

#' Calculate log posterior (one set of parameters at a time)
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
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
log_posterior <- function(.samples, .func, .args, .l_targets, .l_params,
                          .transform) {
  # calculate log prior:
  l_prior <- calibR::log_prior(.samples = .samples, .l_params = .l_params,
                               .transform = .transform)
  # calculate log likelihood:
  l_lilk <- calibR::log_likelihood(.samples = .samples, .func = .func,
                                   .args = .args, .l_targets = .l_targets)
  # calculate log posterior:
  l_posterior <- l_prior + l_lilk

  return(l_posterior)
}

#' Calculate posterior densities
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
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
calculate_posteriors <- function(.samples, .func = .func_, .args = .args_,
                                 .l_targets = .l_targets_,
                                 .l_params = .l_params_,
                                 .transform = FALSE) {
  # calculate the posterior:
  posterior <- exp(calibR::log_posteriors(.samples = .samples, .func = .func,
                                          .args = .args, .l_targets = .l_targets,
                                          .l_params = .l_params,
                                          .transform = .transform))

  return(posterior)
}

#' Calculate posterior (one set of parameters at a time)
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
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
calculate_posterior <- function(.samples, .func = .func_, .args = .args_,
                                .l_targets = .l_targets_,
                                .l_params = .l_params_,
                                .transform = FALSE) {
  # calculate the posterior:
  posterior <- exp(calibR::log_posterior(.samples = .samples, .func = .func,
                                         .args = .args, .l_targets = .l_targets,
                                         .l_params = .l_params,
                                         .transform = .transform))

  return(posterior)
}

#' Calibrate models using Bayesian methods - employing local IMIS_()
#'
#' @param .b_method Character defining the Bayesian method to use in the
#' calibration process. Currently supported methods are `SIR` and `IMIS`.
#' @param .func A function defining the decision analytic model to be
#' calibrated.
#' @param .args A list of arguments passed to the model function.
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .samples A table or vector of sampled parameter values
#' @param .n_resample the desired number of draws from the posterior
#' @param .IMIS_sample the incremental sample size at each IMIS iteration
#' @param .IMIS_iterations the maximum number of iterations in IMIS
#' @param .MCMC_burnIn the number of samples before starting to retain samples
#' @param .MCMC_samples the total number of samples the MCMC algorithm should
#' generate including the burn-in sample including the .MCMC_burnIn. This value
#' should not be equal to or less than .MCMC_burnIn.
#' @param .MCMC_thin the value used to thin the resulting chain
#' @param .MCMC_rerun use the proposal distribution covariance matrix from the
#' first run to re-run the MCMC chain.
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#' @param .diag_ Logical for whether to print diagnostics
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
calibrateModel_beyesian <- function(.b_method = "SIR", .func, .args,
                                    .l_targets, .l_params, .samples,
                                    .n_resample = 1000,
                                    .IMIS_sample = 1000,
                                    .IMIS_iterations = 30,
                                    .MCMC_burnIn = 10000,
                                    .MCMC_samples = 50000,
                                    .MCMC_thin = 5,
                                    .MCMC_rerun = TRUE,
                                    .transform = FALSE,
                                    .diag_ = FALSE) {
  # Ensure that .b_method is supported by the function:
  stopifnot(".b_method is not supported by the function" =
              any(.b_method %in% c('SIR', 'IMIS', 'MCMC')))

  # If user did not set an IMIS sample size:
  if(.b_method == 'IMIS' & is.null(.IMIS_sample))
    .IMIS_sample <- 1000

  # If user set a .MCMC_burnIn equal to or more than .MCMC_samples:
  if(.b_method == 'MCMC' & !is.null(.MCMC_samples) & !is.null(.MCMC_burnIn)) {
    # ensure we can thin the chain as needed:
    if(.MCMC_samples < .n_resample * .MCMC_thin)
      .MCMC_samples <- .n_resample * .MCMC_thin
    # make sure samples are more than burn-in to accommodate it:
    if(.MCMC_burnIn >= .MCMC_samples)
      .MCMC_samples <- .MCMC_burnIn * 2
  }

  # SIR:
  if(.b_method == 'SIR') {
    if(nrow(.samples) < .n_resample)
      stop(paste("Please pass", .n_resample, "samples to the function."))
    ## Calculate log-likelihood for each sample value:
    llik <- calibR::log_likelihood(.samples = .samples, .func = .func,
                                   .args = .args, .l_targets = .l_targets)

    ## Calculate weights for the re-sample:
    # Note: subtracting off the maximum log-likelihood before
    # exponentiating helps avoid numerical under/overflow, which would
    # result in weights of Inf or 0.
    weight <- exp(llik - max(llik)) / sum(exp(llik - max(llik)))
    ## Re-sample from samples with wt as sampling weights:
    SIR_resample  <- sample.int(
      n = length(weight),
      size = .n_resample,
      replace = TRUE,
      prob = weight)
    posterior_SIR <- .samples[SIR_resample, ]
    ## Combine log-likelihood & posterior probability of each sample:
    SIR_results <- cbind(posterior_SIR,
                         "Overall_fit" = llik[SIR_resample],
                         "Posterior_prob" = weight[SIR_resample]) %>%
      as.data.frame() %>%
      dplyr::arrange(dplyr::desc(Overall_fit))

    return(list('Results' = SIR_results, 'Method' = "SIR"))

  } else if(.b_method == 'IMIS') { # IMIS:
    ## Run IMIS:
    fit_IMIS <- calibR::IMIS_(
      B = .IMIS_sample, # the incremental sample size at each IMIS iteration
      B.re = .n_resample, # the desired posterior sample size
      number_k = .IMIS_iterations, # the maximum number of iterations in IMIS
      D = 1, # use optimizer >= 1, do not use = 0.
      sample.prior = calibR::sample_prior_IMIS,
      prior = calibR::calculate_prior,
      priors = calibR::calculate_priors,
      likelihood = calibR::calculate_likelihood,
      .l_params_ = .l_params, # prior/sample.prior
      .func_ = .func, # calculate_likelihood
      .args_ = .args, # calculate_likelihood
      .l_targets_ = .l_targets, # calculate_likelihood
      .transform_ = .transform) # prior
    ## Calculate log-likelihood (overall fit) and posterior probability:
    m_calib_res <- fit_IMIS$resample
    Overall_fit <- calibR::log_likelihood(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets)
    Posterior_prob <- calibR::calculate_posteriors(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets, .l_params = .l_params)
    ## Group results into one object:
    IMIS_results <- m_calib_res %>%
      dplyr::as_tibble(
        ~ vctrs::vec_as_names(...,
                              repair = "unique",
                              quiet = TRUE)) %>%
      dplyr::mutate(
        "Overall_fit" = Overall_fit,
        "Posterior_prob" = Posterior_prob / sum(Posterior_prob)) %>%
      dplyr::arrange(dplyr::desc(Overall_fit))
    ## Assign column names in the IMIS stats object:
    stats <- fit_IMIS$stat %>%
      as.data.frame() %>%
      `colnames<-`(c("MargLike", "UniquePoint", "MaxWeight", "ESS",
                     "ImpWt", "ImpWtVar"))

    return(list('Results' = IMIS_results, 'Method' = "IMIS",
                'Fit'  = fit_IMIS, 'Stats' = stats))

  } else { # MCMC MH algorithm
    ## Sample a random set of parameters as a starting point for the chain:
    guess <- calibR::sample_prior_RGS_(
      .n_samples = 1,
      .l_params = .l_params)
    if(.diag_)
      cat(paste0(guess, "\n"))
    ## Run the Metropolis-Hastings algorithm:
    fit_MCMC <- tryCatch(
      expr = {
        MHadaptive::Metro_Hastings(
          li_func = calibR::log_posterior,
          pars = guess,
          par_names = .l_params[["v_params_names"]],
          iterations = .MCMC_samples,
          burn_in = .MCMC_burnIn,
          .func = .func,
          .l_targets = .l_targets,
          .l_params = .l_params,
          .args = .args,
          .transform = .transform)
      }, error = function(e) {
        message(paste0("\r", e))
        ### Sample new values:
        guess <- calibR::sample_prior_RGS_(
          .n_samples = 1,
          .l_params = .l_params)
        if(.diag_)
          cat(paste0(guess, "\n"))
        ### Run MH again:
        MHadaptive::Metro_Hastings(
          li_func = calibR::log_posterior,
          pars = guess,
          par_names = .l_params[["v_params_names"]],
          iterations = .MCMC_samples,
          burn_in = .MCMC_burnIn,
          .func = .func,
          .l_targets = .l_targets,
          .l_params = .l_params,
          .args = .args,
          .transform = .transform)
      })

    # If MCMC was to be rerun:
    if(.MCMC_rerun){
      guess <- calibR::sample_prior_RGS_(
        .n_samples = 1,
        .l_params = .l_params)
      if(.diag_)
        cat(paste0(guess, "\n"))
      fit_MCMC <- tryCatch(
        expr = {
          MHadaptive::Metro_Hastings(
            li_func = calibR::log_posterior,
            pars = guess,
            prop_sigma = fit_MCMC$prop_sigma,
            par_names = .l_params[["v_params_names"]],
            iterations = .MCMC_samples,
            burn_in = .MCMC_burnIn,
            .func = .func,
            .l_targets = .l_targets,
            .l_params = .l_params,
            .args = .args,
            .transform = .transform)
        }, error = function(e) {
          message(paste0("\r", e))
          ### Sample new values:
          guess <- calibR::sample_prior_RGS_(
            .n_samples = 1,
            .l_params = .l_params)
          if(.diag_)
            cat(paste0(guess, "\n"))
          ### Run MH again:
          MHadaptive::Metro_Hastings(
            li_func = calibR::log_posterior,
            pars = guess,
            prop_sigma = fit_MCMC$prop_sigma,
            par_names = .l_params[["v_params_names"]],
            iterations = .MCMC_samples,
            burn_in = .MCMC_burnIn,
            .func = .func,
            .l_targets = .l_targets,
            .l_params = .l_params,
            .args = .args,
            .transform = .transform)
          })}
    ## Estimate 95% credible interval:
    cred_int_95 <- MHadaptive::BCI(
      mcmc_object = fit_MCMC,
      interval = c(0.025, 0.975))
    ## Thin the chain if needed:
    if(!is.null(.MCMC_thin))
      fit_MCMC <- MHadaptive::mcmc_thin(
        mcmc_object = fit_MCMC,
        thin = .MCMC_thin)
    ## Calculate log-likelihood (overall fit) and posterior probability:
    m_calib_res <- fit_MCMC$trace
    colnames(m_calib_res) <- .l_params[["v_params_names"]]
    Overall_fit <- calibR::log_likelihood(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets)
    Posterior_prob <- calibR::calculate_posteriors(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets, .l_params = .l_params)
    ## Group results into one object:
    MCMC_results <- m_calib_res %>%
      as.data.frame() %>%
      dplyr::mutate(
        "Overall_fit" = Overall_fit,
        "Posterior_prob" = Posterior_prob / sum(Posterior_prob)) %>%
      dplyr::arrange(dplyr::desc(Overall_fit)) %>%
      # randomly sample from the posterior, if required sample smaller than draws:
      {if(.n_resample < nrow(.)){
        dplyr::slice_sample(.data = ., n = .n_resample)
      } else {
        .
      }}

    ## Assign column names to the MCMC proposal distribution covariance matrix:
    prop_sigma <- fit_MCMC$prop_sigma %>%
      as.data.frame() %>%
      `colnames<-`(.l_params[["v_params_names"]])

    return(list('Results' = MCMC_results, 'Method' = "MCMC",
                'Cred_int_95' = cred_int_95, 'Fit'  = fit_MCMC,
                'Propos_dist_Cov_matrix' = prop_sigma))
  }
}

#' Calibrate models using Bayesian methods - employing IMIS::IMIS()
#'
#' @param .b_method Character defining the Bayesian method to use in the
#' calibration process. Currently supported methods are `SIR` and `IMIS`.
#' @param .func A function defining the decision analytic model to be
#' calibrated.
#' @param .args A list of arguments passed to the model function.
#' @param .l_targets A list containing a vector of targets' names, a vector
#' of targets' weights, a vector of targets' distributions, and a table for
#' each target that contains the values (column name 'value') and standard
#' errors (column name 'sd') of the corresponding target.
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .samples A table or vector of sampled parameter values
#' @param .n_resample the desired number of draws from the posterior
#' @param .IMIS_sample the incremental sample size at each IMIS iteration
#' @param .IMIS_iterations the maximum number of iterations in IMIS
#' @param .transform Logical for whether to back-transform parameters to
#' their original scale.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
calibrateModel_beyesian2 <- function(.b_method = "SIR", .func, .args,
                                     .l_targets, .l_params, .samples,
                                     .n_resample = 1000,
                                     .IMIS_sample = 1000,
                                     .IMIS_iterations = 30,
                                     .transform = FALSE) {
  # Ensure that .b_method is supported by the function:
  stopifnot(".b_method is supported by the function" =
              any(.b_method %in% c('SIR', 'IMIS', 'MCMC')))

  if(.b_method == 'IMIS' & is.null(.IMIS_sample))
    .IMIS_sample <- 1000

  # SIR:
  if(.b_method == 'SIR') {
    ## Calculate log-likelihood for each sample value:
    llik <- calibR::log_likelihood(.samples = .samples, .func = .func,
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
      dplyr::arrange(dplyr::desc(Overall_fit))

    return(list('Results' = SIR_results, 'Method' = "SIR"))

  } else if(.b_method == 'IMIS') { # IMIS:
    ## Define three functions needed by IMIS:
    ### prior(x), likelihood(x), sample.prior(n)
    prior <<- calibR::calculate_priors
    likelihood <<- calibR::calculate_likelihood
    sample.prior <<- calibR::sample_prior_IMIS
    ## Define function inputs:
    .l_params_ <<- .l_params # prior/sample.prior
    .func_ <<- .func # calculate_likelihood
    .args_ <<- .args # calculate_likelihood
    .l_targets_ <<- .l_targets # calculate_likelihood
    .transform_ <<- .transform # prior/
    ## Run IMIS:
    fit_IMIS <- IMIS::IMIS(
      B = .IMIS_sample, # the incremental sample size at each IMIS iteration
      B.re = .n_resample, # the desired posterior sample size
      number_k = 10, # the maximum number of iterations in IMIS
      D = 0)

    ## calculate fitness of the draws from posterior:
    m_calib_res <- fit_IMIS$resample
    Overall_fit <- calibR::log_likelihood(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets)
    Posterior_prob <- calibR::calculate_posteriors(
      .samples = m_calib_res, .func = .func, .args = .args,
      .l_targets = .l_targets, .l_params = .l_params)
    ## Calculate log-likelihood (overall fit) and posterior probability:
    IMIS_results <- m_calib_res %>%
      dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                             repair = "unique",
                                             quiet = TRUE)) %>%
      dplyr::mutate(
        "Overall_fit" = Overall_fit,
        "Posterior_prob" = Posterior_prob) %>%
      dplyr::arrange(dplyr::desc(Overall_fit))
    ## Name column names IMIS stats object:
    stats <- fit_IMIS$stat %>%
      dplyr::as_tibble(~ vctrs::vec_as_names(...,
                                             repair = "unique",
                                             quiet = TRUE)) %>%
      `colnames<-`(c("MargLike", "UniquePoint", "MaxWeight", "ESS",
                     "ImpWt", "ImpWtVar"))

    return(list('Results' = IMIS_results, 'Method' = "IMIS",
                'Fit'  = fit_IMIS, 'Stats' = stats))

  } else {

  }
}

#' Estimate Effective Sample Size from Bayesian calibration outputs
#'
#'
#' @param bayes_calib_output_list List of outputs from the Bayesian
#' calibration
#' @param .results Name of calibration PSA draws table in the list
#' @param .post_prob_ Name of column where posterior probability are found
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
effective_sample_size <- function(bayes_calib_output_list,
                                  .results = "Results",
                                  .post_prob_ = "Posterior_prob") {


  bayes_calib_output_list[[.results]] %>%
    dplyr::count(.data[[.post_prob_]]) %>%
    dplyr::summarise(sum(n)^2/sum(n^2)) %>%
    dplyr::pull()
}
