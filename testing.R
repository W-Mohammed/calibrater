sample.prior.lhs <- function(n) {
  # n: the number of samples desired
  draws0 <- randomLHS(n=n,k=8)
  draws  <- data.frame( mu_e  = qlnorm(draws0[,1],log(0.05)-1/2*0.5^2,0.5),
                        mu_l  = qlnorm(draws0[,2],log(0.25)-1/2*0.5^2,0.5),
                        mu_t  = qlnorm(draws0[,3],log(0.025)-1/2*0.5^2,0.5),
                        p     = qlnorm(draws0[,4],log(0.1)-1/2*0.5^2,0.5),
                        r_l   = qlnorm(draws0[,5],log(0.5)-1/2*0.5^2,0.5),
                        rho   = qlnorm(draws0[,6],log(0.5)-1/2*0.5^2,0.5),
                        b     = qbeta(draws0[,7],2,8),
                        c     = qlnorm(draws0[,8],log(1000)-1/2*0.2^2,0.2)
  )
  return(as.matrix(draws))
}

sample_prior_LHS <- function(.l_params, .n_samples) {
  # Get the number of parameters:
  n_params <- length(.l_params[["v_params_names"]])
  # Get LHS samples:
  tbl_lhs_unit <- lhs::randomLHS(.n_samples, n_params) %>%
    as_tibble()
  # Make sure parameter names are in a named vector:
  names(.l_params[['v_params_names']]) <- .l_params[['v_params_names']]
  # Define inputs list:
  l_lhs <- list(.l_params[['v_params_names']],
                tbl_lhs_unit,
                paste0('q', .l_params[['v_params_dists']]),
                .l_params[['args']])
  # Map over parameters to scale up LHS samples to appropriate values:
  tbl_lhs_samp <- pmap_dfc(
    .l = l_lhs,
    .f = function(.name, p, .func, .arg) {
      names(.name) = .name
      assign(.name,
             exec(.func, p, !!!.arg))
    })

  return(tbl_lhs_samp)
}

sample_prior_FGS <- function(.l_params, .n_samples) {
  # Define inputs list:
  l_fgs <- list(.l_params[['v_params_names']],
                paste0('r', .l_params[['v_params_dists']]),
                .l_params[['args']])
  # Make sure parameter names are in a named vector:
  names(l_fgs[[1]]) <- l_fgs[[1]]
  # Map over parameters and sample values accordingly:
  tbl_fgs_samp <- pmap_dfc(
    .l = l_fgs,
    .f = function(.name, .func, .arg) {
      assign(.name,
             exec(.func, .n_samples, !!!.arg))
    })

  return(tbl_fgs_samp)
}

sample_prior_RGS <- function(.l_params, .n_samples) {
  # Define inputs list:
  l_rgs <- list(.l_params[['v_params_names']],
                paste0('r', .l_params[['v_params_dists']]),
                .l_params[['args']])
  # Make sure parameter names are in a named vector:
  names(l_rgs[[1]]) <- l_rgs[[1]]
  # Map over parameters to scale up LHS samples to appropriate values:
  tbl_lhs_samp <- pmap_dfc(
    .l = l_lhs,
    .f = function(.name, p, .func, .arg) {
      names(.name) = .name
      assign(.name,
             exec(.func, p, !!!.arg))
    })

  return(tbl_lhs_samp)
}

log_likelihood <- function(.func, .args = NULL, .samples, .l_targets) {
  # Run the model using each set of sampled parameters:
  model_results <- pmap(
    .l = .samples,
    .f = function(.params) {
      exec(.func, .params, !!!args)
    }
  )
  # Define inputs list for the log likelihood function:
  l_llk <- list(.l_targets[['v_targets_names']],
                paste0('d', .l_targets[['v_targets_dists']]),
                .l_targets[['v_targets_dists']],
                model_results)
  log_likelihood <- pmap_dbl(
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
}

## Record start time of calibration:
t_init <- Sys.time()

## Initialize goodness-of-fit vector
m_GOF_llk <- m_GOF_sse <- matrix(nrow = n_samples, ncol = n_target)
colnames(m_GOF_llk) <- colnames(m_GOF_sse) <- paste0(v_target_names, "_fit")

## Loop through sampled sets of input values
tmps = matrix(NA, nrow = nrow(tst), ncol = 1)
for (j in 1:nrow(tst)) {

  tmp = as_vector(tst[j, ])
  ### Run model for a given parameter set:
  model_res <- CRS_markov(v_params = tmp)

  ### Calculate goodness-of-fit of model outputs to targets:

  ### TARGET 1: Survival ("Surv")
  ### Log likelihood:
  tmps[j, 1] <- sum(dnorm(x = lst_targets$Surv$value,
                               mean = model_res$Surv,
                               sd = lst_targets$Surv$se,
                               log = TRUE))


} # End loop over sampled parameter sets
