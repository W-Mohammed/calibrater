#' Use Latin Hypercube Sampling (LHS) to sample from prior distribution
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .n_samples An integer specifying the number of samples to be
#' generated.
#' @param .seed_no Integer for a random number generator seed number.
#'
#' @return A table with each parameter LHS samples in a separate column
#' @export
#'
#' @examples
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#'
#' sample_prior_LHS(.l_params = list(v_params_names = v_params_names,
#'                  v_params_dists = v_params_dists, args = args),
#'                  .n_samples = 10)
#'
sample_prior_LHS <- function(.l_params, .n_samples, .seed_no = 1) {
  set.seed(.seed_no)
  # Get the number of parameters:
  n_params <- length(.l_params[["v_params_names"]])
  # Get LHS samples:
  tbl_lhs_unit <- lhs::randomLHS(.n_samples, n_params) %>%
    as_tibble()
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
             if(.dist != 'lnorm') {
               exec(.func,
                    p,
                    !!!.arg)
             } else {
               exec(.func,
                    p,
                    log(.arg$mean) - (1/2) * .arg$sd^2,
                    .arg$sd)
             })
    })

  return(tbl_lhs_samp)
}

#' Use Full Factorial Grid Sampling (FGS) to sample from prior distribution
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .n_samples An integer specifying the number of samples to be
#' generated.
#' @param .seed_no Integer for a random number generator seed number.
#'
#' @return A table with each parameter FGS samples in a separate column
#' @export
#'
#' @examples
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#'
#' sample_prior_FGS(.l_params = list(v_params_names = v_params_names,
#'                  v_params_dists = v_params_dists, args = args),
#'                  .n_samples = 10)
#'
sample_prior_FGS <- function(.l_params, .n_samples, .seed_no = 1) {
  set.seed(.seed_no)
  # Define inputs list:
  l_fgs <- list(.l_params[['v_params_names']],
                .l_params[['v_params_dists']],
                .l_params[['args']])
  # Make sure parameter names are in a named vector:
  names(l_fgs[[1]]) <- l_fgs[[1]]
  # Get grid points for each variable:
  tbl_grid_points <- pmap_dfc(
    .l = l_fgs,
    .f = function(.name, .dist, .arg) {
      assign(.name,
             if(.dist == 'unif') {
               seq(from = .arg$min,
                   to = .arg$max,
                   length.out = .n_samples)
             } else if (.dist == 'norm'){
               seq(from = .arg$mean - .arg$sd * 3,
                   to = .arg$mean + .arg$sd * 3,
                   length.out = .n_samples)
             })
    })

  tbl_fgs_samp <- do.call(expand.grid, tbl_grid_points)

  return(tbl_fgs_samp)
}

#' Use Random Grid Sampling (RGS) to sample from prior distribution
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .n_samples An integer specifying the number of samples to be
#' generated.
#' @param .seed_no Integer for a random number generator seed number.
#'
#' @return A table with each parameter RGS samples in a separate column
#' @export
#'
#' @examples
#' v_params_names <- c("p_Mets", "p_DieMets")
#' v_params_dists <- c("unif", "unif")
#' args <- list(list(min = 0.04, max = 0.16),
#'              list(min = 0.04, max = 0.12))
#'
#' sample_prior_RGS(.l_params = list(v_params_names = v_params_names,
#'                  v_params_dists = v_params_dists, args = args),
#'                  .n_samples = 10)
#'
sample_prior_RGS <- function(.l_params, .n_samples, .seed_no = 1) {
  set.seed(.seed_no)
  # Define inputs list:
  l_rgs <- list(.l_params[['v_params_names']],
                paste0('r', .l_params[['v_params_dists']]),
                .l_params[['args']],
                .l_params[['v_params_dists']])
  # Make sure parameter names are in a named vector:
  names(l_rgs[[1]]) <- l_rgs[[1]]
  # Map over parameters and sample values accordingly:
  tbl_rgs_samp <- pmap_dfc(
    .l = l_rgs,
    .f = function(.name, .func, .arg, .dist) {
      assign(.name,
             if(.dist != 'lnorm') {
               exec(.func,
                    .n_samples,
                    !!!.arg)
             } else {
               exec(.func,
                    .n_samples,
                    log(.arg$mean) - (1/2) * .arg$sd^2,
                    .arg$sd)
             })
    })

  return(tbl_rgs_samp)
}
