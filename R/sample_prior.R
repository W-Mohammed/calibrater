#' Use Latin Hypercube Sampling (LHS) to sample from prior distribution
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
sample_prior_LHS <- function(.n_samples = 1, .l_params = .l_params_,
                             ...) {
  # Grab additional arguments:
  dots = list(...)
  if(!is.null(dots[['.ssed_no']]))
    set.seed(dots[['.ssed_no']])
  # Get the number of parameters:
  n_params <- length(.l_params[["v_params_names"]])
  # Get LHS samples:
  tbl_lhs_unit <- lhs::randomLHS(.n_samples, n_params) %>%
    dplyr::as_tibble()
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
    }
  )

  return(tbl_lhs_samp)
}

#' Use Full Factorial Grid Sampling (FGS) to sample from prior distribution
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .n_samples An integer specifying the number of samples to be
#' generated.
#' @param ... additional arguments, for example: .seed_no to set a seed
#' number.
#'
#' @return A table with each parameter FGS samples in a separate column
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
#' sample_prior_FGS(.l_params = l_params,
#'                  .n_samples = 10)
#' }
sample_prior_FGS <- function(.n_samples = 1, .l_params = .l_params_,
                             ...) {
  # Grab additional arguments:
  dots = list(...)
  if(!is.null(dots[['.ssed_no']]))
    set.seed(dots[['.ssed_no']])
  # Adjust .n_samples to get right number of grid points:
  .n_samples_ <- ceiling(
    exp(log(.n_samples)/length(.l_params[['v_params_names']]))
  )
  # Define inputs list:
  l_fgs <- list(.l_params[['v_params_names']],
                .l_params[['v_params_dists']],
                .l_params[['Xargs']])
  # Make sure parameter names are in a named vector:
  names(l_fgs[[1]]) <- l_fgs[[1]]
  # Get grid points for each variable:
  tbl_grid_points <- purrr::pmap_dfc(
    .l = l_fgs,
    .f = function(.name, .dist, .xarg) {
      assign(.name,
             seq(from = .xarg$min,
                 to = .xarg$max,
                 length.out = .n_samples_)
      )
    }
  )

  tbl_fgs_samp <- do.call(expand.grid, tbl_grid_points) %>%
    dplyr::as_tibble() %>%
    dplyr::slice_sample(n = .n_samples)

  return(tbl_fgs_samp)
}

#' Use Random Grid Sampling (RGS) to sample from prior distribution
#'
#' @param .l_params A list that contains a vector of parameter names,
#' distributions and distributions' arguments.
#' @param .n_samples An integer specifying the number of samples to be
#' generated.
#' @param ... additional arguments, for example: .seed_no to set a seed
#' number.
#'
#' @return A table with each parameter RGS samples in a separate column
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
#' sample_prior_RGS(.l_params = l_params,
#'                  .n_samples = 10)
#' }
sample_prior_RGS <- function(.n_samples = 1, .l_params = .l_params_,
                             ...) {
  # Grab additional arguments:
  dots = list(...)
  if(!is.null(dots[['.ssed_no']]))
    set.seed(dots[['.ssed_no']])
  # Define inputs list:
  l_rgs <- list(.l_params[['v_params_names']],
                paste0('r', .l_params[['v_params_dists']]),
                .l_params[['args']],
                .l_params[['v_params_dists']])
  # Make sure parameter names are in a named vector:
  names(l_rgs[[1]]) <- l_rgs[[1]]
  # Map over parameters and sample values accordingly:
  tbl_rgs_samp <- purrr::pmap_dfc(
    .l = l_rgs,
    .f = function(.name, .func, .arg, .dist) {
      assign(.name,
             purrr::exec(.func,
                  .n_samples,
                  !!!.arg)
      )
    }
  )

  return(tbl_rgs_samp)
}
