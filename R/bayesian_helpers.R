#' Title
#'
#' @param .samples
#' @param .l_params
#'
#' @return
#' @export
#'
#' @examples
calc_log_prior <- function(.samples, .l_params) {
  v_params_names <- .l_params[['v_params_names']]
  n_params <- length(v_params_names)

  if(is.null(dim(.samples))) { # If vector, change to matrix
    .samples <- t(.samples)
  }
  colnames(.samples) <- v_params_names
  # Define inputs list for the pmap function:
  l_lprior <- list(.l_params[['v_params_names']],
                   paste0('d', .l_params[['v_params_dists']]),
                   .l_params[['v_params_dists']],
                   .l_params[['args']],
                   .samples)
  # Estimate the log prior:
  v_lprior <- pmap_dbl(
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
  )

  v_lprior <- sum(v_lprior)

  return(v_lprior)

}

