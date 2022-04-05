#' Hypothetical Infectious Disease (HID) Markov Model
#'
#' @param .v_params_ a named vector of model parameters in the following
#' order: "mu_e", "mu_l", "mu_t", "p", "r_l", "r_e", "rho", "b", "c".
#' @param calibrate_ If \code{TRUE} (default), the model outputs natural
#' history data; otherwise, discounted outcomes \code{(costs and QALYs)}
#' are returned.
#' @param mu_e Cause-specific mortality rate with early-stage disease
#' @param mu_l Cause-specific mortality rate with late-stage disease
#' @param mu_t Cause-specific mortality rate on treatment
#' @param p Transition rate from early to late-stage disease
#' @param r_l Rate of uptake onto treatment (r_l = late-stage disease)
#' @param rho Effective contact rate
#' @param b Fraction of population in at-risk group
#'
#' @return
#' @export
#'
#' @examples
HID_markov <- function(.v_params_ = NULL, calibrate_ = TRUE, mu_e = 0.05,
                       mu_l = 0.25, mu_t = 0.025, p = 0.10, r_l = 0.5,
                       rho = 0.025, b = 0.20) {
  with(as.list(.v_params_), {
    # mu_e: 0.05 [0.02, 0.12] Cause-specific mortality rate with early-stage disease
    # mu_l: 0.25 [0.08, 0.59] Cause-specific mortality rate with late-stage disease
    # mu_t: 0.025 [0.01, 0.06] Cause-specific mortality rate on treatment
    # p: 0.10 [0.03, 0.24] Transition rate from early to late-stage disease
    # r_l: 0.50 [0.17, 1.18] Rate of uptake onto treatment (r_l = late-stage disease)
    # r_e: 0 Rate of uptake onto treatment (r_e = early-stage disease)
    # rho: 0.025 [0.01, 0.06] Effective contact rate
    # a: 15,000 Annual birth rate
    # b: 0.20 [0.03, 0.48] Fraction of population in at-risk group
    # c: $1000 [662, 1451] Annual cost of treatment
    pop_size <- 1e6; mu_b <- 0.015; c <- 1000; r_e <- r_l

    # Prepare to run model:
    # Years to simulate (30 to present, 51 for 20 year analytic horizon):
    n_yrs    <- if(!calibrate_) { 51 } else { 30 }
    # Scenarios to simulate: 1 = base case, 2 = expanded treatment access:
    sim      <- if(!calibrate_) { 1:2 } else { 1 }
    # Vector of mortality rates:
    v_mu     <- c(0, 0, mu_e, mu_l, mu_t) + mu_b
    # Calculate birth rate for equilibrium population before epidemic:
    births   <- pop_size * mu_b * c(1-b, b)
    # Creates starting vector for population:
    init_pop <- pop_size * c(1 - b, b - 0.001, 0.001, 0, 0, 0)
    # Creates a table to store simulation trace:
    trace    <- matrix(NA, 12 * n_yrs, 6)
    colnames(trace) <- c("N", "S", "E", "L", "T", "D")
    # Creates a list to store results:
    results  <- list()

    # Run model:
    for(s in sim) {
      P0 <- P1 <- init_pop
      for(m in 1:(12 * n_yrs)) {
        # Calculates force of infection: "Lambda"
        lambda    <- rho * sum(P0[3:4]) / sum(P0[2:5])
        # Births
        P1[1:2]   <- P1[1:2] + births / 12
        # Deaths: N, S, E, L, T, to D
        P1[-6]    <- P1[-6] - P0[-6] * v_mu / 12
        # Deaths: N, S, E, L, T, to D
        P1[6]     <- P1[6] + sum(P0[-6] * v_mu / 12)
        # Infection: S to E
        P1[2]     <- P1[2] - P0[2] * lambda / 12
        # Infection: S to E
        P1[3]     <- P1[3] + P0[2] * lambda / 12
        # Progression: E to L
        P1[3]     <- P1[3] - P0[3] * p / 12
        # Progression: E to L
        P1[4]     <- P1[4] + P0[3] * p / 12
        # Treatment uptake: L to T
        P1[4]     <- P1[4] - P0[4] * r_l / 12
        # Treatment uptake: L to T
        P1[5]     <- P1[5] + P0[4] * r_l / 12
        if(s == 2 & m > (12 * 30)) {
          # Treatment uptake: E to T (scenario 2)
          P1[3]   <- P1[3] - P0[3] * r_e / 12
          # Treatment uptake: E to T (scenario 2)
          P1[5]   <- P1[5] + P0[3] * r_e / 12
        }
        # Fill trace, reset pop vectors:
        trace[m,] <- P0 <- P1
      }
      # Save results for each scenario:
      results[[s]] <- trace
    }

    # Report results:
    if(calibrate_) {
      # Return calibration metrics:
      return(
        list(
          # Prevalence at 10, 20, 30 years:
          Prev = (rowSums(trace[, 3:5]) /
                    rowSums(trace[, 1:5]))[c(10, 20, 30) * 12],
          # HIV survival without treatment:
          Surv = 1/(v_mu[3] + p) + p / (v_mu[3] + p) * (1 / v_mu[4]),
          # Treatment volume at 30 years:
          Trt_vol = trace[30 * 12, 5]
        ) )
    } else {
      # Policy projections for CE analysis:
      return(
        list(
          # # Trace without expanded treatment access:
          # trace0 = results[[1]],
          # # Trace with expanded treatment access:
          # trace1 = results[[2]],
          # Incremental LY lived with expanded tx:
          'inc_LY' = sum(results[[2]][(30 * 12 + 1):(51 * 12), -6] -
                         results[[1]][(30 * 12 + 1):(51 * 12), -6]) / 12,
          # Incremental costs with expanded tx:
          'inc_cost' = sum(results[[2]][(30 * 12 + 1):(51 * 12), 5] -
                           results[[1]][(30 * 12 + 1):(51 * 12), 5]) *
            c / 12))}
  })
}

#' Hypothetical Infectious Disease (HID) Markov Model
#'
#' @param .v_params_ a named vector of model parameters in the following
#' order: "mu_e", "mu_l", "mu_t", "p", "r_l", "r_e", "rho", "b", "c".
#' @param calibrate_ If \code{TRUE} (default), the model outputs natural
#' history data; otherwise, discounted outcomes \code{(costs and QALYs)}
#' are returned.
#' @param transform_ Expect transformed parameters and therefore
#' back-transform them before they go into the model
#' @param mu_e Cause-specific mortality rate with early-stage disease
#' @param mu_l Cause-specific mortality rate with late-stage disease
#' @param mu_t Cause-specific mortality rate on treatment
#' @param p Transition rate from early to late-stage disease
#' @param r_l Rate of uptake onto treatment (r_l = late-stage disease)
#' @param rho Effective contact rate
#' @param b Fraction of population in at-risk group
#'
#' @return
#' @export
#'
#' @examples
HID_markov_2 <- function(.v_params_ = NULL, calibrate_ = TRUE,
                       transform_ = TRUE, mu_e = log(0.05),
                       mu_l = log(0.25), mu_t = log(0.025),
                       p = prob_to_logit(0.10), r_l = log(0.5),
                       rho = log(0.025), b = prob_to_logit(0.20)) {
  with(as.list(.v_params_), {
    # mu_e: 0.05 [0.02, 0.12] Cause-specific mortality rate with early-stage disease
    # mu_l: 0.25 [0.08, 0.59] Cause-specific mortality rate with late-stage disease
    # mu_t: 0.025 [0.01, 0.06] Cause-specific mortality rate on treatment
    # p: 0.10 [0.03, 0.24] Transition rate from early to late-stage disease
    # r_l: 0.50 [0.17, 1.18] Rate of uptake onto treatment (r_l = late-stage disease)
    # r_e: 0 Rate of uptake onto treatment (r_e = early-stage disease)
    # rho: 0.025 [0.01, 0.06] Effective contact rate
    # a: 15,000 Annual birth rate
    # b: 0.20 [0.03, 0.48] Fraction of population in at-risk group
    # c: $1000 [662, 1451] Annual cost of treatment
    pop_size <- 1e6; mu_b <- 0.015; c <- 1000; r_e <- r_l

    # Back transform transformed parameters:
    if(transform_) {
      mu_e <- exp(mu_e)
      mu_l <- exp(mu_l)
      mu_t <- exp(mu_t)
      rho <- exp(rho)
      p <- logit_to_prob(p)
      r_l <- exp(r_l)
      b <- logit_to_prob(b)
    }

    # Prepare to run model:
    # Years to simulate (30 to present, 51 for 20 year analytic horizon):
    n_yrs    <- if(!calibrate_) { 51 } else { 30 }
    # Scenarios to simulate: 1 = base case, 2 = expanded treatment access:
    sim      <- if(!calibrate_) { 1:2 } else { 1 }
    # Vector of mortality rates:
    v_mu     <- c(0, 0, mu_e, mu_l, mu_t) + mu_b
    # Calculate birth rate for equilibrium population before epidemic:
    births   <- pop_size * mu_b * c(1-b, b)
    # Creates starting vector for population:
    init_pop <- pop_size * c(1 - b, b - 0.001, 0.001, 0, 0, 0)
    # Creates a table to store simulation trace:
    trace    <- matrix(NA, 12 * n_yrs, 6)
    colnames(trace) <- c("N", "S", "E", "L", "T", "D")
    # Creates a list to store results:
    results  <- list()

    # Run model:
    for(s in sim) {
      P0 <- P1 <- init_pop
      for(m in 1:(12 * n_yrs)) {
        # Calculates force of infection: "Lambda"
        lambda    <- rho * sum(P0[3:4]) / sum(P0[2:5])
        # Births
        P1[1:2]   <- P1[1:2] + births / 12
        # Deaths: N, S, E, L, T, to D
        P1[-6]    <- P1[-6] - P0[-6] * v_mu / 12
        # Deaths: N, S, E, L, T, to D
        P1[6]     <- P1[6] + sum(P0[-6] * v_mu / 12)
        # Infection: S to E
        P1[2]     <- P1[2] - P0[2] * lambda / 12
        # Infection: S to E
        P1[3]     <- P1[3] + P0[2] * lambda / 12
        # Progression: E to L
        P1[3]     <- P1[3] - P0[3] * p / 12
        # Progression: E to L
        P1[4]     <- P1[4] + P0[3] * p / 12
        # Treatment uptake: L to T
        P1[4]     <- P1[4] - P0[4] * r_l / 12
        # Treatment uptake: L to T
        P1[5]     <- P1[5] + P0[4] * r_l / 12
        if(s == 2 & m > (12 * 30)) {
          # Treatment uptake: E to T (scenario 2)
          P1[3]   <- P1[3] - P0[3] * r_e / 12
          # Treatment uptake: E to T (scenario 2)
          P1[5]   <- P1[5] + P0[3] * r_e / 12
        }
        # Fill trace, reset pop vectors:
        trace[m,] <- P0 <- P1
      }
      # Save results for each scenario:
      results[[s]] <- trace
    }

    # Report results:
    if(calibrate_) {
      # Return calibration metrics:
      return(
        list(
          # Prevalence at 10, 20, 30 years:
          Prev = (rowSums(trace[, 3:5]) /
                    rowSums(trace[, 1:5]))[c(10, 20, 30) * 12],
          # HIV survival without treatment:
          Surv = 1/(v_mu[3] + p) + p / (v_mu[3] + p) * (1 / v_mu[4]),
          # Treatment volume at 30 years:
          Trt_vol = trace[30 * 12, 5]
        ) )
    } else {
      # Policy projections for CE analysis:
      return(
        list(
          # # Trace without expanded treatment access:
          # trace0 = results[[1]],
          # # Trace with expanded treatment access:
          # trace1 = results[[2]],
          # Incremental LY lived with expanded tx:
          'inc_LY' = sum(results[[2]][(30 * 12 + 1):(51 * 12), -6] -
                         results[[1]][(30 * 12 + 1):(51 * 12), -6]) / 12,
          # Incremental costs with expanded tx:
          'inc_cost' = sum(results[[2]][(30 * 12 + 1):(51 * 12), 5] -
                           results[[1]][(30 * 12 + 1):(51 * 12), 5]) *
            c / 12))}
  })
}

#' Helper function to set the hypothetical infectious disease (HID) model
#'
#' @param .v_params an un-named vector of model parameters in the following
#' order: "mu_e", "mu_l", "mu_t", "p", "r_l", "r_e", "rho", "b", "c".
#'
#' @return
#' @export
#'
#' @examples
name_HID_params <- function(.v_params) {
  names(.v_params) <- c("mu_e", "mu_l", "mu_t", "p", "r_l", "r_e", "rho",
                        "b", "c")

  return(.v_params)
}
