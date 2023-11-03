#' Infectious Disease Markov Model with transformation
#'
#' @param .v_params_ Named numeric vector of model parameters. If not named,
#' then the expected order is: `"mu_e"`, `"mu_l"`, `"mu_t"`, `"p"`, `"r_l"`,
#' `"r_e"`, `"rho"`, `"b"`, `"c"`. Default is `NULL`.
#' @param calibrate_ Logical scalar, if `TRUE` (default), the model outputs
#' natural history data; otherwise, discounted outcomes (costs and QALYs)
#' are returned.
#' @param transform_ Logical scalar, for whether to expect transformed
#' parameters and therefore back-transform them before they go into the model.
#' Default is `TRUE`.
#' @param mu_e Numeric scalar representing the cause-specific mortality rate
#' with early-stage disease. Default model value is `0.04 [0.036, 0.044]` or
#' `log(0.04)` if `transform_` is `TRUE`.
#' @param mu_l Numeric scalar representing the cause-specific mortality rate
#' with late-stage disease. Default model value is `0.15 [0.13, 0.17]` or
#' `log(0.15)` if `transform_` is `TRUE`.
#' @param mu_t Numeric scalar representing the cause-specific mortality rate on
#' treatment. Default model value is `0.016 [0.013, 0.018]` or `log(0.016)` if
#' `transform_` is `TRUE`.
#' @param p Numeric scalar representing the transition rate from early to
#' late-stage disease. Default model value is `0.12 [0.09, 0.15]` or
#' `log(0.12)` if `transform_` is `TRUE`.
#' @param r_l Numeric scalar representing the rate of uptake onto treatment for
#' those at late-stage disease. Default model value is `0.41 [0.35, 0.48]` or
#' `log(0.41)` if `transform_` is `TRUE`.
#' @param rho Numeric scalar representing the effective contact rate. Default
#' model value is `0.53 [0.50, 0.55]` or `log(0.53)` if `transform_` is `TRUE`.
#' @param b Numeric scalar representing the fraction of population in at-risk
#' group. Default model value is `0.21 [0.18, 0.23]` or
#' `calibR::prob_to_logit(0.21)` if `transform_` is `TRUE`.
#'
#' @return This function generates two sets of outputs depending on whether the
#' `calibrate_` was set to `TRUE` or `FALSE`. These outputs include Survival,
#' Prevalence, and Treatment Volume under calibration and Costs and Life Years
#' otherwise.
#' @export
#'
#' @examples
#' \dontrun{
#' CE_results <- infectious_disease_stm()
#' }
infectious_disease_stm <- function(
    .v_params_ = NULL,
    calibrate_ = TRUE,
    transform_ = TRUE,
    mu_e = log(0.04),
    mu_l = log(0.15),
    mu_t = log(0.016),
    p = log(0.12),
    r_l = log(0.41),
    rho = log(0.53),
    b = calibR::prob_to_logit(0.21)) {

  with(as.list(.v_params_), {
    # r_e: 0 Rate of uptake onto treatment (r_e = early-stage disease)
    # a: 15,000 Annual birth rate
    # c: $1000 [662, 1451] Annual cost of treatment
    # pop_size: population size hard-coded as 1 million
    # mu_b: background mortality rate hard-coded as 0.015
    pop_size <- 1e6; mu_b <- 0.015; c <- 1000

    # Back transform transformed parameters:
    if(isTRUE(transform_)) {
      mu_e <- exp(mu_e)
      mu_l <- exp(mu_l)
      mu_t <- exp(mu_t)
      rho <- exp(rho)
      p <- exp(p)
      r_l <- exp(r_l)
      b <- calibR::logit_to_prob(b)
    }

    # Assume early and late disease stage treatment uptake are equal:
    r_e <- r_l

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
        if(isTRUE(calibrate_)) {
          trace[m,] <- P0 <- P1
        } else {
          P0 <- P1
          trace[m,] <- P1/sum(P1)
        }
      }
      # Save results for each scenario:
      results[[s]] <- trace
    }

    # Report results:
    if(isTRUE(calibrate_)) {
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
        )
      )
    } else {
      # Return cost-effectivenss results:
      return(
        list(
          # Trace without expanded treatment access:
          # trace0 = results[[1]],
          # Trace with expanded treatment access:
          # trace1 = results[[2]],
          "Status Quo" = list(
            "Costs" = (sum(results[[1]][(30 * 12 + 1):(51 * 12), 5]) / 12) * c,
            "LY" = sum(results[[1]][(30 * 12 + 1):(51 * 12), -6]) / 12
          ),
          "Expanded Treatment" = list(
            "Costs" = (sum(results[[2]][(30 * 12 + 1):(51 * 12), 5]) / 12) * c,
            "LY" = sum(results[[2]][(30 * 12 + 1):(51 * 12), -6]) / 12
          )
        )
      )
    }
  })
}
