#' Sick-Sicker (SS) Markov model
#'
#' @param .v_params_ A vector of named vector with values to replace
#' default model parameter values (usually those that require calibration).
#' @param calibrate_ If \code{TRUE} (default), the model outputs natural
#' history data; otherwise, discounted outcomes \code{(costs and QALYs)}
#' are returned.
#' @param p_S1S2 Probability to become sicker when sick
#' @param hr_S1 Hazard ratio of death in sick vs healthy
#' @param hr_S2 Hazard ratio of death in sicker vs healthy
#'
#' @return
#' @export
#'
#' @examples
SS_markov <- function(.v_params_, calibrate_ = TRUE, p_S1S2 = 0.105,
                      hr_S1 = 3, hr_S2 = 10) {
  with(as.list(.v_params_), {
    ## Markov model parameters
    age     <- 25                   # age at baseline
    max_age <- 55                   # maximum age of follow up
    n_t  <- max_age - age           # time horizon, number of cycles
    v_n  <- c("H", "S1", "S2", "D") # the 4 states of the model: Healthy (H), Sick (S1), Sicker (S2), Dead (D)
    n_s <- length(v_n)              # number of health states

    # Transition probabilities and hazard ratios
    p_HD    <- 0.005    # probability to die when healthy
    p_HS1   <- 0.15     # probability to become sick when healthy
    p_S1H   <- 0.5      # probability to become healthy when sick
    # p_S1S2  <- 0.105  # probability to become sicker when sick
    # hr_S1   <- 3      # hazard ratio of death in sick vs healthy
    # hr_S2   <- 10     # hazard ratio of death in sicker vs healthy
    # compute internal paramters as a function of external parameters
    r_HD    <- - log(1 - p_HD) # rate of death in healthy
    r_S1D   <- hr_S1 * r_HD 	 # rate of death in sick
    r_S2D   <- hr_S2 * r_HD  	 # rate of death in sicker
    p_S1D   <- 1 - exp(-r_S1D) # probability to die in sick
    p_S2D   <- 1 - exp(-r_S2D) # probability to die in sicker
    # Cost inputs
    c.H     <- 2000                # cost of remaining one cycle healthy
    c.S1    <- 4000                # cost of remaining one cycle sick
    c.S2    <- 15000               # cost of remaining one cycle sicker
    c.Trt   <- 12000               # cost of treatment (per cycle)
    # Utility inputs
    u.H     <- 1                   # utility when healthy
    u.S1    <- 0.75                # utility when sick
    u.S2    <- 0.5                 # utility when sicker
    u.Trt   <- 0.95                # utility when being treated


    ####### INITIALIZATION ##########################################
    # create the cohort trace
    m_M <- matrix(NA, nrow = n_t + 1 ,
                  ncol = n_s,
                  dimnames = list(0:n_t, v_n))     # create Markov trace (n_t + 1 because R doesn't understand  Cycle 0)

    m_M[1, ] <- c(1, 0, 0, 0)                      # initialize Markov trace

    # create transition probability matrix for NO treatment
    m_P <- matrix(0,
                  nrow = n_s,
                  ncol = n_s,
                  dimnames = list(v_n, v_n))
    # fill in the transition probability array
    ### From Healthy
    m_P["H", "H"]  <- 1 - (p_HS1 + p_HD)
    m_P["H", "S1"] <- p_HS1
    m_P["H", "D"]  <- p_HD
    ### From Sick
    m_P["S1", "H"]  <- p_S1H
    m_P["S1", "S1"] <- 1 - (p_S1H + p_S1S2 + p_S1D)
    m_P["S1", "S2"] <- p_S1S2
    m_P["S1", "D"]  <- p_S1D
    ### From Sicker
    m_P["S2", "S2"] <- 1 - p_S2D
    m_P["S2", "D"]  <- p_S2D
    ### From Dead
    m_P["D", "D"] <- 1

    # check rows add up to 1
    if (!isTRUE(all.equal(as.numeric(rowSums(m_P)), as.numeric(rep(1, n_s))))) {
      stop("This is not a valid transition Matrix")
    }

    ############# PROCESS ###########################################

    for (t in 1:n_t){                              # throughout the number of cycles
      m_M[t + 1, ] <- m_M[t, ] %*% m_P           # estimate the Markov trace for cycle the next cycle (t + 1)
    }

    ####### EPIDEMIOLOGICAL OUTPUT  ###########################################
    #### Overall Survival (OS) ####
    v_os <- 1 - m_M[, "D"]                # calculate the overall survival (OS) probability for no treatment

    #### Disease prevalence #####
    v_prev <- rowSums(m_M[, c("S1", "S2")])/v_os

    #### Proportion of sick in S1 state #####
    v_prop_S1 <- m_M[, "S1"] / v_prev

    ####### RETURN OUTPUT  ###########################################
    out <- list(Surv = v_os[-1],
                Prev = v_prev[-1],
                PropSick = v_prop_S1[c(11, 21, 31)])

    return(out)
  })
}

#' Sick-Sicker (SS) Micro-simulation model
#'
#' @param .v_params_ A vector of named vector with values to replace
#' default model parameter values (usually those that require calibration).
#' @param calibrate_ If \code{TRUE} (default), the model outputs natural
#' history data; otherwise, discounted outcomes \code{(costs and QALYs)}
#' are returned.
#' @param n_i Number of individuals to simulate
#' @param p_S1S2 Probability to become sicker when sick
#' @param hr_S1 Hazard ratio of death in sick vs healthy
#' @param hr_S2 Hazard ratio of death in sicker vs healthy
#'
#' @return
#' @export
#'
#' @examples
SS_MicroSim <- function(.v_params_ = NULL, calibrate_ = TRUE, n_i = 10000,
                        p_S1S2 = 0.105, hr_S1 = 3, hr_S2 = 10) {
  with(as.list(.v_params_), {
    ## Micro-simulation model parameters:----
    #n_i <- 10000                   # number of simulated individuals
    age     <- 25                   # age at baseline
    max_age <- 55                   # maximum age of follow up
    n_t  <- max_age - age           # time horizon, number of cycles
    v_n  <- c("H", "S1", "S2", "D") # the 4 states of the model: Healthy (H), Sick (S1), Sicker (S2), Dead (D)
    n_s <- length(v_n)              # number of health states
    v_M_1 <- rep(1, n_i)            # everyone begins in the healthy state

    # Discount rates
    d_c <- d_e <- 0.035

    # Transition probabilities and hazard ratios
    p_HD    <- 0.005           # probability to die when healthy
    p_HS1   <- 0.15            # probability to become sick when healthy
    p_S1H   <- 0.5             # probability to become healthy when sick
    # p_S1S2  <- 0.105         # probability to become sicker when sick
    # hr_S1   <- 3             # hazard ratio of death in sick vs healthy
    # hr_S2   <- 10            # hazard ratio of death in sicker vs healthy
    # compute internal paramters as a function of external parameters
    r_HD    <- - log(1 - p_HD) # rate of death in healthy
    r_S1D   <- hr_S1 * r_HD 	 # rate of death in sick
    r_S2D   <- hr_S2 * r_HD    # rate of death in sicker
    p_S1D   <- 1 - exp(-r_S1D) # probability to die in sick
    p_S2D   <- 1 - exp(-r_S2D) # probability to die in sicker
    # Cost inputs
    c_H     <- 2000                # cost of remaining one cycle healthy
    c_S1    <- 4000                # cost of remaining one cycle sick
    c_S2    <- 15000               # cost of remaining one cycle sicker
    c_Trt   <- 12000               # cost of treatment (per cycle)
    # utility inputs
    u_H     <- 1                   # utility when healthy
    u_S1    <- 0.75                # utility when sick
    u_S2    <- 0.5                 # utility when sicker
    u_Trt   <- 0.95                # utility when being treated

    # create transition probability matrix for NO treatment
    m_P <- matrix(0,
                  nrow = n_s,
                  ncol = n_s,
                  dimnames = list(v_n, v_n))
    # fill in the transition probability array
    ### From Healthy
    m_P["H", "H"]  <- 1 - (p_HS1 + p_HD)
    m_P["H", "S1"] <- p_HS1
    m_P["H", "D"]  <- p_HD
    ### From Sick
    m_P["S1", "H"]  <- p_S1H
    m_P["S1", "S1"] <- 1 - (p_S1H + p_S1S2 + p_S1D)
    m_P["S1", "S2"] <- p_S1S2
    m_P["S1", "D"]  <- p_S1D
    ### From Sicker
    m_P["S2", "S2"] <- 1 - p_S2D
    m_P["S2", "D"]  <- p_S2D
    ### From Dead
    m_P["D", "D"] <- 1

    # Create a named vector of transition probabilities:
    t_p = c(p_HD, p_HS1, p_S1H, p_S1S2, p_S1D, p_S2D)
    names(t_p) = c("p.HD", "p.HS1", "p.S1H", "p.S1S2", "p.S1D", "p.S2D")

    # Create a named vector containing costs parameters:
    c_vec = c(c_H, c_S1, c_S2, c_Trt)
    names(c_vec) = c("c.H", "c.S1", "c.S2", "c.Trt")

    # Create a named vector containing utilities parameters:
    u_vec = c(u_H, u_S1, u_S2, u_Trt)
    names(u_vec) = c("u.H", "u.S1", "u.S2", "u.Trt")

    # Calibration:----
    if(calibrate_) {
      l_results <- SickSickerMicroSim_Cpp(
        v_S_t = v_M_1, t_P = t_p, v_C = c_vec, v_U = u_vec,
        n_I = n_i, n_S = n_s, n_T = n_t, n_Cl = 1, d_dC = d_c,
        d_dE = d_e, b_Trt = FALSE, n_Seed = 1
      )

      Surv <- l_results[["m.M"]] < 4
      nSurv <- Surv %>% colSums()
      Surv <- Surv %>% colMeans()
      Prev <- l_results[["m.M"]] >= 2 & l_results[["m.M"]] <= 3
      nSick <- Prev %>% colSums()
      nPrev <- nSick / nSurv
      Prev <- Prev %>% colMeans() / Surv
      nS1 <- l_results[["m.M"]] == 2
      nS1 <- nS1 %>% colSums()
      PropSick <- nS1 / nSick
      PropSick <- PropSick[c(11, 21, 31)]

      return(list('results' = l_results, 'Surv' = Surv[-1],
                  'nSurv' = nSurv, 'Prev' = Prev[-1], 'nSick' = nSick,
                  'nPrev' = nPrev, 'nS1' = nS1, 'PropSick' = PropSick))
    } else {
      l_results['Treatment'] <- SickSickerMicroSim_Cpp(
        v_S_t = v_M_1, t_P = t_p, v_C = c_vec, v_U = u_vec,
        n_I = n_i, n_S = n_s, n_T = n_t, n_Cl = 1, d_dC = d.c,
        d_dE = d.e, b_Trt = TRUE, n_Seed = 1
      )

      l_results['No treatment'] <- SickSickerMicroSim_Cpp(
        v_S_t = v_M_1, t_P = t_p, v_C = c_vec, v_U = u_vec,
        n_I = n_i, n_S = n_s, n_T = n_t, n_Cl = 1, d_dC = d.c,
        d_dE = d.e, b_Trt = FALSE, n_Seed = 1
      )

      return(l_results)
    }
  })
}
