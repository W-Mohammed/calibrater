#' Cancer Relative Survival (CRS) 3-State Markov Model
#'
#' @param .v_params_ A vector of named vector with values to replace
#' \code{p_Mets} and \code{p_DieMets} default values.
#' @param p_Mets A numeric representing the probability of contracting cancer.
#' @param p_DieMets A numeric representing the probability of dying from cancer.
#' @param calibrate_ If \code{TRUE} (default), the model outputs natural
#' history data; otherwise, discounted outcomes \code{(costs and QALYs)}
#' are returned.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
CRS_markov <- function(.v_params_ = NULL, p_Mets = 0.10, p_DieMets = 0.05,
                       calibrate_ = TRUE) {
  with(as.list(.v_params_), {
    ## Markov model parameters
    n_t  <- 60                        # time horizon, number of cycles
    v_n  <- c("NED", "Mets", "Death") # the 3 states of the model
    n_s  <- length(v_n)                # number of health states

    # Transition probabilities
    # p_Mets    = 0.10         	  # probability to become sicker when sick
    # p_DieMets = 0.05        	  # hazard ratio of death in sick vs healthy

    ####### INITIALIZATION ##########################################
    # create the cohort trace
    m_M <- matrix(NA, nrow = n_t + 1 ,
                  ncol = n_s,
                  dimnames = list(0:n_t, v_n)) # create Markov trace (n_t + 1 because R doesn't understand  Cycle 0)

    m_M[1, ] <- c(1, 0, 0)                     # initialize Markov trace

    # create transition probability matrix for NO treatment
    m_P <- matrix(0,
                  nrow = n_s,
                  ncol = n_s,
                  dimnames = list(v_n, v_n))
    # fill in the transition probability array
    ### From NED
    m_P["NED", "NED"]   <- 1 - (p_Mets)
    m_P["NED", "Mets"]  <- p_Mets
    m_P["NED", "Death"] <- 0            # Not allowed to die from cancer in NED state
    ### From Mets
    m_P["Mets", "NED"]   <- 0
    m_P["Mets", "Mets"]  <- 1 - (p_DieMets)
    m_P["Mets", "Death"] <- p_DieMets
    ### From Death
    m_P["Death", "Death"] <- 1

    # check rows add up to 1
    if (!isTRUE(all.equal(as.numeric(rowSums(m_P)), as.numeric(rep(1, n_s))))) {
      stop("This is not a valid transition Matrix")
    }

    ############# PROCESS ###########################################

    for (t in 1:n_t){                   # throughout the number of cycles
      m_M[t + 1, ] <- m_M[t, ] %*% m_P  # estimate the Markov trace for cycle the next cycle (t + 1)
    }

    if(calibrate_){
      ## EPIDEMIOLOGICAL OUTPUT:----
      ### Overall Survival (OS):----
      # calculate the overall survival (OS) probability
      v_os <- 1 - m_M[, "Death"]

      ### Return outputs:----
      out <- list(Surv = v_os[-c(1:2)])

      return(out)
    } else {

    }
  })
}

#' Cancer Relative Survival (CRS) 3-State Markov Model
#'
#' @param .v_params_ A vector of named vector with values to replace
#' \code{p_Mets} and \code{p_DieMets} default values.
#' @param p_Mets probability to become sicker when sick.
#' @param p_DieMets hazard ratio of death in sick vs healthy.
#' @param calibrate_ If \code{TRUE} (default), the model outputs natural
#' history data; otherwise, discounted outcomes \code{(costs and QALYs)}
#' are returned.
#' @param Trt Treatment, Zero (no treatment) and One (treatment)
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
CRS_markov_2 <- function(.v_params_ = NULL, p_Mets = 0.10, p_DieMets = 0.05,
                       calibrate_ = TRUE) {
  with(as.list(.v_params_), {
    ## Markov model parameters
    n_t  <- 60 # time horizon, number of cycles
    v_n  <- c("NED", "Mets", "Death") # the 3 states of the model
    n_s  <- length(v_n) # number of health states
    treats <- list(c(0, 0), c(1, 0), c(0, 1), c(1, 1)) # No, trt, scr, both
    treats_names <- c("None", "Medication", "Screening", "Both")
    rr_Trt1 <- 0.5 # cancer treatment
    rr_Trt2 <- 0.5 # cancer screening
    # Cost and utility inputs
    ## Costs
    c_NED   <- 0 # cost of remaining one cycle healthy
    c_Mets  <- 4000 # cost of remaining one cycle sick
    c_Trt   <- c(24000, 12000) # cost of treatment (per cycle)
    ## Utilities
    u_NED   <- 1 # utility when healthy
    u_Mets  <- 0.55 # utility when sick
    u_Trt   <- 0.75 # utility when being treated

    # Transition probabilities
    # p_Mets    = 0.10         	  # probability to become sicker when sick
    # p_DieMets = 0.05        	  # hazard ratio of death in sick vs healthy

    ####### FUNCTIONS ##########################################

    ## Costs function
    ### The Costs function estimates the costs at every cycle.

    Costs <- function (M_t, Trt = FALSE) {
      # M_t: health state occupied by individual i at cycle t (character variable)
      # Trt:  is the individual being treated? (default is FALSE)

      c_it <- M_t * c(
        c_NED + c_Trt[2] * Trt[2], # update the cost if healthy
        c_Mets + c_Trt[1] * Trt[1], # update the cost if sick conditional on treatment
        0
      )

      return(c_it) # return the costs
    }

    ## Health outcome function
    ### The Effs function to update the utilities at every cycle.

    Effs <- function (M_t, Trt = FALSE, cl = 1) {
      # M_t: health state occupied by individual i at cycle t (character variable)
      # Trt:  is the individual treated? (default is FALSE)
      # cl:   cycle length (default is 1)

      u_it <- 0 # by default the utility for everyone is zero
      QALYs <- M_t * c(
        u_NED * cl,
        (Trt[2] * u_Trt + (1 - Trt[2]) * u_Mets) * cl,
        0
      )

      return(QALYs) # return the QALYs
    }

    ####### INITIALIZATION ##########################################
    # create the cohort trace
    m_M <- m_C <- m_E <- matrix(
      NA, # create Markov trace (n_t + 1 because R doesn't understand  Cycle 0)
      nrow = n_t + 1,
      ncol = n_s,
      dimnames = list(
        0:n_t,
        v_n))

    m_M[1, ] <- c(1, 0, 0) # initialize Markov trace

    ############# PROCESS ###########################################
    if(calibrate_){
      ## Natural History of Disease (no treatment)
      Trt = c(0, 0) # no treatment

      ## create transition probability matrix for NO treatment
      m_P <- matrix(0,
                    nrow = n_s,
                    ncol = n_s,
                    dimnames = list(v_n, v_n))

      ## Fill in the transition probability array
      ### From NED
      m_P["NED", "NED"]   <- 1 - (p_Mets * (1 - Trt[2]) + (p_Mets * rr_Trt2 * Trt[2]))
      m_P["NED", "Mets"]  <- (p_Mets * (1 - Trt[2]) + (p_Mets * rr_Trt2 * Trt[2]))
      m_P["NED", "Death"] <- 0 # Not allowed to die from cancer in NED state
      ### From Mets
      m_P["Mets", "NED"]   <- 0
      m_P["Mets", "Mets"]  <- 1 - (p_DieMets * (1 - Trt[1]) + (p_DieMets * rr_Trt1 * Trt[1]))
      m_P["Mets", "Death"] <- (p_DieMets * (1 - Trt[1]) + (p_DieMets * rr_Trt1 * Trt[1]))
      ### From Death
      m_P["Death", "Death"] <- 1

      ### check rows add up to 1
      if (!isTRUE(all.equal(as.numeric(rowSums(m_P)), as.numeric(rep(1, n_s))))) {
        stop("This is not a valid transition Matrix")
      }

      ## Run simulation:----
      for (t in 1:n_t){                   # throughout the number of cycles
        m_M[t + 1, ] <- m_M[t, ] %*% m_P  # estimate the Markov trace for cycle the next cycle (t + 1)
      }

      ## EPIDEMIOLOGICAL OUTPUT:----
      ### Overall Survival (OS):----
      # calculate the overall survival (OS) probability
      v_os <- 1 - m_M[, "Death"]
      v_prop <- m_M[, "Mets"] / v_os

      ### Return outputs:----
      out <- list(Surv = v_os[-c(1:2)],
                  PropSick = v_prop[-c(1:2)])

      return(out)
      } else {
        out = list()

        for (i in 1:length(treats)) {
          ## Grab treatment
          Trt = treats[[i]]

          ## Create transition probability matrix for NO treatment
          m_P <- matrix(0,
                        nrow = n_s,
                        ncol = n_s,
                        dimnames = list(v_n, v_n))

          ## Fill in the transition probability array
          ### From NED
          m_P["NED", "NED"]   <- 1 - (p_Mets * (1 - Trt[2]) + (p_Mets * rr_Trt2 * Trt[2]))
          m_P["NED", "Mets"]  <- (p_Mets * (1 - Trt[2]) + (p_Mets * rr_Trt2 * Trt[2]))
          m_P["NED", "Death"] <- 0 # Not allowed to die from cancer in NED state
          ### From Mets
          m_P["Mets", "NED"]   <- 0
          m_P["Mets", "Mets"]  <- 1 - (p_DieMets * (1 - Trt[1]) + (p_DieMets * rr_Trt1 * Trt[1]))
          m_P["Mets", "Death"] <- (p_DieMets * (1 - Trt[1]) + (p_DieMets * rr_Trt1 * Trt[1]))
          ### From Death
          m_P["Death", "Death"] <- 1

          ### check rows add up to 1
          if (!isTRUE(all.equal(as.numeric(rowSums(m_P)), as.numeric(rep(1, n_s))))) {
            stop("This is not a valid transition Matrix")
          }

          ## Initiate matrices:----
          m_M[1, ] <- c(1, 0, 0) # initialize Markov trace
          m_C[1, ] <- Costs(m_M[1, ], Trt = Trt)
          m_E[1, ] <- Effs(m_M[1, ], Trt = Trt)

          ## Run simulation:----
          for (t in 1:n_t){
            m_M[t + 1, ] <- m_M[t, ] %*% m_P
            m_C[t + 1, ] <- Costs(m_M[t + 1, ], Trt)
            m_E[t + 1, ] <- Effs(m_M[t + 1, ], Trt)
          }

          ## Return results:----
          out[[treats_names[i]]] = list(
            "costs" = sum(m_C),
            "Effects" = sum(m_E)
          )
        }

        return(out)
    }
  })
}
