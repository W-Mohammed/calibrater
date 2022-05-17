pacman::p_load(devtools)
load_all()
#########################################################################
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

### A LINEAR REGRESSION EXAMPLE ####
## Define a Bayesian linear regression model
li_reg<-function(pars,data)
{
  a<-pars[1] #intercept
  b<-pars[2] #slope
  sd_e<-pars[3] #error (residuals)
  if(sd_e<=0){return(NaN)}
  pred <- a + b * data[,1]
  LLK<-sum( dnorm(data[,2],pred,sd_e, log=TRUE) )
  prior<- prior_reg(pars)
  return(LLK + prior)
}
## Define the Prior distributions
prior_reg<-function(pars)
{
  a<-pars[1] #intercept
  b<-pars[2] #slope
  epsilon<-pars[3] #error
  prior_a<-dnorm(a,0,100,log=TRUE) ## non-informative (flat) priors on all
  prior_b<-dnorm(b,0,100,log=TRUE) ## parameters.
  prior_epsilon<-dgamma(epsilon,1,1/100,log=TRUE)
  return(prior_a + prior_b + prior_epsilon)
}

# simulate data
x<-runif(30,5,15)
y<-x+rnorm(30,0,5)
d<-cbind(x,y)
mcmc_r<-Metro_Hastings(li_func=li_reg,pars=c(0,1,1),
                       par_names=c('a','b','epsilon'),data=d)
## For best results, run again with the previously
## adapted variance-covariance matrix.
mcmc_r<-Metro_Hastings(li_func=li_reg,pars=c(0,1,1),
                       prop_sigma=mcmc_r$prop_sigma,par_names=c('a','b','epsilon'),data=d)
mcmc_r<-mcmc_thin(mcmc_r)
plotMH(mcmc_r)


tst = HID_markov(.v_params = name_HID_params(rep(0.5, 9)))

tst2 = HID_markov(.v_params = name_HID_params(rep(0.5, 9)), project_future = T)


v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))

tst = sample_prior_LHS(.l_params = list(v_params_names = v_params_names,
                                        v_params_dists = v_params_dists, args = args), 10)

tst2 = sample_prior_FGS(.l_params = list(v_params_names = v_params_names,
                                         v_params_dists = v_params_dists, args = args),.n_samples = 10)
tst2

tst3 = sample_prior_RGS(.l_params = list(v_params_names = v_params_names,
                                         v_params_dists = v_params_dists, args = args),.n_samples = 10)
tst3
#########################################################################
# Number of initial starting points - NM:
n_init <- 100
# Number of random samples:
n_samples <- 10
# Names and number of input parameters to be calibrated:
v_params_names <- c("p_Mets", "p_DieMets")
n_params <- length(v_params_names)
v_params_init <- samples[1:10,]
n_init <- nrow(v_params_init)
## Run Gradient-based for each starting point:
res_llk = list()
res_sse = list()
m_calib_res_llk <- m_calib_res_sse <-
  matrix(nrow = n_init, ncol = n_params + 1)
colnames(m_calib_res_llk) <- colnames(m_calib_res_sse) <-
  c(v_params_names, "Overall_fit")

for (j in 1:n_init) {
  fit_sa <- optim(par = v_params_init[j, ],
                  fn = LLK, # GOF is log likelihood
                  method = "SANN",
                  control = list(  fnscale = -1,
                                   temp = 10,
                                   tmax = 10,
                                   maxit = 1000), # maximum iterations
                  hessian = TRUE,
                  .func = CRS_markov, # model to be optimised
                  .args = NULL, # arguments to be passed to the model
                  .l_targets = l_targets, # targets passed to .gof
                  .maximise = TRUE, # .gof should maximise
                  .optim = TRUE)
  m_calib_res_llk[j, ] <- c(fit_sa$par, fit_sa$value)
}

#########################################################################
load(file.path(here::here(), "data", "CRS_targets.rda"))
v_targets_names <- c("Surv")
v_targets_dists <- c('norm')
data("CRS_targets")
Surv <- CRS_targets$Surv
l_targets <- list(
  'v_targets_names' = v_targets_names,
  'Surv' = Surv,
  'v_targets_dists' = v_targets_dists)

testing <- LLK(.func = CRS_markov,
                          .samples = tst,
                          .l_targets = l_targets)
###########################
data("CRS_targets")
Surv <- CRS_targets$Surv
v_targets_names <- c("Surv", "Surv")
v_targets_dists <- c('norm', 'norm')
v_targets_weights <- c(0.2, 0.8)
l_targets <- list('v_targets_names' = v_targets_names, 'Surv' = Surv, 'v_targets_dists' = v_targets_dists, 'v_targets_weights' = v_targets_weights)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))

samples <- sample_prior_LHS(
  .l_params = list(v_params_names = v_params_names,                             v_params_dists = v_params_dists, args = args), .n_samples = 10000)

GOF_llik <- LLK(.func = CRS_markov, .samples = samples,
                           .l_targets = l_targets, .sample_method = "LHS")
#LLK##########################
data("CRS_targets")
Surv <- CRS_targets$Surv
v_targets_names <- c("Surv", "Surv")
v_targets_weights <- c(0.5, 0.5)
v_targets_dists <- c('norm')
# v_targets_names <- c("Surv")
# v_targets_weights <- c(1)
l_targets <-
  list('v_targets_names' = v_targets_names,
       'Surv' = Surv, 'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))
l_params <- list(v_params_names = v_params_names,                             v_params_dists = v_params_dists, args = args)

samples <- sample_prior_LHS(
  .l_params = l_params, .n_samples = 10000)

GOF_llik1 <- LLK(.func = CRS_markov, .samples = samples,
                            .l_targets = l_targets,
                            .sample_method = "LHS")
# GOF_llik2 <- LLK(.func = CRS_markov, .samples = samples,
#                             .l_targets = l_targets, .optim = TRUE)
# GOF_llik4 <- LLK(.func = CRS_markov, .samples = samples,
#                             .l_targets = l_targets, .optim = TRUE)
#wSSE_GOF##########################
data("CRS_targets")
Surv <- CRS_targets$Surv
v_targets_names <- c("Surv", "Surv")
v_targets_weights <- c(0.5, 0.5)
v_targets_dists <- c('norm')
# v_targets_names <- c("Surv")
# v_targets_weights <- c(1)
l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_weights' = v_targets_weights,
       'Surv' = Surv)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))

# samples <- sample_prior_LHS(
#   .l_params = list(v_params_names = v_params_names,                             v_params_dists = v_params_dists, args = args), .n_samples = 10000)

GOF_wsse1 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
                      .l_targets = l_targets, .sample_method = "LHS")
# GOF_wsse2 <- wSSE_GOF(.func = CRS_markov, .samples = samples[1:2,],
#                       .l_targets = l_targets)
# GOF_wsse5 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
#                       .l_targets = l_targets, .optim = TRUE)
# GOF_wsse4 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
#                       .l_targets = l_targets)
# GOF_wsse5 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
#                       .l_targets = l_targets)
compare(GOF_wsse, GOF_wsse2)
###########################

tsts = function(...) {
  dots = list(...)
  # moz = NULL
  moz = dots[['moz']]
  if(is.null(moz))
    cat('I can find the default')
  cat(moz)
}
tsts(2, 3)
`if`(0, "test", "not")
##########################################################
DEoptim::DEoptim(
  fn = wSSE_GOF,
  lower = lb,
  upper = ub,
  control = DEoptim::DEoptim.control( # control parameters
    trace = FALSE), # printing a trace
  .func = CRS_markov, # model to be optimised
  .args = NULL, # arguments to be passed to the model
  .l_targets = l_targets, # targets passed to .gof
  .maximise = FALSE, # .gof should minimise
  .optim = TRUE)
#Directed_search####################################################
load_all()

data("CRS_targets")
Surv <- CRS_targets$Surv
v_targets_names <- c("Surv", "Surv")
v_targets_weights <- c(0.5, 0.5)
v_targets_dists <- c("norm", "norm")
# v_targets_names <- c("Surv")
# v_targets_weights <- c(1)
l_targets <-
  list('v_targets_names' = v_targets_names,
       'Surv' = Surv,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = args)
rm(v_params_names, v_params_dists, v_targets_dists, v_targets_weights,
   v_targets_names, args)

set.seed(1)
samples <- sample_prior_LHS(.l_params = l_params,
                            .n_samples = 50)

NM_optimise_wSSE <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

GB_optimise_wSSE <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

SA_optimise_wSSE <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = list(NULL),
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

GA_optimise_wSSE <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = list(NULL),
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

NM_optimise_lLLK <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

GB_optimise_lLLK <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

SA_optimise_lLLK <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = l_targets,
  fnscale = -1,
  temp = 10,
  tmax = 10,
  maxit = 1000)

GA_optimise_lLLK <- calibrateModel_directed(
  .l_params = l_params,
  .func = CRS_markov,
  .args = list(NULL),
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

#PSA_values##################################################

l_optim_lists <- list(GA_optimise_lLLK, GA_optimise_wSSE, GB_optimise_lLLK,
                      GB_optimise_wSSE, NM_optimise_lLLK, NM_optimise_wSSE,
                      SA_optimise_lLLK, SA_optimise_wSSE)

testing <- PSA_calib_values(.l_optim_lists = l_optim_lists,
                            .search_method = "Directed")
testing %>% transpose() %>% View()


l_optim_lists2 <- list(GOF_llik1, GOF_wsse1)

testing2 <- PSA_calib_values(.l_optim_lists = l_optim_lists2,
                             .search_method = "Random")

#Bayesian_calibration_helpers###########################################
##### prior:
log_prior(.samples = samples[1,], .l_params = l_params)
calc_log_prior <- function(.n_param = n_params, .v_params,
                           .v_params_names = v_params_names) {
  if(is.null(dim(.v_params))) { # If vector, change to matrix
    .v_params <- t(.v_params)
  }
  n_samp <- nrow(.v_params)
  colnames(.v_params) <- .v_params_names
  lprior <- rep(0, n_samp)
  for (i in 1:.n_param){
    lprior <- lprior + dunif(.v_params[, i],
                             min = lb[i],
                             max = ub[i],
                             log = TRUE)
    # ALTERNATIVE prior using beta distributions
    # lprior <- lprior + dbeta(v_params[, i],
    #                          shape1 = 1,
    #                          shape2 = 1,
    #                          log = T)
  }
  return(lprior)
}
lb <- c(p_Mets = 0.04, p_DieMets = 0.04) # lower bound
ub <- c(p_Mets = 0.16, p_DieMets = 0.12) # upper bound
v = samples %>% as.matrix()
calc_log_prior(.v_params = v, .n_param = 2,
               .v_params_names = v_params_names) ==
  log_prior(.samples = samples, .l_params = l_params)

log_prior(.samples = samples, .l_params = l_params)
calculate_prior(.samples = samples[1,], .l_params = l_params)
##### likelihood:
calc_log_lik <- function(.func = CRS_markov, .lst_targets = lst_targets,
                         .v_params, .n_target = n_target){
  if(is.null(dim(.v_params))) { # If vector, change to matrix
    .v_params <- t(.v_params)
  }
  n_samp <- nrow(.v_params)
  v_llik <- matrix(0, nrow = n_samp, ncol = .n_target)
  llik_overall <- numeric(n_samp)
  for(j in 1:n_samp) { # j=1
    jj <- tryCatch( {
      ### Run model for a given parameter set:
      model_res <- exec(.fn = .func, .v_params[j, ])

      ###  Calculate log-likelihood of model outputs to targets  ###
      # TARGET 1: Survival ("Surv")
      # log likelihood
      v_llik[j, 1] <- sum(dnorm(x = .lst_targets$Surv$value,
                                mean = model_res$Surv,
                                sd = .lst_targets$Surv$se,
                                log = TRUE))

      # TARGET 2: (if you had more...)
      # log likelihood
      # v_llik[j, 2] <- sum(dnorm(x = lst_targets$Target2$value,
      #                        mean = model_res$Target2,
      #                        sd = lst_targets$Target2$se,
      #                        log = T))

      # OVERALL
      llik_overall[j] <- sum(v_llik[j, ])
    }, error = function(e) NA)
    if(is.na(jj)) { llik_overall <- -Inf }
  } # End loop over sampled parameter sets
  # return LLIK
  return(llik_overall)
}

LLK(.samples = samples, .func = CRS_markov,
               .args = NULL, .l_targets = l_targets)
calc_log_lik(.v_params = v)
calculate_likelihood(.samples = samples, .func = CRS_markov,
                     .args = NULL, .l_targets = l_targets)
calc_likelihood(.v_params = v)
calc_likelihood(.v_params = v) ==
  calculate_likelihood(.samples = samples, .func = CRS_markov,
                       .args = NULL, .l_targets = l_targets)
##### posterior:
calc_log_post <- function(.v_params, .target = lst_targets) {
  # Call log-likelihood function:
  LLK <- calc_log_lik(.v_params = .v_params,
                                 .lst_targets = .target)
  # Call log-prior function:
  lprior <- calc_log_prior(.v_params = .v_params)
  # Compute log-posterior:
  lpost <- LLK + lprior

  return(lpost)
}
calc_log_post(.v_params = v[1,], .target = lst_targets)
exp(calc_log_post(.v_params = v[1,], .target = lst_targets))

log_posterior(.samples = samples, .func = CRS_markov, .args = NULL,
              .l_targets = l_targets, .l_params = l_params)
calculate_posterior(.samples = samples, .func = CRS_markov,
                    .args = NULL, .l_targets = l_targets,
                    .l_params = l_params)

#Bayesian_calibration##################################################
data("CRS_targets")
Surv <- CRS_targets$Surv
v_targets_names <- c("Surv", "Surv")
v_targets_weights <- c(0.5, 0.5)
v_targets_dists <- c("norm", "norm")
# v_targets_names <- c("Surv")
# v_targets_weights <- c(1)
l_targets <-
  list('v_targets_names' = v_targets_names,
       'Surv' = Surv,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = args)
rm(v_params_names, v_params_dists, v_targets_dists, v_targets_weights,
   v_targets_names, args)

set.seed(1)
samples <- sample_prior_LHS(.l_params = l_params,
                            .n_samples = 1000)

test_Bayesian = calibrateModel_beyesian(
  .b_method = 'SIR', .func = CRS_markov, .args = NULL,
  .l_targets = l_targets, .l_params = l_params, .samples = samples)

test_Bayesian2 = calibrateModel_beyesian(
  .b_method = 'IMIS', .func = CRS_markov, .args = NULL,
  .l_targets = l_targets, .l_params = l_params, .samples = samples,
  .n_resample = 1000)

#CRS_model################################################################
library(devtools)
load_all()

samples_CRS_data <- sample_prior_LHS(
  .n_samples = 1000,
  .l_params = CRS_data$l_params)
samples1_CRS_data <- sample_prior_FGS(
  .n_samples = 5,
  .l_params = CRS_data$l_params)
samples2_CRS_data <- sample_prior_RGS(
  .n_samples = 1000,
  .l_params = CRS_data$l_params)

GOF_wsse_CRS_model <- wSSE_GOF(
  .func = CRS_markov,
  .optim = FALSE,
  .args = NULL,
  .samples = samples_CRS_data,
  .l_targets = CRS_data$l_targets,
  .sample_method = "LHS")

GOF_llik_CRS_model <- LLK_GOF(
  .func = CRS_markov, .optim = FALSE,
  .args = NULL,
  .samples = samples_CRS_data,
  .l_targets = CRS_data$l_targets,
  .sample_method = "LHS")

samples <- sample_prior_LHS(.n_samples = 100,
                            .l_params = CRS_data$l_params)

NM_optimise_wSSE_CRS_model <- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  maxit = 1000)

GB_optimise_wSSE_CRS_model <- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  maxit = 1000)

SA_optimise_wSSE_CRS_model <- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

GA_optimise_wSSE_CRS_model <- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

NM_optimise_LLK_CRS_model <- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  maxit = 1000)

GB_optimise_LLK_CRS_model <- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  maxit = 1000)

SA_optimise_LLK_CRS_model <- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  fnscale = -1,
  temp = 10,
  tmax = 10,
  maxit = 1000)

GA_optimise_LLK_CRS_model<- calibrateModel_directed(
  .l_params = CRS_data$l_params,
  .func = CRS_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = CRS_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

samples <- sample_prior_LHS(.n_samples = 1000,
                            .l_params = CRS_data$l_params)

SIR_CRS_data = calibrateModel_beyesian(
  .b_method = 'SIR', .func = CRS_markov,
  .args = NULL,
  .l_targets = CRS_data$l_targets,
  .l_params = CRS_data$l_params, .samples = samples)

set.seed(1) # Function crashes on set.seed(1)
IMIS2_CRS_data = calibrateModel_beyesian2(
  .b_method = 'IMIS', .func = CRS_markov,
  .args = NULL,
  .l_targets = CRS_data$l_targets,
  .l_params = CRS_data$l_params,
  .n_resample = 1000)

rm(likelihood, prior, sample.prior)
set.seed(1) # Function crashes on set.seed(1)
IMIS_CRS_data = calibrateModel_beyesian(
  .b_method = 'IMIS', .func = CRS_markov,
  .args = NULL,
  .l_targets = CRS_data$l_targets,
  .l_params = CRS_data$l_params,
  .n_resample = 1000)

l_optim_lists_CRS_data <- list(
  GA_optimise_LLK_CRS_model, GA_optimise_wSSE_CRS_model,
  GB_optimise_LLK_CRS_model, GB_optimise_wSSE_CRS_model,
  NM_optimise_LLK_CRS_model, NM_optimise_wSSE_CRS_model,
  SA_optimise_LLK_CRS_model, SA_optimise_wSSE_CRS_model)

l_optim_lists_CRS_data2 <- list(
  GOF_wsse_CRS_model, GOF_llik_CRS_model)

l_optim_lists_CRS_data3 <- list(
  SIR_CRS_data, IMIS_CRS_data, IMIS2_CRS_data)

PSA_values_CRS_markov <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_CRS_data,
  .search_method = 'Directed',
  .PSA_runs = 1000,
  .l_params = CRS_data$l_params)

PSA_values_CRS_markov2 <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_CRS_data2,
  .search_method = 'Random',
  .PSA_runs = 1000,
  .l_params = CRS_data$l_params)

PSA_values_CRS_markov3 <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_CRS_data3,
  .search_method = 'Bayesian',
  .PSA_runs = 1000,
  .l_params = CRS_data$l_params)

#Run_PSA#############################################################
# calib_values_ <- c(PSA_values_HID_data,
#                    PSA_values_HID_data2,
#                    PSA_values_HID_data3)
#
# PSA_results <- run_PSA(
#   .func_ = HID_markov,
#   .args_ = NULL,
#   .PSA_calib_values_ = c(PSA_values_HID_data,
#                          PSA_values_HID_data2,
#                          PSA_values_HID_data3),
#   .PSA_unCalib_values_ = NULL)

PSA_results_CRS_markov <- run_PSA(
  .func_ = CRS_markov,
  .args_ = NULL,
  .PSA_calib_values_ = c(PSA_values_CRS_markov,
                         PSA_values_CRS_markov2,
                         PSA_values_CRS_markov3),
  .PSA_unCalib_values_ = NULL)

















#HID_model#############################################################
l_prior <- function(par_vector) {
  # par_vector: a vector (or matrix) of model parameters (omits c)
  if(is.null(dim(par_vector))) par_vector <- t(par_vector)
  lprior <- rep(0,nrow(par_vector))
  lprior <- lprior+dlnorm(par_vector[,1],log(0.05 )-1/2*0.5^2,0.5,log=TRUE)    # mu_e
  lprior <- lprior+dlnorm(par_vector[,2],log(0.25 )-1/2*0.5^2,0.5,log=TRUE)    # mu_l
  lprior <- lprior+dlnorm(par_vector[,3],log(0.025)-1/2*0.5^2,0.5,log=TRUE)    # mu_t
  lprior <- lprior+dlnorm(par_vector[,4],log(0.1  )-1/2*0.5^2,0.5,log=TRUE)    # p
  lprior <- lprior+dlnorm(par_vector[,5],log(0.5  )-1/2*0.5^2,0.5,log=TRUE)    # r_l
  lprior <- lprior+dlnorm(par_vector[,6],log(0.5  )-1/2*0.5^2,0.5,log=TRUE)    # rho
  lprior <- lprior+dbeta( par_vector[,7],2,8,log=TRUE)                         # b
  return(lprior)
}

tooot = samples_HID2_data[1:5,]
tooot2 = backTransform(.t_data_ = as_tibble(samples_HID2_data[1:5,]), .l_params_ = HID_data2$l_params)

log_prior(.samples = tooot2[1,], .l_params = HID_data$l_params)
l_prior(par_vector = as.matrix(tooot2[1,]))
log_prior(.samples = tooot[1,], .l_params = HID_data2$l_params,
          .transform = TRUE)

microbenchmark::microbenchmark(
  log_prior_(.samples = samples_HID2_data, .l_params = HID_data2$l_params,
             .transform = TRUE),
  l_prior(par_vector = as.matrix(samples_HID2_data)))

mod <- function(par_vector,project_future=FALSE) {
  # par_vector: a vector of model parameters
  # project_future: TRUE/FALSE, whether to project future outcomes for policy comparison
  pop_size   <- 1e6             # population size hard-coded as 1 million
  mu_b       <- 0.015           # background mortality rate hard-coded as 0.015
  mu_e       <- par_vector[1]   # cause-specific mortality rate with early-stage disease
  mu_l       <- par_vector[2]   # cause-specific mortality rate with late-stage disease
  mu_t       <- par_vector[3]   # cause-specific mortality rate on treatment
  p          <- par_vector[4]   # transtion rate from early to late-stage disease
  r_l <- r_e <- par_vector[5]   # rate of uptake onto treatment (r_l = late-stage disease;r_e = early-stage disease)
  rho        <- par_vector[6]   # effective contact rate
  b          <- par_vector[7]   # fraction of population in at-risk group
  c          <- par_vector[8]   # annual cost of treatment

  ######## Prepare to run model ###################
  n_yrs    <- if(project_future) { 51 } else { 30 }  # no. years to simulate (30 to present, 51 for 20 year analytic horizon)
  sim      <- if(project_future) { 1:2 } else { 1 }  # which scenarios to simulate: 1 = base case, 2 = expanded treatment access
  v_mu     <- c(0,0,mu_e,mu_l,mu_t)+mu_b             # vector of mortality rates
  births   <- pop_size*mu_b*c(1-b,b)                 # calculate birth rate for equilibrium population before epidemic
  init_pop <- pop_size*c(1-b,b-0.001,0.001,0,0,0)    # creates starting vector for population
  trace    <- matrix(NA,12*n_yrs,6)                  # creates a table to store simulation trace
  colnames(trace) <- c("N","S","E","L","T","D")
  results  <- list()                                 # creates a list to store results

  ######## Run model ###################
  for(s in sim) {
    P0 <- P1 <- init_pop
    for(m in 1:(12*n_yrs)) {
      lambda    <- rho*sum(P0[3:4])/sum(P0[2:5]) # calculates force of infection
      P1[1:2]   <- P1[1:2]+births/12             # births
      P1[-6]    <- P1[-6]-P0[-6]*v_mu/12         # deaths: N, S, E, L, T, to D
      P1[6]     <- P1[6]+sum(P0[-6]*v_mu/12)     # deaths:N, S, E, L, T, to D
      P1[2]     <- P1[2]-P0[2]*lambda/12         # infection: S to E
      P1[3]     <- P1[3]+P0[2]*lambda/12         # infection: S to E
      P1[3]     <- P1[3]-P0[3]*p/12              # progression: E to L
      P1[4]     <- P1[4]+P0[3]*p/12              # progression: E to L
      P1[4]     <- P1[4]-P0[4]*r_l/12            # treatment uptake: L to T
      P1[5]     <- P1[5]+P0[4]*r_l/12            # treatment uptake: L to T
      if(s==2 & m>(12*30)) {
        P1[3]   <- P1[3]-P0[3]*r_e/12            # treatment uptake: E to T (scenario 2)
        P1[5]   <- P1[5]+P0[3]*r_e/12            # treatment uptake: E to T (scenario 2)
      }
      trace[m,] <- P0 <- P1                      # fill trace, reset pop vectors
    }
    results[[s]] <- trace                        # save results for each scenario
  }

  ######## Report results ###################
  if(project_future==FALSE) {
    ## Return calibration metrics, if project_future = FALSE
    return(list(prev = (rowSums(trace[,3:5])/rowSums(trace[,1:5]))[c(10,20,30)*12],  # Prevalence at 10,20,30 years
                surv = 1/(v_mu[3]+p)+ p/(v_mu[3]+p)*(1/v_mu[4]),                     # HIV survival without treatment
                tx   = trace[30*12,5]                                                # Treatment volume at 30 years
    ) )
  } else {
    ## Policy projections for CE analysis, if project_future = TRUE
    return(list(trace0   = results[[1]],     # Trace without expanded treatment access
                trace1   = results[[2]],     # Trace with expanded treatment access
                inc_LY   = sum(results[[2]][(30*12+1):(51*12),-6]-results[[1]][(30*12+1):(51*12),-6])/12,  # incr. LY lived with expanded tx
                inc_cost = sum(results[[2]][(30*12+1):(51*12),5]-results[[1]][(30*12+1):(51*12),5])*c/12   # incr. cost  with expanded tx
    ) )
  }
}

l_likelihood <- function(par_vector) {
  # par_vector: a vector (or matrix) of model parameters
  if(is.null(dim(par_vector))) par_vector <- t(par_vector)
  llik <- rep(0,nrow(par_vector))
  for(j in 1:nrow(par_vector)) {
    jj <- tryCatch( {
      res_j <- mod(c(as.numeric(par_vector[j,]),1))
      llik[j] <- llik[j]+sum(dbinom(c(25,75,50),500,res_j[["prev"]], log=TRUE)) # prevalence likelihood
      llik[j] <- llik[j]+dnorm(10,res_j[["surv"]],2/1.96, log=TRUE)             # survival likelihood
      llik[j] <- llik[j]+dnorm(75000,res_j[["tx"]],5000/1.96, log=TRUE)         # treatment volume likelihood
    }, error = function(e) NA)
    if(is.na(jj)) { llik[j] <- -Inf }
  }
  return(llik)
}

l_likelihood(par_vector = as.matrix(tooot2[3,]))
LLK(
  .samples = tooot2[3,],
  .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets
)

microbenchmark::microbenchmark(
  l_likelihood(par_vector = as.matrix(tooot2[3,])),
  LLK(
    .samples = tooot2[3,],
    .func = HID_markov,
    .args = NULL,
    .l_targets = HID_data$l_targets
  )
)

l_post <- function(par_vector) {
  return( l_prior(par_vector) + l_likelihood(par_vector) )
}
calculate_posterior(
  .samples = tooot2[3,],
  .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets,
  .l_params = HID_data$l_params)

l_post(as.matrix(tooot2[3,]))
log_posterior(
  .samples = tooot2[3,],
  .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets,
  .l_params = HID_data$l_params)

microbenchmark::microbenchmark(
  l_post(as.matrix(tooot2[3,])),
  log_posterior(
    .samples = tooot2[3,],
    .func = HID_markov,
    .args = NULL,
    .l_targets = HID_data$l_targets,
    .l_params = HID_data$l_params)
)

microbenchmark::microbenchmark(
  l_post(as.matrix(as_tibble(t(tooot)))),
  log_posterior(
    .samples = tooot,
    .func = HID_markov_2,
    .args = NULL,
    .l_targets = HID_data2$l_targets,
    .l_params = HID_data2$l_params)
)

# tst = HID_markov(.v_params = name_HID_params(rep(0.5, 9)))
# tst1 = HID_markov(.v_params = name_HID_params(rep(1, 9)))
# tst2 =  HID_markov()
# tst2 = HID_markov(.v_params = name_HID_params(rep(0.5, 9)), project_future = T)

# v_targets_names <- c("Prev", "Surv", "Trt_vol")
# v_targets_weights <- c(1, 1, 1)
# v_targets_dists <- c("binom", "norm", "norm")
# l_targets <-
#   list('v_targets_names' = v_targets_names,
#        'Prev' = tibble('value' = c(5/100, 15/100, 10/100), # %
#                        'se' = c(5/1000, 15/1000, 10/1000), # 10% of value
#                        'x' = c(25, 50, 75),
#                        'size' = 500,
#                        'lb' = c(3.3, 12, 7.5),
#                        'ub' = c(7.1, 18.3, 12.8)),
#        'Surv' = tibble('value' = 10,
#                        'se' = 2/1.96,
#                        'lb' = 8,
#                        'ub' = 12),
#        'Trt_vol' = tibble('value' = 75000,
#                           'se' = 5000/1.96,
#                           'lb' = 70000,
#                           'ub' = 80000),
#        'v_targets_dists' = v_targets_dists,
#        'v_targets_weights' = v_targets_weights)
# v_params_names <- c("mu_e", "mu_l", "mu_t", "p", "r_l", "rho",
#                     "b")
# v_params_dists <- c("lnorm", "lnorm", "lnorm", "lnorm", "lnorm", "lnorm",
#                     "beta")
# args <- list(list(meanlog = -3.121, sdlog = 0.5),
#              list(meanlog = -1.511, sdlog = 0.5),
#              list(meanlog = -3.814, sdlog = 0.5),
#              list(meanlog = -2.428, sdlog = 0.5),
#              list(meanlog = -0.818, sdlog = 0.5),
#              list(meanlog = -0.818, sdlog = 0.5),
#              list(shape1 = 2, shape2 = 8))
# extra_args <- list(list(min = 0.02, max = 0.12),
#                    list(min = 0.08, max = 0.59),
#                    list(min = 0.01, max = 0.06),
#                    list(min = 0.03, max = 0.24),
#                    list(min = 0.17, max = 1.18),
#                    list(min = 0.01, max = 0.06),
#                    list(min = 0.03, max = 0.48))
# l_params <- list('v_params_names' = v_params_names,
#                  'v_params_dists' = v_params_dists,
#                  'args' = args,
#                  'Xargs' = extra_args)
# rm(v_params_names, v_params_dists, v_targets_dists, v_targets_weights,
#    v_targets_names, args)
library(devtools)
load_all()

samples_HID_data <- sample_prior_LHS(.n_samples = 1000,
                                     .l_params = HID_data$l_params)
samples1_HID_data <- sample_prior_FGS(.n_samples = 5,
                                      .l_params = HID_data$l_params)
samples2_HID_data <- sample_prior_RGS(.n_samples = 50,
                                      .l_params = HID_data$l_params)

GOF_wsse_HID_data <- wSSE_GOF(
  .func = HID_markov, .optim = FALSE,
  .args = NULL,
  .samples = samples_HID_data,
  .l_targets = HID_data$l_targets,
  .sample_method = "LHS")

GOF_llik_HID_data <- LLK_GOF(
  .func = HID_markov, .optim = FALSE,
  .args = NULL,
  .samples = samples_HID_data,
  .l_targets = HID_data$l_targets,
  .sample_method = "LHS")

samples <- sample_prior_LHS(.n_samples = 5,
                            .l_params = HID_data$l_params)

NM_optimise_wSSE_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

GB_optimise_wSSE_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

SA_optimise_wSSE_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

GA_optimise_wSSE_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

NM_optimise_LLK_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

GB_optimise_LLK_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

SA_optimise_LLK_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  fnscale = -1,
  temp = 10,
  tmax = 10,
  maxit = 1000)

GA_optimise_LLK_HID_data <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

samples <- sample_prior_LHS(.n_samples = 1000,
                            .l_params = HID_data$l_params)

SIR_HID_data = calibrateModel_beyesian(
  .b_method = 'SIR', .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets,
  .l_params = HID_data$l_params, .samples = samples)

set.seed(1) # Function crashes on set.seed(1)
IMIS2_HID_data = calibrateModel_beyesian2(
  .b_method = 'IMIS', .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets,
  .l_params = HID_data$l_params,
  .n_resample = 1000)

rm(likelihood, prior, sample.prior)
set.seed(1) # Function crashes on set.seed(1)
IMIS_HID_data = calibrateModel_beyesian(
  .b_method = 'IMIS', .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets,
  .l_params = HID_data$l_params,
  .n_resample = 1000)

l_optim_lists_HID_data <- list(
  GA_optimise_LLK_HID_data, GA_optimise_wSSE_HID_data,
  GB_optimise_LLK_HID_data, GB_optimise_wSSE_HID_data,
  NM_optimise_LLK_HID_data, NM_optimise_wSSE_HID_data,
  SA_optimise_LLK_HID_data, SA_optimise_wSSE_HID_data)

l_optim_lists_HID_data2 <- list(
  GOF_wsse_HID_data, GOF_llik_HID_data)

l_optim_lists_HID_data3 <- list(
  SIR_HID_data, IMIS_HID_data, IMIS2_HID_data)

PSA_values_HID_data <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_HID_data,
  .search_method = 'Directed',
  .PSA_runs = 1000,
  .l_params = HID_data$l_params)

PSA_values_HID_data2 <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_HID_data2,
  .search_method = 'Random',
  .PSA_runs = 1000,
  .l_params = HID_data$l_params)

PSA_values_HID_data3 <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_HID_data3,
  .search_method = 'Bayesian',
  .PSA_runs = 1000,
  .l_params = HID_data$l_params)

#old = options('warning.length')
#options(warning.length=8000)
#options(old)

#Rcpp microsim##############################################
# Arguments:
# v_M_1:   vector of initial states for individuals
# n.i:     number of individuals
# n.t:     total number of cycles to run the model
# v.n:     vector of health state names
# d.c:     discount rate for costs
# d.e:     discount rate for health outcome (QALYs)
# t_p:     vector containing transition probability
# u_vec:   utilities vectors
# c_vec:   costs vectors
# Trt:     are the n.i individuals receiving treatment? (scalar with a Boolean value, default is FALSE)
# seed:    starting seed number for random number generator (default is 1)
# Makes use of:
# ProbsCpp: function for the estimation of transition probabilities
# CostsRcpp2: function for the estimation of cost state values
# EffsRcpp2: function for the estimation of state specific health outcomes (QALYs)

# n.i   <- 100000                # number of simulated individuals
# n.t   <- 30                    # time horizon, 30 cycles
# v.n   <- c("H","S1","S2","D")  # the model states: Healthy (H), Sick (S1), Sicker (S2), Dead (D)
# n.s   <- length(v.n)           # the number of health states
# v.M_1 <- rep("H", n.i)         # everyone begins in the healthy state
# d.c   <- d.e <- 0.03           # equal discounting of costs and QALYs by 3%
# v.Trt <- c("No Treatment", "Treatment") # store the strategy names
#
# # Transition probabilities (per cycle)
# p.HD    <- 0.005               # probability to die when healthy
# p.HS1   <- 0.15          	     # probability to become sick when healthy
# p.S1H   <- 0.5           	     # probability to become healthy when sick
# p.S1S2  <- 0.105         	     # probability to become sicker when sick
# rr.S1   <- 3             	     # rate ratio of death in sick vs healthy
# rr.S2   <- 10            	     # rate ratio of death in sicker vs healthy
# r.HD    <- -log(1 - p.HD) 	   # rate of death in healthy
# r.S1D   <- rr.S1 * r.HD  	     # rate of death in sick
# r.S2D   <- rr.S2 * r.HD  	     # rate of death in sicker
# p.S1D   <- 1 - exp(- r.S1D)    # probability to die in sick
# p.S2D   <- 1 - exp(- r.S2D)    # probability to die in sicker
#
# # Cost and utility inputs
# c.H     <- 2000                # cost of remaining one cycle healthy
# c.S1    <- 4000                # cost of remaining one cycle sick
# c.S2    <- 15000               # cost of remaining one cycle sicker
# c.Trt   <- 12000               # cost of treatment (per cycle)
#
# u.H     <- 1                   # utility when healthy
# u.S1    <- 0.75                # utility when sick
# u.S2    <- 0.5                 # utility when sicker
# u.Trt   <- 0.95                # utility when being treated
#
# # Define starting health state, using numbers instead of characters to identify the health states:
# v_M_1 = rep(1, n.i)
# #v_M_1 = rep(c(1, 2, 3, 4), n.i/4)
#
# # Create a vector of transition probabilities:
# t_p = c(p.HD, p.HS1, p.S1H, p.S1S2, p.S1D, p.S2D)
# names(t_p) = c("p.HD", "p.HS1", "p.S1H", "p.S1S2", "p.S1D", "p.S2D")
#
# # Create a vector containing costs parameters:
# c_vec = c(c.H, c.S1, c.S2, c.Trt)
# names(c_vec) = c("c.H", "c.S1", "c.S2", "c.Trt")
#
# # Create a vector containing utilities parameters:
# u_vec = c(u.H, u.S1, u.S2, u.Trt)
# names(u_vec) = c("u.H", "u.S1", "u.S2", "u.Trt")
#
# ###
#
# ResV_no_trt_Cpp =
#   MicroSimV_Cpp(v_S_t = v_M_1, t_P = t_p, v_C = c_vec, v_U = u_vec, n_I = n.i,
#                 n_S = n.s, n_T = n.t, n_Cl = 1, d_dC = d.c, d_dE = d.e,
#                 b_Trt = FALSE, n_Seed = 1) # run for no treatment
# ResV_trt_Cpp =
#   MicroSimV_Cpp(v_S_t = v_M_1, t_P = t_p, v_C = c_vec, v_U = u_vec, n_I = n.i,
#                 n_S = n.s, n_T = n.t, n_Cl = 1, d_dC = d.c, d_dE = d.e,
#                 b_Trt = TRUE, n_Seed = 1) # run for treatment
#
# ###
#
# Default <- SS_MicroSim(n_i = 1000000)
SS_MicroSim(p_S1S2 = 0.155761, hr_S1 = 3.108213, hr_S2 = 6.030473)
###

library(devtools)
load_all()

samples_SS_MicroSim <- sample_prior_LHS(
  .n_samples = 100,
  .l_params = sickSicker_data$l_params)
samples1_SS_MicroSim <- sample_prior_FGS(
  .n_samples = 5,
  .l_params = sickSicker_data$l_params)
samples2_SS_MicroSim <- sample_prior_RGS(
  .n_samples = 50,
  .l_params = sickSicker_data$l_params)

GOF_wsse_SS_MicroSim <- wSSE_GOF(
  .func = SS_MicroSim, .optim = FALSE,
  .args = NULL,
  .samples = samples_SS_MicroSim,
  .l_targets = sickSicker_data$l_targets,
  .sample_method = "LHS")

GOF_llik_SS_MicroSim <- LLK_GOF(
  .func = SS_MicroSim, .optim = FALSE,
  .args = NULL,
  .samples = samples_SS_MicroSim,
  .l_targets = sickSicker_data$l_targets,
  .sample_method = "LHS")

samples <- sample_prior_LHS(
  .n_samples = 5,
  .l_params = sickSicker_data$l_params)

NM_optimise_wSSE_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  maxit = 1000)

GB_optimise_wSSE_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  maxit = 1000)

SA_optimise_wSSE_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

GA_optimise_wSSE_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'SSE',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

NM_optimise_LLK_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  maxit = 1000)

GB_optimise_LLK_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  maxit = 1000)

SA_optimise_LLK_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  fnscale = -1,
  temp = 10,
  tmax = 10,
  maxit = 1000)

GA_optimise_LLK_SS_MicroSim <- calibrateModel_directed(
  .l_params = sickSicker_data$l_params,
  .func = SS_MicroSim,
  .args = NULL,
  .gof = 'LLK',
  .samples = samples,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = sickSicker_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

samples <- sample_prior_LHS(
  .n_samples = 1000,
  .l_params = sickSicker_data$l_params)

SIR_SS_MicroSim = calibrateModel_beyesian(
  .b_method = 'SIR', .func = SS_MicroSim,
  .args = NULL,
  .l_targets = sickSicker_data$l_targets,
  .l_params = sickSicker_data$l_params, .samples = samples)

set.seed(1) # Function crashes on set.seed(1)
IMIS2_SS_MicroSim = calibrateModel_beyesian2(
  .b_method = 'IMIS', .func = SS_MicroSim,
  .args = NULL,
  .l_targets = sickSicker_data$l_targets,
  .l_params = sickSicker_data$l_params,
  .n_resample = 1000)

rm(likelihood, prior, sample.prior)
set.seed(1) # Function crashes on set.seed(1)
IMIS_SS_MicroSim = calibrateModel_beyesian(
  .b_method = 'IMIS', .func = SS_MicroSim,
  .args = NULL,
  .l_targets = sickSicker_data$l_targets,
  .l_params = sickSicker_data$l_params,
  .n_resample = 1000)

l_optim_lists_SS_MicroSim <- list(
  GA_optimise_LLK_SS_MicroSim, GA_optimise_wSSE_SS_MicroSim,
  GB_optimise_LLK_SS_MicroSim, GB_optimise_wSSE_SS_MicroSim,
  NM_optimise_LLK_SS_MicroSim, NM_optimise_wSSE_SS_MicroSim,
  SA_optimise_LLK_SS_MicroSim, SA_optimise_wSSE_SS_MicroSim)

l_optim_lists_SS_MicroSim2 <- list(
  GOF_wsse_SS_MicroSim, GOF_llik_SS_MicroSim)

l_optim_lists_SS_MicroSim3 <- list(
  SIR_SS_MicroSim, IMIS_SS_MicroSim, IMIS2_SS_MicroSim)

PSA_values_SS_MicroSim <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_SS_MicroSim,
  .search_method = 'Directed',
  .PSA_runs = 1000,
  .l_params = sickSicker_data$l_params)

PSA_values_SS_MicroSim2 <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_SS_MicroSim2,
  .search_method = 'Random',
  .PSA_runs = 1000,
  .l_params = sickSicker_data$l_params)

PSA_values_SS_MicroSim3 <- PSA_calib_values(
  .l_optim_lists = l_optim_lists_SS_MicroSim3,
  .search_method = 'Bayesian',
  .PSA_runs = 1000,
  .l_params = sickSicker_data$l_params)

#Models with transformations#############################################
## HID_markov
set.seed(1)
test <- HID_markov()
set.seed(1)
test2 <- HID_markov_2()
testthat::compare(test, test2)

betas <- rbeta(n = 1e+6, shape1 = 2, shape2 = 8)
prob_to_logit(mean(betas)); prob_to_logit(sd(betas))

logit_to_prob(prob_to_logit(mean(betas))); logit_to_prob(prob_to_logit(sd(betas)))
####
# Proper testing: #######################################################
library(devtools)
load_all()

# Transformed version of the HID_markov:
set.seed(1)
HID2_results <- list()
HID2_results$Prior_samples[['LHS']] <- sample_prior_LHS(
  .n_samples = 10000,
  .l_params = HID_data2$l_params)

HID2_results$Prior_samples[['FGS']] <- sample_prior_FGS(
  .n_samples = 10000,
  .l_params = HID_data2$l_params)

HID2_results$Prior_samples[['RGS']] <- sample_prior_RGS(
  .n_samples = 10000,
  .l_params = HID_data2$l_params)
##
HID2_results$Calib_results$Random[[1]] <- wSSE_GOF(
  .func = HID_markov_2, .optim = FALSE,
  .args = NULL,
  .samples = HID2_results$Prior_samples$LHS,
  .l_targets = HID_data2$l_targets,
  .sample_method = "LHS")

HID2_results$Calib_results$Random[[2]] <- LLK_GOF(
  .func = HID_markov_2, .optim = FALSE,
  .args = NULL,
  .samples = HID2_results$Prior_samples$LHS,
  .l_targets = HID_data2$l_targets,
  .sample_method = "LHS")
##
HID2_results$Prior_samples[['LHS_Directed']] <- sample_prior_LHS(
  .n_samples = 2,
  .l_params = HID_data2$l_params)

HID2_results$Calib_results$Directed[[1]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  maxit = 1000)

HID2_results$Calib_results$Directed[[2]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  maxit = 1000)

HID2_results$Calib_results$Directed[[3]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  maxit = 1000)

HID2_results$Calib_results$Directed[[4]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  maxit = 1000)

HID2_results$Calib_results$Directed[[5]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  fnscale = -1,
  temp = 10,
  tmax = 10,
  maxit = 1000)

HID2_results$Calib_results$Directed[[6]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

HID2_results$Calib_results$Directed[[7]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

HID2_results$Calib_results$Directed[[8]] <- calibrateModel_directed(
  .l_params = HID_data2$l_params,
  .func = HID_markov_2,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID2_results$Prior_samples$LHS_Directed,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = HID_data2$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)
##
HID2_results$Prior_samples[['LHS_Bayesian']] <- sample_prior_LHS(
  .n_samples = 10000,
  .l_params = HID_data2$l_params)

HID2_results$Calib_results$Bayesian[[1]] = calibrateModel_beyesian(
  .b_method = 'SIR', .func = HID_markov_2,
  .args = NULL,
  .l_targets = HID_data2$l_targets,
  .n_resample = 10000,
  .l_params = HID_data2$l_params,
  .samples = HID2_results$Prior_samples$LHS_Bayesian)

set.seed(1) # Function crashes on set.seed(1)
HID2_results$Calib_results$Bayesian[[2]] = calibrateModel_beyesian(
  .b_method = 'IMIS', .func = HID_markov_2,
  .args = NULL,
  .l_targets = HID_data2$l_targets,
  .l_params = HID_data2$l_params,
  .transform = TRUE,
  .n_resample = 10000,
  .IMIS_iterations = 400,
  .IMIS_sample = 100)
##
HID2_results$PSA_samples[["Directed"]] <- PSA_calib_values(
  .l_calib_res_lists = HID2_results$Calib_results$Directed,
  .search_method = 'Directed',
  .PSA_samples = 10000,
  .transform_ = TRUE,
  .l_params = HID_data2$l_params)

HID2_results$PSA_samples[["Random"]] <- PSA_calib_values(
  .l_calib_res_lists = HID2_results$Calib_results$Random,
  .search_method = 'Random',
  .PSA_samples = 10000,
  .transform_ = TRUE,
  .l_params = HID_data2$l_params)

HID2_results$PSA_samples[["Bayesian"]] <- PSA_calib_values(
  .l_calib_res_lists = HID2_results$Calib_results$Bayesian,
  .search_method = 'Bayesian',
  .PSA_samples = 10000,
  .transform_ = TRUE,
  .l_params = HID_data2$l_params)
##
HID2_results$PSA_results <- run_PSA(
  .func_ = HID_markov_2,
  .PSA_calib_values_ = c(HID2_results$PSA_samples$Directed,
                         HID2_results$PSA_samples$Random,
                         HID2_results$PSA_samples$Bayesian),
  .args_ = list(calibrate_ = FALSE,
                transform_ = FALSE),
  .PSA_unCalib_values_ = NULL)

# Untransformed version of HID_markov:
set.seed(1)
HID_results <- list()
HID_results$Prior_samples[['LHS']] <- sample_prior_LHS(
  .n_samples = 10000,
  .l_params = HID_data$l_params)

HID_results$Prior_samples[['FGS']] <- sample_prior_FGS(
  .n_samples = 10000,
  .l_params = HID_data$l_params)

HID_results$Prior_samples[['RGS']] <- sample_prior_RGS(
  .n_samples = 10000,
  .l_params = HID_data$l_params)
##
HID_results$Calib_results$Random[[1]] <- wSSE_GOF(
  .func = HID_markov, .optim = FALSE,
  .args = NULL,
  .samples = HID_results$Prior_samples$LHS[1:3,],
  .l_targets = HID_data$l_targets,
  .sample_method = "LHS")

HID_results$Calib_results$Random[[2]] <- LLK_GOF(
  .func = HID_markov, .optim = FALSE,
  .args = NULL,
  .samples = HID_results$Prior_samples$LHS[1,],
  .l_targets = HID_data$l_targets,
  .sample_method = "LHS")
##
HID_results$Prior_samples[['LHS_Directed']] <- sample_prior_LHS(
  .n_samples = 5,
  .l_params = HID_data$l_params)

HID_results$Calib_results$Directed[[1]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

HID_results$Calib_results$Directed[[2]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'NM',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

HID_results$Calib_results$Directed[[3]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

HID_results$Calib_results$Directed[[4]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'BFGS',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000)

HID_results$Calib_results$Directed[[5]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  fnscale = -1,
  temp = 10,
  tmax = 10,
  maxit = 1000)

HID_results$Calib_results$Directed[[6]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'SANN',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

HID_results$Calib_results$Directed[[7]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'LLK',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

HID_results$Calib_results$Directed[[8]] <- calibrateModel_directed(
  .l_params = HID_data$l_params,
  .func = HID_markov,
  .args = NULL,
  .gof = 'SSE',
  .samples = HID_results$Prior_samples$LHS_Directed,
  .s_method = 'GA',
  .maximise = TRUE,
  .l_targets = HID_data$l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)
##
HID_results$Prior_samples[['LHS_Bayesian']] <- sample_prior_LHS(
  .n_samples = 10000,
  .l_params = HID_data$l_params)

HID_results$Calib_results$Bayesian[[1]] = calibrateModel_beyesian(
  .b_method = 'SIR', .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets,
  .n_resample = 10000,
  .l_params = HID_data$l_params,
  .samples = HID_results$Prior_samples$LHS_Bayesian)

set.seed(1) # Function crashes on set.seed(1)
HID_results$Calib_results$Bayesian[[2]] = calibrateModel_beyesian(
  .b_method = 'IMIS', .func = HID_markov,
  .args = NULL,
  .l_targets = HID_data$l_targets,
  .l_params = HID_data$l_params,
  .transform = FALSE,
  .n_resample = 10000,
  .IMIS_iterations = 400,
  .IMIS_sample = 100)
##
HID_results$PSA_samples[["Directed"]] <- PSA_calib_values(
  .l_calib_res_lists = HID_results$Calib_results$Directed,
  .search_method = 'Directed',
  .PSA_samples = 10000,
  .transform_ = FALSE,
  .l_params = HID_data$l_params)

HID_results$PSA_samples[["Random"]] <- PSA_calib_values(
  .l_calib_res_lists = HID_results$Calib_results$Random,
  .search_method = 'Random',
  .PSA_samples = 10000,
  .transform_ = FALSE,
  .l_params = HID_data$l_params)

HID_results$PSA_samples[["Bayesian"]] <- PSA_calib_values(
  .l_calib_res_lists = HID_results$Calib_results$Bayesian,
  .search_method = 'Bayesian',
  .PSA_samples = 10000,
  .transform_ = FALSE,
  .l_params = HID_data$l_params)
##
HID_results$PSA_results <- run_PSA(
  .func_ = HID_markov,
  .PSA_calib_values_ = c(HID_results$PSA_samples$Directed,
                         HID_results$PSA_samples$Random,
                         HID_results$PSA_samples$Bayesian),
  .args_ = list(calibrate_ = FALSE),
  .PSA_unCalib_values_ = NULL)

#Pete plot ################################
## install coda from CRAN and my MCIR package from github:
## devtools::install_github('petedodd/MCIR')
library(MCIR)
library(coda)

## rosebrock function log likelihood

## with gradient:
rosenbrock <- function(x){
  f <- (1-x[1])^2 + 100*(x[2] - x[1]^2)^2
  g <- c(100*2*(x[2] - x[1]^2)*(-2)*x[1] - 2*(1-x[1]),
         200*(x[2] - x[1]^2) )
  return(list(logp=-f,grad=-g))
}

## w/o gradient
rosen <- function(x) return(rosenbrock(x)$logp)

run <- nuts_da(rosengrad,5e3,1e3,runif(2))
corplot(run)

#R6: ###############################################################

cal = calibrateR_R6$
  new(
    .model = HID_markov_2,
    .params = HID_data2$l_params,
    .targets = HID_data2$l_targets,
    .args = NULL,
    .transform = TRUE
  )
cal$
  sampleR(
    .n_samples = 5,
    .sampling_method = c("LHS", "RGS", "FGS")
  )
cal$
  calibrateR_random(
  .optim = FALSE,
  .maximise = TRUE,
  .weighted = TRUE,
  .sample_method = c("LHS", "RGS", "FGS"),
  .calibration_method = c("LLK", "SSE"))
cal$
  calibrateR_directed(
    .gof = c('LLK', 'SSE'),
    .args = NULL,
    .n_samples = 5,
    .calibration_method = c('NM'),
    .sample_method = c("LHS", "RGS", "FGS"),
    .max_iterations = 1000,
    temp = 10,
    tmax = 10)
cal$
  calibrateR_bayesian(
  .b_method = 'SIR',
  .n_resample = 10000)
cal$calibrateR_bayesian(
  .b_method = 'IMIS',
  .n_resample = 10000,
  .IMIS_iterations = 400,
  .IMIS_sample = 100)




















