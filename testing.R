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
  log_likelihood<-sum( dnorm(data[,2],pred,sd_e, log=TRUE) )
  prior<- prior_reg(pars)
  return(log_likelihood + prior)
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
# Number of initial starting points - Nelder-mead:
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
                  fn = log_likelihood, # GOF is log likelihood
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

testing <- log_likelihood(.func = CRS_markov,
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

GOF_llik <- log_likelihood(.func = CRS_markov, .samples = samples,
                           .l_targets = l_targets)
###########################
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

samples <- sample_prior_LHS(
  .l_params = list(v_params_names = v_params_names,                             v_params_dists = v_params_dists, args = args), .n_samples = 10000)

GOF_llik12 <- log_likelihood(.func = CRS_markov, .samples = samples,
                            .l_targets = l_targets)
GOF_llik2 <- log_likelihood(.func = CRS_markov, .samples = samples,
                            .l_targets = l_targets, .optim = TRUE)
GOF_llik4 <- log_likelihood(.func = CRS_markov, .samples = samples,
                            .l_targets = l_targets, .optim = TRUE)
###########################
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

GOF_wsse12 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
                     .l_targets = l_targets)
GOF_wsse2 <- wSSE_GOF(.func = CRS_markov, .samples = samples[1:2,],
                      .l_targets = l_targets)
GOF_wsse5 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
                      .l_targets = l_targets, .optim = TRUE)
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
##########################################################

data("CRS_targets")
Surv <- CRS_targets$Surv
v_targets_names <- c("Surv", "Surv")
v_targets_weights <- c(0.5, 0.5)
v_targets_dists <- c("norm", "norm")
# v_targets_names <- c("Surv")
# v_targets_weights <- c(1)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))
l_targets <-
  list('v_targets_names' = v_targets_names,
       'Surv' = Surv,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))
set.seed(1)
samples <- sample_prior_LHS(
  .l_params = list(v_params_names = v_params_names,                             v_params_dists = v_params_dists, args = args), .n_samples = 100)

NM_optimise_mod <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = wSSE_GOF,
  .samples = samples,
  .method = 'Nelder-Mead',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

GB_optimise_mod <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = wSSE_GOF,
  .samples = samples,
  .method = 'BFGS',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

SA_optimise_mod <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = list(NULL),
  .gof = wSSE_GOF,
  .samples = samples,
  .method = 'SANN',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

GA_optimise_mod <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = list(NULL),
  .gof = wSSE_GOF,
  .samples = samples,
  .method = 'GA',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

NM_optimise_mod2 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = log_likelihood,
  .samples = samples,
  .method = 'Nelder-Mead',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

GB_optimise_mod2 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = log_likelihood,
  .samples = samples,
  .method = 'BFGS',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

SA_optimise_mod2 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = log_likelihood,
  .samples = samples,
  .method = 'SANN',
  .maximise = TRUE,
  .l_targets = l_targets,
  fnscale = -1,
  temp = 10,
  tmax = 10,
  maxit = 1000)

GA_optimise_mod2 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = list(NULL),
  .gof = log_likelihood,
  .samples = samples,
  .method = 'GA',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

compare(NM_optimise_mod, NM_optimise_mod2)
compare(GB_optimise_mod, GB_optimise_mod2)
compare(SA_optimise_mod, SA_optimise_mod2)
compare(SA_optimise_mod3, SA_optimise_mod2)
compare(SA_optimise_mod3, SA_optimise_mod)

#############################################################

NM_optimise_mod3 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = log_likelihood,
  .samples = samples,
  .method = 'Nelder-Mead',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

GB_optimise_mod3 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = log_likelihood,
  .samples = samples,
  .method = 'BFGS',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

SA_optimise_mod3 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = list(NULL),
  .gof = log_likelihood,
  .samples = samples,
  .method = 'SANN',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

compare(NM_optimise_mod3, NM_optimise_mod2)
compare(GB_optimise_mod3, GB_optimise_mod2)
compare(SA_optimise_mod3, SA_optimise_mod2)

NM_optimise_mod4 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = wSSE_GOF,
  .samples = samples,
  .method = 'Nelder-Mead',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

GB_optimise_mod4 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = NULL,
  .gof = wSSE_GOF,
  .samples = samples,
  .method = 'BFGS',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000)

SA_optimise_mod4 <- optimise_model(.l_params = list(
  v_params_names = v_params_names,
  v_params_dists = v_params_dists,
  args = args),
  .func = CRS_markov,
  .args = list(NULL),
  .gof = wSSE_GOF,
  .samples = samples,
  .method = 'SANN',
  .maximise = TRUE,
  .l_targets = l_targets,
  maxit = 1000,
  temp = 10,
  tmax = 10)

compare(NM_optimise_mod, NM_optimise_mod4)
compare(GB_optimise_mod, GB_optimise_mod4)
compare(SA_optimise_mod, SA_optimise_mod4)


























