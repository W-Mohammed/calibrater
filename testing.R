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
###########################
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
v_targets_names <- c("Surv")
v_targets_dists <- c('norm')
v_targets_weights <- c(1)
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

GOF_llik3 <- log_likelihood(.func = CRS_markov, .samples = samples,
                            .l_targets = l_targets)
###########################
data("CRS_targets")
Surv <- CRS_targets$Surv
# v_targets_names <- c("Surv", "Surv")
# v_targets_weights <- c(1, 1)
v_targets_names <- c("Surv")
v_targets_weights <- c(1)
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

GOF_wsse <- wSSE_GOF(.func = CRS_markov, .samples = samples,
                     .l_targets = l_targets)
GOF_wsse2 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
                      .l_targets = l_targets)
GOF_wsse3 <- wSSE_GOF(.func = CRS_markov, .samples = samples,
                      .l_targets = l_targets)
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
data("CRS_targets")
Surv <- CRS_targets$Surv
v_targets_names <- c("Surv")
v_targets_dists <- c('norm')
v_targets_weights <- c(1)
l_targets <-
  list('v_targets_names' = v_targets_names,
       'Surv' = Surv,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("p_Mets", "p_DieMets")
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))

samples <- sample_prior_LHS(
  .l_params = list(v_params_names = v_params_names,                             v_params_dists = v_params_dists, args = args), .n_samples = 10000)

NM_optimise_mod3 <- optimise_model(.params_name = v_params_names,
                       .func = CRS_markov,
                       .args = NULL,
                       .gof = log_likelihood,
                       .samples = samples[1:10,],
                       .method = 'Nelder-Mead',
                       .maximise = TRUE,
                       .l_targets = l_targets,
                       maxit = 1000)






















