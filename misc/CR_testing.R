##################################################
#      Confirmation Review case studies          #
##################################################
## Load the calibR package:
devtools::load_all()
## Create calibration data:----
internal_data <- list.files(path = "data-raw/", pattern = "CR_",full.names = T)
purrr::walk(internal_data, .f = source)

## Effects of using more calibration targets on CRS_markov_2 model:----
### One target testing:----
CR_CRS_1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_CRS_1T$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_CRS_1T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_CRS_1T$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_CRS_1T$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Two targets testing:----
CR_CRS_2T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_2t$l_params,
    .targets = CR_CRS_data_2t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_CRS_2T$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_CRS_2T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_CRS_2T$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_CRS_2T$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

## Effects of model dimensionality on model calibration using HID_markov model:----
### One parameter testing:----
CR_HID_1P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_1p$l_params,
    .targets = CR_HID_data_1p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_1P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_1P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_1P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_1P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Two parameters testing:----
CR_HID_2P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_2p$l_params,
    .targets = CR_HID_data_2p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_2P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_2P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_2P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_2P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Three parameters testing:----
CR_HID_3P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_3p$l_params,
    .targets = CR_HID_data_3p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_3P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_3P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_3P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_3P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Four parameters testing:----
CR_HID_4P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_4p$l_params,
    .targets = CR_HID_data_4p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_4P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_4P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_4P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_4P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Five parameters testing:----
CR_HID_5P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_5p$l_params,
    .targets = CR_HID_data_5p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_5P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_5P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_5P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_5P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Six parameters testing:----
CR_HID_6P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_6p$l_params,
    .targets = CR_HID_data_6p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_6P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_6P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_6P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_6P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### All parameters testing:----
CR_HID_7P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_7p$l_params,
    .targets = CR_HID_data_7p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_7P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_7P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_7P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_7P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)













### MISC testing:
calibR::sample_prior_RGS(.n_samples = 1, .l_params = CR_CRS_data_1t$l_params)
## Sample a random set of parameters as a starting point for the chain:
guess <- calibR::sample_prior_RGS_(
  .n_samples = 1,
  .l_params = CR_CRS_data_1t$l_params)
## Run the Metropolis-Hastings algorithm
fit_MCMC <- MHadaptive::Metro_Hastings(
  li_func = calibR::log_posterior,
  pars = guess,
  par_names = CR_CRS_data_1t$l_params[["v_params_names"]],
  iterations = 5e4,
  burn_in = 1e4,
  .func = CRS_markov_2,
  .l_targets = CR_CRS_data_1t$l_targets,
  .l_params = CR_CRS_data_1t$l_params,
  .args = NULL,
  .transform = FALSE)

test_Bayesian = calibrateModel_beyesian(
  .b_method = 'SIR', .func = CRS_markov, .args = NULL,
  .l_targets = l_targets, .l_params = l_params, .samples = samples)

test_Bayesian = calibR::calibrateModel_beyesian(
  .b_method = "MCMC",
  .func = CRS_markov,
  .args = NULL,
  .l_targets = CR_CRS_data_1t$l_targets,
  .l_params = CR_CRS_data_1t$l_params,
  .samples = NULL,
  .n_resample = 1e4,
  .MCMC_burnIn = 1e4,
  .MCMC_samples = 6e4,
  .MCMC_thin = 5,
  .transform = FALSE)


CR_CRS_1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_CRS_1T$
  sampleR(
    .n_samples = 1000,
    .sampling_method = c("LHS")
  )
CR_CRS_1T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("LHS"),
    .calibration_method = "LLK")
CR_CRS_1T$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 10,
    .calibration_method = c('NM', 'BFGS', 'SANN'),
    .sample_method = "LHS",
    .max_iterations = 1000,
    temp = 10,
    tmax = 10)
CR_CRS_1T$
  calibrateR_bayesian(
    .b_method = c('SIR', 'IMIS', 'MCMC'),
    .n_resample = 10000,
    .IMIS_iterations = 400,
    .IMIS_sample = 100,
    .MCMC_burnIn = 10000,
    .MCMC_samples = 50000,
    .MCMC_thin = 5,
    .MCMC_rerun = TRUE)
##
##
CR_HID_3P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_3p$l_params,
    .targets = CR_HID_data_3p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_3P$
  sampleR(
    .n_samples = 1000,
    .sampling_method = c("LHS")
  )
CR_HID_3P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("LHS"),
    .calibration_method = "LLK")
CR_HID_3P$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 10,
    .calibration_method = c('NM', 'BFGS', 'SANN'),
    .sample_method = "LHS",
    .max_iterations = 1000,
    temp = 10,
    tmax = 10)
CR_HID_3P$
  calibrateR_bayesian(
    .b_method = c('SIR', 'IMIS', 'MCMC'),
    .n_resample = 10000,
    .IMIS_iterations = 400,
    .IMIS_sample = 100,
    .MCMC_burnIn = 10000,
    .MCMC_samples = 50000,
    .MCMC_thin = 5,
    .MCMC_rerun = TRUE)
