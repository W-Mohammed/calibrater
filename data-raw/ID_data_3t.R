# Infectious Disease Markov Model calibration data - Three targets
# (Prev, Surv, Trt_vol). This script defines the l_params and l_targets list for
# the infectious_disease_stm(). Targets simulated using 10% of true parameters'
# values as the sd in a PSA analysis.

## Targets:----
v_targets_names <- c(
  "Prev" = "Prev",
  "Surv" = "Surv",
  "Trt_vol" = "Trt_vol")
v_targets_labels <- c(
  "Prev" = "Prevalence",
  "Surv" = "Survival",
  "Trt_vol" = "Treatment volume")
v_targets_axis <- list(
  "Prev" = list(
    "x" = 'Year',
    "y" = 'value'),
  "Surv" = list(
    "x" = 'Year',
    "y" = 'value'),
  "Trt_vol" = list(
    "x" = 'Year',
    "y" = 'value'))
v_targets_axis_labels <- list(
  'Prev' = list(
    'x' = "Time in years",
    'y' = "HIV Prevalence without treatment"),
  'Surv' = list(
    'x' = "Time in years",
    'y' = "HIV survival without treatment"),
  'Trt_vol' = list(
    'x' = "Time in years",
    'y' = "Treatment volume at 30 years"))
v_targets_weights <- c(
  'Prev' = 1,
  'Surv' = 1,
  'Trt_vol' = 1)
### Likelihood distributions:----
v_targets_dists <- c(
  'Prev' = "binom",
  'Surv' = "norm",
  'Trt_vol' = "norm")
### Pack targets distributions together:----
l_targets <- list(
  'v_targets_names' = v_targets_names,
  'v_targets_labels' = v_targets_labels,
  'Prev' = dplyr::tibble(
    'value' = c(5/100, 15/100, 10/100), # %
    'se' = c(5/1000, 15/1000, 10/1000), # 10% of value
    'x' = c(25, 75, 50),
    'size' = 500,
    'lb' = c(3.3/100, 12/100, 7.5/100),
    'ub' = c(7.1/100, 18.3/100, 12.8/100),
    'Year' = c(10, 20, 30)),
  'Surv' = dplyr::tibble(
    'value' = 10,
    'se' = 2/1.96,
    'lb' = 8,
    'ub' = 12,
    'Year' = 10),
  'Trt_vol' = dplyr::tibble(
    'value' = 75000,
    'se' = 5000/1.96,
    'lb' = 70000,
    'ub' = 80000,
    'Year' = 30),
  'v_targets_axis' = v_targets_axis,
  'v_targets_dists' = v_targets_dists,
  'v_targets_weights' = v_targets_weights)

## Parameters:----
### Names and labels:----
v_params_names <- c(
  "mu_e" = "mu_e",
  "mu_l" = "mu_l",
  "mu_t" = "mu_t",
  "p" = "p",
  "r_l" = "r_l",
  "rho" = "rho",
  "b" = "b")
v_params_labels <- c(
  "mu_e" = "Cause-specific mortality rate with early-stage disease",
  "mu_l" = "Cause-specific mortality rate with late-stage disease",
  "mu_t" = "Cause-specific mortality rate on treatment",
  "p" = "Transition rate from early to late-stage disease",
  "r_l" = "Rate of uptake onto treatment",
  "rho" = "Effective contact rate",
  "b" = "Fraction of population in at-risk group")
v_params_tex_labels <- c(
  "mu_e" = "Cause-specific mortality rate with early-stage disease",
  "mu_l" = "Cause-specific mortality rate with late-stage disease",
  "mu_t" = "Cause-specific mortality rate on treatment",
  "p" = "Transition rate from early to late-stage disease",
  "r_l" = "Rate of uptake onto treatment",
  "rho" = "Effective contact rate",
  "b" = "Fraction of population in at-risk group")
v_params_true_values <- c(
  "mu_e" = 0.04, # (0.02, 0.08)
  "mu_l" = 0.165, # (0.09, 0.29)
  "mu_t" = 0.022, # (0.01, 0.04)
  "p" = 0.131, # (0.08, 0.21)
  "r_l" = 0.585, # (0.24, 1.24)
  "rho" = 0.54, # (0.49, 0.60)
  "b" = 0.212) # (0.17, 0.26)
### Prior distributions:----
#### Transformed Distribution names:----
v_params_dists <- c(
  "mu_e" = "norm",
  "mu_l" = "norm",
  "mu_t" = "norm",
  "p" = "norm",
  "r_l" = "norm",
  "rho" = "norm",
  "b" = "norm")
#### True Distribution names:----
v_true_params_dists <- c(
  "mu_e" = "lnorm",
  "mu_l" = "lnorm",
  "mu_t" = "lnorm",
  "p" = "lnorm",
  "r_l" = "lnorm",
  "rho" = "lnorm",
  "b" = "beta")
#### Distribution moments/parameters:----
args <- list(
  "mu_e" = list(mean = -3.121,  sd = 0.5),
  "mu_l" = list(mean = -1.511,  sd = 0.5),
  "mu_t" = list(mean = -3.814,  sd = 0.5),
  "p"    = list(mean = -2.428,  sd = 0.5),
  "r_l"  = list(mean = -0.818,  sd = 0.5),
  "rho"  = list(mean = -0.818,  sd = 0.5),
  "b"    = list(mean = -1.5937, sd = 0.88))
#### Transformed Distribution moments/parameters:----
true_args <- list(
  # "mu_e" = list(mean = 0.05,  sd = 0.027),
  # "mu_l" = list(mean = 0.25,  sd = 0.133),
  # "mu_t" = list(mean = 0.025, sd = 0.013),
  # "p"    = list(mean = 0.1,   sd = 0.053),
  # "r_l"  = list(mean = 0.5,   sd = 0.267),
  # "rho"  = list(mean = 0.5,   sd = 0.267),
  "mu_e" = list(meanlog = -3.121,  sdlog = 0.5),
  "mu_l" = list(meanlog = -1.511,  sdlog = 0.5),
  "mu_t" = list(meanlog = -3.814,  sdlog = 0.5),
  "p"    = list(meanlog = -2.428,  sdlog = 0.5),
  "r_l"  = list(meanlog = -0.818,  sdlog = 0.5),
  "rho"  = list(meanlog = -0.818,  sdlog = 0.5),
  "b"    = list(shape1 = 8,   shape2 = 2))
#### Parameter space bounds:----
extra_args <- list(
  "mu_e" = list(min = -3.912023, max = -2.120264),
  "mu_l" = list(min = -2.525729, max = -0.5276327),
  "mu_t" = list(min = -4.60517,  max = -2.813411),
  "p"    = list(min = -3.506558, max = -1.427116),
  "r_l"  = list(min = -1.771957, max = 0.1655144),
  "rho"  = list(min = -4.60517,  max = -2.813411),
  "b"    = list(min = -9.21024,  max = 9.21024))
#### Transformed Parameter space bounds:----
true_extra_args <- list(
  "mu_e" = list(min = 0.02,   max = 0.12),
  "mu_l" = list(min = 0.08,   max = 0.59),
  "mu_t" = list(min = 0.01,   max = 0.06),
  "p"    = list(min = 0.03,   max = 0.24),
  "r_l"  = list(min = 0.17,   max = 1.18),
  "rho"  = list(min = 0.01,   max = 0.06),
  "b"    = list(min = 0.0001, max = 0.9999))
#### Transformation functions:----
backTransform <- list(
  'mu_e' = "exp",
  'mu_l' = "exp",
  'mu_t' = "exp",
  'p'    = "exp",
  'r_l'  = "exp",
  'rho'  = "exp",
  'b'    = "logit_to_prob")
### Pack parameters information together:----
l_params <- list(
  'v_params_names' = v_params_names,
  'v_params_labels' = v_params_labels,
  'v_params_tex_labels' = v_params_tex_labels,
  'v_params_true_values' = v_params_true_values,
  'v_params_dists' = v_params_dists,
  'v_true_params_dists' = v_true_params_dists,
  'args' = args,
  'true_args' = true_args,
  'Xargs' = extra_args,
  'true_Xargs' = true_extra_args,
  'backTransform' = backTransform)

## Interventions:----
v_interv_names <- c(
  "Status Quo" = "Status Quo",
  "Expanded Treatment" = "Expanded Treatment")
v_interv_outcomes <- c(
  'costs' = "Costs",
  'effects' = "LY")
### Pack interventions information together:----
l_intervs <- list(
  'v_interv_names' = v_interv_names,
  'v_interv_outcomes' = v_interv_outcomes)
## Pack targets and parameters together:----
ID_data_3t <- list(
  'l_params' = l_params,
  'l_targets' = l_targets,
  'l_intervs' = l_intervs)
## Save calibration data internally:----
usethis::use_data(
  ID_data_3t,
  overwrite = TRUE)
