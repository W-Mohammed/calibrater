# HID Model calibration data - Three parameters ("mu_e", "mu_l", "mu_t")
# Targets simulated using 10% of true parameters'
# values as the sd in a PSA analysis

## Targets:----
v_targets_names <- c("Prev", "Surv", "Trt_vol")
v_targets_labels <- c('Prev' = "Prevalence",
                      'Surv' = "Survival",
                      'Trt_vol' = "Treatment volume")
v_targets_axis <- list('Prev' = list("x" = 'value', "y" = 'Year'),
                       'Surv' = list("x" = 'value', "y" = 'Year'),
                       'Trt_vol' = list("x" = 'value', "y" = 'Year'))
v_targets_weights <- c('Prev' = 1, 'Surv' = 1, 'Trt_vol' = 1)
### Likelihood distributions:----
v_targets_dists <- c('Prev' = "norm", 'Surv' = "norm", 'Trt_vol' = "norm")
### Pack targets distributions together:----
l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_labels' = v_targets_labels,
       'Prev' = dplyr::tibble('value' = c(0.051, 0.143, 0.116), # %
                              'se' = c(0.0189, 0.0168, 0.01397),
                              'x' = c(527, 1390, 1160), # number of cases
                              'size' = 10000, # simulated PSA runs
                              'lb' = c(0.0222, 0.106, 0.0894),
                              'ub' = c(0.095, 0.172, 0.1439),
                              'Year' = c(10, 20, 30)),
       'Surv' = dplyr::tibble('value' = 9.933,
                              'se' = 0.521,
                              'lb' = 8.994,
                              'ub' = 11.028,
                              'Year' = 10),
       'Trt_vol' = dplyr::tibble('value' = 74396.6,
                                 'se' = 8846.7299,
                                 'lb' = 56022.0837,
                                 'ub' = 90890.4878,
                                 'Year' = 30),
       'v_targets_axis' = v_targets_axis,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)

## Parameters:----
v_params_names <- c("mu_e", "mu_l", "mu_t")
v_params_labels <- c(
  "mu_e" = "Cause-specific mortality rate with early-stage disease",
  "mu_l" = "Cause-specific mortality rate with late-stage disease",
  "mu_t" = "Cause-specific mortality rate on treatment")
v_params_true_values <- c('mu_e' = 0.04, 'mu_l' = 0.15, 'mu_t' = 0.016)
### Prior distributions:----
#### Distribution names:----
v_params_dists <- c('mu_e' = "unif", 'mu_l' = "unif", 'mu_t' = "unif")
v_params_dists2 <- c("lnorm", "lnorm", "lnorm")
#### Distribution moments/parameters:----
args <- list('mu_e' = list(min = 0, max = 5),
             'mu_l' = list(min = 0, max = 5),
             'mu_t' = list(min = 0, max = 5))
args2 <- list(list(meanlog = 0, sdlog = 1),
              list(meanlog = 0, sdlog = 1),
              list(meanlog = 0, sdlog = 1))
### Parameter space bounds:----
extra_args <- list('mu_e' = list(min = 0, max = 15),
                   'mu_l' = list(min = 0, max = 15),
                   'mu_t' = list(min = 0, max = 15))
extra_args2 <- list(list(min = 0, max = 15),
                    list(min = 0, max = 15),
                    list(min = 0, max = 15))
### Pack parameters information together:----
l_params <- list('v_params_names' = v_params_names,
                 'v_params_labels' = v_params_labels,
                 'v_params_true_values' = v_params_true_values,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)
## Pack targets and parameters together:----
CR_HID_data_3p <- list('l_params' = l_params,
                    'l_targets' = l_targets)
## Save calibration data internally:----
usethis::use_data(CR_HID_data_3p, overwrite = TRUE)
