# This script defines the l_params and l_targets list for the HID_Markov():
## Targets:----
v_targets_names <- c("Prev", "Surv", "Trt_vol")
v_targets_labels <- c("Prev" = "Prevalence",
                      "Surv" = "Survival",
                      "Trt_vol" = "Treatment volume")
v_targets_axis <- list("Prev" = list("x" = 'value', "y" = 'Year'),
                    "Surv" = list("x" = 'value', "y" = 'Year'),
                    "Trt_vol" = list("x" = 'value', "y" = 'Year'))
v_targets_weights <- c(1, 1, 1)
v_targets_dists <- c("binom", "norm", "norm")
l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_labels' = v_targets_labels,
       'Prev' = dplyr::tibble('value' = c(5/100, 15/100, 10/100), # %
                              'se' = c(5/1000, 15/1000, 10/1000), # 10% of value
                              'x' = c(25, 75, 50),
                              'size' = 500,
                              'lb' = c(3.3/100, 12/100, 7.5/100),
                              'ub' = c(7.1/100, 18.3/100, 12.8/100),
                              'Year' = c(10, 20, 30)),
       'Surv' = dplyr::tibble('value' = 10,
                              'se' = 2/1.96,
                              'lb' = 8,
                              'ub' = 12,
                              'Year' = 10),
       'Trt_vol' = dplyr::tibble('value' = 75000,
                                 'se' = 5000/1.96,
                                 'lb' = 70000,
                                 'ub' = 80000,
                                 'Year' = 30),
       'v_targets_axis' = v_targets_axis,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
## Parameters:----
v_params_names <- c("mu_e", "mu_l", "mu_t", "p", "r_l", "rho", "b")
v_params_names <- c(
  "mu_e" = "Cause-specific mortality rate with early-stage disease",
  "mu_l" = "Cause-specific mortality rate with late-stage disease",
  "mu_t" = "Cause-specific mortality rate on treatment",
  "p"    = "Transition rate from early to late-stage disease",
  "r_l"  = "Rate of uptake onto treatment from late-stage disease",
  "rho"  = "Effective contact rate",
  "b"    = "Fraction of population in at-risk group")
v_params_true_values <- c('mu_e' = 0.04, 'mu_l' = 0.15, 'mu_t' = 0.016,
                          'p' = 0.12, 'r_l' = 0.41, 'rho' = 0.53, 'b' = 0.21)
v_params_dists <- c("lnorm", "lnorm", "lnorm", "lnorm", "lnorm", "lnorm", "beta")
args <- list(list(meanlog = 0, sdlog = 1),
             list(meanlog = 0, sdlog = 1),
             list(meanlog = 0, sdlog = 1),
             list(meanlog = 0, sdlog = 1),
             list(meanlog = 0, sdlog = 1),
             list(meanlog = 0, sdlog = 1),
             list(shape1 = 1, shape2 = 1))
extra_args <- list(list(min = 0, max = 15),
                   list(min = 0, max = 15),
                   list(min = 0, max = 15),
                   list(min = 0, max = 15),
                   list(min = 0, max = 15),
                   list(min = 0, max = 15),
                   list(min = 0, max = 1))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_true_values' = v_params_true_values,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)

HID_data_flat <- list('l_params' = l_params,
                 'l_targets' = l_targets)

usethis::use_data(HID_data_flat, overwrite = TRUE)
