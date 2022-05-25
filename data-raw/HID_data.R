# This script defines the l_params and l_targets list for the HID_Markov():

v_targets_names <- c("Prev", "Surv", "Trt_vol")
v_targets_labels <- c("Prev" = "Prevalence",
                      "Surv" = "Survival",
                      "Trt_vol" = "Treatment volume")
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
                       'ub' = c(7.1/100, 18.3/100, 12.8/100)),
       'Surv' = dplyr::tibble('value' = 10,
                       'se' = 2/1.96,
                       'lb' = 8,
                       'ub' = 12),
       'Trt_vol' = dplyr::tibble('value' = 75000,
                          'se' = 5000/1.96,
                          'lb' = 70000,
                          'ub' = 80000),
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("mu_e", "mu_l", "mu_t", "p", "r_l", "rho",
                    "b")
v_params_true_values <- c('mu_e' = 0.05300305, 'mu_l' = 0.09492525,
                          'mu_t' = 0.06949119, 'p' = 0.35494867,
                          'r_l' = 0.70669255, 'rho' = 0.62436759,
                          'b' = 0.27229941)
v_params_dists <- c("lnorm", "lnorm", "lnorm", "lnorm", "lnorm", "lnorm",
                    "beta")
args <- list(list(meanlog = -3.121, sdlog = 0.5),
             list(meanlog = -1.511, sdlog = 0.5),
             list(meanlog = -3.814, sdlog = 0.5),
             list(meanlog = -2.428, sdlog = 0.5),
             list(meanlog = -0.818, sdlog = 0.5),
             list(meanlog = -0.818, sdlog = 0.5),
             list(shape1 = 2, shape2 = 8))
extra_args <- list(list(min = 0.02, max = 0.12),
                   list(min = 0.08, max = 0.59),
                   list(min = 0.01, max = 0.06),
                   list(min = 0.03, max = 0.24),
                   list(min = 0.17, max = 1.18),
                   list(min = 0.01, max = 0.06),
                   list(min = 0.03, max = 0.48))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_true_values' = v_params_true_values,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)

HID_data <- list('l_params' = l_params,
                 'l_targets' = l_targets)

usethis::use_data(HID_data, overwrite = TRUE)
