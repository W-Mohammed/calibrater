# This script defines the l_params and l_targets list for the HID_Markov():

v_targets_names <- c("Prev", "Surv", "Trt_vol")
v_targets_labels <- c("Prev" = "Prevalence",
                      "Surv" = "Survival",
                      "Trt_vol" = "Treatment volume")
v_targets_axis <- list("Prev" = list("x" = 'value', "y" = 'Year'),
                       "Surv" = list("x" = 'value', "y" = 'Year'),
                       "Trt_vol" = list("x" = 'value', "y" = 'Year'))
v_targets_weights <- c(1, 1, 1)
v_targets_dists <- c("norm", "norm", "norm")
l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_labels' = v_targets_labels,
       'Prev' = dplyr::tibble('value' = c(0.0527, 0.139, 0.116), # %
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
       'Trt_vol' = dplyr::tibble('value' = 73161.991,
                                 'se' = 8846.7299,
                                 'lb' = 56022.0837,
                                 'ub' = 90890.4878,
                                 'Year' = 30),
       'v_targets_axis' = v_targets_axis,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("mu_e", "b")
v_params_true_values <- c('mu_e' = 0.05300305, 'b' = 0.27229941)
v_params_dists <- c("lnorm", "beta")
args <- list(list(meanlog = 0, sdlog = 1),
             list(shape1 = 1, shape2 = 1))
extra_args <- list(list(min = 0, max = 15),
                   list(min = 0, max = 1))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_true_values' = v_params_true_values,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)

HID_data_flat_2p <- list('l_params' = l_params,
                         'l_targets' = l_targets)

usethis::use_data(HID_data_flat_2p, overwrite = TRUE)
