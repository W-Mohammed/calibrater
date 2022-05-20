# This script defines the l_params and l_targets list for the HID_Markov_2():

v_targets_names <- c("Prev", "Surv", "Trt_vol")
v_targets_weights <- c(1, 1, 1)
v_targets_dists <- c("binom", "norm", "norm")
l_targets <-
  list('v_targets_names' = v_targets_names,
       'Prev' = tibble('value' = c(5/100, 15/100, 10/100), # %
                       'se' = c(5/1000, 15/1000, 10/1000), # 10% of value
                       'x' = c(25, 75, 50),
                       'size' = 500,
                       'lb' = c(3.3, 12, 7.5),
                       'ub' = c(7.1, 18.3, 12.8)),
       'Surv' = tibble('value' = 10,
                       'se' = 2/1.96,
                       'lb' = 8,
                       'ub' = 12),
       'Trt_vol' = tibble('value' = 75000,
                          'se' = 5000/1.96,
                          'lb' = 70000,
                          'ub' = 80000),
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("mu_e", "b")
v_params_dists <- c("norm", "unif")
v_true_params_dists <- c("lnorm", "unif")
args <- list(list(mean = -3.121, sd = 2),
             list(min = 0, max = 1))
true_args <- list(list(meanlog = -3.121, sdlog = 2),
                  list(min = 0, max = 1))
extra_args <- list(list(min = log(0.000001), max = log(50)),
                   list(min = prob_to_logit(0.000001),
                        max = prob_to_logit(0.999999)))
true_extra_args <- list(list(min = 0, max = 50),
                        list(min = 0, max = 1))
backTransform <- list('mu_e' = "exp",
                      'b' = "logit_to_prob")
l_params <- list('v_params_names' = v_params_names,
                 'v_params_dists' = v_params_dists,
                 'v_true_params_dists' = v_true_params_dists,
                 'args' = args,
                 'true_args' = true_args,
                 'Xargs' = extra_args,
                 'backTransform' = backTransform)

HID_data2_flat_2p <- list('l_params' = l_params,
                       'l_targets' = l_targets)

usethis::use_data(HID_data2_flat_2p, overwrite = TRUE)
