# This script defines the l_params and l_targets list for the HID_Markov():

v_targets_names <- "Rosenbrock"
v_targets_labels <- c("Rosenbrock" = "Rosenbrock")
v_targets_axis <- list("Rosenbrock" = list("x" = 'value'))
v_targets_weights <- c(1)
v_targets_dists <- c("norm")
l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_labels' = v_targets_labels,
       'Rosenbrock' = dplyr::tibble('value' = 0, # %
                              'se' = 1, # 10% of value
                              'lb' = 0,
                              'ub' = 0),
       'v_targets_axis' = v_targets_axis,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("x1", "x2", "x3", "x4", "x5",
                    "x6", "x7", "x8", "x9", "10")
v_params_true_values <- c("x1" = 1, "x2" = 1, "x3" = 1, "x4" = 1, "x5" = 1,
                          "x6" = 1, "x7" = 1, "x8" = 1, "x9" = 1, "x10" = 1)
v_params_dists <- c("norm", "norm", "norm", "norm", "norm",
                    "norm", "norm", "norm", "norm", "norm")
args <- list(list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1),
             list(mean = 0, sd = 1))
extra_args <- list(list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20),
                   list(min = -20, max = 20))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_true_values' = v_params_true_values,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)

ROSEN_data <- list('l_params' = l_params,
                      'l_targets' = l_targets)

usethis::use_data(ROSEN_data, overwrite = TRUE)
