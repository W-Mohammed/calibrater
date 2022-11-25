# CRS Model data - One target (Survival)
# Targets simulated using 10% of true parameters'
# values as the sd in a PSA analysis

data("CRS_targets_2")

v_targets_names <- c("Surv")
v_targets_labels <- c("Surv" = "Survival")
v_targets_dists <- c("norm")
v_targets_weights <- c(1)
v_targets_axis <- list("Surv" = list("x" = 'value', "y" = 'Year'))

l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_labels' = v_targets_labels,
       'Surv' = CRS_targets_2$Surv,
       'v_targets_axis' = v_targets_axis,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)

v_params_names <- c("p_Mets", "p_DieMets")
v_params_true_values <- c("p_Mets" = 0.10, "p_DieMets" = 0.05)
v_params_dists <- c("unif", "unif")
args <- list(list(min = 0.04, max = 0.16),
             list(min = 0.04, max = 0.12))
extra_args <- list(list(min = 0, max = 1),
                   list(min = 0, max = 1))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_true_values' = v_params_true_values,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)

CRS_data_1 <- list('l_params' = l_params,
                   'l_targets' = l_targets)

usethis::use_data(CRS_data_1, overwrite = TRUE)
