# This script defines the l_params and l_targets list for the SS_Markov() and SS_MicroSim():

data("sickSicker_targets")
Surv = sickSicker_targets$Surv
Prev = sickSicker_targets$Prev
PropSick = sickSicker_targets$PropSick
v_targets_names <- c("Surv", "Prev", "PropSick")
v_targets_weights <- c(1, 1, 1)
v_targets_dists <- c("norm", "norm", "norm")
l_targets <-
  list('v_targets_names' = v_targets_names,
       'Surv' = Surv,
       'Prev' = Prev,
       'PropSick' = PropSick,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)
v_params_names <- c("p_S1S2", "hr_S1", "hr_S2")
v_params_dists <- c("beta", "lnorm", "lnorm")
args <- list(list(shape1 = 2, shape2 = 7),
             list(meanlog = 0.98, sdlog = 0.5),
             list(meanlog = 2.185, sdlog = 0.5))
extra_args <- list(list(min = 0, max = 1),
                   list(min = 0, max = 10),
                   list(min = 0, max = 10))
l_params <- list('v_params_names' = v_params_names,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)

sickSicker_data <- list('l_params' = l_params,
                        'l_targets' = l_targets)


usethis::use_data(sickSicker_data, overwrite = TRUE)
