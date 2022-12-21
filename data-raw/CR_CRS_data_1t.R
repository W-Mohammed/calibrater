# CRS Model calibration data - One target (Survival)
# Targets simulated using 10% of true parameters'
# values as the sd in a PSA analysis

## Targets:----
v_targets_names <- c("Surv")
v_targets_labels <- c('Surv' = "Survival")
v_targets_axis <- list('Surv' = list("x" = 'value', "y" = 'Year'))
v_targets_weights <- c('Surv' = 1)
### Likelihood distributions:----
v_targets_dists <- c('Surv' = "norm")
### Load saved target data:----
data("CRS_targets_2")
### Pack targets distributions together:----
l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_labels' = v_targets_labels,
       'Surv' = CRS_targets_2$Surv,
       'v_targets_axis' = v_targets_axis,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)

## Parameters:----
v_params_names <- c('p_Mets' = "p_Mets", 'p_DieMets' = "p_DieMets")
v_params_labels <- c('p_Mets' = "Probability of contracting cancer",
                     'p_DieMets' = "Probability of dying from cancer")
v_params_tex_labels <- c(
  'p_Mets' = "\\textbf{p^{nC \\rightarrow C}}",
  'p_DieMets' = "\\textbf{p^{C \\rightarrow D}}")
v_params_true_values <- c('p_Mets' = 0.10, 'p_DieMets' = 0.05)
### Prior distributions:----
#### Distribution names:----
v_params_dists <- c('p_Mets' = "unif", 'p_DieMets' = "unif")
#### Distribution moments/parameters:----
args <- list('p_Mets'    = list(min = 0, max = 1),
             'p_DieMets' = list(min = 0, max = 1))
### Parameter space bounds:----
extra_args <- list('p_Mets'    = list(min = 0, max = 1),
                   'p_DieMets' = list(min = 0, max = 1))
### Pack parameters information together:----
l_params <- list('v_params_names' = v_params_names,
                 'v_params_labels' = v_params_labels,
                 'v_params_tex_labels' = v_params_tex_labels,
                 'v_params_true_values' = v_params_true_values,
                 'v_params_dists' = v_params_dists,
                 'args' = args,
                 'Xargs' = extra_args)
## Pack targets and parameters together:----
CR_CRS_data_1t <- list('l_params' = l_params,
                       'l_targets' = l_targets)
## Save calibration data internally:----
usethis::use_data(CR_CRS_data_1t, overwrite = TRUE)
