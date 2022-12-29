# CRS Model calibration data - One target (Survival) - Full space
# Targets simulated using 10% of true parameters' values as the sd in a PSA
# analysis

## Targets:----
v_targets_names <- c(
  'Surv' = "Surv")
v_targets_labels <- c(
  'Surv' = "Survival")
v_targets_axis <- list(
  'Surv' = list(
    'x' = "time",
    'y' = "value"))
v_targets_axis_labels <- list(
  'Surv' = list(
    'x' = "Time in years",
    'y' = "Proportion of cohort survived"))
v_targets_weights <- c(
  'Surv' = 1)
### Likelihood distributions:----
v_targets_dists <- c(
  'Surv' = "norm")
### Load saved target data:----
data("CRS_targets_2")
### Pack targets distributions together:----
l_targets <-
  list('v_targets_names' = v_targets_names,
       'v_targets_labels' = v_targets_labels,
       'Surv' = CRS_targets_2$Surv,
       'v_targets_axis' = v_targets_axis,
       'v_targets_axis_labels' = v_targets_axis_labels,
       'v_targets_dists' = v_targets_dists,
       'v_targets_weights' = v_targets_weights)

## Parameters:----
v_params_names <- c(
  'p_Mets' = "p_Mets",
  'p_DieMets' = "p_DieMets")
v_params_labels <- c(
  'p_Mets' = "Annual probability of getting cancer",
  'p_DieMets' = "Annual probability of dying from cancer")
v_params_tex_labels <- c(
  'p_Mets' = "\\textbf{p^{nC \\rightarrow C}}",
  'p_DieMets' = "\\textbf{p^{C \\rightarrow D}}")
v_params_true_values <- c(
  'p_Mets' = 0.10,
  'p_DieMets' = 0.05)
### Prior distributions:----
#### Distribution names:----
v_params_dists <- c(
  'p_Mets' = "unif",
  'p_DieMets' = "unif")
#### Distribution moments/parameters:----
args <- list(
  'p_Mets'    = list(min = 0, max = 1),
  'p_DieMets' = list(min = 0, max = 1))
### Parameter space bounds:----
extra_args <- list(
  'p_Mets'    = list(min = 0, max = 1),
  'p_DieMets' = list(min = 0, max = 1))
### Pack parameters information together:----
l_params <- list(
  'v_params_names' = v_params_names,
  'v_params_labels' = v_params_labels,
  'v_params_tex_labels' = v_params_tex_labels,
  'v_params_true_values' = v_params_true_values,
  'v_params_dists' = v_params_dists,
  'args' = args,
  'Xargs' = extra_args)

## Interventions:----
v_interv_names <- c(
  'None' = "None",
  'Medication' = "Medication",
  'Screening' = "Screening",
  'Both' = "Both")
v_interv_outcomes <- c(
  'costs' = "costs",
  'effects' = "Effects")
### Pack interventions information together:----
l_intervs <- list(
  'v_interv_names' = v_interv_names,
  'v_interv_outcomes' = v_interv_outcomes)

## Pack targets and parameters together:----
CR_CRS_data_1t <- list(
  'l_params' = l_params,
  'l_targets' = l_targets,
  'l_intervs' = l_intervs)

## Save calibration data internally:----
usethis::use_data(
  CR_CRS_data_1t,
  overwrite = TRUE)
