##################################################
#      Confirmation Review case studies          #
##################################################
## Load the calibR package:
devtools::load_all()

# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)
# Targets: c(Survival, PropSick)

## Chapter 2 plots:----
### Parameter space:----
seed_no <- 1
set.seed(seed = seed_no)
### Unbounded parameter space:----
parameters_list <- calibR::CR_CRS_data_2t$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
param_space <- calibR::sample_prior_FGS_(
  .n_samples = 100,
  .l_params = parameters_list)
param_space_plot <- plotly::plot_ly(
  data = param_space,
  x = param_space[["p_Mets"]],
  y = param_space[["p_DieMets"]],
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 0.001)) %>%
  plotly::layout(
    xaxis = list(
      title = parameters_list$v_params_labels[["p_Mets"]],
      range = list(
        parameters_list$Xargs[["p_Mets"]]$min,
        parameters_list$Xargs[["p_Mets"]]$max)),
    yaxis = list(
      title = parameters_list$v_params_labels[["p_DieMets"]],
      range = list(
        parameters_list$Xargs[["p_DieMets"]]$min,
        parameters_list$Xargs[["p_DieMets"]]$max)))

reticulate::py_run_string("import sys")
plotly::save_image(
  p = param_space_plot,
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_parameter_space.jpeg",
  scale = 5)

### Explored parameter space:----
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
param_space <- calibR::sample_prior_FGS_(
  .n_samples = 100,
  .l_params = parameters_list)
param_space_plot <- plotly::plot_ly(
  data = param_space,
  x = param_space[["p_Mets"]],
  y = param_space[["p_DieMets"]],
  type = 'scatter',
  mode = 'markers',
  marker = list(size = 0.001)) %>%
  plotly::layout(
    xaxis = list(
      title = parameters_list$v_params_labels[["p_Mets"]],
      range = list(
        parameters_list$Xargs[["p_Mets"]]$min,
        parameters_list$Xargs[["p_Mets"]]$max)),
    yaxis = list(
      title = parameters_list$v_params_labels[["p_DieMets"]],
      range = list(
        parameters_list$Xargs[["p_DieMets"]]$min,
        parameters_list$Xargs[["p_DieMets"]]$max)))

reticulate::py_run_string("import sys")
plotly::save_image(
  p = param_space_plot,
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_parameter_space_lp.jpeg",
  scale = 5)

### SEE fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
#### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
#### SSE fitness function without True values:----
distance_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE")$
  plots$
  GOF_plots$
  blank

reticulate::py_run_string("import sys")
plotly::save_image(
  p = distance_GOF_measure[[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_distance_GOF_measure.jpeg", scale = 5)

### LLK fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
#### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
#### LLK function without True values:----
llik_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "LLK")$
  plots$
  GOF_plots$
  blank

reticulate::py_run_string("import sys")
plotly::save_image(
  p = llik_GOF_measure[[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_log_likelihood_GOF_measure.jpeg", scale = 5)

### RGS, FGS and LHS with SEE fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
#### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
#### Generate samples using FGS, RGS and LHS:----
CR_CRS_2P2T$
  sampleR(
    .n_samples = 1e2,
    .sampling_method = c("RGS", "FGS", "LHS"))
CR_CRS_2P2T$
  prior_samples$FGS <- calibR::sample_prior_FGS_(
    .n_samples = 1e2,
    .l_params = parameters_list)
##### Unguided searching methods:----
CR_CRS_2P2T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = FALSE,
    .weighted = FALSE,
    .sample_method = c("RGS", "FGS", "LHS"),
    .calibration_method = "SSE")
###### Fitness function without True values:----
random_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = FALSE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE")$
  plots$
  GOF_plots$
  random

reticulate::py_run_string("import sys")
plotly::save_image(
  p = random_GOF_measure[[1]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_RGS.jpeg",
  scale = 5)
plotly::save_image(
  p = random_GOF_measure[[2]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_FGS.jpeg",
  scale = 5)
plotly::save_image(
  p = random_GOF_measure[[3]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_LHS.jpeg",
  scale = 5)

###### Fitness function with True values:----
random_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE")$
  plots$
  GOF_plots$
  random

reticulate::py_run_string("import sys")
plotly::save_image(
  p = random_GOF_measure[[1]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_RGS2.jpeg",
  scale = 5)
plotly::save_image(
  p = random_GOF_measure[[2]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_FGS2.jpeg",
  scale = 5)
plotly::save_image(
  p = random_GOF_measure[[3]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_LHS2.jpeg",
  scale = 5)

### BFGS, NM, and SANN with SEE fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
#### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
#### Generate samples using Random Grid Search:----
CR_CRS_2P2T$
  sampleR(
    .n_samples = 10,
    .sampling_method = "RGS")
#### Directed calibration:----
CR_CRS_2P2T$
  calibrateR_directed(
    .gof = 'SSE',
    .n_samples = 10,
    .maximise = FALSE,
    .calibration_method = c("NM", "BFGS", "SANN"),
    .sample_method = "RGS",
    .max_iterations = 1e4,
    temp = 1000,
    trace = FALSE)
##### Fitness function without True values:----
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = FALSE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE")$
  plots$
  GOF_plots$
  directed

reticulate::py_run_string("import sys")
plotly::save_image(
  p = SSE_GOF_measure[[1]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_NM.jpeg",
  scale = 5)
plotly::save_image(
  p = SSE_GOF_measure[[2]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_BFGS.jpeg",
  scale = 5)
plotly::save_image(
  p = SSE_GOF_measure[[3]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_SNN.jpeg",
  scale = 5)
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE",
  .x_axis_lb_ = 0.098578,
  .x_axis_ub_ = 0.098588,
  .y_axis_lb_ = 0.0496,
  .y_axis_ub_ = 0.0502)$
  plots$
  GOF_plots$
  directed
plotly::save_image(
  p = SSE_GOF_measure[[1]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_NM_z.jpeg",
  scale = 5)
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE",
  .x_axis_lb_ = 0.098578,
  .x_axis_ub_ = 0.098592,
  .y_axis_lb_ = 0.04986,
  .y_axis_ub_ = 0.0499)$
  plots$
  GOF_plots$
  directed
plotly::save_image(
  p = SSE_GOF_measure[[2]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_BFGS_z.jpeg",
  scale = 5)
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE",
  .x_axis_lb_ = 0.098580995,
  .x_axis_ub_ = 0.098581003,
  .y_axis_lb_ = 0.049885,
  .y_axis_ub_ = 0.04988515)$
  plots$
  GOF_plots$
  directed
plotly::save_image(
  p = SSE_GOF_measure[[3]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_SNN_z.jpeg",
  scale = 5)

##### Fitness function with True values:----
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE")$
  plots$
  GOF_plots$
  directed

reticulate::py_run_string("import sys")
plotly::save_image(
  p = SSE_GOF_measure[[1]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_NM2.jpeg",
  scale = 5)
plotly::save_image(
  p = SSE_GOF_measure[[2]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_BFGS2.jpeg",
  scale = 5)
plotly::save_image(
  p = SSE_GOF_measure[[3]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_SNN2.jpeg",
  scale = 5)
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE",
  .x_axis_lb_ = 0.098578,
  .x_axis_ub_ = 0.098588,
  .y_axis_lb_ = 0.0496,
  .y_axis_ub_ = 0.0502)$
  plots$
  GOF_plots$
  directed
plotly::save_image(
  p = SSE_GOF_measure[[1]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_NM_z2.jpeg",
  scale = 5)
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE",
  .x_axis_lb_ = 0.098578,
  .x_axis_ub_ = 0.098592,
  .y_axis_lb_ = 0.04986,
  .y_axis_ub_ = 0.0499)$
  plots$
  GOF_plots$
  directed
plotly::save_image(
  p = SSE_GOF_measure[[2]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_BFGS_z2.jpeg",
  scale = 5)
SSE_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = "SSE",
  .x_axis_lb_ = 0.098580995,
  .x_axis_ub_ = 0.098581003,
  .y_axis_lb_ = 0.049885,
  .y_axis_ub_ = 0.04988515)$
  plots$
  GOF_plots$
  directed
plotly::save_image(
  p = SSE_GOF_measure[[3]][[1]][[1]],
  file = "../../2. Confirmation Review/CR_data/Case_study_1/chap_2_SSE_SNN_z2.jpeg",
  scale = 5)

## Chapter 3 plots:----
## Effects of using more calibration targets on CRS_markov_2 model:----
### One target testing:----
seed_no <- 1
set.seed(seed = seed_no)
#### Initiate CalibR R6 object:----
CR_CRS_2P1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE)
#### Generate samples using Random Grid Search:----
CR_CRS_2P1T$
  sampleR(
    .n_samples = 1e4,
    .sampling_method = c("RGS", "FGS", "LHS"))
#### Parameter exploration calibration methods:----
##### Unguided searching methods:----
CR_CRS_2P1T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("RGS", "FGS", "LHS"),
    .calibration_method = "LLK")
##### Guided searching methods:----
CR_CRS_2P1T$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 1e1,
    .calibration_method = c("NM", "BFGS"),
    .sample_method = "RGS",
    .max_iterations = 1e3,
    temp = 1,
    trace = FALSE)
#### Bayesian methods:----
CR_CRS_2P1T$
  calibrateR_bayesian(
    .b_method = c("SIR", "IMIS", "MCMC"),
    .n_resample = 1e3,
    .IMIS_iterations = 200,
    .IMIS_sample = 1e3,
    .MCMC_burnIn = 1e4,
    .MCMC_samples = 4e4,
    .MCMC_thin = 4,
    .MCMC_rerun = TRUE)
#### Plots:----
CR_CRS_2P1T$draw_log_likelihood(.points_ = F)

## * Effects of using more calibration targets on CRS_markov_2 model:----
### One target testing:----
seed_no <- 1
set.seed(seed = seed_no)
#### Initiate CalibR R6 object:----
CR_CRS_2P2T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_2t$l_params,
    .targets = CR_CRS_data_2t$l_targets,
    .args = NULL,
    .transform = FALSE)
#### Generate samples using Random Grid Search:----
CR_CRS_2P2T$
  sampleR(
    .n_samples = 1e2,
    .sampling_method = c("RGS", "FGS", "LHS"))
#### Parameter exploration calibration methods:----
##### Unguided searching methods:----
CR_CRS_2P2T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("RGS", "FGS", "LHS"),
    .calibration_method = "LLK")
##### Guided searching methods:----
CR_CRS_2P2T$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 1e1,
    .calibration_method = c("NM", "BFGS", "SANN"),
    .sample_method = "RGS",
    .max_iterations = 1e3,
    temp = 1,
    trace = FALSE)
#### Bayesian methods:----
CR_CRS_2P2T$
  calibrateR_bayesian(
    .b_method = c("SIR", "IMIS", "MCMC"),
    .n_resample = 1e2,
    .IMIS_iterations = 200,
    .IMIS_sample = 1e3,
    .MCMC_burnIn = 1e4,
    .MCMC_samples = 4e4,
    .MCMC_thin = 30,
    .MCMC_rerun = TRUE,
    .diag_ = TRUE)
#### Sample PSA values:----
CR_CRS_2P2T$
  sample_PSA_values(
    .calibration_methods = c("Random", "Directed", "Bayesian"),
    .PSA_samples = 1e2)
#### Run PSA:----
CR_CRS_2P2T$run_PSA(
  .PSA_unCalib_values_ = NULL)
#### Plots:----
##### Plot fitness function:----
CR_CRS_2P2T$draw_GOF_measure(.points_ = F)
##### Plot targets:----
CR_CRS_2P2T$draw_targets_plots(
  .sim_targets_ = TRUE,
  .calibration_methods_ = c("random", "directed", "bayesian"))

plotly::plot_ly(
  x = test[["p_Mets"]],
  y = test[["p_DieMets"]],
  z = test$Overall_fit,
  type = "contour")

test2 %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = p_Mets,
      y = p_DieMets,
      color = Overall_fit)) +
  ggplot2::geom_density_2d_filled() +
  ggplot2::theme(axis.title.y = ggplot2::element_text(
    angle = 0,
    vjust = 0.5))


CR_CRS_2P2T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_2t$l_params,
    .targets = CR_CRS_data_2t$l_targets,
    .args = NULL,
    .transform = FALSE)
CR_CRS_2P2T$
  draw_log_likelihood(.legend_ = TRUE, .greys_ = TRUE, .scale_ = "YlOrRd")
tmpfile = CR_CRS_2P2T$
  draw_log_likelihood(.legend_ = TRUE, .greys_ = TRUE)

Sys.setenv(RETICULATE_PYTHON = "C:\\ProgramData\\Anaconda3")
library(reticulate)
# py_install("pandas")
# py_install("kaleido")
# library(reticulate)
# path_to_python <- "C:/Program Files/Python311/python.exe"
# use_python(path_to_python)
#
# reticulate::install_miniconda()
# reticulate::conda_install('r-reticulate-test', 'python-kaleido')
# reticulate::conda_install('r-reticulate-test', 'plotly', channel = 'plotly')
# reticulate::use_miniconda('r-reticulate-test')

plotly::export(p = tmpfile[["p_Mets"]][["p_DieMets"]], file = "../../2. Confirmation Review/CR_data/Case_study_1/log_likelihood.pdf")
reticulate::py_run_string("import sys")
plotly::save_image(p = tmpfile[["p_Mets"]][["p_DieMets"]], file = "../../2. Confirmation Review/CR_data/Case_study_1/log_likelihood5.jpeg", scale = 5)
plotly::save_image(p = tmpfile[["p_Mets"]][["p_DieMets"]], file = "images/tmp.pdf")
plotly::kaleido(p = tmpfile[["p_Mets"]][["p_DieMets"]], file = "../../2. Confirmation Review/CR_data/Case_study_1/log_likelihood5.jpeg", scale = 5)
# plotly::kaleido(p = tmpfile[["p_Mets"]][["p_DieMets"]], file = "../../2. Confirmation Review/CR_data/Case_study_1/log_likelihood.png")
# plotly::kaleido(p = tmpfile[["p_Mets"]][["p_DieMets"]], file = "images/tmp.pdf")

# saveRDS(object = tmpfile, file = "../../2. Confirmation Review/CR_data/Case_study_1/log_likelihood.rds")
#
# CRS_true_CE = readRDS(file = "../../2. Confirmation Review/CR_data/Case_study_1/CRS_true_PSA.rds")

## Initial and identified values:----
devtools::load_all()

## Effects of using more calibration targets on CRS_markov_2 model:----
### One target testing:----
#### Initiate CalibR R6 object:----
CR_CRS_2P1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE)
#### Generate samples using Random Grid Search:----
CR_CRS_2P1T$
  sampleR(
    .n_samples = 1e4,
    .sampling_method = c("RGS", "FGS", "LHS"))
#### Parameter exploration calibration methods:----
##### Guided searching methods:----
CR_CRS_2P1T$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 10,
    .calibration_method = c("NM", "BFGS", "SANN"),
    .sample_method = "RGS",
    .max_iterations = 1e4,
    temp = 10,
    tmax = 1000)
#### Log-likelihood:----
CR_CRS_2P1T$draw_log_likelihood(.greys_ = T, .legend_ = F)
plots <- CR_CRS_2P1T[["calibration_results"]]$directed %>%
  purrr::map(
    .x = .,
    .f = function(.calib_res_algorithm) {
      ## Transpose the list to group outputs together:----
      transposed_calib_res <- .calib_res_algorithm %>%
        purrr::transpose()
      ## Extract "Starting values":----
      calib_res <- transposed_calib_res[["Guess"]] %>%
        purrr::map_dfr(
          .x = .,
          .f = function(.x) {
            .x}) %>%
        dplyr::mutate(Points = "Starting values") %>%
        ## Join "Identified values":----
      dplyr::bind_rows(
        ## Extract identified values:----
        transposed_calib_res[["Estimate"]] %>%
          purrr::map_dfr(
            .x = .,
            .f = function(.x) {
              .x}) %>%
          dplyr::mutate(Points = "Identified values"))
      ## Add points to the plots:----
      CR_CRS_2P1T[["plots"]][["log_likelihood"]][["p_Mets"]][["p_DieMets"]] %>%
        plotly::add_trace(
          p = .,
          inherit = FALSE,
          x = calib_res[["p_Mets"]],
          y = calib_res[["p_DieMets"]],
          type = 'scatter',
          mode = 'markers',
          marker = list(size = 5),
          symbol = ~ calib_res[["Points"]],
          symbols = c("triangle-up", "circle-open"))
    })

reticulate::py_run_string("import sys")
plotly::save_image(p = plots[["NM_LLK_RGS"]], file = "../../2. Confirmation Review/CR_data/Case_study_1/NM_LLK_RGS.jpeg", scale = 5)
