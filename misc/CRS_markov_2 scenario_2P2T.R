##################################################
#      Confirmation Review case studies          #
##################################################
## Load the calibR package:
devtools::load_all()

# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)
# Targets: c(Survival, PropSick)
# Case study 1:----
## Saving path:----
path = "../../2. Confirmation Review/CR_data/"
case_study_dir <- "Case_study_1/"
image_dir <- "images/"
data_dir <- "data/"

## Chapter 2 plots:----
## Saving path:----
path = "../../2. Confirmation Review/CR_data/"
case_study_dir <- "Case_study_1/"
chapter_dir <- "Chap_2/"
image_dir <- "images/"
data_dir <- "data/"
image_saving_path <- glue::glue("{path}{chapter_dir}{image_dir}")
data_saving_path <- glue::glue("{path}{chapter_dir}{data_dir}")
### Parameter space:----
seed_no <- 1
set.seed(seed = seed_no)
#### Unbounded parameter space:----
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
##### Save plot:----
image_name = "parameter_space.jpeg"
reticulate::py_run_string("import sys")
plotly::save_image(
  p = param_space_plot,
  file = glue::glue("{image_saving_path}{image_name}"),
  scale = 5)

#### Explored parameter space:----
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
##### Save plot:----
image_name = "parameter_space_lp.jpeg"
reticulate::py_run_string("import sys")
plotly::save_image(
  p = param_space_plot,
  file = glue::glue("{image_saving_path}{image_name}"),
  scale = 5)

### SSE fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
gof_measure <- "SSE"
#### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
#### SSE fitness function without True values:----
sse_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
##### Save plot:----
image_name = glue::glue("{gof_measure}_GOF_measure.jpeg")
reticulate::py_run_string("import sys")
plotly::save_image(
  p = sse_GOF_measure[[1]][[1]],
  file = glue::glue("{image_saving_path}{image_name}"),
  scale = 5)

### LLK fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
gof_measure <- "LLK"
#### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
#### LLK function without True values:----
llk_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
##### Save plot:----
image_name = glue::glue("{gof_measure}_GOF_measure_ln.jpeg")
reticulate::py_run_string("import sys")
plotly::save_image(
  p = llk_GOF_measure[[1]][[1]],
  file = glue::glue("{image_saving_path}{image_name}"),
  scale = 5)

### RGS, FGS and LHS with SSE fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
gof_measure <- "LLK"
sample_method <- c("RGS", "FGS", "LHS")
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
    .sampling_method = sample_method)
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
    .sample_method = sample_method,
    .calibration_method = gof_measure)
###### Fitness function without True values:----
random_SSE_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = FALSE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  random
####### Save plot:----
######## Walk through and save files:----
purrr::walk(
  .x = random_SSE_measure %>%
    names(.) %>%
    `names<-`(names(random_SSE_measure)),
  .f = function(.gof_name) {
    image_name = glue::glue("{.gof_name}.jpeg")
    reticulate::py_run_string("import sys")
    plotly::save_image(
      p = random_SSE_measure[[.gof_name]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })

###### Fitness function with True values:----
random_SSE_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  random

####### Save plot:----
######## Walk through and save files:----
purrr::walk(
  .x = random_SSE_measure %>%
    names(.) %>%
    `names<-`(names(random_SSE_measure)),
  .f = function(.gof_name) {
    image_name = glue::glue("{.gof_name}_w_true.jpeg")
    reticulate::py_run_string("import sys")
    plotly::save_image(
      p = random_SSE_measure[[.gof_name]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })

### BFGS, NM, and SANN with SEE fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
gof_measure <- "LLK"
sample_method <- "RGS"
directed_method <- c("NM", "BFGS", "SANN")
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
    .sampling_method = sample_method)
#### Directed calibration:----
CR_CRS_2P2T$
  calibrateR_directed(
    .gof = gof_measure,
    .n_samples = 10,
    .maximise = FALSE,
    .calibration_method = directed_method,
    .sample_method = sample_method,
    .max_iterations = 100,
    temp = 1,
    trace = FALSE)
##### Fitness function without True values:----
directed_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  directed
####### Save plot:----
######## Walk through and save files:----
######### Save original view:----
purrr::walk(
  .x = directed_GOF_measure %>%
    names(.) %>%
    `names<-`(names(directed_GOF_measure)),
  .f = function(.gof_name) {
    image_name = glue::glue("{.gof_name}.jpeg")
    reticulate::py_run_string("import sys")
    plotly::save_image(
      p = directed_GOF_measure[[.gof_name]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })
######### Save zoomed view:----
list_names <- paste0(directed_method, "_", gof_measure, "_", sample_method)
if(gof_measure == "SSE") {
#(NM, BFGS, SANN)
  v_.x_axis_lb_ <- c(0.098576, 0.098578, 0.098580995)
  v_.x_axis_ub_ <- c(0.098588, 0.098592, 0.098581003)
  v_.y_axis_lb_ <- c(0.0496, 0.04986, 0.049885)
  v_.y_axis_ub_ <- c(0.0502, 0.0499, 0.04988515)
} else {
  v_.x_axis_lb_ <- c(0.098578, 0.098576, 0.098580995)
  v_.x_axis_ub_ <- c(0.098584, 0.098594, 0.098581003)
  v_.y_axis_lb_ <- c(0.0496, 0.04987, 0.049885)
  v_.y_axis_ub_ <- c(0.0502, 0.04991, 0.04988515)
}

names(v_.x_axis_lb_) <- names(v_.x_axis_ub_) <- names(v_.y_axis_lb_) <-
  names(v_.y_axis_ub_) <- names(list_names) <- list_names

purrr::walk(
  .x = list_names,
  .f = function(.calib_name) {
    directed_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
      .true_points_ = FALSE,
      .maximise_ = FALSE,
      .legend_ = FALSE,
      .coloring_ = "none",
      .greys_ = FALSE,
      .scale_ = NULL,
      .gof_ = gof_measure,
      .x_axis_lb_ = v_.x_axis_lb_[.calib_name],
      .x_axis_ub_ = v_.x_axis_ub_[.calib_name],
      .y_axis_lb_ = v_.y_axis_lb_[.calib_name],
      .y_axis_ub_ = v_.y_axis_ub_[.calib_name])$
      plots$
      GOF_plots$
      directed

    image_name = glue::glue("{.calib_name}_zoomed.jpeg")
    reticulate::py_run_string("import sys")
    plotly::save_image(
      p = directed_GOF_measure[[.calib_name]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })

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
### Case study 1: Two target - two parameters:----
#### Saving path:----
path = "../../2. Confirmation Review/CR_data/"
case_study_dir <- "Case_study_1/"
chapter_dir <- "Chap_3/"
image_dir <- "images/"
data_dir <- "data/"
image_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{image_dir}")
data_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{data_dir}")
#### Case study lists:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t$l_params
targets_list <- calibR::CR_CRS_data_2t$l_targets
gof_measure <- "LLK"
sample_method <- "RGS"
sampling_methods <- c("RGS", "FGS", "LHS")
directed_methods <- c("NM", "BFGS", "SANN")
bayesian_methods <- c("SIR", "IMIS", "MCMC")
#### Initiate CalibR R6 object:----
CR_CRS_2P2T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = parameters_list,
    .targets = targets_list,
    .args = NULL,
    .transform = FALSE)
#### Generate samples using Random Grid Search:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  sampleR(
    .n_samples = 1e2,
    .sampling_method = sampling_methods)
#### Parameter exploration calibration methods:----
##### Unguided searching methods:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = sampling_methods,
    .calibration_method = gof_measure)
##### Guided searching methods:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  calibrateR_directed(
    .gof = gof_measure,
    .n_samples = 1e1,
    .calibration_method = directed_methods,
    .sample_method = sample_method,
    .max_iterations = 1e3,
    temp = 1,
    trace = FALSE)
#### Bayesian methods:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  calibrateR_bayesian(
    .b_method = bayesian_methods,
    .n_resample = 1e2,
    .IMIS_iterations = 200,
    .IMIS_sample = 1e3,
    .MCMC_burnIn = 1e4,
    .MCMC_samples = 4e4,
    .MCMC_thin = 30,
    .MCMC_rerun = TRUE,
    .diag_ = FALSE)
#### Sample PSA values:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  sample_PSA_values(
    .calibration_methods = c("Random", "Directed", "Bayesian"),
    .PSA_samples = 1e)
#### Run PSA:----
CR_CRS_2P2T$run_PSA(
  .PSA_unCalib_values_ = NULL)
##### Extract PSA results for post processing:----
CR_CRS_2P2T_PSA_list <- CR_CRS_2P2T$PSA_results %>%
  purrr::map(
    .x = .,
    .f = function(.res_) {
      c(purrr::map(
        .x = CR_CRS_data_2t$l_intervs$v_interv_outcomes,
        .f = function(.outcome_) {
          #### Consequences table
          conseq_df <- .res_ %>%
            dplyr::select(dplyr::contains(.outcome_)) %>%
            dplyr::rename_with(.fn = function(.x) {
              stringr::str_remove(
                string = .x,
                pattern = paste0(".", .outcome_))},
              .cols = dplyr::everything())
        }),
        "Calibration_data" = list(
          .res_ %>%
            dplyr::select(
              -dplyr::contains(CR_CRS_data_2t$l_intervs$
                                 v_interv_outcomes))),
        "Interventions" = list(CR_CRS_data_2t$l_intervs$v_interv_names))
    })

##### Generate summary tables from the PSA list:----
subset_CE_table <- c("NetBenefit", "ProbabilityCE", "EVPI")
###### Tables list:----
CR_CRS_2P2T_PSA_summary_tables <- CR_CRS_2P2T_PSA_list %>%
  purrr::map(
    .x = .,
    .f = function(.calib_method) {
      PSA_summary <- ShinyPSA::summarise_PSA_(
        .effs = .calib_method[["effects"]],
        .costs = .calib_method[["costs"]],
        .params = .calib_method[["params"]],
        .interventions = .calib_method[["Interventions"]],
        .plot = FALSE)
      PSA_table <- PSA_summary %>%
      ShinyPSA::draw_summary_table_(
        .PSA_data = .,
        .latex_subtitle_ = .calib_method[["Calibration_data"]]$Label[[1]],
        .latex_ = TRUE,
        .latex_code_ = FALSE,
        .dominance_footnote_ = FALSE,
        .footnotes_sourcenotes_ = TRUE,
        .all_sourcenotes_ = T,
        .subset_tab_ = TRUE,
        .subset_group_ = subset_CE_table) %>%
        dplyr::mutate(
          Method = .calib_method[["Calibration_data"]]$Label[[1]])
    })
###### Table:----
CR_CRS_2P2T_PSA_summary_table <- CR_CRS_2P2T_PSA_list %>%
  purrr::map_df(
    .x = .,
    .f = function(.calib_method) {
      PSA_summary <- ShinyPSA::summarise_PSA_(
        .effs = .calib_method[["effects"]],
        .costs = .calib_method[["costs"]],
        .params = .calib_method[["params"]],
        .interventions = .calib_method[["Interventions"]],
        .plot = FALSE)
      PSA_table <- PSA_summary %>%
        ShinyPSA::draw_summary_table_(
          .PSA_data = .,
          .latex_subtitle_ = .calib_method[["Calibration_data"]]$Label[[1]],
          .latex_ = TRUE,
          .latex_code_ = FALSE,
          .dominance_footnote_ = FALSE,
          .footnotes_sourcenotes_ = TRUE,
          .all_sourcenotes_ = T,
          .subset_tab_ = TRUE,
          .subset_group_ = subset_CE_table) %>%
        dplyr::mutate(
          Method = .calib_method[["Calibration_data"]]$Label[[1]])
    })
####### Join true CE data:----
true_CE_object <- readRDS(
  file = "../../{path}{chapter_dir}{data_dir}CRS_true_PSA.rds")
true_CE_tab <- ShinyPSA::summarise_PSA_(
  .effs = true_CE_object[["e"]],
  .costs = true_CE_object[["c"]],
  .params = true_CE_object[["p"]],
  .interventions = true_CE_object[["treats"]],
  .plot = FALSE) %>%
  ShinyPSA::draw_summary_table_(
    .PSA_data = .,
    .latex_ = TRUE,
    .latex_code_ = FALSE,
    .dominance_footnote_ = FALSE,
    .footnotes_sourcenotes_ = TRUE,
    .all_sourcenotes_ = T,
    .subset_tab_ = TRUE,
    .subset_group_ = subset_CE_table) %>%
  dplyr::mutate(
    Method = "True")
CR_CRS_2P2T_PSA_summary_table <- true_CE_tab %>%
  dplyr::bind_rows(
    .,
    CR_CRS_2P2T_PSA_summary_table)
####### Beautified table:----
CR_CRS_2P2T_PSA_summary_beutified_table <- CR_CRS_2P2T_PSA_summary_table %>%
  dplyr::mutate(
    Method = dplyr::case_when(
      Method == "log_likelihood_RGS" ~ "RGS (LLK):",
      Method == "SSE_RGS" ~ "RGS (SSE):",
      Method == "log_likelihood_FGS" ~ "FGS (LLK):",
      Method == "SSE_FGS" ~ "FGS (SSE):",
      Method == "log_likelihood_LHS" ~ "LHS (LLK):",
      Method == "SSE_LHS" ~ "LHS (SSE):",
      Method == "NM_LLK_0" ~ "NM (LLK):",
      Method == "NM_SSE_0" ~ "NM (SSE):",
      Method == "NM_LLK_1" ~ "NM (LLK - unconverged):",
      Method == "NM_SSE_1" ~ "NM (SSE - unconverged):",
      Method == "BFGS_LLK_0" ~ "BFGS (LLK):",
      Method == "BFGS_SSE_0" ~ "BFGS (SSE):",
      Method == "BFGS_LLK_1" ~ "BFGS (LLK - unconverged):",
      Method == "BFGS_SSE_1" ~ "BFGS (SSE - unconverged):",
      Method == "SANN_LLK_" ~ "SANN (LLK):",
      Method == "SANN_SSE_" ~ "SANN (SSE):",
      TRUE ~ Method)) %>%
  dplyr::mutate(
    Ranking = dplyr::case_when(
      Method == "True" ~ 0,
      Method == "MCMC" ~ 1,
      Method == "SIR" ~ 2,
      Method == "IMIS" ~ 3,
      Method %in% c("FGS (LLK):", "FGS (SSE):") ~ 4,
      Method %in% c("RGS (LLK):", "RGS (SSE):") ~ 5,
      Method %in% c("LHS (LLK):", "LHS (SSE):") ~ 6,
      Method %in% c("BFGS (LLK):", "BFGS (SSE):", "BFGS (LLK - unconverged):",
                    "BFGS (SSE - unconverged):") ~ 7,
      Method %in% c("NM (LLK):", "NM (SSE):", "NM (LLK - unconverged):",
                    "NM (SSE - unconverged):") ~ 8,
      Method %in% c("SANN (LLK):", "SANN (SSE):") ~ 9)) %>%
  dplyr::arrange(Ranking) %>%
  dplyr::select(-Ranking) %>%
  dplyr::group_by(Method, RowGroup_) %>%
  gt::gt() %>%
  gt::tab_style(
    style = gt::cell_text(
      weight = "bold"),
    locations = list(
      gt::cells_column_labels(),
      gt::cells_row_groups())) %>%
  gt::tab_options(
    row_group.as_column = TRUE) %>%
  gt::tab_footnote(
    data = .,
    footnote = gt::md(
      "_The ICER threshold values used in computing the results are
        preceeded by the \"@\" symbol in the corresponding rows._"),
    locations = gt::cells_row_groups(
      groups = gt::contains(c(
        glue::glue("Expected Value of Perfect Information (£)"),
        glue::glue("Net Benefit (£)"),
        "Probability Cost-Effective"))))
##### Save final table:----
table_name <- "calibration_CE_PSA.rds"
saveRDS(
  object = CR_CRS_2P2T_PSA_summary_beutified_table,
  file = "{data_saving_path}{table_name}")
#### Plots:----
##### Plot fitness function:----
set.seed(seed = seed_no)
CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = FALSE,
  .maximise_ = FALSE,
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = gof_measure)
##### Plot targets:----
CR_CRS_2P2T$draw_targets_plots(
  .sim_targets_ = TRUE,
  .calibration_methods_ = c("random", "directed", "bayesian"))
###### Save plots:----
# ggplot2::ggsave(
#   filename = "../../2. Confirmation Review/CR_data/Case_study_1/chap_3_prop_25_.jpeg",
#   plot = CR_CRS_2P2T[["plots"]][["targets"]][["bayesian"]][["MCMC"]][["PropSick"]],
#   scale = 2.5)
