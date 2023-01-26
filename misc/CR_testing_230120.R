              ##################################################
              #                 Testing code                   #
              ##################################################
# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)
# Targets: c(Survival, PropSick)

## Load the calibR package:
devtools::load_all()

#### Saving path:----
# path = "../../2. Confirmation Review/CR_data/"
# case_study_dir <- "Case_study_1/"
# chapter_dir <- "Chap_3/"
# image_dir <- "images/"
# data_dir <- "data/"
# image_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{image_dir}")
# data_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{data_dir}")
#### Case study lists:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t$l_params
targets_list <- calibR::CR_CRS_data_2t$l_targets
interventions_list <- calibR::CR_CRS_data_2t$l_intervs
gof_measure <- c("LLK", "SSE")
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
    .intervs = interventions_list,
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
    .max_iterations = 1e2,
    temp = 1,
    trace = FALSE)
#### Bayesian methods:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  calibrateR_bayesian(
    .b_method = bayesian_methods,
    .n_resample = 1e2,
    .IMIS_iterations = 200,
    .IMIS_sample = 1e2,
    .MCMC_burnIn = 1e3,
    .MCMC_samples = 4e3,
    .MCMC_thin = 30,
    .MCMC_rerun = TRUE,
    .diag_ = FALSE)
#### Sample PSA values:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  sample_PSA_values(
    .calibration_methods = c("Random", "Directed", "Bayesian"),
    .PSA_samples = 1e2)
#### Run PSA:----
CR_CRS_2P2T$run_PSA(
  .PSA_unCalib_values_ = NULL)
#### Generate PSA tables:----
# CR_CRS_2P2T$draw_PSA_summary_tables(
#   .save_ = TRUE)
#### Plots:----
##### Plot fitness function:----
###### Full view plots:----
set.seed(seed = seed_no)
CR_CRS_2P2T$draw_GOF_measure(
  .blank_contour_ = FALSE,
  .true_points_ = TRUE,
  .coloring_ = "none",
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = gof_measure,
  .save_ = TRUE)
###### Zoomed view plots:----
set.seed(seed = seed_no)
CR_CRS_2P2T$draw_GOF_measure(
  .blank_contour_ = FALSE,
  .true_points_ = TRUE,
  .coloring_ = "none",
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .zoom_ = TRUE,
  .gof_ = gof_measure,
  .save_ = TRUE)
##### Plot targets:----
CR_CRS_2P2T$draw_targets_plots(
  .sim_targets_ = TRUE,
  .save_ = TRUE)
##### Prior posterior plot:----
CR_CRS_2P2T$draw_distributions_plots(
  .save_ = TRUE)
# #### Fitness plots:----
# GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
#   .points_ = FALSE,
#   .true_points_ = FALSE,
#   .legend_ = FALSE,
#   .greys_ = FALSE,
#   .coloring_ = "heatmap",
#   .scale_ = NULL,
#   .gof_ = gof_measure)$
#   plots$
#   GOF_plots$
#   blank
# ##### Save plot:----
# purrr::walk(
#   .x = gof_measure,
#   .f = function(.gof_measure_) {
#     image_name = glue::glue("GOF_{.gof_measure_}.jpeg")
#     reticulate::py_run_string("import sys")
#     plotly::save_image(
#       p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
#       file = glue::glue("{image_saving_path}{image_name}"),
#       scale = 5)
#   })
