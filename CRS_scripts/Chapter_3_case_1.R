##################################################
#      Confirmation Review case studies          #
##################################################
# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)
# Targets: c(Survival, PropSick)

## Load the calibR package:
devtools::load_all() # 1 parent 848d120 commit 06f2f773fc9dcb0c9729c71ba39eeccdbc5b2d6d

## Chapter 3 plots:----
### 1. Case study 1: Two target - two parameters:----
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
interventions_list <- calibR::CR_CRS_data_2t$l_intervs
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
    .intervs = interventions_list,
    .args = NULL,
    .transform = FALSE)
#### Generate samples using Random Grid Search:----
set.seed(seed = seed_no)
CR_CRS_2P2T$
  sampleR(
    .n_samples = 1e4,
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
    .n_resample = 1e3,
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
    .PSA_samples = 1e3)
#### Run PSA:----
CR_CRS_2P2T$run_PSA(
  .PSA_unCalib_values_ = NULL)
#### Generate PSA tables:----
CR_CRS_2P2T$draw_PSA_summary_tables(
  .save_ = TRUE,
  .saving_path_ = data_saving_path,
  .saving_data_dir_ = "PSA tables/")
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
  .save_ = TRUE,
  .saving_path_ = image_saving_path,
  .saving_image_dir_ = "GOFs/",
  .saving_x_params_ = 1,
  .saving_image_scale_ = 2,
  .saving_image_width_ = 1000,
  .saving_image_height_ = 600)
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
  .save_ = TRUE,
  .saving_path_ = image_saving_path,
  .saving_image_dir_ = "GOFs/",
  .saving_x_params_ = 1,
  .saving_image_scale_ = 2,
  .saving_image_width_ = 1000,
  .saving_image_height_ = 600)
##### Plot targets:----
CR_CRS_2P2T$draw_targets_plots(
  .sim_targets_ = TRUE,
  .save_ = TRUE,
  .saving_path_ = image_saving_path,
  .saving_image_dir_ = "Targets/",
  .saving_image_scale_ = 2,
  .saving_image_width_ = 1000,
  .saving_image_height_ = 600,
  .saving_image_units_ = "px")
##### Prior posterior plot:----
CR_CRS_2P2T$draw_distributions_plots(
  .save_ = TRUE,
  .saving_path_ = image_saving_path,
  .saving_image_dir_ = "Prior-posterior/",
  .saving_image_scale_ = 2,
  .saving_image_width_ = 1000,
  .saving_image_height_ = 600,
  .saving_image_units_ = "px")
