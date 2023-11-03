##################################################
#                 VOI case studies               #
##################################################
# Model: infectious_disease_stm
# Parameters: c(mu_e, mu_l, mu_t, p, r_l, rho, b)
# Targets: c(Prev, Surv, Trt_vol)

## Load the calibR package:
devtools::load_all()
calibration_parameters <- c("mu_e", "mu_l", "mu_t", "p", "r_l", "rho", "b")
calibration_targets <- c("Prev", "Surv", "Trt_vol")

### 1. Case study 4: three target - seven parameters:----
#### Saving path:----
path = "../4. Post CR/VOI/"
case_study_dir <- "Case_study_4/"
chapter_dir <- ""
image_dir <- "images/"
data_dir <- "data/"
image_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{image_dir}")
data_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{data_dir}")
#### Case study lists:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- purrr::map(
  .x = calibR::ID_data_3t$l_params,
  .f = function(l_param) {
    l_param[calibration_parameters]
  }
)
PSA_parameters <- purrr::map(
  .x = calibR::ID_data_3t$l_params,
  .f = function(l_param) {
    l_param[!names(l_param) %in% calibration_parameters]
  }
)
if(length(PSA_parameters[[1]]) == 0) {
  PSA_parameters <- NULL
}
targets_list <- c(
  calibR::ID_data_3t$l_targets[calibration_targets],
  purrr::map(
    .x = calibR::ID_data_3t$l_targets[
      grep(pattern = "v_targets_",names(calibR::ID_data_3t$l_targets))
    ],
    .f = function(l_target) {
      l_target[calibration_targets]
    }
  )
)
interventions_list <- calibR::ID_data_3t$l_intervs
gof_measure <- "LLK"
sample_method <- "RGS"
sampling_methods <- c("RGS")
directed_methods <- c("NM")
bayesian_methods <- c("IMIS")
#### Initiate CalibR R6 object:----
VOI_ID_7P3T = calibR_R6$
  new(
    .model = calibR::infectious_disease_stm,
    .params = parameters_list,
    .PSA_params = PSA_parameters,
    .targets = targets_list,
    .intervs = interventions_list,
    .args = NULL,
    .transform = TRUE
  )
#### Generate samples using Random Grid Search:----
set.seed(seed = seed_no)
VOI_ID_7P3T$
  sampleR(
    .n_samples = 1e4,
    .sampling_method = sampling_methods
  )
#### Parameter exploration calibration methods:----
##### Unguided searching methods:----
set.seed(seed = seed_no)
VOI_ID_7P3T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = sampling_methods,
    .calibration_method = gof_measure
  )
##### Guided searching methods:----
set.seed(seed = seed_no)
VOI_ID_7P3T$
  calibrateR_directed(
    .gof = gof_measure,
    .n_samples = 1e1,
    .calibration_method = directed_methods,
    .sample_method = sample_method,
    .max_iterations = 1e3,
    temp = 1,
    trace = FALSE
  )
#### Bayesian methods:----
set.seed(seed = seed_no)
VOI_ID_7P3T$
  calibrateR_bayesian(
    .b_method = bayesian_methods,
    .n_resample = 1e3,
    .IMIS_iterations = 200,
    .IMIS_sample = 1e3,
    .MCMC_burnIn = 1e4,
    .MCMC_samples = 4e4,
    .MCMC_thin = 30,
    .MCMC_rerun = TRUE,
    .diag_ = FALSE
  )
#### Sample PSA values:----
set.seed(seed = seed_no)
VOI_ID_7P3T$
  sample_PSA_values(
    .calibration_methods = c("Random", "Directed", "Bayesian"),
    .PSA_samples = 1e3
  )
#### Run PSA:----
VOI_ID_7P3T$run_PSA()
#### Generate CEA tables:----
VOI_ID_7P3T$
  draw_CEA_results_tables(
    .wtp_key_values_ = c(20000, 30000),
    .label_effects_ = "LY",
    .highlight_optimal_choices_ = FALSE,
    .currency_symbol_ = "\u00A3",
    .output_type_ = "html",
    .full_output_format_ = "long",
    .add_simulated_truth_ = TRUE,
    .truth_PSA_output_list_path_ = "../4. Post CR/VOI/ID_data/ID_true_PSA.rds",
    .truth_PSA_output_list_ = NULL,
    .generate_partial_cea_table_ = TRUE,
    .partial_cea_table_groups_ = c("NMB", "PCE", "EVPI"),
    .generate_relative_values_ = TRUE,
    .relative_values_data_ = c("NMB", "PCE", "EVPI"),
    .save_ = TRUE,
    .saving_path_ = data_saving_path,
    .saving_data_dir_ = "PSA tables/"
  )
### Save outputs for EVPPI:----
l_VOI_ID_7P3T <- list(
  "outcomes" = VOI_ID_7P3T$PSA_results,
  "p" = VOI_ID_7P3T$PSA_samples
)
filename <- paste0(data_saving_path, "l_EVPPI_7P3T.rds")
saveRDS(l_VOI_ID_7P3T, file = filename)
