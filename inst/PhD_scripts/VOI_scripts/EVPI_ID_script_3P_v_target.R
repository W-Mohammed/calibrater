##################################################
#                 VOI case studies               #
##################################################
# Model: infectious_disease_stm
# Parameters: c(rho, b, p)
# Targets: c(Prev, Surv, Trt_vol)

## Load the calibR package:
devtools::load_all()
calibration_parameters <- c("rho", "b", "p")
calibration_targets <- c("Prev", "Surv", "Trt_vol")

### 1. Case study 5: three target - three parameters:----
#### Saving path:----
path = "../4. Post CR/VOI/"
case_study_dir <- "Case_study_5/"
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
targets_list_ <- c(
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
set.seed(seed = seed_no)
l_VOI_ID_3P3T <- purrr::map(
  .x = 1:100,
  .f = function(x_) {
    delta_target <- runif(
      n = 1,
      min = 0,
      max = 2
    )
    targets_list <- c(
      purrr::map(
        .x = calibration_targets %>%
          `names<-`(calibration_targets),
        .f = function(target) {
          targets_list_[[target]][["value"]] <-
            targets_list_[[target]][["value"]] * delta_target
          targets_list_[[target]]
        }),
      purrr::map(
        .x = calibR::ID_data_3t$l_targets[
          grep(pattern = "v_targets_",names(calibR::ID_data_3t$l_targets))
        ],
        .f = function(l_target) {
          l_target[calibration_targets]
        }
      )
    )
    #### Initiate CalibR R6 object:----
    VOI_ID_3P3T <- calibR_R6$
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
    VOI_ID_3P3T$
      sampleR(
        .n_samples = 5e3,
        .sampling_method = sampling_methods
      )
    #### Parameter exploration calibration methods:----
    ##### Unguided searching methods:----
    set.seed(seed = seed_no)
    VOI_ID_3P3T$
      calibrateR_random(
        .optim = FALSE,
        .maximise = TRUE,
        .weighted = TRUE,
        .sample_method = sampling_methods,
        .calibration_method = gof_measure
      )
    ##### Guided searching methods:----
    set.seed(seed = seed_no)
    VOI_ID_3P3T$
      calibrateR_directed(
        .gof = gof_measure,
        .n_samples = 1e1,
        .calibration_method = directed_methods,
        .sample_method = sample_method,
        .max_iterations = 5e2,
        temp = 1,
        trace = FALSE
      )
    #### Bayesian methods:----
    set.seed(seed = seed_no)
    VOI_ID_3P3T$
      calibrateR_bayesian(
        .b_method = bayesian_methods,
        .n_resample = 5e2,
        .IMIS_iterations = 200,
        .IMIS_sample = 5e2,
        .MCMC_burnIn = 5e3,
        .MCMC_samples = 2e3,
        .MCMC_thin = 15,
        .MCMC_rerun = TRUE,
        .diag_ = FALSE
      )
    #### Sample PSA values:----
    set.seed(seed = seed_no)
    VOI_ID_3P3T$
      sample_PSA_values(
        .calibration_methods = c("Random", "Directed", "Bayesian"),
        .PSA_samples = 5e2
      )
    #### Run PSA:----
    VOI_ID_3P3T$run_PSA()
    ### Save outputs for EVPPI:----
    l_VOI_ID_3P3T <- list(
      "outcomes" = VOI_ID_3P3T$PSA_results,
      "p" = VOI_ID_3P3T$PSA_samples,
      "ts" = list(
        "Surv" = targets_list$Surv$value,
        "TrT" = targets_list$Trt_vol$value
        )
    )
    l_VOI_ID_3P3T
  },
  .progress = list(
    type = "iterator",
    format = "Running PSA {cli::pb_bar} {cli::pb_percent}",
    clear = TRUE
  )
)
filename <- paste0(data_saving_path, "l_EVPPI_3P3T.rds")
saveRDS(l_VOI_ID_3P3T, file = filename)
