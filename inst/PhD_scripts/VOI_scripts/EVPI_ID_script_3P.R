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

### 1. Case study 2: three target - three parameters:----
#### Saving path:----
path = "../4. Post CR/VOI/"
case_study_dir <- "Case_study_2/"
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
VOI_ID_3P3T = calibR_R6$
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
    .n_samples = 1e4,
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
    .max_iterations = 1e3,
    temp = 1,
    trace = FALSE
  )
#### Bayesian methods:----
set.seed(seed = seed_no)
VOI_ID_3P3T$
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
VOI_ID_3P3T$
  sample_PSA_values(
    .calibration_methods = c("Random", "Directed", "Bayesian"),
    .PSA_samples = 1e3
  )
#### Run PSA:----
VOI_ID_3P3T$run_PSA()
#### Generate CEA tables:----
VOI_ID_3P3T$
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
l_VOI_ID_3P3T <- list(
  "outcomes" = VOI_ID_3P3T$PSA_results,
  "p" = VOI_ID_3P3T$PSA_samples
)
filename <- paste0(data_saving_path, "l_EVPPI_3P3T.rds")
saveRDS(l_VOI_ID_3P3T, file = filename)

## EVPPI:----
### Prepare data:----
filename <- paste0(data_saving_path, "l_EVPPI_3P3T.rds")
l_EVPPI_3P3T <- readRDS(file = filename)
.model_outcomes_ <- ID_data_3t$l_intervs$v_interv_outcomes
Calibrated_parameters <- parameters_list$v_params_names
unCalibrated_parameters <- PSA_parameters$v_params_names
params_ <- purrr::map(
  .x = l_EVPPI_3P3T[["p"]][-1],
  .f = function(calib_class_results) {
    purrr::map(
      .x = calib_class_results,
      .f = function(calib_method_results) {
        cbind(calib_method_results[["PSA_calib_draws"]] %>%
                dplyr::select(
                  -dplyr::any_of(
                    c("Label", "Plot_label", "Overall_fit")
                  )
                ),
              l_EVPPI_3P3T[["p"]][[1]])
      }
    )
  }
)
params_ <- purrr::map(
  .x = 1:length(l_EVPPI_3P3T$outcomes) %>%
    `names<-`(names(l_EVPPI_3P3T[["outcomes"]])),
  .f = function(calib_method) {
    params_[[calib_method]][[1]]
  }
)

ls_PSA_outputs <- purrr::map(
  .x = names(l_EVPPI_3P3T[["outcomes"]]) %>%
    `names<-`(names(l_EVPPI_3P3T[["outcomes"]])),
  .f = function(.res_) {
    # loop through PSA results of each calibration method:
    outcomes <- purrr::map(
      .x = .model_outcomes_,
      .f = function(.outcome_) {
        # loop through PSA outcomes (costs and effects) to use in PSA:
        conseq_df <- l_EVPPI_3P3T[["outcomes"]][[.res_]] %>%
          # filter na() in either costs or effects:
          dplyr::filter(
            dplyr::if_all(
              .cols = dplyr::contains(.model_outcomes_),
              .fns = function(x_) {
                !is.na(x_)
              }
            )
          ) %>%
          # select all costs or effects columns:
          dplyr::select(dplyr::contains(.outcome_)) %>%
          # remove outcome portion in the column name:
          dplyr::rename_with(.fn = function(.x) {
            stringr::str_remove(
              string = .x,
              pattern = paste0(".", .outcome_))},
            .cols = dplyr::everything())
      }
    )
    outcomes[["p"]] <- params_[[.res_]]
    outcomes[["treats"]] <- ID_data_3t$l_intervs$v_interv_names
    outcomes
  }
)

### Use ShinyPSA:----
ls_EVPPI_res <- purrr::map(
  .x = names(ls_PSA_outputs) %>%
    `names<-`(names(ls_PSA_outputs)),
  .f = function(calib_method_results) {
    #### Summarise PSA results:
    l_PSA_summary <- ShinyPSA::summarise_PSA_(
      .effs = ls_PSA_outputs[[calib_method_results]]$effects,
      .costs = ls_PSA_outputs[[calib_method_results]]$costs,
      .params = ls_PSA_outputs[[calib_method_results]]$p,
      .interventions = ls_PSA_outputs[[calib_method_results]]$treats)

    ##### Estimate EVPPI:
    df_EVPPI_ind_res_1 <- ShinyPSA::compute_EVPPIs_(
      .PSA_data = l_PSA_summary,
      .set_names = Calibrated_parameters,
      .subset_ = TRUE,
      .MAICER_ = 20000,
      .individual_evppi_ = FALSE,
      .evppi_population_ = 1000,
      .time_horion_ = 5
    )
    df_EVPPI_ind_res_2 <- ShinyPSA::compute_EVPPIs_(
      .PSA_data = l_PSA_summary,
      .set_names = unCalibrated_parameters,
      .subset_ = TRUE,
      .MAICER_ = 20000,
      .individual_evppi_ = FALSE,
      .evppi_population_ = 1000,
      .time_horion_ = 5
    )
    notes <- df_EVPPI_ind_res_1[["Table caption"]]
    rbind(df_EVPPI_ind_res_1$EVPPI,
          df_EVPPI_ind_res_2$EVPPI) %>%
      dplyr::select(
        -dplyr::all_of(
          "Population parameters"
        )
      ) %>%
      gt::gt() %>%
      gt::tab_header(
        calib_method_results
      ) %>%
      gt::tab_source_note(
        notes
      )
  }
)


filename <- paste0(data_saving_path, "ls_EVPPI_tables.rds")
saveRDS(ls_EVPPI_res, file = filename)
