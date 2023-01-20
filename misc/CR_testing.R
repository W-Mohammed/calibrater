##################################################
#      Confirmation Review case studies          #
##################################################
## Load the calibR package:
devtools::load_all()
## Create calibration data:----
# internal_data <- list.files(path = "data-raw/", pattern = "CR_",full.names = T)
# purrr::walk(internal_data, .f = source)

## Effects of using more calibration targets on CRS_markov_2 model:----
### One target testing:----
CR_CRS_1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE,
    .intervs = CR_CRS_data_1t$l_intervs
  )
CR_CRS_1T$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_CRS_1T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_CRS_1T$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_CRS_1T$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Two targets testing:----
CR_CRS_2T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_2t$l_params,
    .targets = CR_CRS_data_2t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_CRS_2T$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_CRS_2T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_CRS_2T$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_CRS_2T$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

## Effects of model dimensionality on model calibration using HID_markov model:----
### One parameter testing:----
CR_HID_1P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_1p$l_params,
    .targets = CR_HID_data_1p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_1P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_1P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_1P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_1P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Two parameters testing:----
CR_HID_2P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_2p$l_params,
    .targets = CR_HID_data_2p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_2P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_2P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_2P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_2P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Three parameters testing:----
CR_HID_3P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_3p$l_params,
    .targets = CR_HID_data_3p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_3P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_3P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_3P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_3P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Four parameters testing:----
CR_HID_4P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_4p$l_params,
    .targets = CR_HID_data_4p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_4P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_4P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_4P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_4P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Five parameters testing:----
CR_HID_5P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_5p$l_params,
    .targets = CR_HID_data_5p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_5P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_5P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_5P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_5P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### Six parameters testing:----
CR_HID_6P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_6p$l_params,
    .targets = CR_HID_data_6p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_6P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_6P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_6P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_6P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)

### All parameters testing:----
CR_HID_7P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_7p$l_params,
    .targets = CR_HID_data_7p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_7P$
  sampleR(
    .n_samples = 100000,
    .sampling_method = c("LHS")
  )
CR_HID_7P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = "LHS",
    .calibration_method = c("LLK", "SSE"))
# CR_HID_7P$
#   calibrateR_directed(
#     .gof = 'LLK',
#     .n_samples = 10,
#     .calibration_method = c('NM', 'BFGS', 'SANN'),
#     .sample_method = "LHS",
#     .max_iterations = 10000,
#     temp = 10,
#     tmax = 10)
# CR_HID_7P$
#   calibrateR_bayesian(
#     .b_method = c('SIR', 'IMIS'),
#     .n_resample = 10000,
#     .IMIS_iterations = 400,
#     .IMIS_sample = 100)













### MISC testing:----
calibR::sample_prior_RGS(.n_samples = 1, .l_params = CR_CRS_data_1t$l_params)
## Sample a random set of parameters as a starting point for the chain:
guess <- calibR::sample_prior_RGS_(
  .n_samples = 1,
  .l_params = CR_CRS_data_1t$l_params)
## Run the Metropolis-Hastings algorithm
fit_MCMC <- MHadaptive::Metro_Hastings(
  li_func = calibR::log_posterior,
  pars = guess,
  par_names = CR_CRS_data_1t$l_params[["v_params_names"]],
  iterations = 5e4,
  burn_in = 1e4,
  .func = CRS_markov_2,
  .l_targets = CR_CRS_data_1t$l_targets,
  .l_params = CR_CRS_data_1t$l_params,
  .args = NULL,
  .transform = FALSE)

test_Bayesian = calibrateModel_beyesian(
  .b_method = 'SIR', .func = CRS_markov, .args = NULL,
  .l_targets = l_targets, .l_params = l_params, .samples = samples)

test_Bayesian = calibR::calibrateModel_beyesian(
  .b_method = "MCMC",
  .func = CRS_markov,
  .args = NULL,
  .l_targets = CR_CRS_data_1t$l_targets,
  .l_params = CR_CRS_data_1t$l_params,
  .samples = NULL,
  .n_resample = 1e4,
  .MCMC_burnIn = 1e4,
  .MCMC_samples = 6e4,
  .MCMC_thin = 5,
  .transform = FALSE)


CR_CRS_1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_CRS_1T$
  sampleR(
    .n_samples = 1000,
    .sampling_method = c("LHS")
  )
CR_CRS_1T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("LHS"),
    .calibration_method = "LLK")
CR_CRS_1T$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 10,
    .calibration_method = c('NM', 'BFGS', 'SANN'),
    .sample_method = "LHS",
    .max_iterations = 1000,
    temp = 10,
    tmax = 10)
CR_CRS_1T$
  calibrateR_bayesian(
    .b_method = c('SIR', 'IMIS', 'MCMC'),
    .n_resample = 10000,
    .IMIS_iterations = 400,
    .IMIS_sample = 100,
    .MCMC_burnIn = 10000,
    .MCMC_samples = 50000,
    .MCMC_thin = 5,
    .MCMC_rerun = TRUE)
##
##
CR_HID_3P = calibR_R6$
  new(
    .model = HID_markov,
    .params = CR_HID_data_3p$l_params,
    .targets = CR_HID_data_3p$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_HID_3P$
  sampleR(
    .n_samples = 1000,
    .sampling_method = c("LHS")
  )
CR_HID_3P$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("LHS"),
    .calibration_method = "LLK")
CR_HID_3P$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 10,
    .calibration_method = c('NM', 'BFGS', 'SANN'),
    .sample_method = "LHS",
    .max_iterations = 1000,
    temp = 10,
    tmax = 10)
CR_HID_3P$
  calibrateR_bayesian(
    .b_method = c('SIR', 'IMIS', 'MCMC'),
    .n_resample = 10000,
    .IMIS_iterations = 400,
    .IMIS_sample = 100,
    .MCMC_burnIn = 10000,
    .MCMC_samples = 50000,
    .MCMC_thin = 5,
    .MCMC_rerun = TRUE)

CR_HID_3P_mcmc = calibR_R6$
  new(
    .model = HID_markov_2,
    .params = HID_data2_flat$l_params,
    .targets = HID_data2_flat$l_targets,
    .args = NULL,
    .transform = TRUE
  )
CR_HID_3P_mcmc$
  sampleR(
    .n_samples = 1000,
    .sampling_method = c("RGS")
  )
CR_HID_3P_mcmc$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("RGS"),
    .calibration_method = "LLK")
CR_HID_3P_mcmc$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 10,
    .calibration_method = c('NM', 'BFGS', 'SANN'),
    .sample_method = "RGS",
    .max_iterations = 100,
    temp = 10,
    tmax = 10)
CR_HID_3P_mcmc$
  calibrateR_bayesian(
    .b_method = c('SIR', 'IMIS', 'MCMC'),
    .n_resample = 1e3,
    .IMIS_iterations = 100,
    .IMIS_sample = 1e2,
    .MCMC_burnIn = 1e4,
    .MCMC_samples = 4e4,
    .MCMC_thin = 4,
    .MCMC_rerun = TRUE)

CR_HID_3P_mcmc$
  calibrateR_bayesian(
    .b_method = c('MCMC'),
    .n_resample = 1e3,
    .IMIS_iterations = 100,
    .IMIS_sample = 1e2,
    .MCMC_burnIn = 1e4,
    .MCMC_samples = 4e4,
    .MCMC_thin = 4,
    .MCMC_rerun = TRUE)

## Ploting tests:----
library(ggplot2)

ttt = CR_CRS_1T[["calibration_results"]][["bayesian"]][["IMIS"]][["Results"]]
ggplot2::ggplot(data = ttt) +
  ggplot2::geom_density2d(ggplot2::aes(x = p_Mets, y = p_DieMets))

ttt %>%
  ggplot(aes(p_Mets, p_DieMets)) +
  geom_density2d() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

testparams = CR_CRS_data_1t$l_params
testparams$Xargs$p_Mets$min <- 0.02
testparams$Xargs$p_Mets$max <- 0.20
testparams$Xargs$p_DieMets$min <- 0.02
testparams$Xargs$p_DieMets$max <- 0.20

testparams$args$p_Mets$min <- 0.02
testparams$args$p_Mets$max <- 0.20
testparams$args$p_DieMets$min <- 0.02
testparams$args$p_DieMets$max <- 0.20

CR_CRS_1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_2t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_CRS_1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
CR_CRS_1T$
  sampleR(
    .n_samples = 1000,
    .sampling_method = c("FGS")
  )
CR_CRS_1T$prior_samples$FGS <- sample_prior_FGS_(
  .n_samples = 10000,
  .l_params = testparams)
CR_CRS_1T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("FGS"),
    .calibration_method = "LLK")

ttt2 <- CR_CRS_1T[["calibration_results"]][["random"]][["LLK_FGS"]]
ttt2.2 <- ttt2 %>% dplyr::slice_sample(n = 100)

ggplot(ttt2, aes(x = p_Mets, y = p_DieMets)) +
  geom_contour(aes(z = Overall_fit, colour = ..level..), bins = 30) +
  theme_void() +
  scale_color_continuous("Overall_fit")

library(plotly)
# good plot:
fig_1 <- plot_ly(x = ttt2$p_Mets, y = ttt2$p_DieMets, z = ttt2$Overall_fit,
                 type = "contour")
fig_1 %>% add_trace(inherit = F, x = ttt2.2$p_Mets, y = ttt2.2$p_DieMets,
                    type = 'scatter', mode = 'markers', marker = list(size = 2),
                    symbols = 'x')
fig_2 <- plot_ly(x = ttt2$p_Mets, y = ttt2$p_DieMets, z = ttt2$Overall_fit,
                 type = "contour")
fig_2

# save it:
plotly::export(p = fig, #the graph to export
               file = "graph 1.png") #the name and type of file (can be .png, .jpeg, etc.)

ttt2 %>%
  ggplot(aes(p_Mets, p_DieMets)) +
  geom_density2d() +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5))

# HID model
CR_HID_3P_mcmc = calibR_R6$
  new(
    .model = HID_markov_2,
    .params = HID_data2_flat$l_params,
    .targets = HID_data2_flat$l_targets,
    .args = NULL,
    .transform = TRUE
  )
# CR_HID_3P_mcmc$
#   sampleR(
#     .n_samples = 1000,
#     .sampling_method = c("FGS")
#   )
CR_HID_3P_mcmc$prior_samples$FGS <- sample_prior_FGS_(
  .n_samples = 10000,
  .l_params = HID_data2_flat$l_params)
CR_HID_3P_mcmc$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("FGS"),
    .calibration_method = "LLK")

ttt3 <- CR_HID_3P_mcmc[["calibration_results"]][["random"]][["LLK_FGS"]]

ttt4 = backTransform(.t_data_ = ttt3,
                     .l_params_ = HID_data2_flat$l_params)

# good plot:
  fig <- plot_ly(x = ttt4$mu_e, y = ttt4$b, z = ttt4$Overall_fit, type = "contour")

## Saving tests:----
##### Extract PSA results for post processing:----
###### Get true CE data:----
true_CE_object <- readRDS(
  file = glue::glue("../../2. Confirmation Review/CR_data/Chap_3/data/CRS_true_PSA.rds"))

###### Loop through PSA results of each calibration method:----
PSA_costs_effects <-
  purrr::map(
    .x = CR_CRS_2P2T$PSA_results,
    .f = function(.res_) {
      #### Loop through PSA results of each calibration method:----
      c(purrr::map(
        .x = interventions_list$v_interv_outcomes,
        .f = function(.outcome_) {
          #### Loop through PSA outcomes (costs and effects) to use in PSA:----
          conseq_df <- .res_ %>%
            #### Select all costs or effects column:----
          dplyr::select(dplyr::contains(.outcome_)) %>%
            #### Remove outcome portion in the column name:----
          dplyr::rename_with(.fn = function(.x) {
            stringr::str_remove(
              string = .x,
              pattern = paste0(".", .outcome_))},
            .cols = dplyr::everything())
        }),
        "Calibration_data" = list(
          .res_ %>%
            dplyr::select(
              -dplyr::contains(interventions_list$v_interv_outcomes))),
        "Interventions" = list(interventions_list$v_interv_names))
    })

##### Generate summary tables from the PSA list:----
subset_CE_table <- c("NetBenefit", "ProbabilityCE", "EVPI")
###### Generate tables:----
PSA_summary_tables <- list()
####### Full printable tables:----
######## Calibration methods tables:----
PSA_summary_tables[["Full"]] <- purrr::map(
    .x = PSA_costs_effects,
    .f = function(.calib_method) {
      #### Loop through each calibration method:----
      PSA_summary <- ShinyPSA::summarise_PSA_(
        .effs = .calib_method[["effects"]],
        .costs = .calib_method[["costs"]],
        .params = .calib_method[["params"]],
        .interventions = .calib_method[["Interventions"]],
        .plot = FALSE)
      PSA_table <- PSA_summary %>%
        ShinyPSA::draw_summary_table_(
          .PSA_data = .,
          .long_ = TRUE,
          .beautify_ = TRUE,
          .latex_ = TRUE,
          .latex_title_ = paste("Calibration methods PSA results", "-",
                                .calib_method[["Calibration_data"]]$Label[[1]]),
          .latex_subtitle_ = "The table shows all generated results",
          .latex_code_ = FALSE,
          .footnotes_sourcenotes_ = TRUE,
          .all_sourcenotes_ = FALSE,
          .dominance_footnote_ = FALSE,
          .subset_tab_ = FALSE)
    })
######## Simulated truth table:----
PSA_summary_tables$Full[["True"]] <-
  ShinyPSA::summarise_PSA_(
    .effs = true_CE_object[["e"]],
    .costs = true_CE_object[["c"]],
    .params = true_CE_object[["p"]],
    .interventions = true_CE_object[["treats"]],
    .plot = FALSE) %>%
  ShinyPSA::draw_summary_table_(
    .PSA_data = .,
    .long_ = TRUE,
    .beautify_ = TRUE,
    .latex_ = TRUE,
    .latex_title_ = "Simulated truth PSA results",
    .latex_subtitle_ = "The table shows all generated results",
    .latex_code_ = FALSE,
    .footnotes_sourcenotes_ = TRUE,
    .all_sourcenotes_ = FALSE,
    .dominance_footnote_ = FALSE,
    .subset_tab_ = FALSE)
####### Partial printable tables:----
######## Calibration methods tables:----
PSA_summary_tables[["Partials"]] <- purrr::map(
    .x = PSA_costs_effects,
    .f = function(.calib_method) {
      #### Loop through each calibration method:----
      PSA_summary <- ShinyPSA::summarise_PSA_(
        .effs = .calib_method[["effects"]],
        .costs = .calib_method[["costs"]],
        .params = .calib_method[["params"]],
        .interventions = .calib_method[["Interventions"]],
        .plot = FALSE)
      PSA_table <- PSA_summary %>%
        ShinyPSA::draw_summary_table_(
          .PSA_data = .,
          .long_ = TRUE,
          .beautify_ = TRUE,
          .latex_ = TRUE,
          .latex_title_ = paste("Calibration methods PSA results", "-",
                                .calib_method[["Calibration_data"]]$Label[[1]]),
          .latex_subtitle_ = "The table shows a subset of the generated results.",
          .latex_code_ = FALSE,
          .dominance_footnote_ = FALSE,
          .footnotes_sourcenotes_ = TRUE,
          .all_sourcenotes_ = FALSE,
          .subset_tab_ = TRUE,
          .subset_group_ = subset_CE_table)
    })
######## Simulated truth table:----
PSA_summary_tables$Partials[["True"]] <-
  ShinyPSA::summarise_PSA_(
    .effs = true_CE_object[["e"]],
    .costs = true_CE_object[["c"]],
    .params = true_CE_object[["p"]],
    .interventions = true_CE_object[["treats"]],
    .plot = FALSE) %>%
  ShinyPSA::draw_summary_table_(
    .PSA_data = .,
    .long_ = TRUE,
    .beautify_ = TRUE,
    .latex_ = TRUE,
    .latex_title_ = "Simulated truth PSA results",
    .latex_subtitle_ = "The table shows a subset of the generated results.",
    .latex_code_ = FALSE,
    .dominance_footnote_ = FALSE,
    .footnotes_sourcenotes_ = TRUE,
    .all_sourcenotes_ = FALSE,
    .subset_tab_ = TRUE,
    .subset_group_ = subset_CE_table)

####### Combined printable table:----
######## Partial un-printable calibration methods table:----
PSA_summary_combo_tab_calibs <- purrr::map_df(
  .x = PSA_costs_effects,
  .f = function(.calib_method) {
    #### Loop through each calibration method:----
    PSA_summary <- ShinyPSA::summarise_PSA_(
      .effs = .calib_method[["effects"]],
      .costs = .calib_method[["costs"]],
      .params = .calib_method[["params"]],
      .interventions = .calib_method[["Interventions"]],
      .plot = FALSE)
    PSA_table <- PSA_summary %>%
      ShinyPSA::draw_summary_table_(
        .PSA_data = .,
        .long_ = TRUE,
        .beautify_ = FALSE,
        .latex_ = FALSE,
        .footnotes_sourcenotes_ = FALSE,
        .all_sourcenotes_ = FALSE,
        .dominance_footnote_ = FALSE,
        .subset_tab_ = TRUE,
        .subset_group_ = subset_CE_table) %>%
      dplyr::mutate(
        Method = .calib_method[["Calibration_data"]]$Label[[1]])
  })
######## Partial un-printable truth table:----
PSA_summary_combo_tab_truth <- ShinyPSA::summarise_PSA_(
  .effs = true_CE_object[["e"]],
  .costs = true_CE_object[["c"]],
  .params = true_CE_object[["p"]],
  .interventions = true_CE_object[["treats"]],
  .plot = FALSE) %>%
  ShinyPSA::draw_summary_table_(
    .PSA_data = .,
    .long_ = TRUE,
    .beautify_ = FALSE,
    .latex_ = FALSE,
    .footnotes_sourcenotes_ = FALSE,
    .all_sourcenotes_ = FALSE,
    .dominance_footnote_ = FALSE,
    .subset_tab_ = TRUE,
    .subset_group_ = subset_CE_table) %>%
  dplyr::mutate(
    Method = "Simulated truth")
####### Beautified tables:----
######## Absolute values:----
######### Apply clearer calibration methods' names and generate table:----
PSA_summary_tables[["Combined"]][["Absolute"]] <- dplyr::bind_rows(
    PSA_summary_combo_tab_truth,
    PSA_summary_combo_tab_calibs)  %>%
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
      Method == "Simulated truth" ~ 0,
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
# ######## Save beautified absolute table:----
# table_name <- "calibration_CE_PSA_abs.rds"
# saveRDS(
#   object = CR_CRS_2P2T_PSA_summary_beutified_abs_tb,
#   file = glue::glue("{data_saving_path}{table_name}"))
######## Relative values:----
PSA_summary_combo_tab_calibs_rel <- purrr::map_dfc(
  .x = interventions_list$v_interv_names,
  .f = function(.interv_) {
    ######### Loop over each intervention:----
    PSA_summary_combo_tab_calibs %>%
      ######### Filter Net Benefit to estimate relative values:----
      dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
      ######### Make sure relative values are estimated correctly per method:----
      dplyr::group_by(Method) %>%
      ######### Estimate absolute difference between calibrations and truth:----
      dplyr::mutate(
        ######### Truth and calibration results are estimated by intervention:----
        dplyr::across(
        .cols = .interv_,
        .fns = function(.x) {
          ######### Remove "£" and "," from values to compute differences:----
          x_ = gsub(
            pattern = "£",
            replacement = "",
            x = .x)
          x_ = gsub(
            pattern = ",",
            replacement = "",
            x = x_)
          ######### Convert characters/strings to numeric:----
          x_ = as.numeric(x_)
          ######### Next, grab simulated truth values for same intervention:----
          y_ = PSA_summary_combo_tab_truth %>%
            ######### Keep Net Benefit values:----
            dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
            ######### Mutate the values of the same intervention above:----
            dplyr::mutate(
              dplyr::across(
              .cols = .interv_,
              .fns = function(.x) {
                ######### Remove the "£" and "," to run calculations:----
                x_ = gsub(
                  pattern = "£",
                  replacement = "",
                  x = .x)
                x_ = gsub(
                  pattern = ",",
                  replacement = "",
                  x = x_)
                ######### Convert the values to numeric for calculations:----
                x_ = as.numeric(x_)})) %>%
            ######### Extract the values to estimate abs difference from truth:----
            dplyr::pull(.data[[.interv_]])
          ######### Estimate absolute difference:----
          x_ = abs(x_ - y_)
          x_
        })) %>%
      ######### Remove grouping now that abs differences were obtained:----
      dplyr::ungroup() %>%
      dplyr::mutate(
        dplyr::across(
        .cols = .interv_,
        .fns = function(.x) {
          ######### Re-apply the earlier formatting:----
          scales::dollar(
            x = .x,
            prefix = "£")
        })) %>%
      ######### If this was the first intervention in intervention's list:----
    {if (.interv_ == interventions_list$v_interv_names[1]) {
      ######### Append the auxiliary columns from original data table:----
        dplyr::select(
          .data = .,
          colnames(PSA_summary_combo_tab_calibs)[
            which(
              !colnames(PSA_summary_combo_tab_calibs) %in%
                names(interventions_list$v_interv_names)[
                  which(names(interventions_list$v_interv_names) !=
                          .interv_)])])
      } else {
        dplyr::select(
          .data = .,
          .interv_)
      }}
  }) %>%
  ######### Bind the rows of the other reported results:----
  dplyr::bind_rows(
    PSA_summary_combo_tab_calibs %>%
      dplyr::filter(RowGroup_ != "Net Benefit (£)"),
    .) %>%
  ######### Ensure they are ranked as needed:----
  dplyr::mutate(
    Ranking = dplyr::case_when(
      RowGroup_ == "Net Benefit (£)" ~ 1,
      RowGroup_ == "Probability Cost-Effective" ~ 2,
      RowGroup_ == "Expected Value of Perfect Information (£)" ~ 3,
      TRUE ~ NA_real_)) %>%
  dplyr::arrange(Method, Ranking) %>%
  dplyr::select(-Ranking) %>%
  ######### Bind the simulated truth results to the top of the table:----
  dplyr::bind_rows(
    PSA_summary_combo_tab_truth,
    .)
######### Apply clearer calibration methods' names:----
PSA_summary_combo_tab_calibs_rel <- PSA_summary_combo_tab_calibs_rel %>%
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
      Method == "Simulated truth" ~ 0,
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
  dplyr::group_by(Method, RowGroup_)

######### Locate footnotes locations:----
relative_vals_footnote <- PSA_summary_combo_tab_calibs_rel %>%
  dplyr::filter(Method != "Simulated truth") %>%
  dplyr::pull(Method) %>%
  unique(.) %>%
  paste(., "-", "Net Benefit (£)")

######### Generate beautified tables:----
PSA_summary_tables[["Combined"]][["Relative"]] <- PSA_summary_combo_tab_calibs_rel %>%
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
        "Probability Cost-Effective")))) %>%
  gt::tab_footnote(
    data = .,
    footnote = gt::md(
      "_Absolute values_"),
    locations = gt::cells_row_groups(
      groups = gt::contains("True - Net Benefit"))) %>%
  gt::tab_footnote(
    data = .,
    footnote = gt::md(
      "_Relative to true 'Net Benefit' values_"),
    locations = gt::cells_row_groups(
      groups = relative_vals_footnote))
# ######## Save beautified absolute table:----
# table_name <- "calibration_CE_PSA_rel.rds"
# saveRDS(
#   object = CR_CRS_2P2T_PSA_summary_beutified_rel_tb,
#   file = glue::glue("{data_saving_path}{table_name}"))
