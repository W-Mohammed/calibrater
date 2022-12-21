##################################################
#      Confirmation Review case studies          #
##################################################
## Load the calibR package:
devtools::load_all()

# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)

## Effects of using more calibration targets on CRS_markov_2 model:----
### One target testing:----
#### Initiate CalibR R6 object:----
CR_CRS_2P1T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_1t$l_params,
    .targets = CR_CRS_data_1t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
#### Generate samples using Random Grid Search:----
CR_CRS_2P1T$
  sampleR(
    .n_samples = 1e4,
    .sampling_method = c("RGS", "FGS", "LHS")
  )
#### Parameter exploration calibration methods:----
##### Unguided searching methods:----
CR_CRS_2P1T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("RGS", "FGS", "LHS"),
    .calibration_method = "LLK"
  )
##### Guided searching methods:----
CR_CRS_2P1T$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 1e2,
    .calibration_method = c("NM", "BFGS", "SANN"),
    .sample_method = "RGS",
    .max_iterations = 1e4,
    temp = 10,
    tmax = 10
  )
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
    .MCMC_rerun = TRUE
  )
#### Plots:----
CR_CRS_2P1T$draw_log_likelihood(.points_ = F)

## Effects of using more calibration targets on CRS_markov_2 model:----
### One target testing:----
#### Initiate CalibR R6 object:----
CR_CRS_2P2T = calibR_R6$
  new(
    .model = CRS_markov_2,
    .params = CR_CRS_data_2t$l_params,
    .targets = CR_CRS_data_2t$l_targets,
    .args = NULL,
    .transform = FALSE
  )
#### Generate samples using Random Grid Search:----
CR_CRS_2P2T$
  sampleR(
    .n_samples = 1e4,
    .sampling_method = c("RGS", "FGS", "LHS")
  )
#### Parameter exploration calibration methods:----
##### Unguided searching methods:----
CR_CRS_2P2T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = c("RGS", "FGS", "LHS"),
    .calibration_method = "LLK"
  )
##### Guided searching methods:----
CR_CRS_2P2T$
  calibrateR_directed(
    .gof = 'LLK',
    .n_samples = 1e2,
    .calibration_method = c("NM", "BFGS", "SANN"),
    .sample_method = "RGS",
    .max_iterations = 1e4,
    temp = 10,
    tmax = 10
  )
#### Bayesian methods:----
CR_CRS_2P2T$
  calibrateR_bayesian(
    .b_method = c("SIR", "IMIS", "MCMC"),
    .n_resample = 1e3,
    .IMIS_iterations = 200,
    .IMIS_sample = 1e3,
    .MCMC_burnIn = 1e4,
    .MCMC_samples = 4e4,
    .MCMC_thin = 4,
    .MCMC_rerun = TRUE
  )
#### Plots:----
CR_CRS_2P2T$draw_log_likelihood(.points_ = F)


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

