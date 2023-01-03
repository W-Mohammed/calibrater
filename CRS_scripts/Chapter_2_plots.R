##################################################
#      Confirmation Review case studies          #
##################################################
# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)
# Targets: c(Survival, PropSick)

## Load the calibR package:
devtools::load_all() # 1 parent 6b344f8 commit d93ff4733193a2bcbdc276dab553fb0c2ba3de24

## Chapter 2 plots:----
## Saving path:----
path = "../../2. Confirmation Review/CR_data/"
chapter_dir <- "Chap_2/"
image_dir <- "images/"
data_dir <- "data/"
image_saving_path <- glue::glue("{path}{chapter_dir}{image_dir}")
data_saving_path <- glue::glue("{path}{chapter_dir}{data_dir}")
### 1. Parameter space:----
seed_no <- 1
set.seed(seed = seed_no)
#### 1.1. Full parameter space:----
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
        parameters_list$Xargs[["p_Mets"]]$max),
      showline = TRUE,
      linewidth = 1,
      linecolor = "grey",
      mirror = TRUE),
    yaxis = list(
      title = parameters_list$v_params_labels[["p_DieMets"]],
      range = list(
        parameters_list$Xargs[["p_DieMets"]]$min,
        parameters_list$Xargs[["p_DieMets"]]$max),
      showline = TRUE,
      linewidth = 1,
      linecolor = "grey",
      mirror = TRUE))
##### Save plot:----
image_name = "parameter_space.jpeg"
reticulate::py_run_string("import sys")
plotly::save_image(
  p = param_space_plot,
  file = glue::glue("{image_saving_path}{image_name}"),
  scale = 5)

#### 1.2. Explored parameter space:----
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
        parameters_list$Xargs[["p_Mets"]]$max),
      showline = TRUE,
      linewidth = 1,
      linecolor = "grey",
      mirror = TRUE),
    yaxis = list(
      title = parameters_list$v_params_labels[["p_DieMets"]],
      range = list(
        parameters_list$Xargs[["p_DieMets"]]$min,
        parameters_list$Xargs[["p_DieMets"]]$max),
      showline = TRUE,
      linewidth = 1,
      linecolor = "grey",
      mirror = TRUE))
##### Save plot:----
image_name = "parameter_space_lp.jpeg"
reticulate::py_run_string("import sys")
plotly::save_image(
  p = param_space_plot,
  file = glue::glue("{image_saving_path}{image_name}"),
  scale = 5)

### 2. Fitness plots:----
#### 2.1. Full parameter space:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t$l_targets
gof_measure <- c("SSE", "LLK")
##### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
##### GOF without True values:----
###### Fill plots:----
GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = TRUE,
  .greys_ = FALSE,
  .coloring_ = "heatmap",
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
####### Save plot:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    image_name = glue::glue("GOF_{.gof_measure_}_measure.jpeg")
    plotly::save_image(
      p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })
###### Gray plots:----
GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .coloring_ = "heatmap",
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
####### Save plot:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    image_name = glue::glue("GOF_{.gof_measure_}_measure_bw.jpeg")
    plotly::save_image(
      p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })
###### Line plots:----
GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = FALSE,
  .coloring_ = "none",
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
###### Save plot:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    image_name = glue::glue("GOF_{.gof_measure_}_measure_ln.jpeg")
    plotly::save_image(
      p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })

#### 2.2. Explored parameter space:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
gof_measure <- c("SSE", "LLK")
##### Initiate CalibR R6 object:----
CR_CRS_2P2T <- calibR_R6$new(
  .model = CRS_markov_2,
  .params = parameters_list,
  .targets = targets_list,
  .args = NULL,
  .transform = FALSE)
##### GOF function without True values:----
###### Fill plots:----
GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = TRUE,
  .greys_ = FALSE,
  .coloring_ = "heatmap",
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
####### Save plot:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    image_name = glue::glue("GOF_{.gof_measure_}_measure_lp.jpeg")
    plotly::save_image(
      p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })
###### Gray plots:----
GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = TRUE,
  .coloring_ = "heatmap",
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
####### Save plot:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    image_name = glue::glue("GOF_{.gof_measure_}_measure_lp_bw.jpeg")
    plotly::save_image(
      p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })
###### Line plots:----
GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
  .legend_ = FALSE,
  .greys_ = FALSE,
  .coloring_ = "none",
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
###### Save plot:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    image_name = glue::glue("GOF_{.gof_measure_}_measure_lp_ln.jpeg")
    plotly::save_image(
      p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })

### 3. Undirected methods (RGS, FGS and LHS) with fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
gof_measure <- c("SSE", "LLK")
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
    .weighted = FALSE,
    .sample_method = sample_method,
    .calibration_method = gof_measure)
###### Fitness function without True values:----
random_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = FALSE,
  .zoom_ = FALSE,
  .legend_ = FALSE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  random
####### Save plots:----
######## Walk through and save files:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    purrr::walk(
      .x = random_GOF_measure[[.gof_measure_]] %>%
        names(.) %>%
        `names<-`(names(random_GOF_measure[[.gof_measure_]])),
      .f = function(.gof_name) {
        image_name = glue::glue("{.gof_name}.jpeg")
        reticulate::py_run_string("import sys")
        plotly::save_image(
          p = random_GOF_measure[[.gof_measure_]][[.gof_name]][[1]][[1]],
          file = glue::glue("{image_saving_path}{image_name}"),
          scale = 5)
      })
  })

###### Fitness function with True values:----
random_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .zoom_ = FALSE,
  .legend_ = FALSE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  random
####### Save plots:----
######## Walk through and save files:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    purrr::walk(
      .x = random_GOF_measure[[.gof_measure_]] %>%
        names(.) %>%
        `names<-`(names(random_GOF_measure[[.gof_measure_]])),
      .f = function(.gof_name) {

        image_name = glue::glue("{.gof_name}_w_true.jpeg")
        reticulate::py_run_string("import sys")
        plotly::save_image(
          p = random_GOF_measure[[.gof_measure_]][[.gof_name]][[1]][[1]],
          file = glue::glue("{image_saving_path}{image_name}"),
          scale = 5)
      })
  })

### 4. Directed methods (BFGS, NM, and SANN) with fitness plot:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t_lp$l_params
parameters_list$v_params_labels <- c(
  'p_Mets' = "Parameter 1",
  'p_DieMets' = "Parameter 2")
targets_list <- calibR::CR_CRS_data_2t_lp$l_targets
gof_measure <- c("SSE", "LLK")
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
##### Fitness function with True values:----
####### Save plots:----
######## Walk through and save files:----
######### Save original view:----
directed_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .zoom_ = FALSE,
  .legend_ = FALSE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  directed

purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    purrr::walk(
      .x = directed_GOF_measure[[.gof_measure_]] %>%
        names(.) %>%
        `names<-`(names(directed_GOF_measure[[.gof_measure_]])),
      .f = function(.gof_name) {
        image_name = glue::glue("{.gof_name}.jpeg")
        reticulate::py_run_string("import sys")
        plotly::save_image(
          p = directed_GOF_measure[[.gof_measure_]][[.gof_name]][[1]][[1]],
          file = glue::glue("{image_saving_path}{image_name}"),
          scale = 5)
      })
  })
######### Save zoomed view:----
directed_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .zoom_ = TRUE,
  .legend_ = FALSE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  directed

purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    purrr::walk(
      .x = directed_GOF_measure[[.gof_measure_]] %>%
        names(.) %>%
        `names<-`(names(directed_GOF_measure[[.gof_measure_]])),
      .f = function(.gof_name) {
        image_name = glue::glue("{.gof_name}_zm_true.jpeg")
        reticulate::py_run_string("import sys")
        plotly::save_image(
          p = directed_GOF_measure[[.gof_measure_]][[.gof_name]][[1]][[1]],
          file = glue::glue("{image_saving_path}{image_name}"),
          scale = 5)
      })
  })
##### Fitness function zoomed without True values:----
####### Save plots:----
######## Walk through and save files:----
######### Save zoomed view:----
directed_GOF_measure <- CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = FALSE,
  .legend_ = FALSE,
  .zoom_ = TRUE,
  .coloring_ = "none",
  .greys_ = FALSE,
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  directed

purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    purrr::walk(
      .x = directed_GOF_measure[[.gof_measure_]] %>%
        names(.) %>%
        `names<-`(names(directed_GOF_measure[[.gof_measure_]])),
      .f = function(.gof_name) {
        image_name = glue::glue("{.gof_name}_zm.jpeg")
        reticulate::py_run_string("import sys")
        plotly::save_image(
          p = directed_GOF_measure[[.gof_measure_]][[.gof_name]][[1]][[1]],
          file = glue::glue("{image_saving_path}{image_name}"),
          scale = 5)
      })
  })
