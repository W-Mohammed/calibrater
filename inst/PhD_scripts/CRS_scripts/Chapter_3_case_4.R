##################################################
#      Confirmation Review case studies          #
##################################################
# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)
# Targets: c(Survival, PropSick)

## Load the calibR package:
devtools::load_all() # 1 parent 848d120 commit 06f2f773fc9dcb0c9729c71ba39eeccdbc5b2d6d

## Chapter 3 plots:----
### 1. Case study 4: One target - two parameters - limited space:----
#### Saving path:----
path = "../../2. Confirmation Review/CR_data/"
case_study_dir <- "Case_study_4/"
chapter_dir <- "Chap_3/"
image_dir <- "images/"
data_dir <- "data/"
image_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{image_dir}")
data_saving_path <- glue::glue("{path}{chapter_dir}{case_study_dir}{data_dir}")
#### Case study lists:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_1t_2$l_params
targets_list <- calibR::CR_CRS_data_1t_2$l_targets
interventions_list <- calibR::CR_CRS_data_1t_2$l_intervs
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
##### Extract PSA results for post processing:----
CR_CRS_2P2T_PSA_list <- CR_CRS_2P2T$PSA_results %>%
  purrr::map(
    .x = .,
    .f = function(.res_) {
      c(purrr::map(
        .x = interventions_list$v_interv_outcomes,
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
              -dplyr::contains(interventions_list$v_interv_outcomes))),
        "Interventions" = list(interventions_list$v_interv_names))
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
  file = glue::glue("{path}{chapter_dir}{data_dir}CRS_true_PSA.rds"))
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
####### Beautified tables:----
######## Absolute values:----
CR_CRS_2P2T_PSA_summary_abs_true_tb <- true_CE_tab %>%
  dplyr::bind_rows(
    .,
    CR_CRS_2P2T_PSA_summary_table)
CR_CRS_2P2T_PSA_summary_beutified_abs_tb <- CR_CRS_2P2T_PSA_summary_abs_true_tb %>%
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
######## Save beautified absolute table:----
table_name <- "calibration_CE_PSA_abs.rds"
saveRDS(
  object = CR_CRS_2P2T_PSA_summary_beutified_abs_tb,
  file = glue::glue("{data_saving_path}{table_name}"))
######## Relative values:----
CR_CRS_2P2T_PSA_summary_rel_true_tb <- purrr::map_dfc(
  .x = interventions_list$v_interv_names,
  .f = function(.interv_) {
    CR_CRS_2P2T_PSA_summary_table %>%
      dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
      dplyr::group_by(Method) %>%
      dplyr::mutate(dplyr::across(
        .cols = .interv_,
        .fns = function(.x) {
          x_ = gsub(
            pattern = "£",
            replacement = "",
            x = .x)
          x_ = gsub(
            pattern = ",",
            replacement = "",
            x = x_)
          x_ = as.numeric(x_)
          y_ = true_CE_tab %>%
            dplyr::filter(RowGroup_ == "Net Benefit (£)") %>%
            dplyr::mutate(dplyr::across(
              .cols = .interv_,
              .fns = function(.x) {
                x_ = gsub(
                  pattern = "£",
                  replacement = "",
                  x = .x)
                x_ = gsub(
                  pattern = ",",
                  replacement = "",
                  x = x_)
                x_ = as.numeric(x_)})) %>%
            dplyr::pull(.data[[.interv_]])
          x_ = ifelse(x_ > y_, x_ - y_, y_ - x_)
          x_
        })) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(
        .cols = .interv_,
        .fns = function(.x) {
          scales::dollar(
            x = .x,
            prefix = "£")
        })) %>%
      {if (.interv_ == interventions_list$v_interv_names[1]) {
        dplyr::select(
          .data = .,
          colnames(CR_CRS_2P2T_PSA_summary_table)[
            which(
              !colnames(CR_CRS_2P2T_PSA_summary_table) %in%
                names(interventions_list$v_interv_names)[
                  which(names(interventions_list$v_interv_names) !=
                          .interv_)])])
      } else {
        dplyr::select(
          .data = .,
          .interv_)
      }}
  }) %>%
  dplyr::bind_rows(
    CR_CRS_2P2T_PSA_summary_table %>%
      dplyr::filter(RowGroup_ != "Net Benefit (£)"),
    .) %>%
  dplyr::mutate(
    Ranking = dplyr::case_when(
      RowGroup_ == "Net Benefit (£)" ~ 1,
      RowGroup_ == "Probability Cost-Effective" ~ 2,
      RowGroup_ == "Expected Value of Perfect Information (£)" ~ 3,
      TRUE ~ NA_real_)) %>%
  dplyr::arrange(Method, Ranking) %>%
  dplyr::select(-Ranking) %>%
  dplyr::bind_rows(
    true_CE_tab,
    .)

CR_CRS_2P2T_PSA_summary_beutified_rel_tb <- CR_CRS_2P2T_PSA_summary_rel_true_tb %>%
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
  dplyr::group_by(Method, RowGroup_)

relative_vals_footnote <- CR_CRS_2P2T_PSA_summary_beutified_rel_tb %>%
  dplyr::filter(Method != "True") %>%
  dplyr::pull(Method) %>%
  unique(.) %>%
  paste(., "-", "Net Benefit (£)")

CR_CRS_2P2T_PSA_summary_beutified_rel_tb <-
  CR_CRS_2P2T_PSA_summary_beutified_rel_tb %>%
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
######## Save beautified absolute table:----
table_name <- "calibration_CE_PSA_rel.rds"
saveRDS(
  object = CR_CRS_2P2T_PSA_summary_beutified_rel_tb,
  file = glue::glue("{data_saving_path}{table_name}"))
#### Plots:----
##### Plot fitness function:----
###### Full view plots:----
set.seed(seed = seed_no)
CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .coloring_ = "none",
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .gof_ = gof_measure)
###### Save full view plots:----
purrr::walk(
  .x = CR_CRS_2P2T$plots$GOF_plots %>%
    names(.),
  .f = function(.calib_category_) {
    purrr::walk(
      .x = CR_CRS_2P2T$plots$GOF_plots[[.calib_category_]] %>%
        names(.),
      .f = function(.calib_gof_) {
        if(.calib_category_ == "blank") {
          image_name = glue::glue("GOF_{.calib_gof_}_blank.jpeg")
          reticulate::py_run_string("import sys")
          plotly::save_image(
            p = CR_CRS_2P2T$plots$GOF_plots[[.calib_category_]][[.calib_gof_]]$
              p_Mets$p_DieMets,
            file = glue::glue("{image_saving_path}{image_name}"),
            scale = 5)
        } else {
          purrr::walk(
            .x = CR_CRS_2P2T$plots$GOF_plots[[.calib_category_]][[.calib_gof_]] %>%
              names(.),
            .f = function(.calib_method_) {

              image_name = glue::glue("GOF_{.calib_gof_}_{.calib_method_}.jpeg")
              reticulate::py_run_string("import sys")
              plotly::save_image(
                p = CR_CRS_2P2T$plots
                $GOF_plots[[.calib_category_]][[.calib_gof_]][[.calib_method_]]$p_Mets$p_DieMets,
                file = glue::glue("{image_saving_path}{image_name}"),
                scale = 5)
            })
        }
      })
  })
###### Zoomed view plots:----
set.seed(seed = seed_no)
CR_CRS_2P2T$draw_GOF_measure(
  .true_points_ = TRUE,
  .coloring_ = "none",
  .legend_ = FALSE,
  .greys_ = TRUE,
  .scale_ = NULL,
  .zoom_ = TRUE,
  .gof_ = gof_measure)
###### Save zoomed view plots:----
purrr::walk(
  .x = CR_CRS_2P2T$plots$GOF_plots %>%
    names(.),
  .f = function(.calib_category_) {
    purrr::walk(
      .x = CR_CRS_2P2T$plots$GOF_plots[[.calib_category_]] %>%
        names(.),
      .f = function(.calib_gof_) {
        if(.calib_category_ == "blank") {
          NULL
        } else {
          purrr::walk(
            .x = CR_CRS_2P2T$plots$GOF_plots[[.calib_category_]][[.calib_gof_]] %>%
              names(.),
            .f = function(.calib_method_) {

              image_name = glue::glue("GOF_{.calib_gof_}_{.calib_method_}_zm.jpeg")
              reticulate::py_run_string("import sys")
              plotly::save_image(
                p = CR_CRS_2P2T$plots
                $GOF_plots[[.calib_category_]][[.calib_gof_]][[.calib_method_]]$p_Mets$p_DieMets,
                file = glue::glue("{image_saving_path}{image_name}"),
                scale = 5)
            })
        }
      })
  })
##### Plot targets:----
CR_CRS_2P2T$draw_targets_plots(
  .sim_targets_ = TRUE,
  .calibration_methods_ = c("random", "directed", "bayesian"))
###### Save target plots:----
purrr::walk(
  .x = CR_CRS_2P2T$plots$targets %>%
    names(.),
  .f = function(.calib_category_) {
    if(.calib_category_ == "blank") {
      purrr::walk(
        .x = targets_list$v_targets_names,
        .f = function(.target_) {

          image_name = glue::glue("tar_blank_{.target_}.jpeg")
          ggplot2::ggsave(
            filename = glue::glue("{image_saving_path}{image_name}"),
            plot = CR_CRS_2P2T$plots$targets[[.calib_category_]][[.target_]],
            scale = 2, #2.5
            width = 1000,
            height = 600,
            units = "px")
        })
    } else {
      purrr::walk(
        .x = CR_CRS_2P2T$plots$targets[[.calib_category_]] %>%
          names(.),
        .f = function(.calib_method_) {
          purrr::walk(
            .x = targets_list$v_targets_names,
            .f = function(.target_) {

              image_name = glue::glue("tar_{.calib_method_}_{.target_}.jpeg")
              ggplot2::ggsave(
                filename = glue::glue("{image_saving_path}{image_name}"),
                plot = CR_CRS_2P2T$plots$
                  targets[[.calib_category_]][[.calib_method_]][[.target_]],
                scale = 2, #2.5
                width = 1000,
                height = 600,
                units = "px")
            })
        })
    }
  })

##### Prior posterior plot:----
CR_CRS_2P2T$draw_distributions_plots()
######## save post prior plot:----
purrr::walk(
  .x = CR_CRS_2P2T$plots$distributions %>%
    names(.),
  .f = function(.bayes_method_) {
    purrr::walk(
      .x = CR_CRS_2P2T$plots$distributions[[.bayes_method_]] %>%
        names(.),
      .f = function(.param_) {
        image_name = glue::glue("dst_{.bayes_method_}_{.param_}.jpeg")
        ggplot2::ggsave(
          filename = glue::glue("{image_saving_path}{image_name}"),
          plot = CR_CRS_2P2T$plots$distributions[[.bayes_method_]][[.param_]],
          scale = 2,
          width = 1000,
          height = 700,
          units = "px")
      })
  })

#### Fitness plots:----
GOF_plots_list <- CR_CRS_2P2T$draw_GOF_measure(
  .points_ = FALSE,
  .true_points_ = FALSE,
  .legend_ = FALSE,
  .greys_ = FALSE,
  .coloring_ = "heatmap",
  .scale_ = NULL,
  .gof_ = gof_measure)$
  plots$
  GOF_plots$
  blank
##### Save plot:----
purrr::walk(
  .x = gof_measure,
  .f = function(.gof_measure_) {
    image_name = glue::glue("GOF_{.gof_measure_}.jpeg")
    reticulate::py_run_string("import sys")
    plotly::save_image(
      p = GOF_plots_list[[.gof_measure_]][[1]][[1]],
      file = glue::glue("{image_saving_path}{image_name}"),
      scale = 5)
  })
