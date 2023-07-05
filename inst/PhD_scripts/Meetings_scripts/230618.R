##################################################
#              Meeting task 230619               #
##################################################

# Task: compare correlation between inputs and outputs in a similar way to how
# one-way sensitivity analysis (SA) is performed.

Model <- calibR::CRS_markov_2
Parameters_names <- c(p_Mets = "p_Mets", p_DieMets = "p_DieMets")
Parameters_values <- c(p_Mets = 0.10, p_DieMets = 0.05)
Parameters_configurations <- 1000

#### Case study lists:----
seed_no <- 1
set.seed(seed = seed_no)
parameters_list <- calibR::CR_CRS_data_2t$l_params
targets_list <- calibR::CR_CRS_data_2t$l_targets
interventions_list <- calibR::CR_CRS_data_2t$l_intervs
gof_measure <- "LLK"
sample_method <- "LHS"
sampling_methods <- "LHS"

#### Initiate CalibR R6 object:----
CR_CRS_2P2T = calibR_R6$
  new(
    .model = Model,
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

## Generate configurations for each parameters and estimate correlation:
### Correlation betwwen the parameters:
CR_CRS_2P2T$
  calibrateR_random(
    .optim = FALSE,
    .maximise = TRUE,
    .weighted = TRUE,
    .sample_method = sampling_methods,
    .calibration_method = gof_measure)

### Objective function (overall fit) and parameter value:
corr_obj_function <- purrr::map(
  .x = Parameters_names,
  .f = function(param_name_) {
    #### Generate samples using Random Grid Search:----
    param_colname <- Parameters_names[!Parameters_names %in% param_name_] |>
      unname()
    param_corr_name <- paste0(param_name_, "_cor")

    CR_CRS_2P2T$prior_samples[["LHS"]] <- calibR::sample_prior_LHS(
      .n_samples = Parameters_configurations,
      .l_params = parameters_list
    ) %>%
      dplyr::mutate(
        {{param_colname}} := Parameters_values[param_name_] %>%
          unname()
      )

    CR_CRS_2P2T$calibrateR_random(
      .optim = FALSE,
      .maximise = TRUE,
      .weighted = TRUE,
      .sample_method = sampling_methods,
      .calibration_method = gof_measure)

    calibration_results <- CR_CRS_2P2T$
      calibration_results$
      random$
      LLK_LHS %>%
      dplyr::select(
        dplyr::all_of(
          c("Overall_fit", param_name_)
        )
      )

    calibration_results %>%
      dplyr::mutate(
        {{param_corr_name}} := cor(
          dplyr::pull(calibration_results, {{param_name_}}),
          dplyr::pull(calibration_results, Overall_fit)
        )
      )
  })

# > corr_obj_function
# $p_Mets
# # A tibble: 1,000 × 3
# Overall_fit p_Mets p_Mets_cor
# <dbl>  <dbl>      <dbl>
#   1       -918.  0.108     -0.734
# 2       -918.  0.109     -0.734
# 3       -919.  0.110     -0.734
# 4       -919.  0.107     -0.734
# 5       -922.  0.111     -0.734
# 6       -924.  0.106     -0.734
# 7       -931.  0.113     -0.734
# 8       -931.  0.105     -0.734
# 9       -933.  0.113     -0.734
# 10       -933.  0.105     -0.734
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# $p_DieMets
# # A tibble: 1,000 × 3
# Overall_fit p_DieMets p_DieMets_cor
# <dbl>     <dbl>         <dbl>
#   1      -2288.    0.0385        -0.916
# 2      -2289.    0.0398        -0.916
# 3      -2291.    0.0379        -0.916
# 4      -2292.    0.0402        -0.916
# 5      -2304.    0.0413        -0.916
# 6      -2305.    0.0365        -0.916
# 7      -2318.    0.0358        -0.916
# 8      -2329.    0.0428        -0.916
# 9      -2351.    0.0437        -0.916
# 10      -2364.    0.0340        -0.916
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows

### Cost-effectiveness outcomes and parameter value:
corr_outcomes <- purrr::map(
  .x = Parameters_names,
  .f = function(param_name_) {
    #### Generate samples using Random Grid Search:----
    param_colname <- Parameters_names[!Parameters_names %in% param_name_] |>
      unname()
    param_cost_corr <- paste0(param_name_, "_Costs_cor")
    param_qaly_corr <- paste0(param_name_, "_QALYs_cor")

    CR_CRS_2P2T$prior_samples[["LHS"]] <- calibR::sample_prior_LHS(
      .n_samples = Parameters_configurations,
      .l_params = parameters_list
    ) %>%
      dplyr::mutate(
        {{param_colname}} := Parameters_values[param_name_] %>%
          unname()
      )

    CR_CRS_2P2T$prior_samples$LHS %>%
      purrr::pmap(
        .f = function(...) {
          params <- list(...)
          Model(
            .v_params_ = params,
            calibrate_ = FALSE
          )
        }
      ) %>%
      purrr::transpose() %>%
      purrr::map(
        .f = function(inner_list) {
          inner_list %>%
            purrr::transpose() %>%
            purrr::map(
              .f = function(in_inner_list){
                in_inner_list %>%
                  purrr::list_c()
              }) %>%
            tibble::as_tibble() %>%
            dplyr::mutate(
              {{param_name_}} := CR_CRS_2P2T$
                prior_samples$
                LHS[[param_name_]],
              {{param_cost_corr}} := cor(
                dplyr::pull(., costs),
                CR_CRS_2P2T$
                  prior_samples$
                  LHS[[param_name_]]
              ),
              {{param_qaly_corr}} := cor(
                dplyr::pull(., Effects),
                CR_CRS_2P2T$
                  prior_samples$
                  LHS[[param_name_]]
              )
            )
        })
  })

# $p_Mets
# $p_Mets$None
# # A tibble: 1,000 × 5
# costs Effects p_Mets p_Mets_Costs_cor p_Mets_QALYs_cor
# <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
#   1 39903.    8.80 0.302             0.347           -0.615
# 2 39925.    6.82 0.753             0.347           -0.615
# 3 38922.   18.2  0.0774            0.347           -0.615
# 4 39927.    6.64 0.866             0.347           -0.615
# 5 39922.    7.22 0.579             0.347           -0.615
# 6 38138.   20.6  0.0639            0.347           -0.615
# 7 39870.   10.5  0.199             0.347           -0.615
# 8 39928.    6.56 0.932             0.347           -0.615
# 9 39927.    6.69 0.835             0.347           -0.615
# 10 39925.    6.89 0.714             0.347           -0.615
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# $p_Mets$Medication
# # A tibble: 1,000 × 5
# costs Effects p_Mets p_Mets_Costs_cor p_Mets_QALYs_cor
# <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
#   1 530623.    13.7 0.302             0.424           -0.625
# 2 533749.    11.8 0.753             0.424           -0.625
# 3 498276.    22.6 0.0774            0.424           -0.625
# 4 533989.    11.6 0.866             0.424           -0.625
# 5 533172.    12.2 0.579             0.424           -0.625
# 6 483140.    24.9 0.0639            0.424           -0.625
# 7 527281.    15.4 0.199             0.424           -0.625
# 8 534102.    11.6 0.932             0.424           -0.625
# 9 533929.    11.7 0.835             0.424           -0.625
# 10 533645.    11.9 0.714             0.424           -0.625
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# $p_Mets$Screening
# # A tibble: 1,000 × 5
# costs Effects p_Mets p_Mets_Costs_cor p_Mets_QALYs_cor
# <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
#   1 119352.   14.1  0.302            -0.718           -0.729
# 2  71770.   10.1  0.753            -0.718           -0.729
# 3 316306.   29.9  0.0774           -0.718           -0.729
# 4  67629.    9.79 0.866            -0.718           -0.729
# 5  81385.   10.9  0.579            -0.718           -0.729
# 6 355837.   33.0  0.0639           -0.718           -0.729
# 7 159734.   17.4  0.199            -0.718           -0.729
# 8  65665.    9.63 0.932            -0.718           -0.729
# 9  68672.    9.88 0.835            -0.718           -0.729
# 10  73530.   10.3  0.714            -0.718           -0.729
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# $p_Mets$Both
# # A tibble: 1,000 × 5
# costs Effects p_Mets p_Mets_Costs_cor p_Mets_QALYs_cor
# <dbl>   <dbl>  <dbl>            <dbl>            <dbl>
#   1 602894.    20.6 0.302            -0.868           -0.743
# 2 563597.    16.9 0.753            -0.868           -0.743
# 3 703006.    34.8 0.0774           -0.868           -0.743
# 4 560004.    16.6 0.866            -0.868           -0.743
# 5 571853.    17.7 0.579            -0.868           -0.743
# 6 713170.    37.4 0.0639           -0.868           -0.743
# 7 631978.    23.7 0.199            -0.868           -0.743
# 8 558293.    16.4 0.932            -0.868           -0.743
# 9 560911.    16.7 0.835            -0.868           -0.743
# 10 565118.    17.0 0.714            -0.868           -0.743
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
#
# $p_DieMets
# $p_DieMets$None
# # A tibble: 1,000 × 5
# costs Effects p_DieMets p_DieMets_Costs_cor p_DieMets_QALYs_cor
# <dbl>   <dbl>     <dbl>               <dbl>               <dbl>
#   1 43053.    25.0    0.0834              -0.650              -0.650
# 2  4870.    19.8    0.783               -0.650              -0.650
# 3 16995.    21.5    0.222               -0.650              -0.650
# 4 26346.    22.7    0.142               -0.650              -0.650
# 5 10636.    20.6    0.357               -0.650              -0.650
# 6 33491.    23.7    0.110               -0.650              -0.650
# 7 42593.    25.0    0.0845              -0.650              -0.650
# 8  4390.    19.7    0.869               -0.650              -0.650
# 9  8135.    20.2    0.468               -0.650              -0.650
# 10  7349.    20.1    0.518               -0.650              -0.650
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# $p_DieMets$Medication
# # A tibble: 1,000 × 5
# costs Effects p_DieMets p_DieMets_Costs_cor p_DieMets_QALYs_cor
# <dbl>   <dbl>     <dbl>               <dbl>               <dbl>
#   1 517965.    29.3    0.0834              -0.757              -0.757
# 2  67937.    20.5    0.783               -0.757              -0.757
# 3 232247.    23.7    0.222               -0.757              -0.757
# 4 347424.    25.9    0.142               -0.757              -0.757
# 5 147349.    22.0    0.357               -0.757              -0.757
# 6 425701.    27.5    0.110               -0.757              -0.757
# 7 513818.    29.2    0.0845              -0.757              -0.757
# 8  61269.    20.3    0.869               -0.757              -0.757
# 9 113090.    21.3    0.468               -0.757              -0.757
# 10 102256.    21.1    0.518               -0.757              -0.757
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# $p_DieMets$Screening
# # A tibble: 1,000 × 5
# costs Effects p_DieMets p_DieMets_Costs_cor p_DieMets_QALYs_cor
# <dbl>   <dbl>     <dbl>               <dbl>               <dbl>
#   1 410976.    37.7    0.0834              -0.670              -0.670
# 2 381530.    32.2    0.783               -0.670              -0.670
# 3 391227.    34.0    0.222               -0.670              -0.670
# 4 398480.    35.4    0.142               -0.670              -0.670
# 5 386182.    33.1    0.357               -0.670              -0.670
# 6 403891.    36.4    0.110               -0.670              -0.670
# 7 410638.    37.7    0.0845              -0.670              -0.670
# 8 381140.    32.1    0.869               -0.670              -0.670
# 9 384173.    32.7    0.468               -0.670              -0.670
# 10 383538.    32.6    0.518               -0.670              -0.670
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
#
# $p_DieMets$Both
# # A tibble: 1,000 × 5
# costs Effects p_DieMets p_DieMets_Costs_cor p_DieMets_QALYs_cor
# <dbl>   <dbl>     <dbl>               <dbl>               <dbl>
#   1 765801.    41.9    0.0834              -0.774              -0.774
# 2 432765.    32.9    0.783               -0.774              -0.774
# 3 560307.    36.4    0.222               -0.774              -0.774
# 4 645043.    38.6    0.142               -0.774              -0.774
# 5 495494.    34.6    0.357               -0.774              -0.774
# 6 701035.    40.1    0.110               -0.774              -0.774
# 7 762914.    41.8    0.0845              -0.774              -0.774
# 8 427407.    32.8    0.869               -0.774              -0.774
# 9 468683.    33.9    0.468               -0.774              -0.774
# 10 460125.    33.7    0.518               -0.774              -0.774
# # ℹ 990 more rows
# # ℹ Use `print(n = ...)` to see more rows
