# PSA:
# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)

set.seed(1)
psa_nums <- 10000
mean_vals <- c('p_Mets' = 0.10, 'p_DieMets' = 0.05)
psa_data <- purrr::map_dfc(
  .x = mean_vals,
  .f = function(mean_val) {
    rnorm(
      n = psa_nums,
      mean = mean_val,
      sd = mean_val * 0.1)
  })

psa_outputs <- purrr::pmap(
  .l = psa_data,
  .f = function(...){
    params = c(...)
    CRS_markov_2(.v_params_ = params, calibrate_ = FALSE)
  })

psa_outputs2 <- purrr::transpose(psa_outputs)

psa_outputs3 <- t(data.table::as.data.table(psa_outputs2$None)) %>%
  dplyr::as_tibble()
interventions <- names(psa_outputs2)
costs_cols <- paste0(interventions, "_Costs")
QALYs_cols <- paste0(interventions, "_QALYs")
names(interventions) <- names(costs_cols) <- names(QALYs_cols) <- interventions

psa_outputs3 = purrr::map(
  .x = interventions,
  .f = function(.intervention){
    costs_col <- costs_cols[[.intervention]]
    QALYs_col <- QALYs_cols[[.intervention]]
    t(data.table::as.data.table(psa_outputs2[[.intervention]])) %>%
      dplyr::as_tibble() %>%
      dplyr::rename(
        {{costs_col}} := "V1",
        {{QALYs_col}} := "V2")
    })
# Costs:----
Costs_data <- purrr::map_dfc(
  .x = interventions,
  .f = function(.intervention){
    psa_outputs3[[.intervention]] %>%
      dplyr::select(dplyr::contains("_Costs")) %>%
      unlist() %>%
      dplyr::as_tibble()
    })
# QALYs:----
QALYs_data <- purrr::map_dfc(
  .x = interventions,
  .f = function(.intervention){
    psa_outputs3[[.intervention]] %>%
      dplyr::select(dplyr::contains("_QALYs")) %>%
      unlist() %>%
      dplyr::as_tibble()
  })

CRS_true_CE <- list(
  "c" = Costs_data,
  "e" = QALYs_data,
  "p" = psa_data,
  "treats" = interventions
  )

saveRDS(object = CRS_true_CE, file = "../../2. Confirmation Review/CR_data/Case_study_1/CRS_true_PSA.rds")

CRS_true_CE = readRDS(file = "../../2. Confirmation Review/CR_data/Case_study_1/CRS_true_PSA.rds")

## Summarise PSA:
psa_object <- ShinyPSA::summarise_PSA_(
  .effs = CRS_true_CE$e,
  .costs = CRS_true_CE$c,
  .params = CRS_true_CE$p,
  .interventions = CRS_true_CE$treats,
  .plot = FALSE)

saveRDS(object = psa_object[27:34], file = "../../2. Confirmation Review/CR_data/Case_study_1/CRS_true_PSA_summary.rds",compress = T)

tabo <- ShinyPSA::draw_summary_table_(
  .PSA_data = psa_object,
  .wtp_ = c(2e4, 3e4, 5e4),
  .beautify_ = FALSE,
  .long_ = TRUE)

tabo %>% gt::gt()
