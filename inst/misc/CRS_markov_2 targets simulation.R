# Targets simulations:
# Model: CRS_markov_2
# Parameters: c(p_Mets, p_DieMets)

set.seed(1)
psa_nums = 10000
p_Mets_data = rnorm(
  n = psa_nums,
  mean = 0.10,
  sd = 0.10*0.1)
p_DieMets_data = rnorm(
  n = psa_nums,
  mean = 0.05,
  sd = 0.05*0.1)
psa_data = tibble::tibble(
  "p_Mets" = p_Mets_data,
  "p_DieMets" = p_DieMets_data)

psa_outputs = purrr::pmap(
  .l = psa_data,
  .f = function(...){
    params = c(...)
    CRS_markov_2(.v_params_ = params)
  })

psa_outputs2 <- purrr::transpose(psa_outputs)
class(CRS_markov_2(
))

# Survival:----
Surv_psa <- purrr::map_dfr(
  .x = psa_outputs2$Surv,
  .f = function(.col) {
    .col
  }
)
Surv_psa_summaries <- purrr::map_dfc(
  .x = Surv_psa,
  .f = function(.col) {
    res = c("value" = mean(.col),
            "se" = sd(.col),
            "lb" = quantile(.col, 0.025),
            "ub" = quantile(.col, 0.975))
  }) %>%
  t() %>%
  dplyr::as_tibble() %>%
  dplyr::mutate("time" = 2:(nrow(.)+1)) %>%
  dplyr::rename("value" = "V1",
                "se" = "V2",
                "lb" = "V3",
                "ub" = "V4")

# PropSick:----
PropSick_psa <- purrr::map_dfr(
  .x = psa_outputs2$PropSick,
  .f = function(.col) {
    .col
  }
)
PropSick_psa_summaries <- purrr::map_dfc(
  .x = PropSick_psa,
  .f = function(.col) {
    res = c("value" = mean(.col),
            "se" = sd(.col),
            "lb" = quantile(.col, 0.025),
            "ub" = quantile(.col, 0.975))
  }) %>%
  t() %>%
  dplyr::as_tibble() %>%
  dplyr::mutate("time" = 2:(nrow(.)+1)) %>%
  dplyr::rename("value" = "V1",
                "se" = "V2",
                "lb" = "V3",
                "ub" = "V4")
CRS_targets_2 = list(
  "Surv" = Surv_psa_summaries,
  "PropSick" = PropSick_psa_summaries)


usethis::use_data(CRS_targets_2, overwrite = TRUE)
