# Targets simulations:
# Model: HID_markov
# Parameters: c(mu_e, mu_l, mu_t, p, r_l, rho, b)

mean_vals = c('mu_e' = 0.04, 'mu_l' = 0.15, 'mu_t' = 0.016, 'p' = 0.12,
              'r_l' = 0.41, 'rho' = 0.53, 'b' = 0.21)
sd_vals = mean_vals * 0.1

set.seed(1)
psa_nums = 10000
psa_data = purrr::map_dfc(
  .x = mean_vals,
  .f = function(mean_val) {
    rnorm(
      n = psa_nums,
      mean = mean_val,
      sd = mean_val * 0.1)
  })

psa_outputs = purrr::pmap(
  .l = psa_data,
  .f = function(...){
    params = c(...)
    CRS_markov_2(.v_params_ = params)
  })

psa_outputs2 <- purrr::transpose(psa_outputs)

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
