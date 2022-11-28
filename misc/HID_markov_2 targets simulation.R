# Targets simulations:
## Model: HID_markov
## Parameters: c(mu_e, mu_l, mu_t, p, r_l, rho, b)

mean_vals = c('mu_e' = 0.04, 'mu_l' = 0.15, 'mu_t' = 0.016, 'p' = 0.12,
              'r_l' = 0.41, 'rho' = 0.53, 'b' = 0.21)

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
    HID_markov(.v_params_ = params)
  })

psa_outputs2 <- purrr::transpose(psa_outputs)


# Prevalence:----
Prev_psa <- t(data.table::as.data.table(psa_outputs2$Prev)) %>%
  dplyr::as_tibble()
colnames(Prev_psa) <- c("10yrs", "20yrs", "30yrs")
Prev_psa_summaries <- purrr::map_dfc(
  .x = Prev_psa,
  .f = function(.col) {
    res = c("value" = mean(.col),
            "se" = sd(.col),
            "lb" = quantile(.col, 0.025),
            "ub" = quantile(.col, 0.975))
  }) %>%
  t() %>%
  dplyr::as_tibble() %>%
  dplyr::rename("value" = "V1",
                "se" = "V2",
                "lb" = "V3",
                "ub" = "V4")

# Survival:----
Surv_psa <- t(data.table::as.data.table(psa_outputs2$Surv)) %>%
  dplyr::as_tibble()
colnames(Surv_psa) <- c("Surv")
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
  dplyr::rename("value" = "V1",
                "se" = "V2",
                "lb" = "V3",
                "ub" = "V4")

# Trt_vol:----
Trt_vol_psa <- t(data.table::as.data.table(psa_outputs2$Trt_vol)) %>%
  dplyr::as_tibble()
colnames(Trt_vol_psa) <- c("Trt_vol")
Trt_vol_psa_summaries <- purrr::map_dfc(
  .x = Trt_vol_psa,
  .f = function(.col) {
    res = c("value" = mean(.col),
            "se" = sd(.col),
            "lb" = quantile(.col, 0.025),
            "ub" = quantile(.col, 0.975))
  }) %>%
  t() %>%
  dplyr::as_tibble() %>%
  dplyr::rename("value" = "V1",
                "se" = "V2",
                "lb" = "V3",
                "ub" = "V4")

HID_targets_3 = list(
  "Prev" = Prev_psa_summaries,
  "Surv" = Surv_psa_summaries,
  "Trt_vol" = Trt_vol_psa_summaries)

dput(x = HID_targets_3, file = "") # send to console

#usethis::use_data(HID_targets_3, overwrite = TRUE)
