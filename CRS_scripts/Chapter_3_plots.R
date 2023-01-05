##################################################
#      Confirmation Review case studies          #
##################################################
# Model: CRS_markov_2
# Targets: c(Survival, PropSick)

## Load the calibR package:
devtools::load_all() # 1 parent 6b344f8 commit d93ff4733193a2bcbdc276dab553fb0c2ba3de24
library(ShinyPSA)

## Chapter 3 plots:----

### CE plots:----
psa_object <- readRDS(file = "../../2. Confirmation Review/CR_data/Chap_3/data/CRS_true_PSA.rds")
psa_sum_object <- ShinyPSA::summarise_PSA_(
  .effs = CRS_true_PSA$e,
  .costs = CRS_true_PSA$c,
  .params = CRS_true_PSA$p,
  .interventions = CRS_true_PSA$treats)
#### CEP:----
CE_plane <- ShinyPSA::plot_CEplane_(
  psa_sum_object,
  .legend_pos = c(0.3, 0.8),
  .show_ICER = FALSE,
  .show_title = FALSE)
ggplot2::ggsave(
  filename = "true_ce_plane.jpeg",
  plot = CE_plane,
  scale = 1.5,
  width = 1000,
  height = 700,
  units = "px")
#### CEAC:----
CEAC <- ShinyPSA::plot_CEAC_(
  psa_sum_object,
  .legend_pos = "none",
  .show_title = FALSE)
ggplot2::ggsave(
  filename = "true_ceac.jpeg",
  plot = CEAC,
  scale = 1.5,
  width = 1000,
  height = 700,
  units = "px")

### Calibration targets:----
#### Read data:----
CRS_data <- calibR::CR_CRS_data_2t
#### Plotting "Surv":----
Surv_plot <- ggplot2::ggplot(
  data = CRS_data$l_targets$Surv,
  ggplot2::aes(
    x = time,
    y = value)) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lb,
      ymax = ub)) +
  ggplot2::geom_point() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, color = 'black')) +
  ggplot2::labs(
    x = "Time in years",
    y = "Proportion of cohort survived")
ggplot2::ggsave(
  filename = "surv_plot.jpeg",
  plot = Surv_plot,
  scale = 2.5,
  width = 1000,
  height = 600,
  units = "px")
#### Plotting "PropSik":----
Prev_plot <- ggplot2::ggplot(
  data = CRS_data$l_targets$PropSick,
  ggplot2::aes(
    x = time,
    y = value)) +
  ggplot2::geom_errorbar(
    ggplot2::aes(
      ymin = lb,
      ymax = ub)) +
  ggplot2::geom_point() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, color = 'black')) +
  ggplot2::labs(
    x = "Time in years",
    y = "Proportion of cohort developed cancer")
ggplot2::ggsave(
  filename = "prev_plot.jpeg",
  plot = Prev_plot,
  scale = 2.5,
  width = 1000,
  height = 600,
  units = "px")
