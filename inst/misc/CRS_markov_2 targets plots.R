# Targets plots:
# Model: CRS_markov_2
# Targets: c(Prev, Surv)

# Plotting "Surv":----
Surv_plot <- ggplot2::ggplot(
  data = CR_CRS_data_2t$l_targets$Surv,
  ggplot2::aes(
    x = time,
    y = value)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lb, ymax = ub)) +
  ggplot2::geom_point() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, color = 'black')) +
  ggplot2::labs(
    x = "Year",
    y = "Proportion survived")

Prev_plot <- ggplot2::ggplot(
  data = CR_CRS_data_2t$l_targets$PropSick,
  ggplot2::aes(
    x = time,
    y = value)) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = lb, ymax = ub)) +
  ggplot2::geom_point() +
  ggplot2::theme(
    panel.border = ggplot2::element_rect(fill = NA, color = 'black')) +
  ggplot2::labs(
    x = "Year",
    y = "Proportion sick")

# saveRDS(object = CR_CRS_data_2t, file = "../../2. Confirmation Review/CR_data/Case_study_1/CRS_targets.rds")
