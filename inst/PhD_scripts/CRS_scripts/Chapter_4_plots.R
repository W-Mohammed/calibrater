##################################################
#       Confirmation Review Gantt Chart          #
##################################################

# Gantt Chart:----
## Load library:----
### new() will not recognise "gantt" unless the "plan" library is loaded:----
library("plan")
### Saving path:----
path = "../../2. Confirmation Review/CR_data/"
chapter_dir <- "Chap_4/"
image_dir <- "images/"
image_saving_path <- glue::glue("{path}{chapter_dir}{image_dir}")
## Create the gantt chart:----
### Define an object of class "gantt":----
g <- new("gantt")
### Define headings and tasks:----
g <- plan::ganttAddTask(
  g = g,
  description = "1. Methods review:")
g <- plan::ganttAddTask(
  g = g,
  description = "Identification of studies",
  "2021-08-01",
  "2022-02-01",
  done = 100)
g <- plan::ganttAddTask(
  g = g,
  description = "Identification of common methods",
  "2021-08-01",
  "2022-04-01",
  done = 90)
g <- plan::ganttAddTask(
  g = g,
  description = "Understanding the methods",
  "2021-08-01",
  "2022-04-01",
  done = 90)
g <- plan::ganttAddTask(
  g = g,
  description = "Comparative studies",
  "2022-02-01",
  "2022-11-01",
  done = 100)
g <- plan::ganttAddTask(
  g = g,
  description = "2. Methods development:")
g <- plan::ganttAddTask(
  g = g,
  description = "Non-Bayesian methods",
  "2023-02-01",
  "2024-03-01",
  done = 0)
g <- plan::ganttAddTask(
  g = g,
  description = "Calibration targets",
  "2023-08-01",
  "2024-04-01",
  done = 0)
g <- plan::ganttAddTask(
  g = g,
  description = "3. Methods testing:")
g <- plan::ganttAddTask(
  g = g,
  description = "Simulation study 2",
  "2023-06-01",
  "2024-02-01",
  done = 0)
g <- plan::ganttAddTask(
  g = g,
  description = "Simulation study 3",
  "2023-12-01",
  "2024-04-01",
  done = 0)
g <- plan::ganttAddTask(
  g = g,
  description = "4. Methods application:")
g <- plan::ganttAddTask(
  g = g,
  description = "Case study 1",
  "2024-02-01",
  "2024-06-01",
  done = 0)
g <- plan::ganttAddTask(
  g = g,
  description = "Case study 2",
  "2024-04-01",
  "2024-09-01",
  done = 0)
g <- plan::ganttAddTask(
  g = g,
  description = "5. Training:")
g <- plan::ganttAddTask(
  g = g,
  description = "Bayesian methods",
  "2021-10-01",
  "2023-04-01",
  done = 80)
g <- plan::ganttAddTask(
  g = g,
  description = "Conferences and courses",
  "2021-10-01",
  "2024-09-01",
  done = 30)
g <- plan::ganttAddTask(
  g = g,
  description = "6. Writing and review:")
g <- plan::ganttAddTask(
  g = g,
  description = "Confirmation Review",
  "2022-03-01",
  "2023-01-01",
  done = 100)
g <- plan::ganttAddTask(
  g = g,
  description = "Thesis",
  "2023-09-01",
  "2024-10-01",
  done = 0)
## Plot and save gantt chart:----
# Plot was exported: plot_zoom_png?width=1164&height=598
### Without legend:----
#### Open graphics device:----
image_name <- "Gantt_chart.jpeg"
jpeg(
  filename = glue::glue("{image_saving_path}{image_name}"),
  width = 1700,
  height = 850,
  res = 180)
#### Plot the chart:----
plot(
  x = g,
  ylabel = list(
    font = ifelse(is.na(g[["start"]]), 2, 1)),
  event.time = "2023-01-24",
  event.label = "Confirmation Review",
  main = "PhD Gantt Chart")
#### Close graphics device:----
dev.off()
# ### With legend:----
# #### Open graphics device:----
# image_name <- "Gantt_chart_w_legend.jpeg"
# jpeg(
#   filename = glue::glue("{image_saving_path}{image_name}"),
#   width = 1164,
#   height = 598)
# #### Plot the chart:----
# plot(
#   x = g,
#   ylabel = list(
#     font = ifelse(is.na(g[["start"]]), 2, 1)),
#   event.time = "2023-01-24",
#   event.label = "Confirmation Review",
#   main = "PhD Gantt Chart")
# #### Add legend:----
# legend(
#   x = 0.7, #"topright",
#   y = 1.4,
#   pch = 22,
#   pt.cex = 2,
#   cex = 0.9,
#   pt.bg = gray(c(0.3, 0.9)),
#   border = "black",
#   legend = c("Completed", "Not Yet Done"),
#   bg="white",
#   xpd=TRUE)
# #### Close graphics device:----
# dev.off()
