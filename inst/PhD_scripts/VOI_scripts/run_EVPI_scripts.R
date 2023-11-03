##################################################
#                 VOI case studies               #
##################################################
files <- list.files(
  path = paste(
    here::here(), "inst", "PhD_scripts", "VOI_scripts",
    sep = "/"
  ),
  pattern = "EVPI_ID",
  full.names = TRUE
)
for (filename in files) {
  print(paste("Processing script:", filename))
  sourcing_env <- environment()
  source(
    file = filename,
    local = sourcing_env
  )
  rm(sourcing_env)
}
