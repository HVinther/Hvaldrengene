library(targets)
library(tarchetypes)
library(tidyverse)
library(crew)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(data_summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# Set target-specific options such as packages:
tar_option_set(
  packages = c("reticulate","purrr"),
  controller = crew_controller_local(workers = 4)) # nolint

source("R/cmw.R")

var_frame <- tibble(year = 2000+c(1:2))

tar_fetch_data_from_cm<-tar_map(
  values = var_frame,
  tar_target(name = SST,
             command = cmw_subset(
               serviceId = "GLOBAL_MULTIYEAR_PHY_001_030",
               productId = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
               variable = "thetao",
               dates = c(paste0(year,"-01-01T00:00:00"),paste0(year,"-12-31T00:00:00")),
               depth = c(0.494024991989136,0.494024991989136)),
             format = "file",
             repository = "local"
  ),
  tar_target(name = ZOOC,
             command = cmw_subset(
               serviceId = "GLOBAL_MULTIYEAR_BGC_001_033",
               productId = "cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i",
               variable = "zooc",
               dates = c(paste0(year,"-01-01T00:00:00"),paste0(year,"-12-31T00:00:00"))),
             format = "file",
             repository = "local"
  )
)

# End this file with a list of target objects.
list(
  tar_fetch_data_from_cm
)
