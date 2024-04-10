# Loads packages needed for defining pipeline
library(targets)
library(tarchetypes)
library(tidyverse)
library(crew)

# Set target-specific options such as packages and parallelization
tar_option_set(
  packages = c("reticulate","purrr"),
  controller = crew_controller_local(workers = 10)) # nolint

# Loads all custom functions
list.files("R",full.names = T) %>%
  purrr::map(source)

# Defines a tibble to map over
var_frame <- tibble(year = 2000+c(1:11,17:19))

# Creates a target for each year
tar_fetch_data_from_cm<-tar_map(
  values = var_frame,
  tar_target(name = SST,
             command = cmw_subset(
               serviceId = "GLOBAL_MULTIYEAR_PHY_001_030",
               productId = "cmems_mod_glo_phy_my_0.083deg_P1D-m",
               variable = "thetao",
               dates = c(paste0(year,"-01-01T00:00:00"),paste0(year,"-12-31T00:00:00")),
               depth = c(0.494024991989136,0.494024991989136),
               out_dir = paste0(getwd(),"/cmw_data")),
             format = "file",
             repository = "local"
  ),
  tar_target(name = ZOOC,
             command = cmw_subset(
               serviceId = "GLOBAL_MULTIYEAR_BGC_001_033",
               productId = "cmems_mod_glo_bgc_my_0.083deg-lmtl_PT1D-i",
               variable = "zooc",
               dates = c(paste0(year,"-01-01T00:00:00"),paste0(year,"-12-31T00:00:00")),
               out_dir = paste0(getwd(),"/cmw_data")),
             format = "file",
             repository = "local"
  )
)

# The list of instructions to build 
# Run using targets::tar_make() - note only builds changed parts and their descendants
# Show dependency graph using targets::tar_visnetwork()
list(
  tar_fetch_data_from_cm
)
