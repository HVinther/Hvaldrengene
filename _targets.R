# Loads packages needed for defining pipeline
library(targets)
library(tarchetypes)
library(tidyverse)
library(crew)

# Set target-specific options such as packages and parallelization
tar_option_set(
  packages = c("reticulate","purrr","raster"),
  controller = crew_controller_local(workers = 10)) # nolint

# Loads all custom functions
list.files("R",full.names = T) %>%
  purrr::map(source)

# procedure
list(
  # fetching covariates from copernicus
  tar_map(
    values = tibble(year = 2000+c(1:11,17:19)),
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
  ),
  # create date vector from rasters
  tar_target(Z, 
             makeZ(list(SST_2001,SST_2002,SST_2003,SST_2004,SST_2005,SST_2006,SST_2007,SST_2008,SST_2009,SST_2010,SST_2011,SST_2017,SST_2018,SST_2019)))
)

