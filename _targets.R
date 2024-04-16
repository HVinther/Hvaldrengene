# Loads packages needed for defining pipeline
library(targets)
library(tarchetypes)
library(tidyverse)
library(crew)

# Set target-specific options such as packages and parallelization
tar_option_set(
  packages = c("reticulate","purrr","raster","sf","pathroutr",
               "tidyverse","maps","sfheaders","sfnetworks",
               "aniMotum","rnaturalearth","rnaturalearthdata","plotly",
               "R.utils","gridExtra","ggmagnify"),
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
  
  # Create list of paths to SST rasters
  tar_target(SSTpaths,list(SST_2001,SST_2002,SST_2003,SST_2004,SST_2005,SST_2006,SST_2007,SST_2008,SST_2009,SST_2010,SST_2011,SST_2017,SST_2018,SST_2019)),
  
  # Create list of paths to ZOOC rasters
  tar_target(ZOOCpaths,list(ZOOC_2001,ZOOC_2002,ZOOC_2003,ZOOC_2004,ZOOC_2005,ZOOC_2006,ZOOC_2007,ZOOC_2008,ZOOC_2009,ZOOC_2010,ZOOC_2011,ZOOC_2017,ZOOC_2018,ZOOC_2019)),
  
  # create date vector from rasters making sure the date vectors are identical between covariates
  tar_target(Z,makeZ(SSTpaths,ZOOCpaths)),
  
  # Extract the swimspeed filtered data
  tar_target(whales, read.csv("SwimSpeedFitleredWhaleData.txt")),
  
  # fit state space model
  tar_target(ssm,ani_analysis_fit_ssm(whales)),
  
  # identify tracks for which simulations cannot be made
  tar_target(error_index,ani_analysis_find_error_index(ssm)),
  
  # simulating tracks from state space model for track for which simulation is possible
  tar_target(sim,sim_post(ssm[-error_index,],reps=10)),
  
  # preparation for rerouting
  tar_target(sim_fit,ani_analysis_sim_post_to_sim_fit(
    state_space_model = ssm,
    sim_post = sim,
    error_index = error_index))
)