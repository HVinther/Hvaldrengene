library(targets)
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
tar_option_set(packages = "reticulate") # nolint

source("R/cmw.R")

# End this file with a list of target objects.
list(
  tar_target(name = sst,
             command = cmw_subset("GLOBAL_MULTIYEAR_PHY_001_030",
                        "cmems_mod_glo_phy_my_0.083deg_P1D-m",
                        variable = "thetao",
                        dates = c("2001-01-01T00:00:00","2001-12-31T00:00:00"),
                        depth = c(0.494024991989136,0.494024991989136)),
             format = "file",
             repository = "local"
  )
)