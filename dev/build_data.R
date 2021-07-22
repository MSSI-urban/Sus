#### Build geometries ##########################################################
# Caller Script 


# Create raw borough/CT/DA/grid tables ------------------------------------

# Import DA, CT and borough geometries (independent script)
source("dev/callee_scripts/borough_geometries.R")

# Add centroids and buffers to DA (dependent script: needs 'DA')
source("dev/callee_scripts/DA_centroids.R")

# Import grid geometries (independent script)
source("dev/callee_scripts/grid_geometries.R")

# Geocode grid centroids (dependent script: needs 'borough')
source("dev/callee_scripts/grid_geocode.R")

# Add metadata to grid (dependent: needs 'borough', 'CT', 'DA' and 'grid')
source("dev/callee_scripts/grid_process.R")


# Add topic variables (modules) -------------------------------------------

var_exp <- tibble(var_code = character(), var_name = character(),
        explanation = character())
source("dev/modules/census.R")
source("dev/modules/canale.R")
source("dev/modules/climate_risk.R")
# source("dev/modules/dmti.R")
source("dev/modules/alley.R")
source("dev/modules/crash.R")
# source("dev/modules/ped.R")


# Save data files ---------------------------------------------------------

qsave(var_exp, file = "data/var_exp.qs")
qsavem(borough, CT, DA, file = "data/census.qsm")
qsave(grid, file = "data/grid.qs")
qsave(green_space, file = "data/green_space.qs")
# qsavem(green_space, ..., file = "data/alley.qsm")
# qsave(crash, file = "data/crash.qs")


# Produce left and right maps ---------------------------------------------

library(patchwork)
source("dev/callee_scripts/colours.R")

# Dependent script: needs 'borough' object
source("dev/callee_scripts/produce_maps.R")
