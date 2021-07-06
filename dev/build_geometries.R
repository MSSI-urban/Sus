#### Build geometries ##########################################################
# Caller Script 


# Import DA, CT and borough geometries ------------------------------------
# Independent script 

source("dev/callee_scripts/borough_geometries.R")

# Import grid geometries --------------------------------------------------
# Independent script 

source("dev/callee_scripts/grid_geometries.R")

# Geocode grid centroids --------------------------------------------------
# Dependent script: needs 'borough' object

source("dev/callee_scripts/grid_geocode.R")

# Add CSDUID, population and households to grid ---------------------------

DA_data <- 
  DA %>% 
  st_transform(32618) %>% 
  select(ID, population, households) %>% 
  mutate(area = st_area(geometry), .before = geometry) %>% 
  st_set_agr("constant")

grid_census <-
  grid %>% 
  select(ID) %>% 
  st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_intersection(DA_data) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(across(population:households, 
                ~{.x * units::drop_units(area_prop)})) %>% 
  select(ID, population, households, geometry) %>% 
  st_drop_geometry() %>% 
  arrange(ID) %>% 
  group_by(ID) %>% 
  summarize(across(population:households, sum, na.rm = TRUE))

grid <- 
  grid %>% 
  left_join(grid_census, by = "ID") %>% 
  relocate(geometry, .after = last_col()) %>% 
  st_set_agr("constant")

borough_index <- 
  grid %>% 
  st_transform(32618) %>% 
  st_centroid() %>% 
  st_nearest_feature(st_transform(borough, 32618))

grid <- 
  grid %>% 
  mutate(CSDUID = map_chr(borough_index, ~borough$ID[.x]), .after = name) %>% 
  st_set_agr("constant")

grid <- 
  grid %>% 
  left_join(select(st_drop_geometry(borough), CSDUID = ID, name_2 = name), 
            by = "CSDUID") %>% 
  relocate(name_2, .after = name)

rm(borough_index, DA_data, grid_census)


# Add topic variables (modules) -----------------------------------------------------

var_exp <- tibble(var_code = character(), var_name = character(),
        explanation = character())
source("dev/modules/census.R")
source("dev/modules/canale.R")
source("dev/modules/climate_risk.R")
# source("dev/modules/ped.R")


# Save data files ---------------------------------------------------------

qsavem(borough, CT, DA, file = "data/census.qsm")
qsave(grid, file = "data/grid.qs")
qsave(var_exp, file = "data/var_exp.qs")


# Produce left and right maps ---------------------------------------------

library(patchwork)
source("dev/callee_scripts/colours.R")

# Dependent script: needs 'borough' object
source("dev/callee_scripts/produce_maps.R")
