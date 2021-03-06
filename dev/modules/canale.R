#### CanALE data setup #########################################################

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

canale <- 
  read_sf("dev/data/Mtl_DA_CANALE/Mtl_DA_CANALE.shp") %>% 
  st_transform(4326) %>% 
  st_cast("MULTIPOLYGON") %>% 
  st_set_agr("constant") %>% 
  st_drop_geometry() %>% 
  select(DAUID, CTUID, canale_ind = ale_index)


# Add to existing geographies ---------------------------------------------

DA <- 
  DA %>% 
  left_join(canale, by = c("ID" = "DAUID", "CTUID")) %>% 
  relocate(canale_ind, .before = geometry) %>% 
  mutate(canale_ind_q3 = ntile(canale_ind, 3), .after = canale_ind) %>% 
  st_set_agr("constant")

CT <- 
  DA %>% 
  st_drop_geometry() %>% 
  select(CTUID, households, canale_ind) %>% 
  group_by(CTUID) %>% 
  summarize(canale_ind = weighted.mean(canale_ind, 
                                       households, na.rm = TRUE)) %>% 
  left_join(CT, ., by = c("ID" = "CTUID")) %>% 
  relocate(canale_ind, .before = geometry) %>% 
  mutate(canale_ind_q3 = ntile(canale_ind, 3), .after = canale_ind) %>% 
  st_set_agr("constant")

borough <- 
  DA %>% 
  st_drop_geometry() %>% 
  select(CSDUID, households, canale_ind) %>% 
  group_by(CSDUID) %>% 
  summarize(canale_ind = weighted.mean(canale_ind, 
                                       households, na.rm = TRUE)) %>% 
  left_join(borough, ., by = c("ID" = "CSDUID")) %>% 
  relocate(canale_ind, .before = geometry) %>% 
  mutate(canale_ind_q3 = ntile(canale_ind, 3), .after = canale_ind) %>% 
  st_set_agr("constant")

DA_data <-
  DA %>% 
  select(ID, canale_ind) %>% 
  st_transform(32618) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_set_agr("constant")

grid_data <-
  grid %>% 
  select(ID, households) %>% 
  st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_intersection(DA_data) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(canale_ind = canale_ind * units::drop_units(area_prop)) %>% 
  select(-ID.1, -area, -area_prop) %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarize(canale_ind = weighted.mean(canale_ind, 
                                       households, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), 0)))

grid <- 
  grid %>% 
  left_join(grid_data, by = "ID") %>% 
  relocate(geometry, .after = last_col())

rm(canale, DA_data, grid_data)


# Add variable explanations -----------------------------------------------

var_exp <- 
  var_exp %>% 
  add_row(
    var_code = "canale_ind",
    var_name = "CanALE index",
    explanation = "the potential for active living")
  
# To save output, run dev/build_geometries.R, which calls this script