#### Reverse geocode grid centroids ############################################
# Dependent script: needs 'borough', 'CT', 'DA' and 'grid' objects

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
