#### Import grid geometries ####################################################
# Independent script 

grid <-
  read_sf("dev/data/climate_shp/VulnerabilitÇ_secheresses_2016.shp") %>% 
  st_zm() %>% 
  st_make_valid() %>% 
  st_transform(32618) %>% 
  transmute(ID = seq_along(geometry)) %>% 
  filter(units::drop_units(st_area(geometry)) > 10) %>% 
  distinct(geometry, .keep_all = TRUE) %>% 
  st_transform(4326) %>% 
  st_set_agr("constant")
