#### Green alley data setup #################################################### 

# This script relies on objects created in dev/census.R

# Get data ----------------------------------------------------------------

green_space <- 
  read_sf("dev/data/green_alleys/Espace_Vert.shp") %>% 
  select(ID = OBJECTID, name = Nom, type_1 = TYPO1, type_2 = TYPO2, geometry) %>% 
  st_transform(4326) %>% 
  st_set_agr("constant") %>% 
  st_cast("POLYGON")
