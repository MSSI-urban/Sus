library(osmdata)
library(tidyverse)
library(sf)
library(qs)
library(stplanr)
library(igraph)

# Variables ---------------------------------------------------------------

# Bounding box of CMA Montreal 
CMA_MTL_BB <- c(-74.32797, 45.21754, -73.12856, 45.96849)

# Highway key values for Cars
KV_Highway_Cars <- c("motorway", "trunk", "primary", "secondary", "tertiary",
                     "residential", "unclassified", "service", "motorway_link",
                     "trunk_link", "primary_link", "secondary_link", "tertiary_link")

# Highway key values for People
KV_Highway_People <- c("cycleway", "living_street", "pedestrian", "track", "road",
                       "footway", "path", "steps", "crossing")


# Load Road ---------------------------------------------------------------

# Retrieve OSM road features for Montreal
streets <-
  CMA_MTL_BB %>%
  opq(timeout = 200) %>%
  # Retrieve features with the key "highway" which means street
  add_osm_feature(key = "highway") %>%
  # Get the data as a sf data frame
  osmdata_sf()

# Cast "non-area" polygons into line and bind with existing lines
streets <-
  streets$osm_polygons %>%
  filter(is.na(area) | area=='no') %>%
  st_set_agr("constant") %>%
  st_cast("LINESTRING") %>%
  bind_rows(streets$osm_lines) %>%
  as_tibble() %>%
  st_as_sf() %>%
  st_set_agr("constant")

# Line streets with highway value in KV_Highway_Cars
car_streets <-
  streets %>% 
  filter(highway %in% KV_Highway_Cars) %>%
  select_if(~!all(is.na(.))) 

car_streets_with_names <-
  car_streets %>%
  filter(!is.na(name)) %>%
  select(osm_id, name, geometry)


# Slice streets by intersection -------------------------------------------

# Unnecessary for this file, and unverified if ok to use with bridges:
# car_streets_sliced <- stplanr::rnet_breakup_vertices(car_streets)
# ppl_streets_sliced <- stplanr::rnet_breakup_vertices(ppl_streets)

rm(CMA_MTL_BB, KV_Highway_Cars, KV_Highway_People, streets, car_streets)

# Group streets by name ---------------------------------------------------

# Names of the car streets in CMA MTL
# Takes a while to run (< 5 mins)
names_of_car_streets <-
  car_streets_with_names %>%
  st_transform(32618) %>%
  group_by(name) %>%
  summarise(count = n())

# Add ID
id <- as.numeric(rownames(names_of_car_streets))
names_of_car_streets <-
  names_of_car_streets %>%
  mutate(id = id, .before = name)

rm(id, car_streets_with_names)

# Check if in one connected cluster ---------------------------------------



# ***** FUNCTIONS for the names_of_car_streets *****

# returns the geometry at rownumber in LINESTRING if possible
# (those cannot be in linear format are returned as MULTILINESTRING)
merged_geom <- function(rownumber){
  geom <- names_of_car_streets$geometry[[rownumber]]
  tryCatch( return(st_line_merge(geom)),
            error = function(c) return(geom) )
}

# returns the number of strongly connected clusters (components) of the geometry at rownumber
com <- function(rownumber) {
  merged_geom(rownumber) %>%
  st_sfc() %>%
  st_cast("LINESTRING") %>%
  st_touches() %>%
  graph.adjlist() %>%
  components() %>%
  return()
}


# ***** VECTORS to be merged into names_of_car_streets *****

# T/F on whether the geometry at rownumber can be converted to LINESTRING format
is_in_a_line <- foreach(i = 1:nrow(names_of_car_streets), .combine=c) %do%{
  return(merged_geom(i) %>% st_geometry_type() == "LINESTRING")
}

# numbers of strongly connected clusters for each geometry
ngroup <- foreach(i = 1:nrow(names_of_car_streets), .combine=c) %do% com(i)$no


# ***** Merge the vectors *****

names_of_car_streets <-
  names_of_car_streets %>%
  mutate(linear = is_in_a_line) %>%
  mutate(numCluster = ngroup) %>%
  mutate(connected = (ngroup==1), .before = ngroup) %>%
  st_set_agr("constant")

rm(i, is_in_a_line, ngroup, com, merged_geom)

# Examples ----------------------------------------------------------------

names_of_car_streets %>%
  filter(name == 'Rue Sherbrooke')
