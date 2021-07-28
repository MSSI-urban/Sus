library(osmdata)
library(tidyverse)
library(sf)
library(qs)
library(stplanr)
library(igraph)


# Bounding Box, Highway Key Values -----------------------------------------

# Bounding box of CMA Montreal
CMA_MTL_BB <- c(-74.32797, 45.21754, -73.12856, 45.96849)

# Highway key values for Cars
KV_Highway_Cars <- c("motorway", "trunk", "primary", "secondary", "tertiary",
                     "residential", "unclassified", "service", "motorway_link",
                     "trunk_link", "primary_link", "secondary_link", "tertiary_link")

# # Highway key values for People
# KV_Highway_People <- c("cycleway", "living_street", "pedestrian", "track", "road",
#                        "footway", "path", "steps", "crossing")


# Load and Save OSM Road ---------------------------------------------------

# # Retrieve ALL OSM road features for Montreal
# streets <-
#   CMA_MTL_BB %>%
#   opq(timeout = 200) %>%
#   # Retrieve features with the key "highway" which means street
#   add_osm_feature(key = "highway") %>%
#   # Get the data as a sf data frame
#   osmdata_sf()
# 
# # Cast "non-area" POLYGONS into LINESTRING and bind with existing LINESTRINGS
# streets <-
#   streets$osm_polygons %>%
#   filter(is.na(area) | area=='no') %>%
#   st_set_agr("constant") %>%
#   st_cast("LINESTRING") %>%
#   bind_rows(streets$osm_lines) %>%
#   as_tibble() %>%
#   st_as_sf() %>%
#   st_set_agr("constant")
# 
# # Save file
# qsave(streets, "dev/streets.qs")

# Process Car Streets ------------------------------------------------------

# Read from cache
streets <- qread("dev/streets.qs")

# Line streets with highway value in KV_Highway_Cars and name value not NA
car_streets <-
  streets %>%
  filter(highway %in% KV_Highway_Cars) %>%
  select_if(~!all(is.na(.))) %>%
  filter(!is.na(name)) %>%
  select(osm_id, name, highway, geometry) %>%
  mutate(name = str_to_title(name))

rm(CMA_MTL_BB, streets)

# Clip by borough ---------------------------------------------------------
# Need borough level data!

# Function that gives the mode of the group
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Join car_streets with borough, in order to group(union) streets by name and borough
# For highway column, take the most frequent type
# Simplify the geometry with 5 degree tolerance in order to reduce the geometry size 
# This code takes a couple of minutes to run
clipped_car_streets <-
  car_streets %>%
  st_transform(32618) %>%
  st_intersection(st_transform(borough, 32618) %>% select(name)) %>%
  group_by(name, borough = name.1) %>%
  summarise(highway = Mode(highway), .groups = 'drop') %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 5) %>%
  st_transform(4326) %>%
  st_as_sf() %>%
  st_set_agr("constant")

# Add ID, as well as more general road level
id <- as.numeric(rownames(clipped_car_streets))
street_category <- data.frame(
  highway = KV_Highway_Cars,
  significance = c(1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 4)
)

clipped_car_streets <-
  clipped_car_streets %>%
  mutate(id = id, .before = name) %>%
  left_join(street_category) %>%
  relocate(significance, .after=highway)

rm(car_streets, id, street_category, KV_Highway_Cars, Mode)

# Check if in one connected cluster (clipped_car_streets) ----------------
# relies on borough data after running 'callee-scripts/borough_geometries.R'

# FUNCTIONS

# merged_geom <- function(geom) {
#   tryCatch (
#     return (st_line_merge(geom)),
#     error = function(c) return(geom)
#   )
# }
# 
# is_in_a_line <- function(geom){
#   return(st_geometry_type(merged_geom(geom)) == "LINESTRING")
# }
# 
# com <- function(geom) {
#   c <- merged_geom(geom) %>%
#     st_sfc() %>%
#     st_cast("LINESTRING") %>%
#     st_touches() %>%
#     graph.adjlist() %>%
#     components()
#   return(c$no)
# }

# Applying Functions

# c <- sapply(clipped_car_streets$geometry, function(i) com(i))

# clipped_car_streets <-
#   clipped_car_streets %>%
#   mutate(linear = is_in_a_line(geometry), .before = geometry) %>%
#   mutate(numCluster = c, .before = geometry) %>%
#   mutate(connected = (numCluster==1), .before = numCluster) %>%
#   mutate(regionID = ID, .after=ID) %>%
#   mutate(regionName = name.1, .after=name.1) %>%
#   mutate(regionType = name_2, .after=name_2) %>%
#   select(-count, -ID, -name.1, -name_2) %>%
#   arrange(id)

# mutate(connected = com(geometry)) somehow does not work

# rm(c, com, is_in_a_line, merged_geom)
