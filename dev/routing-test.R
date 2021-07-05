install.packages("osmextract")
library(osmextract)


streets <-

  opq(timeout = 200) %>%
  # Retrieve features with the key "highway" which means street
  add_osm_feature(key = "highway") %>%
  # Get the data as a sf data frame
  osmdata_sf()



# allocate RAM memory to Java
options(java.parameters = c("-XX:+UseConcMarkSweepGC", "-Xmx28192m"))
options(java.parameters = '-Xmx2G')

# 1) build transport network, pointing to the path where OSM and GTFS data are
library(r5r)
path <- system.file("extdata/poa", package = "r5r")
r5r_core <- setup_r5(data_path = path, verbose = FALSE)


# 2) load origin/destination points and set arguments
points <- read.csv(system.file("extdata/poa/poa_hexgrid.csv", package = "r5r"))
mode <- c("WALK", "BUS")
max_walk_dist <- 3000   # meters
max_trip_duration <- 60 # minutes
departure_datetime <- as.POSIXct("13-05-2019 14:00:00",
                                 format = "%d-%m-%Y %H:%M:%S")

# 3.1) calculate a travel time matrix
ttm <- travel_time_matrix(r5r_core = r5r_core,
                          origins = points,
                          destinations = points,
                          mode = mode,
                          departure_datetime = departure_datetime,
                          max_walk_dist = max_walk_dist,
                          max_trip_duration = max_trip_duration)

# 3.2) or get detailed info on multiple alternative routes
det <- detailed_itineraries(r5r_core = r5r_core,
                            origins = points[370, ],
                            destinations = points[200, ],
                            mode = mode,
                            departure_datetime = departure_datetime,
                            max_walk_dist = max_walk_dist,
                            max_trip_duration = max_trip_duration,
                            shortest_path = FALSE)
