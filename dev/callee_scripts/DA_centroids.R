#### Add DA centroids and buffers ##############################################
# Dependent script: needs 'DA'

centroids <- 
  read_csv("dev/data/2016_92-151_XBB.csv") %>%
  filter(`CMAuid/RMRidu` == "462") %>%
  distinct(`DAuid/ADidu`, .keep_all = TRUE) %>%
  st_as_sf(coords = c("DArplong/ADlong", "DArplat/Adlat"), crs = 4326) %>%
  st_transform(32618) %>%
  mutate(buffer = st_buffer(geometry, 1000)) %>%
  select(ID = `DAuid/ADidu`, centroid = geometry, buffer) %>%
  st_transform(4326) %>%
  mutate(ID = as.character(ID)) %>%
  as_tibble()

DA <- 
  DA %>%
  left_join(centroids, by = "ID") %>%
  relocate(centroid, buffer, .before = geometry)

rm(centroids)
