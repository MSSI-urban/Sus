library(sf) 
library(qs) 

CMA <-
  cancensus::get_census(
    dataset = "CA16", regions = list(CSD = c("24462")), level = "CMA",
    vectors = c("v_CA16_4840"),
    geo_format = "sf") %>% 
  select(GeoUID)

postal_code_raw <- 
  read_csv("dev/data/ZipCodeFiles/CanadianPostalCodes202103.csv")

postal_codes <- 
  postal_code_raw %>% 
  filter(PROVINCE_ABBR == "QC") %>% 
  select(-PROVINCE_ABBR, -TIME_ZONE) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>% 
  setNames(c("postal_code", "city", "geometry")) %>% 
  st_filter(CMA) %>% 
  st_transform(32618) %>% 
  as_tibble() %>% 
  st_as_sf()

# save output in qs

qsave(postal_codes, file = "data/postal_codes.qs")
