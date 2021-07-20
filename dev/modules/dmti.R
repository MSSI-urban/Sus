#### DMTI data setup ###########################################################

# Get data ----------------------------------------------------------------

# Original read of data, not kept for space reasons
# dmti <- 
#   read_sf("dev/data/dmti_2020/dmti_2016.shp") %>% 
#   st_transform(4326) %>% 
#   st_set_agr("constant") %>% 
#   select(UID, NAME, ADDRESS, CITY, MUNICIPAL, POST_CODE, SIC_1, SIC_DIV)
# qsave(dmti, "dev/data/dmti_2016.qs")

# Read DMTI shapefiles and add 'year' column

dmti_2006 <- st_read("dev/data/dmti_shp/dmti_2006.shp") %>% 
  mutate(year = 2006, .before = geometry)
dmti_2011 <- st_read("dev/data/dmti_shp/dmti_2011.shp") %>% 
  mutate(year = 2011, .before = geometry) 
dmti_2016 <- st_read("dev/data/dmti_shp/dmti_2016.shp") %>% 
  mutate(year = 2016, .before = geometry) 
dmti_2020 <- st_read("dev/data/dmti_shp/dmti_2020.shp") %>% 
  mutate(year = 2020, .before = geometry)

# transform df, select relevant variables

clean_dmti <- function(d) {
  d %>%
    st_transform(32618) %>% 
    st_set_agr("constant") %>% 
    select(poi_id = POI_ID, name = NAME, address = ADDRESS, city = CITY, 
           post_code = POST_CODE, sic_1 = SIC_1, sic_div = SIC_DIV, year)
}

dmti_2006 <- dmti_2006 %>% clean_dmti()
dmti_2011 <- dmti_2011 %>% clean_dmti()
dmti_2016 <- dmti_2016 %>% clean_dmti()
dmti_2020 <- dmti_2020 %>% clean_dmti()

# Row bind all DMTI data sets to create new df, remove old dfs

dmti <- rbind(dmti_2006, dmti_2011, dmti_2016, dmti_2020)
rm(dmti_2006, dmti_2011, dmti_2016, dmti_2020)


# Define filter vectors ---------------------------------------------------

filter_vectors <- c("healthy_food_prop", "walkable_prop")

healthy_food <- c("54110000", "54210000", "54310000", "54990000")
unhealthy_food <- c("54410000", "54510000", "54610000", "58120000", "58130000", 
                    "59120000", "59210000")
walkable <- c("52310000", "52510000", "53110000", "53310000", "53990000",
            "54110000", "54210000", "54310000", "54410000", "54510000",
            "54610000", "54990000", "56110000", "56210000", "56320000",
            "56410000", "56510000", "56610000", "56990000", "57140000",
            "57190000", "57310000", "57340000", "57350000", "57360000",
            "58120000", "58130000", "59120000", "59210000", "59410000",
            "59420000", "59430000", "59440000", "59450000", "59460000",
            "59470000", "59480000", "59490000", "59620000", "59920000",
            "59930000", "59940000", "59950000")
non_walkable <- c("52110000", "52610000", "52710000", "55110000", "55210000",
                 "55310000", "55410000", "55510000", "55610000", "55710000",
                 "55990000", "57120000", "57130000", "57220000", "59320000",
                 "59610000", "59830000", "59840000", "59890000", "59990000",
                 "59630000")
healthy_city <- c("01810000", "07420000", "40110000", "41110000", "54110000", 
                  "54310000", "59120000", "59950000", "72310000", "72410000", 
                  "72990000", "78320000", "79220000", "79910000", "79970000", 
                  "799990000", "80110000", "80210000", "80310000", "80410000", 
                  "80420000", "80430000", "80490000", "80510000", "80520000", 
                  "80590000", "80620000", "80630000", "80690000", "80710000", 
                  "80720000", "80820000", "80920000", "80930000", "80990000", 
                  "83220000", "83510000", "86410000", "92240000")
unhealthy_city <- c("29110000", "29990000", "34330000", "55110000", "55210000",
                    "55310000", "55410000", "55510000", "55610000", "55710000", 
                    "55990000", "58130000", "59210000", "59830000", "59840000", 
                    "59890000", "59930000", "75140000", "75210000", "75320000", 
                    "75330000", "75340000", "75360000", "75370000", "75380000", 
                    "75390000", "76990000")
commercial_gentrification <- c("54110000", "54210000", "54310000", "54410000", 
                               "54510000", "54610000", "54990000", "58120000", 
                               "58130000", "59120000", "59210000")


# Filter data -------------------------------------------------------------

process_DMTI <- function(x) {
  points <- 
    dmti %>% 
    st_join(x) %>% 
    st_drop_geometry() %>% 
    group_by(ID) %>% 
    summarize(
      healthy_food_prop = sum(SIC_1 %in% healthy_food) / 
        (sum(SIC_1 %in% healthy_food) + sum(SIC_1 %in% unhealthy_food)),
      walkable_prop = sum(SIC_1 %in% walkable) / 
        (sum(SIC_1 %in% walkable) + sum(SIC_1 %in% non_walkable))) 
  
  x %>% 
    left_join(points, by = "ID") %>% 
    relocate(all_of(filter_vectors), .before = geometry) %>% 
    mutate(across(filter_vectors, ntile, n = 3, .names = "{.col}_q3"), 
           .before = geometry) %>% 
    st_set_agr("constant")
}

points <- map(list(borough, CT, DA, grid), process_DMTI)


# Add to existing geographies ---------------------------------------------

borough <- points[[1]]
CT <- points[[2]]
DA <- points[[3]]
grid <- points[[4]]

rm(dmti, points, filter_vectors, healthy_food, non_walkable, unhealthy_food,
   walkable, healthy_city, unhealthy_city, commercial_gentrification,
   process_DMTI)


# Add variable explanations -----------------------------------------------

var_exp <- 
  var_exp %>% 
  add_row(
    var_code = "healthy_food_prop",
    var_name = "Healthy food destinations (%)",
    explanation = "the percentage of food destinations which sell healthy food") %>% 
  add_row(
    var_code = "walkable_prop",
    var_name = "Walkable retail destinations (%)",
    explanation = "the percentage of retail destinations which sell small things")

# To save output, run dev/build_geometries.R, which calls this script
