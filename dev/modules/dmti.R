#### DMTI data setup ###########################################################

library(tidyverse)
library(sf)
library(stats)

# Get and clean data ------------------------------------------------

load_clean_dmti <- function(path) {
  
  d <-
    # Load df using path
    st_read(path) %>%
    mutate(year = str_extract(path, "[[:digit:]]+"), .before = geometry) %>%
    # Transform df, select relevant variables
    st_transform(32618) %>%
    st_set_agr("constant") %>%
    select(poi_id = POI_ID, name = NAME, address = ADDRESS, city = CITY,
           post_code = POST_CODE, sic = SIC_1, sic_div = SIC_DIV, year) %>%
    mutate(sic = as.integer(unlist(sic)),
           year = as.integer(unlist(year)))
  
  return (d)
  
}

dmti_2006 <- "dev/data/dmti_shp/dmti_2006.shp" %>% load_clean_dmti()
dmti_2011 <- "dev/data/dmti_shp/dmti_2011.shp" %>% load_clean_dmti()
dmti_2016 <- "dev/data/dmti_shp/dmti_2016.shp" %>% load_clean_dmti()

# Bind all DMTI data sets to create new df, remove old dfs
dmti <- bind_rows(dmti_2006, dmti_2011, dmti_2016)
rm(dmti_2006, dmti_2011, dmti_2016, load_clean_dmti)


# Create 'type' variable ---------------------------------------

# Define SIC code strings:

# food
healthy_food_sic <- c("54110000", "54210000", "54310000", "54990000")
unhealthy_food_sic <- c("54410000", "54510000", "54610000", "58120000", 
                        "58130000", "59210000")

# healthy
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
unhealthy_street <-c("29110000", "29990000", "34330000", "54410000", "54510000",
                     "54610000", "55110000", "55210000", "55310000", "55410000",
                     "55510000", "55610000", "55710000", "55990000", "58120000",
                     "58130000", "59210000", "59830000", "59840000", "59890000",
                     "59930000", "75140000", "75210000", "75320000", "75330000",
                     "75340000", "75360000", "75370000", "75380000", "75390000",
                     "76990000", "40110000", "59120000", "80590000", "80620000",
                     "80630000", "80690000", "80930000", "92240000") 
healthy_street <- c("01810000", "41110000", "54110000", "54210000", "54310000", 
                    "54990000", "59950000", "79910000", "79970000", "80110000", 
                    "80210000", "80310000", "80410000", "80420000", "80430000",
                    "80490000", "80510000", "80520000", "80710000", "80720000",
                    "80820000", "80920000", "80990000", "83220000", "83510000",
                    "86410000")
healthcare <- c("59120000", "59950000", "80110000", "80210000", "80310000", 
                "80410000", "80420000", "80430000", "80490000", "80510000", 
                "80520000", "80590000", "80620000", "80630000", "80690000", 
                "80710000", "80720000", "80820000", "80920000", "80930000", 
                "80990000") 


# Conditionally add values to 'type' column
dmti_test <- 
  dmti %>% 
  mutate(foodtype = case_when(sic %in% healthy_food_sic ~ "healthy",
                              (sic %in% unhealthy_food_sic) | 
                                (grepl("jean coutu", name, ignore.case = TRUE) |  
                                   grepl("uniprix", name, ignore.case = TRUE) |  
                                   grepl("pharmaprix", name, ignore.case = TRUE)) ~ "unhealthy"),
         citytype = case_when(sic %in% healthy_city ~ "healthy",
                              sic %in% unhealthy_city ~ "unhealthy"),
         streettype = case_when(sic %in% healthy_street ~ "healthy",
                                sic %in% unhealthy_street ~ "unhealthy"),
         subtype = case_when(sic %in% healthcare ~ "healthcare",
                             (sic >= 20110000) & (sic <= 39990000) ~ "manufacturing"),
         .before = geometry) %>% 
  # # Remove rows that are not relevant to any modules to improve processing time
  mutate(na_count = rowSums(is.na(.))) %>% 
  filter(na_count < 2, select = -na_count)

# Process data ---------------------------------------------------------------

DA_test <- 
  DA %>% 
  st_set_geometry("buffer") %>% 
  st_transform(32618) %>% 
  select(ID) %>% 
  st_join(., dmti_test) %>% #change back to "dmti" 
  st_drop_geometry()


# making year-specific columns for foodtype
food_vars <- 
  DA_test %>%
  count(ID, year, foodtype) %>% 
  drop_na(year, foodtype) %>%
  group_by(ID, year) %>% 
  summarize(foodtype = c(foodtype, "total"), n = c(n, sum(n, na.rm = TRUE)),
            .groups = "drop") %>% 
  tidyr::pivot_wider(id_cols = "ID",
                     names_from = c("foodtype", "year"),
                     names_prefix = "food_",
                     names_sep = "_",
                     values_from = n) %>% 
  left_join(DA %>% select(ID, population) %>% st_drop_geometry(), by = "ID") %>%  # population variable shouldn't include ".x"
  mutate(across(starts_with("food"), 
                ~{.x / (population/1000)}, # population variable shouldn't include ".x"
                .names = "{.col}_prop_pop")) %>% 
  mutate(food_healthy_prop_2006 = food_healthy_2006 / food_total_2006,
         food_unhealthy_prop_2006 = food_unhealthy_2006 / food_total_2006,
         food_healthy_prop_2011 = food_healthy_2011 / food_total_2011,
         food_unhealthy_prop_2011 = food_unhealthy_2011 / food_total_2011,
         food_healthy_prop_2016 = food_healthy_2016 / food_total_2016,
         food_unhealthy_prop_2016 = food_unhealthy_2016 / food_total_2016)

city_vars <-
  DA_test %>%
  # making year-specific columns for citytype
  count(ID, year, citytype) %>% 
  drop_na(year, citytype) %>%
  group_by(ID, year) %>%
  summarize(citytype = c(citytype, "total"), n = c(n, sum(n, na.rm = TRUE)),
            .groups = "drop") %>%
  tidyr::pivot_wider(id_cols = "ID",
                     names_from = c("citytype", "year"),
                     names_prefix = "city_",
                     names_sep = "_",
                     values_from = n) %>%
  mutate(city_healthy_prop_2006 = city_healthy_2006 / city_total_2006,
         city_unhealthy_prop_2006 = city_unhealthy_2006 / city_total_2006,
         city_healthy_prop_2011 = city_healthy_2011 / city_total_2011,
         city_unhealthy_prop_2011 = city_unhealthy_2011 / city_total_2011,
         city_healthy_prop_2016 = city_healthy_2016 / city_total_2016,
         city_unhealthy_prop_2016 = city_unhealthy_2016 / city_total_2016)

# making year-specific columns for streettype
street_vars <- 
  DA_test %>%
  count(ID, year, streettype) %>%
  drop_na(year, streettype) %>%
  group_by(ID, year) %>%
  summarize(streettype = c(streettype, "total"), n = c(n, sum(n, na.rm = TRUE)),
            .groups = "drop") %>%
  tidyr::pivot_wider(id_cols = "ID",
                     names_from = c("streettype", "year"),
                     names_prefix = "street_",
                     names_sep = "_",
                     values_from = n) %>%
  left_join(DA %>% select(ID, population) %>% st_drop_geometry(), by = "ID") %>%
  mutate(across(starts_with("street_healthy"),
                ~{.x / (population / 1000)},
                .names = "{.col}_prop_pop")) %>%
  mutate(street_healthy_prop_2006 = street_healthy_2006 / street_total_2006,
         street_unhealthy_prop_2006 = street_unhealthy_2006 / street_total_2006,
         street_healthy_prop_2011 = street_healthy_2011 / street_total_2011,
         street_unhealthy_prop_2011 = street_unhealthy_2011 / street_total_2011,
         street_healthy_prop_2016 = street_healthy_2016 / street_total_2016,
         street_unhealthy_prop_2016 = street_unhealthy_2016 / street_total_2016)

# making year-specific columns for subtype
sub_vars <- 
  DA_test %>%
  count(ID, year, subtype) %>%
  drop_na(year, subtype) %>%
  group_by(ID, year) %>%
  tidyr::pivot_wider(id_cols = "ID",
                     names_from = c("subtype", "year"),
                     names_prefix = "city_",
                     names_sep = "_",
                     values_from = n) %>%
  left_join(DA %>% select(ID, population) %>% st_drop_geometry(), by = "ID") %>%
  mutate(across(starts_with(c("city_healthcare", "city_manufacturing")),
                ~{.x / (population/1000)},
                .names = "{.col}_prop_pop"))

DA <- 
  DA %>%
  left_join(food_vars, by = c("ID", "population")) %>%
  left_join(city_vars, by = c("ID")) %>%
  left_join(street_vars, by = c("ID", "population")) %>%
  left_join(sub_vars, by = c("ID", "population")) %>%
  # make q3 columns for all new metrics
  mutate(across(starts_with(c("food", "city", "street")), ntile, n = 3, 
                .names = "{.col}_q3")) %>% 
  relocate(starts_with(c("food","city","street")), .before = geometry) %>%
  st_set_agr("constant")

rm(food_vars, street_vars, city_vars, sub_vars)


# Interpolate DA values to CT and borough ------------------------------------

interpolate_dmti <- function(x) {
  
  avg_list_food <- 
    str_subset(names(DA %>% select(starts_with(c("food","city","street")))), pattern = "avg|median|prop", negate = FALSE) %>% 
    str_subset("q3", negate = TRUE)
  
  agg_list_food <- 
    names(DA %>% select(starts_with(c("food","city","street")))) %>% 
    setdiff(avg_list_food) %>% 
    setdiff("geometry") %>% 
    str_subset("q3", negate = TRUE)
  
  x <- 
    DA %>% 
    select(c(avg_list_food, agg_list_food)) %>% 
    st_transform(32618) %>% 
    st_intersection(st_transform(x, 32618), .) %>% 
    mutate(area = st_area(geometry)) %>% 
    mutate(area_prop = st_area(geometry) / area) %>% 
    mutate(across(agg_list_food, ~{.x * units::drop_units(area_prop)})) %>% 
    st_drop_geometry() %>% 
    group_by(ID) %>% 
    summarize(across(all_of(avg_list_food), weighted.mean, w = population, na.rm = TRUE),
              across(all_of(agg_list_food), sum, na.rm = TRUE)) %>% 
    mutate(across(starts_with(c("food","city","street")), ntile, n = 3, .names = "{.col}_q3")) %>% 
    left_join(x, ., by = "ID") %>% 
    st_as_sf() %>% 
    relocate(starts_with(c("food","city","street")), .before = geometry) %>% 
    st_set_agr("constant") 
  
}

dmti_results <- map(list(borough, CT), interpolate_dmti)

borough <- dmti_results[[1]]
CT <- dmti_results[[2]]

rm(dmti_results, interpolate_dmti, healthy_food_sic, unhealthy_food_sic)

# Add variable explanations -----------------------------------------------

var_exp <- 
  var_exp %>% 
  add_row(
    var_code = "food_total",
    var_name = "Food retail",
    explanation = "the number of food retail amenities present") %>% 
  add_row(
    var_code = "food_healthy",
    var_name = "Healthy food retail",
    explanation = "the number of food retail amenities selling mostly healthy options") %>% 
  add_row(
    var_code = "food_unhealthy",
    var_name = "Unhealthy food retail",
    explanation = "the number of food retail amenities selling mostly unhealthy options") %>% 
  add_row(
    var_code = "food_healthy_prop",
    var_name = "Healthy food retail (%)",
    explanation = "the percentage of food retail amenities selling mostly healthy options") %>% 
  add_row(
    var_code = "food_unhealthy_prop",
    var_name = "Unhealthy food retail (%)",
    explanation = "the percentage of food retail amenities selling mostly unhealthy options") %>% 
  add_row(
    var_code = "food_total_prop_pop",
    var_name = "Food retail per 1000 residents",
    explanation = "the number of food retail amenities per 1000 residents") %>% 
  add_row(
    var_code = "food_healthy_prop_pop",
    var_name = "Healthy food retail per 1000 residents",
    explanation = "the number of food retail amenities selling mostly healthy options per 1000 residents") %>% 
  add_row(
    var_code = "food_unhealthy_prop_pop",
    var_name = "Unhealthy food retail per 1000 residents",
    explanation = "the number of food retail amenities selling mostly unhealthy options per 1000 residents") %>% 
  
  add_row(
    var_code = "city_healthy",
    var_name = "Healthy city features",
    explanation = "the number of health promoting features at a city scale") %>% 
  add_row(
    var_code = "city_unhealthy",
    var_name = "Unhealthy city features",
    explanation = "the number of detrimental health features at a city scale") %>% 
  add_row(
    var_code = "city_total",
    var_name = "Healthy and unhealthy city features",
    explanation = "the percentage of features associated with positive and negative health outcomes at a city scale") %>% 
  add_row(
    var_code = "city_healthy_prop",
    var_name = "Healthy city features (%)",
    explanation = "the percentage of health promoting features at a city scale") %>% 
  add_row(
    var_code = "city_unhealthy_prop",
    var_name = "Unhealthy city features (%)",
    explanation = "the percentage of detrimental health features at a city scale") %>% 
  
  add_row(
    var_code = "street_healthy",
    var_name = "Healthy street features",
    explanation = "the number of health promoting features at a street scale") %>% 
  add_row(
    var_code = "street_unhealthy",
    var_name = "Unhealthy street features",
    explanation = "the number of detrimental health features at a street scale") %>% 
  add_row(
    var_code = "street_total",
    var_name = "Healthy and unhealthy street features",
    explanation = "the percentage of features associated with positive and negative health outcomes at a street scale") %>%
  add_row(
    var_code = "street_healthy_prop",
    var_name = "Healthy street features (%)",
    explanation = "the percentage of healthy promoting features at a street scale") %>% 
  add_row(
    var_code = "street_unhealthy_prop",
    var_name = "Unhealthy street features (%)",
    explanation = "the percentage of detrimental health features at a street scale") %>% 
  add_row(
    var_code = "street_healthy_prop_pop",
    var_name = "Healthy street features per 1000 residents",
    explanation = "health promoting features at a street scale, per 1000 residents") %>%
  
  add_row(
    var_code = "city_healthcare",
    var_name = "Healthcare features",
    explanation = "the number of healthcare facilities") %>% 
  add_row(
    var_code = "city_manufacturing",
    var_name = "Manufacturing infrastructure",
    explanation = "manufacturing-related infrastructure") %>%
  add_row(
    var_code = "city_healthcare_prop_pop",
    var_name = "Healthcare per 1000 residents",
    explanation = "the number of healthcare facilities per 1000 residents") %>% 
  add_row(
    var_code = "city_manufacturing_prop_pop",
    var_name = "Unhealthy infrastructure per 1000 residents",
    explanation = "manufacturing-related infrastructure per 1000 residents")

# To save output, run dev/build_geometries.R, which calls this script
