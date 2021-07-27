#### DMTI data setup ###########################################################

library(tidyverse)
library(sf)
library(stats)

# Get and clean data ------------------------------------------------

load_clean_dmti <- function(path) {
  d <-
    # load df
    st_read(path) %>%
    mutate(year = str_extract(path, "[[:digit:]]+"), .before = geometry) %>%
    # transform df, select relevant variables
    st_transform(32618) %>%
    st_set_agr("constant") %>%
    select(poi_id = POI_ID, name = NAME, address = ADDRESS, city = CITY,
           post_code = POST_CODE, sic_1 = SIC_1, sic_div = SIC_DIV, year) %>%
    mutate(sic_1 = as.integer(unlist(sic_1)),
           year = as.integer(unlist(year)))
  return (d)
}

dmti_2006 <- "C:/MSSI_GIS/DMTI/Github/Sus/dev/data/dmti_shp/dmti_2006.shp" %>% load_clean_dmti()
dmti_2011 <- "C:/MSSI_GIS/DMTI/Github/Sus/dev/data/dmti_shp/dmti_2011.shp" %>% load_clean_dmti()
dmti_2016 <- "C:/MSSI_GIS/DMTI/Github/Sus/dev/data/dmti_shp/dmti_2016.shp" %>% load_clean_dmti()

# row bind all DMTI data sets to create new df, remove old dfs

dmti <- rbind(dmti_2006, dmti_2011, dmti_2016)
rm(dmti_2006, dmti_2011, dmti_2016)




# Create 'type' variable ---------------------------------------

# define SIC code strings

healthy_food_sic <- c("54110000", "54210000", "54310000", "54990000")
unhealthy_food_sic <- c("54410000", "54510000", "54610000", "58120000", 
                        "58130000", "59210000")

# conditionally add values to 'type' column

dmti_test <- 
  dmti %>% 
  mutate(type = case_when(sic_1 %in% healthy_food_sic ~ "foodhealthy",
                          (sic_1 %in% unhealthy_food_sic) | 
                            (grepl("jean coutu", name, ignore.case = TRUE) |  
                               grepl("uniprix", name, ignore.case = TRUE) |  
                               grepl("pharmaprix", name, ignore.case = TRUE)) ~ "foodunhealthy"),
         .before = geometry) %>% 
  # remove rows that are not relevant to any modules to improve processing time
  drop_na(type)




# Process data ---------------------------------------------------------------

#process_dmti <- function(x) {
DA_test <- 
  DA %>% 
  st_set_geometry("buffer") %>% 
  st_transform(32618) %>% 
  select(ID) %>% 
  st_join(., dmti_test) %>%
  st_drop_geometry() %>% 
  count(ID, year, type) %>%
  filter(!is.na(type)) %>%
  tidyr::pivot_wider(id_cols = "ID",
                     names_from = c("type", "year"),
                     names_sep = "_",
                     values_from = n) %>% 
  left_join(DA, ., by = "ID") %>%
  
  relocate(starts_with("food"), .before = geometry) %>%
  mutate(across(starts_with("food"), 
                .fns = list(
                  prop = ~{.x /  }
                )))
  st_as_sf() %>%
  st_set_agr("constant")
#}

#
left_join(join_results, by = "ID") |> 
  relocate(starts_with("crash"), .before = geometry) |> 
  mutate(across(starts_with("crash"), 
                .fns = list(
                  prop_area = ~{.x / units::drop_units(st_area(geometry))},
                  prop_pop = ~{.x / population}),
                .names = "{.col}_{.fn}"), .before = geometry) |> 
  mutate(across(starts_with("crash"), ntile, n = 3, .names = "{.col}_q3"), 
         .before = geometry) %>% 
  st_set_agr("constant")
#


DA_data <-
  DA_test %>% 
  select(ID, CTUID, CSDUID, population, starts_with("food")) %>% 
  mutate(
    foodhealthy_prop_2006 = foodhealthy_2006 / (foodhealthy_2006 + foodunhealthy_2006),
    foodhealthy_prop_2011 = foodhealthy_2011 / (foodhealthy_2011 + foodunhealthy_2011),
    foodhealthy_prop_2016 = foodhealthy_2016 / (foodhealthy_2016 + foodunhealthy_2016),
    food_prop_popthous_2006 = (foodhealthy_2006 + foodunhealthy_2006) / (population / 1000),
    food_prop_popthous_2011 = (foodhealthy_2011 + foodunhealthy_2011) / (population / 1000),
    food_prop_popthous_2016 = (foodhealthy_2016 + foodunhealthy_2016) / (population / 1000),
    foodhealthy_prop_popthous_2006 = foodhealthy_2006 / (population / 1000),
    foodhealthy_prop_popthous_2011 = foodhealthy_2011 / (population / 1000),
    foodhealthy_prop_popthous_2016 = foodhealthy_2016 / (population / 1000),
    .before = geometry) %>% 
  st_set_agr("constant")

#CT_from_DAbuff <-
DA_data %>%
  st_drop_geometry() %>%
  select(CTUID, population, starts_with("food")) %>%
  group_by(CTUID) %>%
  summarize(across(starts_with("food"),
                   .fns = list(
                     weighted_mean = ~weighted.mean(.x, population, na.rm=TRUE)),
                   .names = "{.col}_{.fn}")) %>% 
  left_join(CT, ., by = c("ID" = "CTUID")) %>%
  relocate(contains(c("healthyfood", "food_prop")), .before = geometry) %>%
  mutate(across(contains(c("healthyfood","food_prop")), ntile, n = 3, .names = "{.col}_q3"),
         .before = geometry) %>%
  st_set_agr("constant")

borough_from_DAbuff <- 
  # identical to DT_from_DAbuff except replace "CTUID" with "CSDUID"
  
  
  # METRIC CREATION ---------------------------------------------------------

process_dmti <- function(x) {
  x %>% 
    st_transform(32618) %>%
    st_join(., dmti) %>% 
    st_drop_geometry() %>% 
    group_by(ID, year) %>% 
    filter(!is.na(type)) %>% 
    print() %>% 
    summarize(
      healthyfood_prop_2006 = healthyfood_2006 / (healthyfood_2006 + unhealthyfood_2006),
      healthyfood_prop_2011 = healthyfood_2011 / (healthyfood_2011 + unhealthyfood_2011),
      healthyfood_prop_2016 = healthyfood_2016 / (healthyfood_2016 + unhealthyfood_2016)
      ,
      #   healthy_food_prop_2020 = healthyfood_2020 / (healthyfood_2020 + unhealthyfood_2020),
      food_prop_popthous_2006 = (healthyfood_2006 + unhealthyfood_2006) / (x$population / 1000),
      food_prop_popthous_2011 = (healthyfood_2011 + unhealthyfood_2011) / (x$population / 1000),
      food_prop_popthous_2016 = (healthyfood_2016 + unhealthyfood_2016) / (x$population / 1000)
      ,
      #  food_prop_popthous_2020 = (healthyfood_2020 + unhealthyfood_2020) / (x$population / 1000),
      healthyfood_prop_popthous_2006 = healthyfood_2006 / (x$population / 1000),
      healthyfood_prop_popthous_2011 = healthyfood_2011 / (x$population / 1000),
      healthyfood_prop_popthous_2016 = healthyfood_2016 / (x$population / 1000),
      # food_prop_popthous_2020 = (healthyfood_2020 + unhealthyfood_2020) / (x$population / 1000)
    ) %>%
    left_join(points, by = "ID") %>% 
    relocate(contains("food_prop"), .before = geometry) %>% 
    mutate(across("food_prop", ntile, n = 3, .names = "{.col}_q3"), 
           .before = geometry) %>% # new tertile syntax
    st_set_agr("constant")
}

# Add to existing geographies ---------------------------------------------

dmti_geom_list <- map(list(borough, CT, DA, grid), dmti_geoadd_fun)

borough <- dmti_geom_list[[1]]
CT <- dmti_geom_list[[2]]
DA <- dmti_geom_list[[3]]
grid <- dmti_geom_list[[4]]

dmti_metrics_list <- map(list(borough, CT, DA, grid), process_dmti)

borough <- dmti_geom_list[[1]]
CT <- dmti_geom_list[[2]]
DA <- dmti_geom_list[[3]]
grid <- dmti_geom_list[[4]]

rm(dmti, points, filter_vectors, healthy_food, non_walkable, unhealthy_food,
   walkable, healthy_city, unhealthy_city, commercial_gentrification,
   process_DMTI)


# RETURN SCALE GEOMETRIES TO 'GEOMETRY' COLUMN ----------------------------

DA %>% 
  st_set_geometry("geometry")



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
