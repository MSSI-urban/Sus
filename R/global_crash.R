### CRASH MODULE GLOBALS #######################################################

# Map token
token_crash <- paste0("pk.eyJ1IjoiZHdhY2hzbXV0aCIsImEiOiJja2g2Y2JpbDc",
                      "wMDc5MnltbWpja2xpYTZhIn0.BXdU7bsQYWcSwmmBx8DNqQ")

# Initialize reactive values
rv_crash <- reactiveValues(poly_selected = NA, zoom = "borough")

# Time slider values
crash_slider <- list(
  min = 2012,
  max = 2019,
  interval = 1,
  init = 2019)

# Dropdown menu
var_list_left_crash_1 <- 
  list("Total" = "total",
       "Pedestrian" = "ped",
       "Cyclist" = "cyc",
       "Other" = "other")

var_list_left_crash_2 <- 
  list("Count" = " ",
       "Per sq km" = "prop_area",
       "Per 1000 residents" = "prop_pop")

var_list_right_crash <- 
  list("----" = " ", 
       "Housing" = list(
         "Tenant-occupied (%)" = "housing_tenant_prop",
         "Average rent ($)" = "housing_rent_avg_dollar",
         "Average property value ($)" = "housing_prop_value_avg_dollar",
         "Unaffordable housing (%)" = "housing_unafford_prop",
         "Unsuitable housing (%)" = "housing_unsuit_prop"),
       "Income" = list(
         "Median household income ($)" = "inc_median_dollar",
         "Income under $50k (%)" = "inc_50_prop",
         "Income between $50k-$100k (%)" = "inc_100_prop",
         "Income above $100k (%)" = "inc_high_prop"),
       "Immigration" = list(
         "Immigrants (%)" =  "imm_prop",
         "New immigrants (%)" = "imm_new_prop"),
       "Transportation" = list(
         "Drive to work (%)" = "trans_car_prop",
         "Walk or cycle to work (%)" = "trans_walk_or_bike_prop",
         "Public transit to work (%)" = "trans_transit_prop",
         "15 minutes to work (%)" = "trans_t_15_prop",
         "15-45 minutes to work (%)" = "trans_t_45_prop",
         "More than 45 minutes to work (%)" = "trans_t_45_plus_prop"))
