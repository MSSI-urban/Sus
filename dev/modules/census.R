#### Census data setup #########################################################

# This script relies on objects created in dev/build_geometries.R

# Topic vectors -----------------------------------------------------------

census_housing <- c(
  tenure_households = "v_CA16_4836",
  renter = "v_CA16_4838",
  rent_avg_total = "v_CA16_4897",
  housing_rent_avg_dollar = "v_CA16_4901",
  value_avg_total = "v_CA16_4890",
  major_repairs = "v_CA16_4872",
  repairs_total = "v_CA16_4870",
  housing_value_avg_dollar = "v_CA16_4896",
  housing_unafford_total = "v_CA16_4886",
  housing_unafford = "v_CA16_4888",
  housing_unsuit_total = "v_CA16_4859",
  housing_unsuit = "v_CA16_4860",
  housing_stress_renter_prop = "v_CA16_4899",
  housing_stress_owner_prop = "v_CA16_4892")

census_income <- c(
  inc_median_total = "v_CA16_2396",
  inc_median_dollar = "v_CA16_2397",
  inc_total = "v_CA16_2405",
  inc_5  = "v_CA16_2406", 
  inc_10 = "v_CA16_2407",
  inc_15 = "v_CA16_2408",
  inc_20 = "v_CA16_2409",
  inc_25 = "v_CA16_2410",
  inc_30 = "v_CA16_2411",
  inc_35 = "v_CA16_2412",
  inc_40 = "v_CA16_2413",
  inc_45 = "v_CA16_2414",
  inc_50 = "v_CA16_2415",
  inc_60 = "v_CA16_2416",
  inc_70 = "v_CA16_2417",
  inc_80 = "v_CA16_2418",
  inc_90 = "v_CA16_2419",
  inc_100 = "v_CA16_2420",
  inc_high = "v_CA16_2421",
  inc_limat_prop = "v_CA16_2540",
  inc_limat_total = "v_CA16_2510")

census_immigration <- c(
  imm_total = "v_CA16_3405",
  imm = "v_CA16_3411",
  imm_new = "v_CA16_3432",
  imm_vm_total = "v_CA16_3954",
  imm_vm = "v_CA16_3957")

census_transport <- c(
  trans_total = "v_CA16_5792",
  trans_driver = "v_CA16_5795",
  trans_passenger = "v_CA16_5798",
  trans_transit = "v_CA16_5801",
  trans_walk = "v_CA16_5804",
  trans_bike = "v_CA16_5807",
  trans_t_total = "v_CA16_5813",
  trans_t_15 = "v_CA16_5816",
  trans_t_30 = "v_CA16_5819",
  trans_t_45 = "v_CA16_5822",
  trans_t_60 = "v_CA16_5825",
  trans_t_60_plus = "v_CA16_5828")


# Helper function for processing data -------------------------------------

process_census_data <- function(data) {
  
  # Process housing
  var_list <- c("housing_tenant_prop", "housing_rent_avg_dollar", 
                "housing_value_avg_dollar", "housing_unafford_prop",
                "housing_unsuit_prop", "housing_repairs_prop",
                "housing_stress_renter_prop", "housing_stress_owner_prop")
  
  data <- 
    data %>% 
    mutate(housing_tenant_prop = renter / tenure_households,
           housing_unafford_prop = housing_unafford / housing_unafford_total,
           housing_unsuit_prop = housing_unsuit / housing_unsuit_total,
           housing_repairs_prop = major_repairs / repairs_total,
           housing_stress_renter_prop = housing_stress_renter_prop / 100,
           housing_stress_owner_prop = housing_stress_owner_prop / 100) %>% 
    select(-c(renter, tenure_households, housing_unafford, 
              housing_unafford_total, housing_unsuit, housing_unsuit_total,
              rent_avg_total, value_avg_total, major_repairs, repairs_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry)
  
  # Process income
  var_list <- c("inc_median_dollar", "inc_50_prop", "inc_100_prop", 
                "inc_high_prop", "inc_limat_prop")
  
  data <- 
    data %>% 
    mutate(inc_50_prop = (inc_5 + inc_10 + inc_15 + inc_20 +
                            inc_25 + inc_30 + inc_35 + inc_40 + 
                            inc_45 + inc_50) / inc_total,
           inc_100_prop = (inc_60 + inc_70 + inc_80 + inc_90 + 
                             inc_100) / inc_total,
           inc_high_prop = inc_high / inc_total,
           inc_limat_prop = inc_limat_prop / 100) %>% 
    select(-c(inc_median_total, inc_5, inc_10, inc_15, inc_20, inc_25, inc_30, 
              inc_35, inc_40, inc_45, inc_50, inc_60, inc_70, inc_80, inc_90, 
              inc_100, inc_high, inc_total, inc_limat_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry)
  
  # Process immigration
  var_list <- c("imm_prop", "imm_new_prop", "imm_vm_prop")
  
  data <- 
    data %>% 
    mutate(imm_prop = imm / imm_total, 
           imm_new_prop = imm_new / imm_total,
           imm_vm_prop = imm_vm / imm_vm_total) %>% 
    select(-c(imm, imm_new, imm_total, imm_vm, imm_vm_total)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry)
  
  # Process transport
  var_list <- c("trans_car_prop", "trans_walk_or_bike_prop", 
                "trans_transit_prop", "trans_t_15_prop", "trans_t_45_prop", 
                "trans_t_45_plus_prop")
  
  data <- 
    data %>% 
    mutate(trans_car_prop = (trans_driver + trans_passenger) / trans_total,
           trans_walk_or_bike_prop = (trans_walk + trans_bike) / trans_total,
           trans_transit_prop = trans_transit / trans_total,
           trans_t_15_prop = trans_t_15 / trans_t_total,
           trans_t_45_prop = (trans_t_30 + trans_t_45) / trans_t_total,
           trans_t_45_plus_prop = (trans_t_60 + trans_t_60_plus) / 
             trans_t_total) %>% 
    select(-c(trans_total, trans_driver, trans_passenger, trans_transit, 
              trans_walk, trans_bike, trans_t_total, trans_t_15, trans_t_30, 
              trans_t_45, trans_t_60, trans_t_60_plus)) %>% 
    mutate(across(all_of(var_list), ntile, 3, .names = "{.col}_q3")) %>% 
    relocate(all_of(var_list), paste0(var_list, "_q3"), .before = geometry)
  
  data <- st_set_agr(data, "constant")
  
  data
  
}


# Download data -----------------------------------------------------------

vars_to_remove <- 
  c("Shape Area", "Type", "Dwellings", "CD_UID", "CMA_UID", "Region Name",
    "Area (sq km)", "Adjusted Population (previous Census)", "PR_UID",
    "Population", "Households", "CSD_UID", "CT_UID", "rguid")

DA_census <- 
  get_census("CA16", list(CMA = "24462"), "DA", 
             vectors = c(census_housing, census_income, census_immigration, 
                         census_transport), quiet = TRUE) %>% 
  as_tibble() %>% 
  select(-any_of(vars_to_remove)) %>% 
  rename(ID = GeoUID) %>% 
  arrange(ID)

CT_census <-
  get_census("CA16", list(CMA = "24462"), "CT",
             vectors = c(census_housing, census_income, census_immigration, 
                         census_transport), quiet = TRUE) %>% 
  as_tibble() %>% 
  select(-any_of(vars_to_remove)) %>% 
  rename(ID = GeoUID) %>% 
  arrange(ID)

CSD_census <-
  get_census("CA16", list(CMA = "24462"), "CSD",
             vectors = c(census_housing, census_income, census_immigration, 
                         census_transport), quiet = TRUE) %>% 
  as_tibble() %>% 
  select(-any_of(vars_to_remove)) %>% 
  rename(ID = GeoUID) %>% 
  arrange(ID) %>% 
  filter(ID != "2466023")

rm(census_housing, census_immigration, census_income, census_transport,
   vars_to_remove)


# Process data ------------------------------------------------------------

DA <- 
  DA %>% 
  inner_join(DA_census, by = "ID") %>% 
  process_census_data()

CT <- 
  CT %>% 
  inner_join(CT_census, by = "ID") %>% 
  process_census_data()

CSD <- 
  borough %>% 
  inner_join(CSD_census, by = "ID") %>% 
  process_census_data()


# Process boroughs --------------------------------------------------------

borough <-
  DA %>% 
  st_drop_geometry() %>% 
  select(ID, CSDUID) %>% 
  filter(str_starts(CSDUID, "2466023")) %>% 
  inner_join(DA_census, by = "ID") %>% 
  group_by(ID = CSDUID) %>% 
  summarize(
    housing_rent_avg_dollar = 
      weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
    housing_value_avg_dollar = 
      weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
    inc_median_dollar = weighted.mean(inc_median_dollar, inc_median_total, 
                                      na.rm = TRUE),
    housing_stress_renter_prop = 
      weighted.mean(housing_stress_renter_prop, rent_avg_total, na.rm = TRUE),
    housing_stress_owner_prop = 
      weighted.mean(housing_stress_owner_prop, value_avg_total, na.rm = TRUE),
    inc_limat_prop = weighted.mean(inc_limat_prop, inc_limat_total, 
                                   na.rm = TRUE),
    across(c(tenure_households:repairs_total, 
             housing_unafford_total:housing_unsuit,
             inc_median_total,
             inc_total:inc_high,
             inc_limat_total:trans_t_60_plus), sum, na.rm = TRUE), 
    .groups = "drop") %>% 
  inner_join(borough, ., by = "ID") %>% 
  relocate(geometry, .after = last_col())
  
borough <- process_census_data(borough)

# Rbind CSD and borough
borough <-
  bind_rows(CSD, borough) %>% 
  arrange(ID) %>% 
  st_set_agr("constant")


# Process grid ------------------------------------------------------------

DA_data <-
  DA %>% 
  select(ID) %>% 
  inner_join(DA_census, by = "ID") %>% 
  st_transform(32618) %>% 
  mutate(area = st_area(geometry)) %>% 
  st_set_agr("constant")

# Identify variables to be averaged
avg_list <- str_subset(names(DA_data), "avg|median") %>% 
  str_subset("total", negate = TRUE)

# Identify variables to be aggregated
agg_list <-
  setdiff(names(DA_data), c("ID", "name", "CTUID", "CSDUID", "geometry", 
                             "area")) %>% 
  setdiff(avg_list)

grid_census <-
  grid %>% 
  select(ID) %>% 
  st_transform(32618) %>% 
  st_set_agr("constant") %>% 
  st_intersection(DA_data) %>% 
  mutate(area_prop = st_area(geometry) / area) %>% 
  mutate(across(all_of(agg_list), ~{.x * units::drop_units(area_prop)})) %>% 
  select(-ID.1, -area, -area_prop) %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarize(
    housing_value_avg_dollar = 
      weighted.mean(housing_value_avg_dollar, value_avg_total, na.rm = TRUE),
    housing_rent_avg_dollar = 
      weighted.mean(housing_rent_avg_dollar, rent_avg_total, na.rm = TRUE),
    inc_median_dollar = 
      weighted.mean(inc_median_dollar, inc_median_total, na.rm = TRUE),
    housing_stress_renter_prop = weighted.mean(housing_stress_renter_prop, rent_avg_total, 
                                               na.rm = TRUE),
    housing_stress_owner_prop = weighted.mean(housing_stress_owner_prop, value_avg_total, 
                                              na.rm = TRUE),
    inc_limat_prop = weighted.mean(inc_limat_prop, inc_limat_total, 
                                   na.rm = TRUE),
    across(all_of(agg_list), sum, na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.nan(.), 0)))

grid <- 
  grid %>% 
  left_join(grid_census, by = "ID") %>% 
  process_census_data()

rm(CSD, CSD_census, CT_census, DA_census, DA_data, grid_census, agg_list,
   avg_list, process_census_data)


# Add variable explanations -----------------------------------------------

var_exp <- 
  var_exp %>% 
  add_row(
    var_code = "housing_tenant_prop",
    var_name = "Tenant-occupied (%)",
    explanation = "the percentage of private dwellings occupied by tenants") %>% 
  add_row(
    var_code = "housing_rent_avg_dollar",
    var_name = "Average rent ($)",
    explanation = "the average rent paid by tenants per month") %>% 
  add_row(
    var_code = "housing_value_avg_dollar",
    var_name = "Average property value ($)",
    explanation = "the average value of owner-occupied dwellings") %>% 
  add_row(
    var_code = "housing_unafford_prop",
    var_name = "Unaffordable housing (%)",
    explanation = paste0("the percentage of dwellings for which residents pay ",
                         "more than 30% of income on housing costs")) %>% 
  add_row(
    var_code = "housing_unsuit_prop",
    var_name = "Unsuitable housing (%)",
    explanation = paste0("the percentage of households living in ", 
                         "accommodations without enough bedrooms according to ",
                         "the National Occupancy Standard")) %>% 
  add_row(
    var_code = "housing_repairs_prop",
    var_name = "Housing requiring major repairs (%)",
    explanation = paste0("the percentage of households living in ", 
                         "dwellings requiring major repairs")) %>% 
  add_row(
    var_code = "housing_stress_owner_prop",
    var_name = "Owner housing stress (%)",
    explanation = paste0("the percentage of owner households that ", 
                         "spend more than 30% of their income on ",
                         "shelter costs")) %>% 
  add_row(
    var_code = "housing_stress_renter_prop",
    var_name = "Renter housing stress (%)",
    explanation = paste0("the percentage of renter households that ", 
                         "spend more than 30% of their income on ",
                         "shelter costs")) %>% 
  add_row(
    var_code = "inc_median_dollar",
    var_name = "Median household income ($)",
    explanation = "median before-tax household income") %>% 
  add_row(
    var_code = "inc_50_prop",
    var_name = "Income under $50k (%)",
    explanation = paste0("the percentage of households with an income less ", 
                         "then $50,000")) %>% 
  add_row(
    var_code = "inc_100_prop",
    var_name = "Income beetween $50k-$100k (%)",
    explanation = paste0("the percentage of households with an income between ",
                         "$50,000 and $100,000")) %>% 
  add_row(
    var_code = "inc_high_prop",
    var_name = "Income above $100k (%)",
    explanation = paste0("the percentage of households with an income higher ",
                         "than $100,000")) %>% 
  add_row(
    var_code = "inc_limat_prop",
    var_name = "Prevalence of low income (after-tax) (%)",
    explanation = paste0("the prevalence of low income in private households ",
                         "based on the Low income measure, after-tax", 
                         "(LIM-AT)")) %>% 
  add_row(
    var_code = "imm_prop",
    var_name = "Immigrants (%)",
    explanation = "the percentage of residents who are foreign-born") %>% 
  add_row(
    var_code = "imm_new_prop",
    var_name = "New immigrants (%)",
    explanation = paste0("the percentage of people who have immigrated in ", 
                         "the last five years")) %>% 
  add_row(
    var_code = "imm_vm_prop",
    var_name = "Visible minorities (%)",
    explanation = paste0("the percentage of people who identify as part ", 
                         "of one or more visible minority groups")) %>% 
  add_row(
    var_code = "trans_car_prop",
    var_name = "Drive to work (%)",
    explanation = paste0("the percentage of people who drive a privately ", 
                         "owned car or truck to work")) %>% 
  add_row(
    var_code = "trans_walk_or_bike_prop",
    var_name = "Walk or cycle to work (%)",
    explanation = "the percentage of people who walk or cycle to work") %>% 
  add_row(
    var_code = "trans_walk_or_bike_prop",
    var_name = "Public transit to work (%)",
    explanation = paste0("the percentage of people who use public transit to ",
                         "get to work")) %>% 
  add_row(
    var_code = "trans_t_15_prop",
    var_name = "15 minutes to work (%)",
    explanation = paste0("the percentage of people whose commute time is less ",
                         "than 15 minutes")) %>% 
  add_row(
    var_code = "trans_t_45_prop",
    var_name = "15-45 minutes to work (%)",
    explanation = paste0("the percentage of people whose commute time is ", 
                         "between 15 and 45 minutes")) %>% 
  add_row(
    var_code = "trans_t_45_plus_prop",
    var_name = "More than 45 minutes to work (%)",
    explanation = paste0("the percentage of people whose commute time is ", 
                         "longer than 45 minutes"))

# To save output, run dev/build_geometries.R, which calls this script
