#### Crash data setup #################################################### 

# get data ----------------------------------------------------------------

MTLcrash <- 
  read_sf("dev/data/collisions_routieres/collisions_routieres.shp") %>% 
  st_transform(4326) %>% 
  st_set_agr("constant") %>% 
  mutate(type = case_when(CD_GENRE_A == 32 ~ "ped",
                          CD_GENRE_A == 31 ~ "cyc",
                          CD_GENRE_A != c(32, 31) ~ "other")) %>% 
  mutate(inj_total = (NB_BLESSES + NB_BLESS_1)) %>% 
  select(date = DT_ACCDN, street = RUE_ACCDN, death_total = NB_MORTS, 
         death_ped = NB_DECES_P, death_cyc = NB_DECES_V, 
         inj_total, inj_ped = NB_BLESS_2, 
         inj_cyc = NB_BLESS_4, type) 
qsave(MTLcrash, "dev/data/crash.qs")
rm(MTLcrash)

crash <- qread("dev/data/crash.qs")

# Add to existing geographies ---------------------------------------------

process_crash <- function(x) {
  
  sum_year <- 
    x %>% 
    st_transform(4326) %>% 
    select(ID) %>% 
    st_join(., crash) %>% 
    st_drop_geometry() %>% 
    group_by(ID, year = year(date)) %>%
    filter(!is.na(year), !is.na(type)) %>% 
    summarize(n = n()) %>% 
    mutate(type = "total")
  
  x %>% 
    st_transform(4326) %>% 
    select(ID) %>% 
    st_join(., crash) %>% 
    st_drop_geometry() %>% 
    group_by(ID, year = year(date), type) %>% 
    summarize(n = n()) %>% 
    rbind(., sum_year) %>%
    filter(!is.na(year), !is.na(type)) %>% 
    tidyr::pivot_wider(id_cols = "ID",
                       names_from = c("type", "year"), 
                       names_prefix = "crash_",
                       names_sep = "_", 
                       values_from = n) %>% 
    left_join(x, ., by = "ID") %>%
    st_as_sf() %>% 
    mutate(across(starts_with("crash"),
                  ~{.x / units::drop_units(st_area(geometry))},
                  .names = "{.col}_prop_area"),
           across(starts_with("crash"),
                  ~{.x / population},
                  .names = "{.col}_prop_pop")) %>% 
    relocate(starts_with("crash"), .before = geometry) %>% 
    st_set_agr("constant")
  
}

crash_results <- map(list(borough, CT, DA, grid), process_crash)

borough <- crash_results[[1]]
CT <- crash_results[[2]]
DA <- crash_results[[3]]
grid <- crash_results[[4]]

rm(crash_results, process_crash)
