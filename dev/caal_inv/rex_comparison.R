
# pak::pak("BenWilliams-NOAA/surveyISS")

# load libraries
library(afscdata)
library(surveyISS)
library(tidyverse)


db = 'akfin'
conn = afscdata::connect(db)

# set parameters
survey = 47
region = 'goa'
species = 10200
yrs = 1993

# get surveyISS data together ----

## specimen data ----

dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                    by = c('CRUISEJOIN')) %>% 
  dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen')),
                    by = c('HAULJOIN')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::select(year,
                survey_definition_id,
                species_code,
                stratum,
                hauljoin,
                latitude_dd_start,
                latitude_dd_end,
                longitude_dd_start,
                longitude_dd_end,
                sex,
                length_mm,
                age) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                species_code %in% species,
                year >= yrs) %>% 
  dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                long_mid = (longitude_dd_start + longitude_dd_end) / 2) %>% 
  dplyr::select(year, 
                survey = survey_definition_id,
                species_code,
                stratum,
                hauljoin,
                sex,
                length = length_mm,
                age,
                lat_mid,
                long_mid) %>% 
  dplyr::collect() -> specimen_iss

specimen_iss <- specimen_iss %>% 
  tidytable::drop_na(age)


## strata data ----

# strata with area sizes
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                area_type == 'STRATUM') %>% 
  dplyr::select(survey = survey_definition_id,
                design_year,
                stratum = area_id,
                area = area_km2) %>% 
  dplyr::collect() -> st_area

# subregion level with description (e.g., wgoa, etc)
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                area_type == 'REGULATORY AREA') %>% 
  dplyr::select(area_id,
                subarea_name = description,
                design_year) %>% 
  dplyr::collect() -> subreg

# strata within subregions
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_stratum_groups')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey) %>% 
  dplyr::select(stratum, 
                area_id) %>% 
  dplyr::collect() -> st_subreg

# join all to get strata with area sizes and subregion ids
st_area %>% 
  tidytable::left_join(st_subreg %>% 
                         tidytable::left_join(subreg) %>% 
                         tidytable::drop_na()) %>%
  tidytable::filter(design_year == max(design_year), .by = c(stratum)) %>% 
  tidytable::distinct() -> strata


# get carey data together ----

# raw specimen data
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen_v')) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::filter(species_code %in% species) %>%
  dplyr::select(hauljoin = hauljoin,
                performance = performance,
                design_year = design_year,
                date_time_start = date_time_start,
                regulatory_area = regulatory_area,
                depth = depth,
                species_code = species_code,
                length_mm = length_mm,
                sex = sex,
                age = age,
                weight_g = weight_g,
                gear = gear,
                gear_temperature_c = gear_temperature_c,
                specimen_subsample_method = specimen_subsample_method,
                specimen_sample_type = specimen_sample_type,
                age_determination_method = age_determination_method) %>%
  dplyr::collect() -> specimen_cm

# note that i did this slightly different, filtering to years >= 1993
specimen_cm <- specimen_cm  %>%
  dplyr::mutate(year = year(date_time_start),
                length = length_mm / 10) %>% 
  dplyr::filter(!is.na(age),
                performance >= 0,
                year >= yrs)


# get surveyISS data like carey, but without >= 1993 year filter ----

data_iss_cm <- surveyISS::query_data(survey = 47, 
                                     region = 'goa', 
                                     species = 10200,
                                     yrs = yrs)

specimen_iss_cm <- data_iss_cm$specimen %>% 
  tidytable::drop_na(age)


# compare data ----

## check overall dimensions ----
if(nrow(specimen_iss) == nrow(specimen_cm) & nrow(specimen_iss_cm) == nrow(specimen_cm)){"Number of rows match"}

## join data to see if there are data missing ----

specimen_iss %>% 
  tidytable::mutate(length = length / 10) %>% 
  tidytable::count(year, age, length) %>% 
  tidytable::rename(n_iss = n) -> iss_test

specimen_cm %>% 
  tidytable::select(year, hauljoin, sex, length, age) %>% 
  tidytable::count(year, age, length) %>% 
  tidytable::rename(n_cm = n) -> cm_test

specimen_iss_cm %>% 
  tidytable::mutate(length = length / 10) %>% 
  tidytable::count(year, age, length) %>% 
  tidytable::rename(n_iss_cm = n) -> iss_cm_test

test1 <- iss_test %>% 
  tidytable::inner_join(cm_test) %>% 
  tidytable::inner_join(iss_cm_test) %>% 
  tidytable::drop_na() # this will catch if there is data in one and not in the other as na's will be created

if(nrow(test1) == nrow(iss_test)){"Lengths and ages recorded matches"}

## now test whether the same numnber of ages and lengths have been observed
test2 <- test1 %>% 
  tidytable::mutate(avg_n = (n_iss + n_cm + n_iss_cm) / 3,
                    test_n = avg_n - floor(avg_n),
                    test = case_when(test_n > 0 ~ 'Different',
                                     test_n == 0 ~ 'Same')) %>% 
  tidytable::filter(test == 'Different')

if(nrow(test2) == 0){"Data matches"}


# notes on data:
# - dimensions of data matches, the ages/lengths observed matches, and the number of ages/lengths observed matches
# - in carey's script, surveyISS::query_data() was filtered to >= 1993 (line 435 of goa_rex.qmd), but not filtered in other data queries
# - filtering to performance >= doesn't do anything, they're already >= 0 (i.e., gap already filters out bad data before sending to akfin)
# - pulling from gap_products.akfin_specimen_v takes waaaay longer than doing it the surveyISS route
# - overall, the data is the same regardless of source/method queried



# compute caal data ----

## compute caal with surveyISS ----
# note: this is the function carey used (after renaming data)

surveyISS::srvy_iss_goa_wc_e_caal(iters = 1, 
                                  specimen_data = data_iss_cm$specimen, 
                                  cpue_data = data_iss_cm$cpue, 
                                  strata_data = data_iss_cm$strata,
                                  yrs = 1993, 
                                  bin = seq(from = 9, to = 65, by = 2), 
                                  boot_hauls = FALSE,
                                  boot_ages = FALSE,
                                  al_var = FALSE,
                                  al_var_ann = FALSE,
                                  age_err = FALSE,
                                  age_samples = NULL,
                                  plus_len = 65,
                                  plus_age = 20,
                                  region = 'goa',
                                  save_interm = TRUE, 
                                  save_stats = TRUE,
                                  save="2026_goa_rex")

iss_caal <- vroom::vroom(here::here("output","goa","2026_goa_rex_base_caal_wc_egoa.csv")) %>%
  tidytable::filter(sex > 0) %>%
  tidytable::mutate(sex = case_when(sex==1 ~ "male", 
                                    sex == 2 ~ "female"))


## carey's caal functions ----

format_spatial_caal<-function(op_dir = op_dir,specimen_data = specimen_data, low_length_bin = 9, hi_length_bin = 65,length_bin_size =2, low_age_bin = 1,hi_age_bin =20, age_bin_size = 1, do_2025 = FALSE) {
  min_length_data <- min(specimen_data %>% dplyr::distinct(length))
  max_length_data <- max(specimen_data %>% dplyr::distinct(length))
  
  min_age_data <- min(specimen_data %>% dplyr::distinct(age))
  max_age_data <- max(specimen_data %>% dplyr::distinct(age))
  
  
  bin_config <- list(
    length = list(bin_size = length_bin_size, low_bin = low_length_bin, high_bin = hi_length_bin),
    age    = list(bin_size = age_bin_size, low_bin = low_age_bin, high_bin = hi_age_bin)
  )
  
  specimen_data<-bin_columns_custom(specimen_data, bin_config)
  write.csv(specimen_data,file.path(op_dir,"binned_spatial_specimen_data.csv"))
  
  if (do_2025==TRUE) {
    .specimen_data<-specimen_data %>% 
      dplyr::filter(year>=1993) %>%
      dplyr::select(c(sex,year,regulatory_area,binned_length,binned_age)) %>%
      dplyr::mutate(fleet = case_when(regulatory_area =="CENTRAL GOA - NMFS" ~ 2,
                                      regulatory_area == "WESTERN GOA - NMFS" ~ 2,
                                      regulatory_area == "EASTERN GOA - NMFS" ~ 3,
                                      TRUE ~ 0),
                    sex_ss3 = case_when(sex == 1 ~ 2,
                                        sex == 2 ~ 1,
                                        TRUE ~ 0)) %>%
      dplyr::filter(sex_ss3!=0,fleet!=0) %>%
      dplyr::select(-c(regulatory_area,sex))
  } else {
    .specimen_data<-specimen_data %>% 
      dplyr::filter(year>=1993) %>%
      dplyr::select(c(sex,year,regulatory_area,binned_length,binned_age)) %>%
      dplyr::mutate(fleet = case_when(regulatory_area =="CENTRAL GOA - INPFC" ~ 2,
                                      regulatory_area == "WESTERN GOA - INPFC" ~ 2,
                                      regulatory_area == "EASTERN GOA - INPFC" ~ 3,
                                      TRUE ~ 0),
                    sex_ss3 = case_when(sex == 1 ~ 2,
                                        sex == 2 ~ 1,
                                        TRUE ~ 0)) %>%
      dplyr::filter(sex_ss3!=0,fleet!=0) %>%
      dplyr::select(-c(regulatory_area,sex))
    
  }
  
  
  combos <- tidyr::expand_grid(fleet = c(2,3),sex_ss3 = c(1,2),year = unique(.specimen_data$year),binned_length = seq(from = low_length_bin, to = hi_length_bin, by=length_bin_size), binned_age = seq(from = low_age_bin,to=hi_age_bin,by = age_bin_size))
  
  sample_combos <- tidyr::expand_grid(fleet = c(2,3),sex_ss3 = c(1,2),year = unique(.specimen_data$year),binned_length = seq(from = low_length_bin, to = hi_length_bin, by=length_bin_size))
  
  sample_size<-.specimen_data %>% 
    dplyr::count(fleet,sex_ss3,year,binned_length) %>%
    dplyr::rename(sample_size = n) %>%
    dplyr::full_join(sample_combos) %>% 
    dplyr::mutate(sample_size = tidyr::replace_na(sample_size, 0))
  
  caal_long<- .specimen_data %>%
    dplyr::count(fleet, sex_ss3, year, binned_length, binned_age) %>%
    dplyr::full_join(combos) %>% 
    dplyr::mutate(n = tidyr::replace_na(n, 0)) %>%
    dplyr::arrange(fleet, sex_ss3,year,binned_length,binned_age) |>
    dplyr::rename(freq = n)
  
  
  caal_wide<-caal_long %>%
    tidyr::pivot_wider(names_from = binned_age,values_from = freq,id_cols = c(fleet,sex_ss3,year,binned_length)) %>%
    dplyr::left_join(sample_size) %>%
    dplyr::mutate(month = 7, part = 0,ageerr = 1,Lbin_lo = binned_length) %>%
    dplyr::arrange(fleet, sex_ss3,year, Lbin_lo,binned_length) %>%
    dplyr::relocate(year, month, fleet, sex_ss3, part, ageerr,Lbin_lo, binned_length, sample_size)
  
  #copy age data again to the right because of ss3 formatting quirk
  males_right<-caal_wide %>% dplyr::filter(sex_ss3 == 2) %>% 
    dplyr::select(-c(year, month, fleet, sex_ss3, part, ageerr,Lbin_lo, binned_length, sample_size))
  
  females_right<-caal_wide %>% dplyr::filter(sex_ss3==1) %>% 
    dplyr::select(-c(year, month, fleet, sex_ss3, part, ageerr,Lbin_lo, binned_length, sample_size))
  
  fem_wide_data<-caal_wide %>% dplyr::filter(sex_ss3==1) %>% 
    dplyr::bind_cols(females_right)  
  male_wide_data <-caal_wide %>% dplyr::filter(sex_ss3 == 2) %>%
    dplyr::bind_cols(males_right)
  
  formatted_caal<-fem_wide_data %>% dplyr::bind_rows(male_wide_data) %>%
    dplyr::filter(sample_size >0) %>%
    dplyr::arrange(fleet, sex_ss3,year, Lbin_lo,binned_length) %>%
    dplyr::relocate(year, month, fleet, sex_ss3, part, ageerr,Lbin_lo, binned_length, sample_size) 
  
  if (do_2025==FALSE) {
    write.csv(formatted_caal,file = file.path(op_dir,"formatted_caal.csv"))
  } else {
    write.csv(formatted_caal,file = file.path(op_dir,"formatted_caal_2025.csv"))
  }
  return(formatted_caal)
}

bin_columns_custom <- function(df, config) {
  # Sequentially update the dataframe for each configured column
  reduce(names(config), function(.df, col_name) {
    # Extract specific configuration parameters for the current column
    cfg       <- config[[col_name]]
    bin_size  <- cfg$bin_size
    low_bin   <- cfg$low_bin
    high_bin  <- cfg$high_bin
    new_col   <- paste0("binned_", col_name)
    
    .df %>%
      mutate(
        !!new_col := case_when(
          .data[[col_name]] < low_bin ~ low_bin,
          .data[[col_name]] >= high_bin ~ high_bin,
          TRUE ~ low_bin + floor((.data[[col_name]] - low_bin) / bin_size) * bin_size
        )
      )
  }, .init = df)
}


## compute caal with carey functions ----

spatial_caal <- format_spatial_caal(op_dir = here::here("output","goa"),
                                    specimen_data = specimen_cm,
                                    low_length_bin = 9, 
                                    hi_length_bin = 65,
                                    length_bin_size =2,                        
                                    low_age_bin = 1,
                                    hi_age_bin =20, 
                                    age_bin_size = 1, 
                                    do_2025 = FALSE)

spatial_caal_2025 <- format_spatial_caal(op_dir = here::here("output","goa"),
                                         specimen_data = specimen_cm,
                                         low_length_bin = 9, 
                                         hi_length_bin = 65,
                                         length_bin_size =2,
                                         low_age_bin = 1,
                                         hi_age_bin =20, 
                                         age_bin_size = 1,
                                         do_2025 = TRUE)

# format cm output to match surveyISS
spatial_caal %>% 
  tidytable::bind_rows(spatial_caal_2025) %>% 
  tidytable::select(-c(month, part, ageerr, binned_length, sample_size, paste0(1:20, "...", 30:49))) %>% 
  tidytable::pivot_longer(cols = paste0(1:20, "...", 10:29),
                          names_to = "age_bin",
                          values_to = "count") %>% 
  tidytable::rename(sex = sex_ss3, length = Lbin_lo) %>% 
  tidytable::mutate(age = as.numeric(stringr::str_split_i(age_bin, fixed("..."), 1)),
                    region = case_when(fleet == 2 ~ 'wcgoa',
                                       fleet == 3 ~ 'egoa'),
                    sex = case_when(sex == 1 ~ 'female',
                                    sex == 2 ~ 'male')) %>% 
  tidytable::select(region, year, sex, length, age, count) %>% 
  tidytable::filter(count > 0) %>% 
  tidytable::mutate(caal_cm = count / sum(count), .by = c(region, year, length)) -> cm_caal

## compare caal and sample size values ----
iss_caal %>% 
  tidytable::rename(n_age_iss = n_age, caal_iss = caal) %>% 
  tidytable::left_join(cm_caal %>% 
                         tidytable::rename(n_age_cm = count)) %>% 
  tidytable::mutate(test1 = n_age_iss - n_age_cm,
                    test2 = caal_iss - caal_cm) %>% 
  tidytable::summarise(test1 = sum(test1),
                       test2 = sum(test2))

# notes: surveyISS output matches rex output!




# test getting iss values ----

surveyISS::srvy_iss_goa_wc_e_caal(iters = 100, 
                                  specimen_data = data_iss_cm$specimen, 
                                  cpue_data = data_iss_cm$cpue, 
                                  strata_data = data_iss_cm$strata,
                                  yrs = 1993, 
                                  bin = seq(from = 9, to = 65, by = 2), 
                                  boot_hauls = TRUE,
                                  boot_ages = TRUE,
                                  al_var = TRUE,
                                  al_var_ann = TRUE,
                                  age_err = TRUE,
                                  plus_len = 65,
                                  plus_age = 20,
                                  region = 'goa',
                                  save_interm = TRUE, 
                                  save_stats = TRUE,
                                  save="2026_goa_rex")

