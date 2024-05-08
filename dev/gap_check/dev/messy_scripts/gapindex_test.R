library(gapindex)
library(tidyverse)

# check goa data ----
year_set = seq(1990,2023)
survey_set = "GOA"
spp_codes = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
query = FALSE

if(isTRUE(query)){
  gapdata <- gapindex::get_data(year_set = year_set,
                                survey_set = survey_set,
                                spp_codes = spp_codes,
                                pull_lengths = TRUE)
  
  saveRDS(gapdata, here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))
} else{
  gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))
}


# check specimen data
nrow(vroom::vroom(here::here('data', 'specimen_goa.csv')) %>% 
  tidytable::drop_na())
nrow(gapdata$specimen)

# check length frequency data
nrow(vroom::vroom(here::here('data', 'lfreq_goa.csv')))
nrow(gapdata$size)

# check cpue data
cpue_gap <- gapindex::calc_cpue(gapdata)

nrow(vroom::vroom(here::here('data', 'cpue_goa.csv')))
nrow(cpue_gap)

cpue_gap %>% 
  dplyr::rename_all(tolower) %>%
  summarise(rows = length(cpue_nokm2), .by = year) %>% 
  tidytable::left_join(vroom::vroom(here::here('data', 'cpue_goa.csv')) %>% 
                         summarise(rows_iss = length(numcpue), .by = year)) %>% 
  tidytable::mutate(diff = rows - rows_iss)

# # computing cpue from catch data
# catch_iss <- vroom::vroom(here::here('data', 'catch_goa.csv'))
# cpue_iss <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
# 
# catch_iss %>% 
#   tidytable::mutate(numcpue = count / (distance_fished_km * (0.001 * net_width_m))) %>% 
#   tidytable::select(-count, -distance_fished_km, -net_width_m) -> cpue_iss_calc
# 
# # filling in 0's like gapindex
# tidytable::expand_grid(hauljoin = unique(cpue_iss$hauljoin), species_code = spp_codes) %>% 
#   tidytable::left_join(cpue_iss %>% 
#                          tidytable::select(-species_code, -numcpue) %>% 
#                          tidytable::slice_head(n = 1, .by = c(year, survey, stratum, lat_mid, long_mid)), .by = hauljoin) %>% 
#   tidytable::left_join(cpue_iss_calc %>% 
#                          tidytable::replace_na(list(numcpue = -1))) %>% 
#   tidytable::replace_na(list(numcpue = 0)) -> cpue_iss_calc_0
# 
# # check 0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 == 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue == 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # check non0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 > 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue > 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # chack all
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)


# check ai data ----
year_set = seq(1991,2023)
survey_set = "AI"
spp_codes = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)
query = FALSE

if(isTRUE(query)){
  gapdata <- gapindex::get_data(year_set = year_set,
                                survey_set = survey_set,
                                spp_codes = spp_codes,
                                pull_lengths = TRUE)
  
  saveRDS(gapdata, here::here('dev', 'gapindex', 'data', 'gapdata_ai.RDS'))
} else{
  gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_ai.RDS'))
}


# check specimen data
nrow(vroom::vroom(here::here('data', 'specimen_ai.csv')) %>% 
       tidytable::drop_na())
nrow(gapdata$specimen)

# check length frequency data
nrow(vroom::vroom(here::here('data', 'lfreq_ai.csv')))
nrow(gapdata$size)

# check cpue data
cpue_gap <- gapindex::calc_cpue(gapdata)

nrow(vroom::vroom(here::here('data', 'cpue_ai.csv')))
nrow(cpue_gap)

cpue_gap %>% 
  dplyr::rename_all(tolower) %>%
  summarise(rows = length(cpue_nokm2), .by = year) %>% 
  tidytable::left_join(vroom::vroom(here::here('data', 'cpue_ai.csv')) %>% 
                         summarise(rows_iss = length(numcpue), .by = year)) %>% 
  tidytable::mutate(diff = rows - rows_iss)

# # computing cpue from catch data
# catch_iss <- vroom::vroom(here::here('data', 'catch_ai.csv'))
# cpue_iss <- vroom::vroom(here::here('data', 'cpue_ai.csv'))
# 
# catch_iss %>% 
#   tidytable::mutate(numcpue = count / (distance_fished_km * (0.001 * net_width_m))) %>% 
#   tidytable::select(-count, -distance_fished_km, -net_width_m) -> cpue_iss_calc
# 
# # filling in 0's like gapindex
# tidytable::expand_grid(hauljoin = unique(cpue_iss$hauljoin), species_code = spp_codes) %>% 
#   tidytable::left_join(cpue_iss %>% 
#                          tidytable::select(-species_code, -numcpue) %>% 
#                          tidytable::slice_head(n = 1, .by = c(year, survey, stratum, lat_mid, long_mid)), .by = hauljoin) %>% 
#   tidytable::left_join(cpue_iss_calc %>% 
#                          tidytable::replace_na(list(numcpue = -1))) %>% 
#   tidytable::replace_na(list(numcpue = 0)) -> cpue_iss_calc_0
# 
# # check 0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 == 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue == 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # check non0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 > 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue > 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # chack all
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)



# check ebs data ----
year_set = seq(1979,2023)
survey_set = "EBS"
spp_codes = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
query = FALSE

if(isTRUE(query)){
  gapdata <- gapindex::get_data(year_set = year_set,
                                survey_set = survey_set,
                                spp_codes = spp_codes,
                                pull_lengths = TRUE)
  
  saveRDS(gapdata, here::here('dev', 'gapindex', 'data', 'gapdata_ebs.RDS'))
} else{
  gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_ebs.RDS'))
}


# check specimen data
nrow(vroom::vroom(here::here('data', 'specimen_ebs.csv')) %>% 
       tidytable::drop_na())
nrow(gapdata$specimen)

# check length frequency data
nrow(vroom::vroom(here::here('data', 'lfreq_ebs.csv')))
nrow(gapdata$size)

iss_lfreq <- vroom::vroom(here::here('data', 'lfreq_ebs.csv'))
gapdata$size %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::summarise(rows = length(frequency), .by = species_code) %>% 
  left_join(iss_lfreq %>% 
              tidytable::summarise(rows_iss = length(frequency), .by = species_code)) %>% 
  tidytable::mutate(diff = rows - rows_iss)

gapdata$size %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::summarise(nss = sum(frequency), .by = species_code) %>% 
  left_join(iss_lfreq %>% 
              tidytable::summarise(nss_iss = sum(frequency, na.rm = TRUE), .by = species_code)) %>% 
  tidytable::mutate(diff = nss - nss_iss)

# check cpue data
cpue_gap <- gapindex::calc_cpue(gapdata)

nrow(vroom::vroom(here::here('data', 'cpue_ebs.csv')))
nrow(cpue_gap)

cpue_gap %>% 
  dplyr::rename_all(tolower) %>%
  summarise(rows = length(cpue_nokm2), .by = year) %>% 
  tidytable::left_join(vroom::vroom(here::here('data', 'cpue_ebs.csv')) %>% 
                         summarise(rows_iss = length(numcpue), .by = year)) %>% 
  tidytable::mutate(diff = rows - rows_iss)

# # computing cpue from catch data
# catch_iss <- vroom::vroom(here::here('data', 'catch_ebs.csv'))
# cpue_iss <- vroom::vroom(here::here('data', 'cpue_ebs.csv'))
# 
# catch_iss %>% 
#   tidytable::mutate(numcpue = count / (distance_fished_km * (0.001 * net_width_m))) %>% 
#   tidytable::select(-count, -distance_fished_km, -net_width_m) -> cpue_iss_calc
# 
# # filling in 0's like gapindex
# tidytable::expand_grid(hauljoin = unique(cpue_iss$hauljoin), species_code = spp_codes) %>% 
#   tidytable::left_join(cpue_iss %>% 
#                          tidytable::select(-species_code, -numcpue) %>% 
#                          tidytable::slice_head(n = 1, .by = c(year, survey, stratum, lat_mid, long_mid)), .by = hauljoin) %>% 
#   tidytable::left_join(cpue_iss_calc %>% 
#                          tidytable::replace_na(list(numcpue = -1))) %>% 
#   tidytable::replace_na(list(numcpue = 0)) -> cpue_iss_calc_0
# 
# # check 0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 == 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue == 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # check non0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 > 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue > 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # chack all
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss) %>% 
#   print(n = 41)



# check nbs data ----
year_set = seq(1979,2023)
survey_set = "NBS"
spp_codes = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
query = FALSE

if(isTRUE(query)){
  gapdata <- gapindex::get_data(year_set = year_set,
                                survey_set = survey_set,
                                spp_codes = spp_codes,
                                pull_lengths = TRUE)
  
  saveRDS(gapdata, here::here('dev', 'gapindex', 'data', 'gapdata_nbs.RDS'))
} else{
  gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_nbs.RDS'))
}


# check specimen data
nrow(vroom::vroom(here::here('data', 'specimen_nbs.csv')) %>% 
       tidytable::drop_na())
nrow(gapdata$specimen)

# check length frequency data
nrow(vroom::vroom(here::here('data', 'lfreq_nbs.csv')))
nrow(gapdata$size)

# check cpue data
cpue_gap <- gapindex::calc_cpue(gapdata)

nrow(vroom::vroom(here::here('data', 'cpue_nbs.csv')))
nrow(cpue_gap)

cpue_gap %>% 
  dplyr::rename_all(tolower) %>%
  summarise(rows = length(cpue_nokm2), .by = year) %>% 
  tidytable::left_join(vroom::vroom(here::here('data', 'cpue_nbs.csv')) %>% 
                         summarise(rows_iss = length(numcpue), .by = year)) %>% 
  tidytable::mutate(diff = rows - rows_iss)

# # computing cpue from catch data
# catch_iss <- vroom::vroom(here::here('data', 'catch_nbs.csv'))
# cpue_iss <- vroom::vroom(here::here('data', 'cpue_nbs.csv'))
# 
# catch_iss %>% 
#   tidytable::mutate(numcpue = count / (distance_fished_km * (0.001 * net_width_m))) %>% 
#   tidytable::select(-count, -distance_fished_km, -net_width_m) -> cpue_iss_calc
# 
# # filling in 0's like gapindex
# tidytable::expand_grid(hauljoin = unique(cpue_iss$hauljoin), species_code = spp_codes) %>% 
#   tidytable::left_join(cpue_iss %>% 
#                          tidytable::select(-species_code, -numcpue) %>% 
#                          tidytable::slice_head(n = 1, .by = c(year, survey, stratum, lat_mid, long_mid)), .by = hauljoin) %>% 
#   tidytable::left_join(cpue_iss_calc %>% 
#                          tidytable::replace_na(list(numcpue = -1))) %>% 
#   tidytable::replace_na(list(numcpue = 0)) -> cpue_iss_calc_0
# 
# # check 0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 == 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue == 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # check non0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 > 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue > 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # chack all
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)

# check ebs slope data ----
year_set = seq(2002,2023)
survey_set = "BSS"
spp_codes = c(10110, 10112, 10115,30060)
query = FALSE

if(isTRUE(query)){
  gapdata <- gapindex::get_data(year_set = year_set,
                                survey_set = survey_set,
                                spp_codes = spp_codes,
                                pull_lengths = TRUE)
  
  saveRDS(gapdata, here::here('dev', 'gapindex', 'data', 'gapdata_bss.RDS'))
} else{
  gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_bss.RDS'))
}


# check specimen data
nrow(vroom::vroom(here::here('data', 'specimen_ebs_slope.csv')) %>% 
       tidytable::drop_na())
nrow(gapdata$specimen)

# check length frequency data
nrow(vroom::vroom(here::here('data', 'lfreq_ebs_slope.csv')))
nrow(gapdata$size)

# check cpue data
cpue_gap <- gapindex::calc_cpue(gapdata)

nrow(vroom::vroom(here::here('data', 'cpue_ebs_slope.csv')))
nrow(cpue_gap)

cpue_gap %>% 
  dplyr::rename_all(tolower) %>%
  summarise(rows = length(cpue_nokm2), .by = year) %>% 
  tidytable::left_join(vroom::vroom(here::here('data', 'cpue_ebs_slope.csv')) %>% 
                         summarise(rows_iss = length(numcpue), .by = year)) %>% 
  tidytable::mutate(diff = rows - rows_iss)

# # computing cpue from catch data
# catch_iss <- vroom::vroom(here::here('data', 'catch_ebs_slope.csv'))
# cpue_iss <- vroom::vroom(here::here('data', 'cpue_ebs_slope.csv'))
# 
# catch_iss %>% 
#   tidytable::mutate(numcpue = count / (distance_fished_km * (0.001 * net_width_m))) %>% 
#   tidytable::select(-count, -distance_fished_km, -net_width_m) -> cpue_iss_calc
# 
# # filling in 0's like gapindex
# tidytable::expand_grid(hauljoin = unique(cpue_iss$hauljoin), species_code = spp_codes) %>% 
#   tidytable::left_join(cpue_iss %>% 
#                          tidytable::select(-species_code, -numcpue) %>% 
#                          tidytable::slice_head(n = 1, .by = c(year, survey, stratum, lat_mid, long_mid)), .by = hauljoin) %>% 
#   tidytable::left_join(cpue_iss_calc %>% 
#                          tidytable::replace_na(list(numcpue = -1))) %>% 
#   tidytable::replace_na(list(numcpue = 0)) -> cpue_iss_calc_0
# 
# # check 0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 == 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue == 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # check non0's
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>% 
#   filter(cpue_nokm2 > 0) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          filter(numcpue > 0) %>%
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)
# 
# # chack all
# cpue_gap %>% 
#   dplyr::rename_all(tolower) %>%
#   summarise(rows = length(cpue_nokm2), .by = year) %>% 
#   tidytable::left_join(cpue_iss_calc_0 %>% 
#                          summarise(rows_iss = length(numcpue), .by = year)) %>% 
#   tidytable::mutate(diff = rows - rows_iss)

