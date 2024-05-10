# load/source libraries/functions
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# check goa ----

# get survey ISS output

region = 'ebs'

species_test = 21740

cpue_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv')))) %>% 
  tidytable::filter(species_code == species_test)
lfreq_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv')))) %>% 
  tidytable::filter(species_code == species_test)
strata_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv')))) %>% 
  tidytable::filter(species_code == species_test)

yrs <- 1979

# complete cases by year/length/sex/strata for all years
tidytable::expand_grid(year = unique(lfreq_data$year),
                       species_code = unique(specimen_data$species_code),
                       sex = unique(specimen_data$sex),
                       length = seq(from = min(lfreq_data$length, na.rm = TRUE), 
                                    to = max(lfreq_data$length, na.rm = TRUE), 
                                    by = 10),
                       age = seq(from = min(specimen_data$age, na.rm = TRUE), 
                                 to = max(specimen_data$age, na.rm = TRUE),
                                 by = 1)) -> lngs

###############

# first pass of filtering
data.table::setDT(cpue_data) %>%
  tidytable::filter(year >= yrs) %>% 
  tidytable::left_join(strata_data) -> cpue

data.table::setDT(lfreq_data) %>%
  tidytable::filter(year >= yrs & !is.na(frequency)) -> lfreq

lfreq %>% 
  tidytable::uncount(frequency) -> lfreq_un

lfreq_un %>% 
  tidytable::bind_rows(lfreq_un %>% 
                         tidytable::mutate(sex = 0)) -> lfreq_un

data.table::setDT(specimen_data) %>%
  tidytable::filter(year >= yrs) %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(length = round(length / 10) * 10) -> agedat

# get length pop'n
lpop_gap(lfreq_un, cpue, by_strata = TRUE) -> lpop


# start of apop fcn ----


# Calculate distribution of age proportions for a given length, `p_yklm`. This is the non-global age-length key.
# female/male/unsexed
lngs %>% 
  tidytable::left_join(agedat %>%
                         tidytable::filter(sex != 0) %>%
                         tidytable::filter(!(sex %in% c(0, 3))) %>%
                         tidytable::bind_rows(agedat %>%
                                                tidytable::filter(sex != 0) %>%
                                                tidytable::mutate(sex = 3)) %>%
                         tidytable::summarise(age_num = .N,
                                              .by = c(year, species_code, sex, length, age)) %>%
                         tidytable::mutate(age_frac = age_num/sum(age_num), 
                                           .by = c(year, species_code, sex, length))) -> .p_yklm

# Append the globally-filled lengths with the the non-global `p_yklm` alk to get a now global alk. 
# female/male/unsexed
lngs %>% 
  tidytable::left_join(.p_yklm %>% 
                         tidytable::filter(!is.na(age_frac)) %>% 
                         tidytable::bind_rows(.p_yklm %>% 
                                                # Determine missing lengths
                                                tidytable::summarise(age_frac = sum(age_frac, na.rm = TRUE), .by = c(year, species_code, sex, length)) %>% 
                                                tidytable::filter(age_frac == 0) %>% 
                                                tidytable::select(-age_frac) %>% 
                                                # for missing lengths, merge age probabilities from golbal ALK
                                                tidytable::left_join(agedat %>%
                                                                       # Aggregate specimen information over years to calculate a global ALK,
                                                                       tidytable::filter(sex != 0) %>%
                                                                       tidytable::summarise(age_num = .N,
                                                                                            .by = c(species_code, sex, length, age)) %>%
                                                                       tidytable::mutate(age_frac = age_num/sum(age_num), 
                                                                                         .by = c(species_code, sex, length))) %>% 
                                                tidytable::filter(!is.na(age_frac)))) %>% 
  tidytable::select(-age_num) %>% 
  tidytable::replace_na(list(age_frac = 0)) -> p_yklm


# compare alks

nrow(alk)
nrow(p_yklm)

alk %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, species_code, sex, length = length_mm, age, age_frac_gap = age_frac) %>% 
  tidytable::left_join(p_yklm) %>% 
  tidytable::mutate(diff = age_frac - age_frac_gap) %>% 
  tidytable::summarise(test = sum(diff))


# alks match




# Calculate numbers at age as the product of the age_frac and the numbers at length
# at strata level
  # female/male/unsexed
lpop %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::left_join(p_yklm) %>% 
  tidytable::replace_na(list(age = -9)) %>% 
  tidytable::replace_na(list(age_frac = 1)) %>% 
  tidytable::mutate(agepop = abund * age_frac) -> age_comp_srvyISS

# compare stratum level agepop

nrow(age_comp_srvyISS)
nrow(gap_age_comp_st$length_at_age)

gap_age_comp_st$length_at_age %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, species_code, stratum, sex, length = length_mm, age, age_frac_gap = age_frac, agepop_gap = agepop) %>% 
  tidytable::left_join(age_comp_srvyISS %>% 
                         tidytable::select(year, stratum, species_code, length, sex, age, age_frac, agepop)) %>% 
  tidytable::mutate(diff_af = age_frac - age_frac_gap,
                    diff_ap = agepop - agepop_gap) %>% 
  tidytable::summarise(test1 = sum(diff_af),
                       test2 = sum(diff_ap))

# compare stratum level agepop agg across lengths

age_comp_srvyISS %>% 
  tidytable::summarise(agepop = round(sum(agepop)), .by = c(year, stratum, species_code, sex, age)) -> age_comp_srvyISS_st

nrow(age_comp_srvyISS_st)
nrow(gap_age_comp_st$age_comp)

gap_age_comp_st$age_comp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, species_code, stratum, sex, age, agepop_gap = population_count) %>% 
  tidytable::left_join(age_comp_srvyISS_st) %>% 
  tidytable::mutate(diff_ap = agepop - agepop_gap) %>% 
  tidytable::summarise(test = sum(diff_ap, na.rm = TRUE))

# stratum level agepop matches






# compare region level

age_comp_srvyISS_st %>% 
  # summarize numbers at age across length, and compute mean length at region level
  tidytable::summarise(agepop = sum(agepop),
                       .by = c(year, species_code, sex, age)) -> age_comp_srvyISS_wNW

age_comp_srvyISS_st %>% 
  tidytable::filter(!(stratum %in% c(82, 90))) %>% 
  # summarize numbers at age across length, and compute mean length at region level
  tidytable::summarise(agepop = sum(agepop),
                       .by = c(year, species_code, sex, age)) -> age_comp_srvyISS_ebs

gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(area_id == 99900) %>% 
  tidytable::select(year, species_code, sex, age, agepop_gap = population_count) %>% 
  tidytable::left_join(age_comp_srvyISS_wNW) %>% 
  tidytable::mutate(diff_ap = agepop - agepop_gap) %>% 
  tidytable::summarise(test2 = sum(diff_ap), .by = year) %>% 
  print(n = 200)


gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(area_id == 99901) %>% 
  tidytable::select(year, species_code, sex, age, agepop_gap = population_count) %>% 
  tidytable::left_join(age_comp_srvyISS_ebs) %>% 
  tidytable::mutate(diff_ap = agepop - agepop_gap) %>% 
  tidytable::summarise(test2 = sum(diff_ap), .by = year) %>% 
  print(n = 200)



gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(area_id == 99900) %>% 
  tidytable::select(year, species_code, sex, age, agepop_gap = population_count) %>% 
  tidytable::left_join(t_wNW) %>% 
  tidytable::mutate(diff_ap = agepop - agepop_gap) %>% 
  tidytable::summarise(test2 = sum(diff_ap, na.rm = TRUE), .by = year) %>% 
  print(n = 200)




gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(area_id == 99901) %>% 
  tidytable::select(year, species_code, sex, age, agepop_gap = population_count) %>% 
  tidytable::left_join(t_ebs) %>% 
  tidytable::mutate(diff_ap = agepop - agepop_gap) %>% 
  tidytable::summarise(test2 = sum(diff_ap, na.rm = TRUE), .by = year) %>% 
  print(n = 200)





