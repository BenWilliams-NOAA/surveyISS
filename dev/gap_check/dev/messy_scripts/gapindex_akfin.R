# script to test match between GAP and surveyISS using GOA Pcod as example

# load/source libraries/functions
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)
library(gapindex)

# species to test
species_test = c(10110, 21720, 21740, 30060)


# compute using gapindex ----

gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))

cruise <- gapdata$cruise
haul <- gapdata$haul
gapdata$catch %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$catch
gapdata$size %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$size
gapdata$specimen %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$specimen
gapdata$species %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$species

# get cpue
cpue <- gapindex::calc_cpue(gapdata)
# get stratum pop'n
racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                        cpue)
# get pop'n at length
gap_lc <- gapindex::calc_sizecomp_stratum(gapdata,
                                          cpue,
                                          racebase_stratum_popn) 
# get age-length key
alk <- gapindex::calc_alk(gapdata,
                          unsex = "all")
# get stratum-level age pop'n
gap_age_comp_st <- gapindex::calc_agecomp_stratum(gapdata,
                                                  alk,
                                                  gap_lc)
# get regional level age pop'n
gap_ac <- gapindex::calc_agecomp_region(gapdata,
                                        gap_age_comp_st)

# rename and summarise
gap_lc %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(length_mm > 0) %>% 
  tidytable::summarise(abund_gapindex = sum(population_count), .by = c(year, species_code, length_mm)) %>% 
  tidytable::rename(length = 'length_mm') -> gap_lc
  
gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(age > 0) %>% 
  tidytable::summarise(agepop_gapindex = sum(population_count), .by = c(year, species_code, age)) -> gap_ac

# compute using surveyISS ----

region = 'goa'

cpue_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv')))) %>% 
  tidytable::filter(species_code %in% species_test)
lfreq_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv')))) %>% 
  tidytable::filter(species_code %in% species_test)
strata_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv')))) %>% 
  tidytable::filter(species_code %in% species_test)

# get age/length pop'n values following gap

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

surveyISS_gap <- srvy_comps(lfreq_data = lfreq_data, 
                            specimen_data = specimen_data, 
                            cpue_data = cpue_data, 
                            strata_data = strata_data,
                            r_t = NULL,
                            yrs = 1990, 
                            bin = 1,
                            boot_hauls = FALSE, 
                            boot_lengths = FALSE, 
                            boot_ages = FALSE,
                            al_var = FALSE,
                            al_var_ann = FALSE,
                            age_err = FALSE)
surveyISS_ac_gap <- surveyISS_gap$age %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::summarise(agepop_srvyISS_gap = sum(agepop), .by = c(year, species_code, age))
surveyISS_lc_gap <- surveyISS_gap$length %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::summarise(abund_srvyISS_gap = sum(abund), .by = c(year, species_code, length))

# get data from gap_products on akfin and old akfin data ----

gap_prod_ac <- vroom::vroom(here::here('dev', 'gapindex', 'data', 'gap_apop_full_goa.csv')) %>% 
  tidytable::filter(species_code %in% species_test & stratum == 99903 & age > 0) %>% 
  tidytable::summarise(agepop_gapproducts = sum(population_count), .by = c(year, species_code, age))

gap_prod_lc <- vroom::vroom(here::here('dev', 'gapindex', 'data', 'gap_lpop_full_goa.csv')) %>% 
  tidytable::filter(species_code %in% species_test & stratum == 99903 & length > 0) %>% 
  tidytable::summarise(abund_gapproducts = sum(population_count), .by = c(year, species_code, length))

# make comparisons ----

# length tests

# gapindex to surveyISS using gapindex fcns
gap_lc %>% 
  tidytable::left_join(surveyISS_lc_gap) %>% 
  tidytable::mutate(diff = abund_gapindex - abund_srvyISS_gap) %>% 
  tidytable::summarise(test = sum(diff), .by = c(species_code))
# note: length exactly matches

# gapindex to gap_products on akfin
gap_lc %>% 
  tidytable::left_join(gap_prod_lc) %>% 
  tidytable::mutate(diff = abund_gapindex - abund_gapproducts) %>% 
  tidytable::summarise(test = sum(diff), .by = c(species_code))
# note: length exactly matches


# age tests


# gapindex to surveyISS using gapindex fcns
gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, species_code, sex, age, population_count) %>% 
  tidytable::left_join(surveyISS_ac_gap) %>% 
  tidytable::mutate(diff = agepop - population_count) %>% 
  tidytable::summarise(test = sum(diff, na.rm = TRUE), .by = c(species_code))

gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, species_code, sex, age, population_count) %>% 
  tidytable::summarise(population_count = sum(population_count), .by = c(year, species_code, age)) %>% 
  tidytable::left_join(surveyISS_ac_gap %>% 
                         tidytable::filter(sex != 0) %>% 
                         tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age))) %>% 
  tidytable::mutate(diff = agepop - population_count) %>% 
  tidytable::summarise(test = sum(diff, na.rm = TRUE), .by = c(species_code))



# note: does not match

# gapindex to gap_products on akfin
gap_ac %>% 
  tidytable::left_join(gap_prod_ac) %>% 
  tidytable::mutate(diff = agepop_gapindex - agepop_gapproducts) %>% 
  print(n = 200)
# does not match


gapdata$stratum_groups


gapdata$strata$STRATUM

sort(unique(lpop$stratum))


gapdata$subarea



lpop %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::left_join(p_yklm) %>% 
  tidytable::filter(species_code %in% species_test) %>% 
  tidytable::replace_na(list(age = -9)) %>% 
  tidytable::replace_na(list(age_frac = 1)) %>% 
  tidytable::mutate(agepop = abund * age_frac) %>% 
  tidytable::select(-age_frac, -abund) %>% 
  tidytable::summarise(agepop = round(sum(agepop)), .by = c(year, species_code, stratum, sex, age)) %>% 
  tidytable::left_join(gap_age_comp_st$age_comp %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::filter(species_code %in% species_test) %>% 
                         tidytable::select(year, species_code, sex, stratum, agepop_gap = population_count, age) %>% 
                         tidytable::summarise(agepop_gap = sum(agepop_gap), .by = c(year, species_code, stratum, sex, age))) %>% 
  tidytable::mutate(diff_abund = agepop - agepop_gap) %>% 
  tidytable::summarise(test_ab = sum(diff_abund, na.rm = TRUE), .by = c(species_code))
# when lpop calculated at stratum level, it matches exactly with gapindex
# when calc'd at region level, there are rounding differences (as gapindex rounds sum across lengths at stratum level)




# getting at gapindex differences in calc_agecomp_region

survey_designs <- gapdata$survey
strata <- gapdata$strata
age_comps <- gap_age_comp_st$age_comp
count_length_age <- gap_age_comp_st$length_at_age

region_age_comp_df <- data.frame()

isurvey = 1

subareas <- subset(x = gapdata$subarea,
                   subset = SURVEY_DEFINITION_ID == 
                     survey_designs$SURVEY_DEFINITION_ID[isurvey] &
                     AREA_TYPE == "REGION" &
                     DESIGN_YEAR == survey_designs$DESIGN_YEAR[isurvey])

iregion = 1

strata_in_iregion <- 
  subset(x = gapdata$stratum_groups,
         subset = AREA_ID %in% subareas$AREA_ID[iregion])$STRATUM

age_comp_iregion_by_strata <- 
  subset(x = age_comps,
         subset = SURVEY_DEFINITION_ID == 
           subareas$SURVEY_DEFINITION_ID[iregion] &
           STRATUM %in% strata_in_iregion)


age_comp_iregion <-
  stats::aggregate(POPULATION_COUNT ~ SURVEY + YEAR + SPECIES_CODE + 
                     SEX + AGE,
                   data = age_comp_iregion_by_strata,
                   FUN = sum) %>% 
  tidytable::filter(AGE > 0)



lpop %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::left_join(p_yklm) %>% 
  tidytable::filter(species_code %in% species_test) %>% 
  tidytable::replace_na(list(age = -9)) %>% 
  tidytable::replace_na(list(age_frac = 1)) %>% 
  tidytable::mutate(agepop = abund * age_frac) %>% 
  tidytable::select(-age_frac, -abund) %>% 
  tidytable::summarise(agepop = round(sum(agepop)), .by = c(year, species_code, stratum, sex, age)) -> age_comp_iregion_by_strata_t

age_comp_iregion_by_strata_t %>% 
  tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, sex, age)) %>% 
  tidytable::left_join(age_comp_iregion %>% 
                         dplyr::rename_all(tolower)) %>% 
  tidytable::mutate(diff = agepop - population_count) %>% 
  tidytable::summarise(test = sum(diff, na.rm = TRUE), .by = species_code)


gap_age_comp_st$age_comp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(species_code %in% species_test) %>% 
  tidytable::select(year, species_code, sex, stratum, agepop_gap = population_count, age) %>% 
  tidytable::summarise(agepop_gap = sum(agepop_gap), .by = c(year, species_code, sex, age)) %>% 
  tidytable::filter(age > 0) %>% 
  tidytable::left_join(age_comp_iregion %>% 
                         dplyr::rename_all(tolower)) %>% 
  tidytable::mutate(diff = agepop_gap - population_count) %>% 
  tidytable::summarise(test = sum(diff), .by = species_code)






nrow(age_comp_iregion_by_strata)
nrow(gap_age_comp_st$age_comp)

age_comp_iregion_by_strata %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(stratum, year, species_code, sex, age, agepop_t = population_count) %>% 
  tidytable::left_join(gap_age_comp_st$age_comp %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(stratum, year, species_code, sex, age, agepop = population_count)) %>% 
  tidytable::mutate(diff = agepop - agepop_t) %>% 
  tidytable::summarise(test = sum(diff), .by = species_code)







