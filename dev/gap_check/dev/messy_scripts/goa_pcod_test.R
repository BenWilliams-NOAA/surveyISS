# script to test match between GAP and surveyISS using GOA Pcod as example

# load/source libraries/functions
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)
library(gapindex)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# species to test
species_test = 21720


# compute using gapindex ----

gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))

cruise <- gapdata$cruise
haul <- gapdata$haul
gapdata$catch %>% 
  tidytable::filter(SPECIES_CODE == species_test) -> gapdata$catch
gapdata$size %>% 
  tidytable::filter(SPECIES_CODE == species_test) -> gapdata$size
gapdata$specimen %>% 
  tidytable::filter(SPECIES_CODE == species_test) -> gapdata$specimen
gapdata$species %>% 
  tidytable::filter(SPECIES_CODE == species_test) -> gapdata$species

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
  tidytable::filter(species_code == species_test)
lfreq_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv')))) %>% 
  tidytable::filter(species_code == species_test)
strata_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv')))) %>% 
  tidytable::filter(species_code == species_test)

# get age/length pop'n values following gap
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

# get age/length pop'n values following orig method
surveyISS <- srvy_comps(lfreq_data = lfreq_data, 
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
surveyISS_ac <- surveyISS$age %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::summarise(agepop_srvyISS = sum(agepop), .by = c(year, species_code, age))
surveyISS_lc <- surveyISS$length %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::summarise(abund_srvyISS = sum(abund), .by = c(year, species_code, length))




# get data from gap_products on akfin and old akfin data ----

gap_prod_ac <- vroom::vroom(here::here('dev', 'gapindex', 'data', 'gap_apop_full_goa.csv')) %>% 
  tidytable::filter(species_code == species_test & stratum == 99903 & age > 0) %>% 
  tidytable::summarise(agepop_gapproducts = sum(population_count), .by = c(year, species_code, age))

gap_prod_lc <- vroom::vroom(here::here('dev', 'gapindex', 'data', 'gap_lpop_full_goa.csv')) %>% 
  tidytable::filter(species_code == species_test & stratum == 99903 & length > 0) %>% 
  tidytable::summarise(abund_gapproducts = sum(population_count), .by = c(year, species_code, length))


gap_old_ac <- vroom::vroom(here::here('dev', 'gapindex', 'data', 'old_bts_ac_goapcod.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::mutate(species_code = species_test) %>% 
  tidytable::rename(agepop_old = 'agepop')


# make comparions ----

# length tests

# gapindex to surveyISS using gapindex fcns
gap_lc %>% 
  tidytable::left_join(surveyISS_lc_gap) %>% 
  tidytable::mutate(diff = abund_gapindex - abund_srvyISS_gap) %>% 
  tidytable::summarise(test = sum(diff))
# note: length exactly matches

# gapindex to gap_products on akfin
gap_lc %>% 
  tidytable::left_join(gap_prod_lc) %>% 
  tidytable::mutate(diff = abund_gapindex - abund_gapproducts) %>% 
  tidytable::summarise(test = sum(diff))
# note: length exactly matches







# age tests


# gapindex to surveyISS using gapindex fcns
gap_ac %>% 
  tidytable::left_join(surveyISS_ac_gap) %>% 
  tidytable::mutate(diff = agepop_gapindex - agepop_srvyISS_gap) %>% 
  tidytable::summarise(test = sum(diff))
# note: does not match

# gapindex to surveyISS using original fcns (age only)
gap_ac %>% 
  tidytable::left_join(surveyISS_ac) %>% 
  tidytable::mutate(diff = agepop_gapindex - agepop_srvyISS) %>% 
  tidytable::summarise(test = sum(diff))
# note: does not match, missing ages due to not using global alk (resulting in NA)

# gapindex to gap_products on akfin
gap_ac %>% 
  tidytable::left_join(gap_prod_ac) %>% 
  tidytable::mutate(diff = agepop_gapindex - agepop_gapproducts) %>% 
  tidytable::summarise(test = sum(diff))
# does not match

# gap_products on akfin to last year's data
gap_prod_ac %>% 
  tidytable::left_join(gap_old_ac) %>% 
  tidytable::mutate(diff = agepop_gapproducts - agepop_old)
# does not match

# gapindex to last year's data
gap_ac %>% 
  tidytable::left_join(gap_old_ac) %>% 
  tidytable::mutate(diff = agepop_gapindex - agepop_old)
# does not match
  
  
  
  

