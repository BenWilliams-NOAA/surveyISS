# script to test match between gapindex and gap_products

# load/source libraries/functions
library(tidyverse)
library(tidytable)
library(gapindex)
library(afscdata)
library(DBI)

# species/region/year to test
species_test = c(10110, 21720, 21740, 30060)
region = 'goa'
yrs = 1990
survey = 47
stratum = 99903

# compute using gapindex ----

# get data
gapdata <- gapindex::get_data(year_set = seq(yrs, 2025),
                              survey_set = toupper(region),
                              spp_codes = species_test,
                              pull_lengths = TRUE)

# get cpue
cpue <- gapindex::calc_cpue(gapdata)

# get stratum pop'n
racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                        cpue)

# get stratum pop'n at length
gap_lc <- gapindex::calc_sizecomp_stratum(gapdata,
                                          cpue,
                                          racebase_stratum_popn) 

# get age-length key
alk <- gapindex::calc_alk(gapdata,
                          unsex = "all",
                          global = FALSE)

# get stratum-level age pop'n
gap_age_comp_st <- gapindex::calc_agecomp_stratum(gapdata,
                                                  alk,
                                                  gap_lc)

# get regional level age pop'n
gap_ac <- gapindex::calc_agecomp_region(gapdata,
                                        gap_age_comp_st)

# rename and sum over strata
gap_lc %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::summarise(abund_gapindex = sum(population_count), .by = c(year, species_code, sex, length_mm)) %>% 
  tidytable::rename(length = 'length_mm') -> gapindx_sizecomp
  
gap_ac %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::summarise(agepop_gapindex = sum(population_count), .by = c(year, species_code, sex, age)) -> gapindx_agecomp


# get data from gap_products on akfin ----

# get connected to akfin
db = 'akfin'
conn = afscdata::connect(db)

# pull akfin_sizecomp table
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_sizecomp')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                species_code %in% species_test,
                year >= yrs,
                area_id == stratum) %>% 
  dplyr::select(year, species_code, sex, length = length_mm, abund_gapprod = population_count) %>% 
  collect() -> gapprod_sizecomp

# pull akfin_agecomp table
dplyr::tbl(conn, dplyr::sql('gap_products.akfin_agecomp')) %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::filter(survey_definition_id %in% survey,
                species_code %in% species_test,
                year >= yrs,
                area_id == stratum) %>% 
  dplyr::select(year, species_code, sex, age, agepop_gapprod = population_count) %>% 
  collect() -> gapprod_agecomp


# make comparisons ----

# sizecomp test

# gapindex to gap_products on akfin
gapindx_sizecomp %>% 
  tidytable::left_join(gapprod_sizecomp) %>% 
  tidytable::mutate(diff = abund_gapindex - abund_gapprod) %>% 
  # print(n = 250)
  tidytable::summarise(test = sum(diff, na.rm = TRUE), .by = species_code)
# note: length exactly matches, 
#   but, some years in gapindex but not in gap_products, like 1990 for arrowtooth 


# agecomp test

# gapindex to gap_products on akfin
gapindx_agecomp %>% 
  tidytable::left_join(gapprod_agecomp) %>% 
  tidytable::mutate(diff = agepop_gapindex - agepop_gapprod) %>% 
  # print(n = 250)
  tidytable::summarise(test = sum(diff, na.rm = TRUE), .by = species_code)
# note: does not match - some years in gapindex but not gap_products, some ages in gapindex but not gap_products


