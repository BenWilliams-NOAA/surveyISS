# example script to obtain age/length input sample size for production run

# load surveyISS library ----
# devtools::unload('surveyISS')
# devtools::install_github("BenWilliams-NOAA/surveyISS", force = TRUE)
library(surveyISS)

## load/source libraries/functions for testing ----
# library(purrr)
# library(tidyverse)
# library(tidytable)
# library(psych)
# library(vroom)
# library(here)
# 
# source_files <- list.files(here::here("R"), "*.R$")
# map(here::here("R", source_files), source)

# set iterations ----
iters = 2

# for testing stuff ----
bin = 5
plus_len = 20
plus_age = 8

# get data ----
# if query = TRUE then will run data queries, if FALSE will read previously run data
# set = TRUE if first time running, or if data has changed

data <- surveyISS::query_data_t3(query = FALSE)

data_goa <- data$data_goa
data_ai <- data$data_ai

# gulf of alaska ----

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
data_goa$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .specimen
strata_data <- data_goa$strata

# for testing
# lfreq_data = .lfreq
# specimen_data = .specimen
# cpue_data = .cpue
# strata_data = strata
# r_t = .read_test
# bin = 1
# boot_hauls = TRUE
# boot_lengths = TRUE
# boot_ages = TRUE
# al_var = TRUE
# al_var_ann = TRUE
# age_err = TRUE
# use_gapindex = TRUE
# by_strata = TRUE
# global = FALSE

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = .lfreq,
                    specimen_data = .specimen, 
                    cpue_data = .cpue, 
                    strata_data = strata_data,
                    yrs = 1990, 
                    bin = bin,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    plus_len = plus_len,
                    plus_age = plus_age,
                    region = 'goa', 
                    save = 'prodtest')

## run for goa pollock (west of 140) ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(21740)) -> .cpue_poll
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(21740)) -> .lfreq_poll
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(21740)) -> .specimen_poll

surveyISS::srvy_iss_w140(iters = iters, 
                         lfreq_data = .lfreq_poll,
                         specimen_data = .specimen_poll, 
                         cpue_data = .cpue_poll, 
                         strata_data = strata_data,
                         yrs = 1990,  
                         bin = bin,
                         boot_hauls = TRUE, 
                         boot_lengths = TRUE, 
                         boot_ages = TRUE, 
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         plus_len = plus_len,
                         plus_age = plus_age,
                         region = 'goa', 
                         save = 'prodtest')

## run for goa rougheye-blackspotted stock complex ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs

surveyISS::srvy_iss_goa_cmplx(iters = iters, 
                              lfreq_data = .lfreq_rebs, 
                              specimen_data = .specimen_rebs, 
                              cpue_data = .cpue_rebs, 
                              strata_data = strata_data, 
                              yrs = 1990,  
                              bin = bin,
                              boot_hauls = TRUE, 
                              boot_lengths = TRUE, 
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              plus_len = plus_len,
                              plus_age = plus_age,
                              cmplx_code = 3005012,
                              cmplx = 'rebs',
                              region = 'goa', 
                              save = 'prodtest')

## run for goa northern/southern rock sole ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .cpue_nsrs
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .lfreq_nsrs
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .specimen_nsrs

surveyISS::srvy_iss_goa_w_c_e(iters = iters, 
                              lfreq_data = .lfreq_nsrs, 
                              specimen_data = .specimen_nsrs, 
                              cpue_data = .cpue_nsrs, 
                              strata_data = strata_data, 
                              yrs = 1990, 
                              bin = bin,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE,
                              al_var = TRUE,
                              al_var_ann = TRUE,
                              age_err = TRUE,
                              plus_len = plus_len,
                              plus_age = plus_age,
                              region = 'goa',
                              save = 'prodtest')

## run for goa rex sole ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(10200)) -> .cpue_rex
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(10200)) -> .lfreq_rex
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(10200)) -> .specimen_rex

surveyISS::srvy_iss_goa_wc_e(iters = iters, 
                             lfreq_data = .lfreq_rex, 
                             specimen_data = .specimen_rex, 
                             cpue_data = .cpue_rex,
                             strata_data = strata_data, 
                             yrs = 1990, 
                             bin = bin,
                             boot_hauls = TRUE,
                             boot_lengths = TRUE,
                             boot_ages = TRUE,
                             al_var = TRUE,
                             al_var_ann = TRUE,
                             age_err = TRUE,
                             plus_len = plus_len,
                             plus_age = plus_age,
                             region = 'goa',
                             save = 'prodtest')


# aleutian islands ----
strata_data <- data_ai$strata

## run for ai blackspotted-rougheye stock complex ----
data_ai$cpue %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_bsre
data_ai$lfreq %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_bsre
data_ai$specimen %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_bsre

surveyISS::srvy_iss_ai_cmplx(iters = iters, 
                             lfreq_data = .lfreq_bsre, 
                             specimen_data = .specimen_bsre, 
                             cpue_data = .cpue_bsre, 
                             strata_data = strata_data,
                             yrs = 1991, 
                             bin = bin,
                             boot_hauls = TRUE, 
                             boot_lengths = TRUE, 
                             boot_ages = TRUE, 
                             al_var = TRUE, 
                             al_var_ann = TRUE, 
                             age_err = TRUE,
                             plus_len = plus_len,
                             plus_age = plus_age,
                             cmplx_code = 3005012,
                             cmplx = 'bsre',
                             region = 'ai',  
                             save = 'prodtest')

## run for ai northern rockfish (subregion expansion) ----
data_ai$cpue %>% 
  tidytable::filter(species_code %in% c(30420)) -> .cpue_nr
data_ai$lfreq %>% 
  tidytable::filter(species_code %in% c(30420)) -> .lfreq_nr
data_ai$specimen %>% 
  tidytable::filter(species_code %in% c(30420)) -> .specimen_nr

surveyISS::srvy_iss_ai_subreg(iters = iters,
                              lfreq_data = .lfreq_nr, 
                              specimen_data = .specimen_nr, 
                              cpue_data = .cpue_nr, 
                              strata_data = strata_data, 
                              yrs = 1991, 
                              bin = bin,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              plus_len = plus_len,
                              plus_age = plus_age,
                              region = 'ai',  
                              save = 'prodtest')

# caal ----

## run for goa pcod example ----
region = 'goa'
species = c(21720)

# filter data
data_goa$cpue %>% 
  tidytable::filter(species_code %in% species) -> .cpue
data_goa$specimen %>% 
  tidytable::filter(species_code %in% species) -> .specimen

surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = .specimen, 
                         cpue_data = .cpue, 
                         yrs = 1990, 
                         bin = bin,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         plus_len = plus_len,
                         plus_age = plus_age,
                         region = region, 
                         save = 'prodtest')


