# example script to obtain age/length input sample size for production run

# load surveyISS library
#devtools::install_github("afsc-assessments/surveyISS", force = TRUE)
#library(surveyISS)

# load/source libraries/functions for testing ----
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
# iters = 500
iters = 10

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# gulf of alaska ----
region = 'goa'
yrs = 1990
species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
# species = c(21740, 30060) # pollock and pop for testing

# pull data for Tier 3 species in Gulf of Alaska (1990 on)
cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .cpue
lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .lfreq
specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .specimen
read_test %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .read_test

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

srvy_iss(iters = iters, 
         lfreq_data = .lfreq,
         specimen_data = .specimen, 
         cpue_data = .cpue, 
         strata_data = strata, 
         r_t = .read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE, 
         al_var = TRUE, 
         al_var_ann = TRUE, 
         age_err = TRUE,
         use_gapindex = TRUE,
         by_strata = FALSE,
         global = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'prodtest')

## run for goa pollock (west of 140) ----
cpue %>% 
  tidytable::filter(species_code %in% c(21740)) -> .cpue_poll
lfreq %>% 
  tidytable::filter(species_code %in% c(21740)) -> .lfreq_poll
specimen %>% 
  tidytable::filter(species_code %in% c(21740)) -> .specimen_poll
read_test %>% 
  tidytable::filter(species_code %in% c(21740)) -> .read_test_poll

srvy_iss_w140(iters = iters, 
              lfreq_data = .lfreq_poll,
              specimen_data = .specimen_poll, 
              cpue_data = .cpue_poll, 
              strata_data = strata, 
              r_t = .read_test_poll, 
              yrs = yrs, 
              bin = 1, 
              boot_hauls = TRUE, 
              boot_lengths = TRUE, 
              boot_ages = TRUE, 
              al_var = TRUE, 
              al_var_ann = TRUE, 
              age_err = TRUE,
              use_gapindex = TRUE,
              by_strata = FALSE,
              global = FALSE,
              region = region, 
              save_interm = FALSE, 
              save = 'prodtest')

## run for goa rougheye-blackspotted stock complex ----
cpue %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
lfreq %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
specimen %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs
read_test %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .read_test_rebs

srvy_iss_goa_cmplx(iters = iters, 
                   lfreq_data = .lfreq_rebs, 
                   specimen_data = .specimen_rebs, 
                   cpue_data = .cpue_rebs, 
                   strata_data = strata, 
                   r_t = .read_test_rebs, 
                   yrs = yrs, 
                   bin = 1, 
                   boot_hauls = TRUE, 
                   boot_lengths = TRUE, 
                   boot_ages = TRUE, 
                   al_var = TRUE, 
                   al_var_ann = TRUE, 
                   age_err = TRUE,
                   cmplx_code = 3005012,
                   use_gapindex = TRUE,
                   by_strata = FALSE,
                   global = FALSE,
                   cmplx = 'rebs',
                   region = region, 
                   save_interm = FALSE, 
                   save = 'prodtest')

## run for goa dusky stock (has different historical species codes) ----
cpue %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .cpue_dr
lfreq %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .lfreq_dr
specimen %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .specimen_dr
read_test %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .read_test_dr

srvy_iss_goa_cmplx(iters = iters, 
                   lfreq_data = .lfreq_dr, 
                   specimen_data = .specimen_dr, 
                   cpue_data = .cpue_dr, 
                   strata_data = strata, 
                   r_t = .read_test_dr, 
                   yrs = yrs, 
                   bin = 1, 
                   boot_hauls = TRUE, 
                   boot_lengths = TRUE, 
                   boot_ages = TRUE, 
                   al_var = TRUE, 
                   al_var_ann = TRUE, 
                   age_err = TRUE,
                   cmplx_code = 301502,
                   use_gapindex = TRUE,
                   by_strata = FALSE,
                   global = FALSE,
                   cmplx = 'dr',
                   region = region, 
                   save_interm = FALSE, 
                   save = 'prodtest')

## run for goa northern/southern rock sole ----
cpue %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .cpue_nsrs
lfreq %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .lfreq_nsrs
specimen %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .specimen_nsrs
read_test %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .read_test_nsrs

srvy_iss_goa_w_c_e(iters = iters, 
                   lfreq_data = .lfreq_nsrs, 
                   specimen_data = .specimen_nsrs, 
                   cpue_data = .cpue_nsrs, 
                   strata_data = strata,
                   r_t = .read_test_nsrs, 
                   yrs = yrs, 
                   bin = 1, 
                   boot_hauls = TRUE,
                   boot_lengths = TRUE,
                   boot_ages = TRUE,
                   al_var = TRUE,
                   al_var_ann = TRUE,
                   age_err = TRUE,
                   use_gapindex = TRUE,
                   by_strata = FALSE,
                   global = FALSE,
                   region = region,
                   save_interm = FALSE,
                   save = 'prodtest')

## run for goa rex sole ----
cpue %>% 
  tidytable::filter(species_code %in% c(10200)) -> .cpue_rex
lfreq %>% 
  tidytable::filter(species_code %in% c(10200)) -> .lfreq_rex
specimen %>% 
  tidytable::filter(species_code %in% c(10200)) -> .specimen_rex
read_test %>% 
  tidytable::filter(species_code %in% c(10200)) -> .read_test_rex

srvy_iss_goa_wc_e(iters = iters, 
                  lfreq_data = .lfreq_rex, 
                  specimen_data = .specimen_rex, 
                  cpue_data = .cpue_rex,
                  strata_data = strata,
                  r_t = .read_test_rex, 
                  yrs = yrs,
                  bin = 1, 
                  boot_hauls = TRUE,
                  boot_lengths = TRUE,
                  boot_ages = TRUE,
                  al_var = TRUE,
                  al_var_ann = TRUE,
                  age_err = TRUE,
                  use_gapindex = TRUE,
                  by_strata = FALSE,
                  global = FALSE,
                  region = region,
                  save_interm = FALSE,
                  save = 'prodtest')


# aleutian islands ----
region = 'ai'
yrs = 1991
species = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)

cpue <- vroom::vroom(here::here('data', 'cpue_ai.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_ai.csv'))
strata <- vroom::vroom(here::here('data', 'strata_ai.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_ai.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

## run for all species (and subsetting out species cases so we don't have two places with those results) ----
cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .cpue
lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .lfreq
specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .specimen
read_test %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .read_test

srvy_iss(iters = iters, 
         lfreq_data = .lfreq,
         specimen_data = .specimen, 
         cpue_data = .cpue, 
         strata_data = strata, 
         r_t = .read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE, 
         al_var = TRUE, 
         al_var_ann = TRUE, 
         age_err = TRUE,
         use_gapindex = TRUE,
         by_strata = FALSE,
         global = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'prodtest')

## Run for ai blackspotted-rougheye stock complex ----
cpue %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_bsre
lfreq %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_bsre
specimen %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_bsre
read_test %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .read_test_bsre

srvy_iss_ai_cmplx(iters = iters, 
                  lfreq_data = .lfreq_bsre, 
                  specimen_data = .specimen_bsre, 
                  cpue_data = .cpue_bsre, 
                  strata_data = strata, 
                  r_t = .read_test_bsre, 
                  yrs = yrs, 
                  bin = 1, 
                  boot_hauls = TRUE, 
                  boot_lengths = TRUE, 
                  boot_ages = TRUE, 
                  al_var = TRUE, 
                  al_var_ann = TRUE, 
                  age_err = TRUE,
                  cmplx_code = 3005012,
                  use_gapindex = TRUE,
                  by_strata = FALSE,
                  global = FALSE,
                  cmplx = 'bsre',
                  region = region, 
                  save_interm = FALSE, 
                  save = 'prodtest')

## Run for ai northern rockfish (subregion expansion) ----
cpue %>% 
  tidytable::filter(species_code %in% c(30420)) -> .cpue_nr
lfreq %>% 
  tidytable::filter(species_code %in% c(30420)) -> .lfreq_nr
specimen %>% 
  tidytable::filter(species_code %in% c(30420)) -> .specimen_nr
read_test %>% 
  tidytable::filter(species_code %in% c(30420)) -> .read_test_nr

srvy_iss_ai_subreg(iters = iters,
                   lfreq_data = .lfreq_nr, 
                   specimen_data = .specimen_nr, 
                   cpue_data = .cpue_nr, 
                   strata_data = strata, 
                   r_t = .read_test_nr, 
                   yrs = yrs,
                   bin = 1, 
                   boot_hauls = TRUE,
                   boot_lengths = TRUE,
                   boot_ages = TRUE, 
                   al_var = TRUE, 
                   al_var_ann = TRUE, 
                   age_err = TRUE,
                   use_gapindex = TRUE,
                   by_strata = FALSE,
                   global = FALSE,
                   region = region, 
                   save_interm = FALSE, 
                   save = 'prodtest')


# ebs shelf ----
region = 'ebs'
yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)

# run for ebs stocks
cpue <- vroom::vroom(here::here('data', 'cpue_ebs.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_ebs.csv'))
strata <- vroom::vroom(here::here('data', 'strata_ebs.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_ebs.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata, 
         r_t = read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE, 
         al_var = TRUE, 
         al_var_ann = TRUE, 
         age_err = TRUE,
         use_gapindex = TRUE,
         by_strata = FALSE,
         global = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'prodtest')


# ebs slope ----
region = 'ebs_slope'
yrs = 2002
species = c(10110, 10112, 10115,30060)

# run for slope stocks
cpue_data <- vroom::vroom(here::here('data', 'cpue_ebs_slope.csv'))
lfreq_data <- vroom::vroom(here::here('data', 'lfreq_ebs_slope.csv'))
strata_data <- vroom::vroom(here::here('data', 'strata_ebs_slope.csv'))
specimen_data <- vroom::vroom(here::here('data', 'specimen_ebs_slope.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

srvy_iss(iters = iters, 
         lfreq_data = lfreq_data,
         specimen_data = specimen_data, 
         cpue_data = cpue_data, 
         strata_data = strata_data, 
         r_t = read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE, 
         al_var = TRUE, 
         al_var_ann = TRUE, 
         age_err = TRUE,
         use_gapindex = TRUE,
         by_strata = FALSE,
         global = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'prodtest')

# nbs & ebs ----
region = 'nebs'
yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)

# run for nbs & ebs stocks
cpue <- vroom::vroom(here::here('data', 'cpue_nebs.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_nebs.csv'))
strata <- vroom::vroom(here::here('data', 'strata_nebs.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_nebs.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata, 
         r_t = read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE, 
         al_var = TRUE, 
         al_var_ann = TRUE, 
         age_err = TRUE,
         use_gapindex = TRUE,
         by_strata = FALSE,
         global = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'prodtest')

# caal ----
region = 'goa'
yrs = 1990
species = c(21720)

# pull data for pcod caal
cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

# run for all species (and subsetting out special cases so we don't have two places with those results)
cpue %>% 
  tidytable::filter(species_code %in% species) -> .cpue
specimen %>% 
  tidytable::filter(species_code %in% species) -> .specimen
read_test %>% 
  tidytable::filter(species_code %in% species) -> .read_test

srvy_iss_caal(iters = iters, 
              specimen_data = .specimen, 
              cpue_data = .cpue, 
              r_t = .read_test, 
              yrs = yrs, 
              bin = 1,
              boot_hauls = TRUE, 
              boot_ages = TRUE,
              al_var = TRUE, 
              al_var_ann = TRUE, 
              age_err = TRUE,
              region = region, 
              save_interm = FALSE,
              save = 'prodtest')


# For testing run time of 500 iterations ----
if(iters < 500){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}
