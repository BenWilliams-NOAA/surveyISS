# example script to obtain age/length input sample size for production run

# load surveyISS library ----
devtools::install_github("BenWilliams-NOAA/surveyISS")
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
# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
# iters = 500
iters = 10

# get data ----
# if query = TRUE then will run data queries, if FALSE will read previously run data
# set = TRUE if first time running, or if data has changed
query = TRUE

## gulf of alaska ----
region = 'goa'
yrs = 1990
species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
# species = c(21740, 30060) # pollock and pop for testing
survey = 47

if(isTRUE(query)){
  data_goa <- surveyISS::query_data(survey = survey,
                                    region = region,
                                    species = species,
                                    yrs = yrs)
  
  saveRDS(data, file = here::here('data', region, 'data.RDS'))
} else{
  data_goa <- readRDS(file = here::here('data', region, 'data.RDS'))
}

## aleutian islands ----
region = 'ai'
yrs = 1991
species = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)
survey = 52

if(isTRUE(query)){
  data_ai <- surveyISS::query_data(survey = survey,
                                   region = region,
                                   species = species,
                                   yrs = yrs)
  
  saveRDS(data, file = here::here('data', region, 'data.RDS'))
} else{
  data_ai <- readRDS(file = here::here('data', region, 'data.RDS'))
}

## ebs slope ----
region = 'ebs_slope'
yrs = 2002
species = c(10110, 10112, 10115,30060)
survey = 78

if(isTRUE(query)){
  data_ebss <- surveyISS::query_data(survey = survey,
                                     region = region,
                                     species = species,
                                     yrs = yrs)
  
  saveRDS(data, file = here::here('data', region, 'data.RDS'))
} else{
  data_ebss <- readRDS(file = here::here('data', region, 'data.RDS'))
}

## ebs ----
region = 'ebs'
yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
survey = 98

if(isTRUE(query)){
  data_ebs <- surveyISS::query_data(survey = survey,
                                    region = region,
                                    species = species,
                                    yrs = yrs)
  
  saveRDS(data, file = here::here('data', region, 'data.RDS'))
} else{
  data_ebs <- readRDS(file = here::here('data', region, 'data.RDS'))
}


## nbs ----
region = 'nbs'
yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
survey = 143

if(isTRUE(query)){
  data_nbs <- surveyISS::query_data(survey = survey,
                                    region = region,
                                    species = species,
                                    yrs = yrs)
  
  saveRDS(data, file = here::here('data', region, 'data.RDS'))
} else{
  data_nbs <- readRDS(file = here::here('data', region, 'data.RDS'))
}

## ebs & nbs ----
region = 'nebs'
yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
survey = c(98, 143)

if(isTRUE(query)){
  data_nebs <- surveyISS::query_data(survey = survey,
                                     region = region,
                                     species = species,
                                     yrs = yrs)
  
  saveRDS(data, file = here::here('data', region, 'data.RDS'))
} else{
  data_nebs <- readRDS(file = here::here('data', region, 'data.RDS'))
}


# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# gulf of alaska ----

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
data_goa$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .specimen
strata_data <- data_goa$strata




surveyISS::r_t

read_test %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .read_test

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
                    r_t = .read_test, 
                    yrs = yrs,  
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = region, 
                    save = 'prodtest')

## run for goa pollock (west of 140) ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(21740)) -> .cpue_poll
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(21740)) -> .lfreq_poll
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(21740)) -> .specimen_poll




read_test %>% 
  tidytable::filter(species_code %in% c(21740)) -> .read_test_poll

surveyISS::srvy_iss_w140(iters = iters, 
                         lfreq_data = .lfreq_poll,
                         specimen_data = .specimen_poll, 
                         cpue_data = .cpue_poll, 
                         strata_data = strata_data, 
                         r_t = .read_test_poll, 
                         yrs = yrs, 
                         boot_hauls = TRUE, 
                         boot_lengths = TRUE, 
                         boot_ages = TRUE, 
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = region, 
                         save = 'prodtest')

## run for goa rougheye-blackspotted stock complex ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs




read_test %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .read_test_rebs

surveyISS::srvy_iss_goa_cmplx(iters = iters, 
                              lfreq_data = .lfreq_rebs, 
                              specimen_data = .specimen_rebs, 
                              cpue_data = .cpue_rebs, 
                              strata_data = strata_data, 
                              r_t = .read_test_rebs, 
                              yrs = yrs, 
                              boot_hauls = TRUE, 
                              boot_lengths = TRUE, 
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              cmplx_code = 3005012,
                              cmplx = 'rebs',
                              region = region, 
                              save = 'prodtest')

## run for goa dusky stock (has different historical species codes) ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .cpue_dr
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .lfreq_dr
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .specimen_dr




read_test %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .read_test_dr

surveyISS::srvy_iss_goa_cmplx(iters = iters, 
                              lfreq_data = .lfreq_dr, 
                              specimen_data = .specimen_dr, 
                              cpue_data = .cpue_dr, 
                              strata_data = strata_data, 
                              r_t = .read_test_dr, 
                              yrs = yrs, 
                              boot_hauls = TRUE, 
                              boot_lengths = TRUE, 
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              cmplx_code = 301502,
                              cmplx = 'dr',
                              region = region,  
                              save = 'prodtest')

## run for goa northern/southern rock sole ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .cpue_nsrs
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .lfreq_nsrs
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .specimen_nsrs



read_test %>% 
  tidytable::filter(species_code %in% c(10261, 10262)) -> .read_test_nsrs

surveyISS::srvy_iss_goa_w_c_e(iters = iters, 
                              lfreq_data = .lfreq_nsrs, 
                              specimen_data = .specimen_nsrs, 
                              cpue_data = .cpue_nsrs, 
                              strata_data = strata_data,
                              r_t = .read_test_nsrs, 
                              yrs = yrs,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE,
                              al_var = TRUE,
                              al_var_ann = TRUE,
                              age_err = TRUE,
                              region = region,
                              save = 'prodtest')

## run for goa rex sole ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(10200)) -> .cpue_rex
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(10200)) -> .lfreq_rex
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(10200)) -> .specimen_rex



read_test %>% 
  tidytable::filter(species_code %in% c(10200)) -> .read_test_rex

surveyISS::srvy_iss_goa_wc_e(iters = iters, 
                             lfreq_data = .lfreq_rex, 
                             specimen_data = .specimen_rex, 
                             cpue_data = .cpue_rex,
                             strata_data = strata_data,
                             r_t = .read_test_rex, 
                             yrs = yrs,
                             boot_hauls = TRUE,
                             boot_lengths = TRUE,
                             boot_ages = TRUE,
                             al_var = TRUE,
                             al_var_ann = TRUE,
                             age_err = TRUE,
                             region = region,
                             save = 'prodtest')


# aleutian islands ----

## run for all species (and subsetting out species cases so we don't have two places with those results) ----
data_ai$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .cpue
data_ai$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .lfreq
data_ai$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .specimen
strata_data <- data_ai$strata

read_test %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30420))) -> .read_test

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = .lfreq,
                    specimen_data = .specimen, 
                    cpue_data = .cpue, 
                    strata_data = strata_data, 
                    r_t = .read_test, 
                    yrs = yrs, 
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'ai', 
                    save = 'prodtest')

## Run for ai blackspotted-rougheye stock complex ----
data_ai$cpue %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .cpue_bsre
data_ai$lfreq %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .lfreq_bsre
data_ai$specimen %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .specimen_bsre


data_ai$read_test %>% 
  tidytable::filter(species_code %in% c(30050, 30051, 30052)) -> .read_test_bsre

surveyISS::srvy_iss_ai_cmplx(iters = iters, 
                             lfreq_data = .lfreq_bsre, 
                             specimen_data = .specimen_bsre, 
                             cpue_data = .cpue_bsre, 
                             strata_data = strata_data, 
                             r_t = .read_test_bsre, 
                             yrs = yrs,
                             boot_hauls = TRUE, 
                             boot_lengths = TRUE, 
                             boot_ages = TRUE, 
                             al_var = TRUE, 
                             al_var_ann = TRUE, 
                             age_err = TRUE,
                             cmplx_code = 3005012,
                             cmplx = 'bsre',
                             region = 'ai',  
                             save = 'prodtest')

## Run for ai northern rockfish (subregion expansion) ----
data_ai$cpue %>% 
  tidytable::filter(species_code %in% c(30420)) -> .cpue_nr
data_ai$lfreq %>% 
  tidytable::filter(species_code %in% c(30420)) -> .lfreq_nr
data_ai$specimen %>% 
  tidytable::filter(species_code %in% c(30420)) -> .specimen_nr




read_test %>% 
  tidytable::filter(species_code %in% c(30420)) -> .read_test_nr

surveyISS::srvy_iss_ai_subreg(iters = iters,
                              lfreq_data = .lfreq_nr, 
                              specimen_data = .specimen_nr, 
                              cpue_data = .cpue_nr, 
                              strata_data = strata_data, 
                              r_t = .read_test_nr, 
                              yrs = yrs,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              region = 'ai',  
                              save = 'prodtest')


# ebs shelf ----

read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = data_ebs$lfreq,
                    specimen_data = data_ebs$specimen, 
                    cpue_data = data_ebs$cpue, 
                    strata_data = data_ebs$strata, 
                    r_t = read_test, 
                    yrs = yrs,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'ebs', 
                    save = 'prodtest')


# ebs slope ----

read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = data_ebss$lfreq,
                    specimen_data = data_ebss$specimen, 
                    cpue_data = data_ebss$cpue, 
                    strata_data = data_ebss$strata,
                    r_t = read_test, 
                    yrs = yrs,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'ebs_slope', 
                    save = 'prodtest')

# nbs & ebs ----

read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = data_nebs$lfreq,
                    specimen_data = data_nebs$specimen, 
                    cpue_data = data_nebs$cpue, 
                    strata_data = data_nebs$strata,  
                    r_t = read_test, 
                    yrs = yrs,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'nebs',  
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


read_test %>% 
  tidytable::filter(species_code %in% species) -> .read_test

surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = .specimen, 
                         cpue_data = .cpue, 
                         r_t = .read_test, 
                         yrs = yrs,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = region, 
                         save = 'prodtest')


# For testing run time of 500 iterations ----
if(iters < 500){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}
