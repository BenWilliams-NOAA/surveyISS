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
# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
# iters = 500
iters = 10

# get data ----
# if query = TRUE then will run data queries, if FALSE will read previously run data
# set = TRUE if first time running, or if data has changed

data <- surveyISS::query_data_t3(query = FALSE)

data_goa <- data$data_goa
data_ai <- data$data_ai
data_ebs <- data$data_ebs
data_ebss <- data$data_ebss
data_nbs <- data$data_nbs
data_nebs <- data$data_nebs

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# gulf of alaska ----

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
data_goa$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152))) -> .specimen
strata_data <- data_goa$strata

# for testing
# lfreq_data = .lfreq
# specimen_data = .specimen
# cpue_data = .cpue
# bin = 1
# boot_hauls = TRUE
# boot_lengths = TRUE
# boot_ages = TRUE
# al_var = TRUE
# al_var_ann = TRUE
# age_err = TRUE
# len_samples = NULL
# age_samples = NULL
# plus_len = NULL
# plus_age = NULL
# by_strata = FALSE
# global = FALSE

# age/length
surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = .lfreq,
                    specimen_data = .specimen, 
                    cpue_data = .cpue, 
                    strata_data = strata_data,
                    yrs = 1990,  
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'goa', 
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = .specimen, 
                         cpue_data = .cpue, 
                         yrs = 1990,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'goa', 
                         save_stats = TRUE,
                         save = 'prod')

## run w-c-e goa ----
surveyISS::srvy_iss_goa_w_c_e(iters = iters, 
                              lfreq_data = .lfreq,
                              specimen_data = .specimen, 
                              cpue_data = .cpue, 
                              strata_data = strata_data, 
                              yrs = 1990,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE,
                              al_var = TRUE,
                              al_var_ann = TRUE,
                              age_err = TRUE,
                              region = 'goa', 
                              save_stats = TRUE,
                              save = 'prod')

## run wc-e goa ----
surveyISS::srvy_iss_goa_wc_e(iters = iters, 
                             lfreq_data = .lfreq,
                             specimen_data = .specimen, 
                             cpue_data = .cpue, 
                             strata_data = strata_data, 
                             yrs = 1990,
                             boot_hauls = TRUE,
                             boot_lengths = TRUE,
                             boot_ages = TRUE,
                             al_var = TRUE,
                             al_var_ann = TRUE,
                             age_err = TRUE,
                             region = 'goa', 
                             save_stats = TRUE,
                             save = 'prod')

## run for west of 140 ----
# note: only run for pollock
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
                         boot_hauls = TRUE, 
                         boot_lengths = TRUE, 
                         boot_ages = TRUE, 
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'goa',  
                         save_stats = TRUE,
                         save = 'prod')

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
                              boot_hauls = TRUE, 
                              boot_lengths = TRUE, 
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              cmplx_code = 3005012,
                              cmplx = 'rebs',
                              region = 'goa', 
                              save_stats = TRUE, 
                              save = 'prod')

## run for goa dusky stock (has different historical species codes) ----
data_goa$cpue %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .cpue_dr
data_goa$lfreq %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .lfreq_dr
data_goa$specimen %>% 
  tidytable::filter(species_code %in% c(30150, 30152)) -> .specimen_dr

surveyISS::srvy_iss_goa_cmplx(iters = iters, 
                              lfreq_data = .lfreq_dr, 
                              specimen_data = .specimen_dr, 
                              cpue_data = .cpue_dr, 
                              strata_data = strata_data, 
                              yrs = 1990, 
                              boot_hauls = TRUE, 
                              boot_lengths = TRUE, 
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              cmplx_code = 301502,
                              cmplx = 'dr',
                              region = 'goa',  
                              save_stats = TRUE, 
                              save = 'prod')

# aleutian islands ----

## run for all species (and subsetting out species cases so we don't have two places with those results) ----
data_ai$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052))) -> .cpue
data_ai$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052))) -> .lfreq
data_ai$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052))) -> .specimen
strata_data <- data_ai$strata

# age/length
surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = .lfreq,
                    specimen_data = .specimen, 
                    cpue_data = .cpue, 
                    strata_data = strata_data,
                    yrs = 1991, 
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'ai',  
                    save_stats = TRUE, 
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = .specimen, 
                         cpue_data = .cpue, 
                         yrs = 1991,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'ai', 
                         save_stats = TRUE,
                         save = 'prod')

## run for ai subregion ----
surveyISS::srvy_iss_ai_subreg(iters = iters,
                              lfreq_data = .lfreq,
                              specimen_data = .specimen, 
                              cpue_data = .cpue, 
                              strata_data = strata_data, 
                              yrs = 1991,
                              boot_hauls = TRUE,
                              boot_lengths = TRUE,
                              boot_ages = TRUE, 
                              al_var = TRUE, 
                              al_var_ann = TRUE, 
                              age_err = TRUE,
                              region = 'ai',   
                              save_stats = TRUE, 
                              save = 'prod')


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
                             boot_hauls = TRUE, 
                             boot_lengths = TRUE, 
                             boot_ages = TRUE, 
                             al_var = TRUE, 
                             al_var_ann = TRUE, 
                             age_err = TRUE,
                             cmplx_code = 3005012,
                             cmplx = 'bsre',
                             region = 'ai',  
                             save_stats = TRUE,  
                             save = 'prod')

# ebs shelf ----
# age/length
surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = data_ebs$lfreq,
                    specimen_data = data_ebs$specimen, 
                    cpue_data = data_ebs$cpue, 
                    strata_data = data_ebs$strata,
                    yrs = 1979,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'ebs',   
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = data_ebs$specimen, 
                         cpue_data = data_ebs$cpue, 
                         yrs = 1979,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'ebs', 
                         save_stats = TRUE,
                         save = 'prod')

# ebs slope ----
# age/length
surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = data_ebss$lfreq,
                    specimen_data = data_ebss$specimen, 
                    cpue_data = data_ebss$cpue, 
                    strata_data = data_ebss$strata, 
                    yrs = 2002,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'ebs_slope',   
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = data_ebss$specimen, 
                         cpue_data = data_ebss$cpue, 
                         yrs = 2002,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'ebs_slope', 
                         save_stats = TRUE,
                         save = 'prod')


# nbs & ebs ----
# age/length
surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = data_nebs$lfreq,
                    specimen_data = data_nebs$specimen, 
                    cpue_data = data_nebs$cpue, 
                    strata_data = data_nebs$strata,  
                    yrs = 1979,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    region = 'nebs',    
                    save_stats = TRUE,
                    save = 'prod')

# caal
surveyISS::srvy_iss_caal(iters = iters, 
                         specimen_data = data_nebs$specimen, 
                         cpue_data = data_nebs$cpue, 
                         yrs = 1979,
                         boot_hauls = TRUE, 
                         boot_ages = TRUE,
                         al_var = TRUE, 
                         al_var_ann = TRUE, 
                         age_err = TRUE,
                         region = 'nebs', 
                         save_stats = TRUE,
                         save = 'prod')


# For testing run time of 500 iterations ----
if(iters < 500){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}
