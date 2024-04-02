# example script to obtain age/length input sample size for production run

# load surveyISS library
#devtools::install_github("afsc-assessments/surveyISS", force = TRUE)
#library(surveyISS)

# load/source libraries/functions for testing
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

# nbs & ebs data ----
region = 'nebs'
yrs = 1979
species = c(10210, 21720, 21740)

# run for nbs & ebs stocks
cpue <- vroom::vroom(here::here('data', 'cpue_nebs.csv')) %>% 
  tidytable::filter(species_code %in% species)
lfreq <- vroom::vroom(here::here('data', 'lfreq_nebs.csv')) %>% 
  tidytable::filter(species_code %in% species)
strata <- vroom::vroom(here::here('data', 'strata_nebs.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_nebs.csv')) %>% 
  tidytable::filter(species_code %in% species)
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)

# hauls only ----

srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata, 
         r_t = read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = TRUE, 
         boot_lengths = FALSE, 
         boot_ages = FALSE, 
         al_var = FALSE, 
         al_var_ann = FALSE, 
         age_err = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'hl')

# lengths only ----

srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata, 
         r_t = read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = FALSE, 
         boot_lengths = TRUE, 
         boot_ages = FALSE, 
         al_var = FALSE, 
         al_var_ann = FALSE, 
         age_err = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'len')

# ages only ----

srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata, 
         r_t = read_test, 
         yrs = yrs, 
         bin = 1, 
         boot_hauls = FALSE, 
         boot_lengths = FALSE, 
         boot_ages = TRUE, 
         al_var = FALSE, 
         al_var_ann = FALSE, 
         age_err = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'age')

# hauls & lengths only ----

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
         boot_ages = FALSE, 
         al_var = FALSE, 
         al_var_ann = FALSE, 
         age_err = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'hl_len')

# all sources ----

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
         boot_ages = FALSE, 
         al_var = FALSE, 
         al_var_ann = FALSE, 
         age_err = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'all')


# For testing run time of 500 iterations ----
if(iters < 500){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

