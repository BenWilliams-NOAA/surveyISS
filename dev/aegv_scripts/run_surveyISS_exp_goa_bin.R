# example script to obtain age/length input sample size for experimental runs

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
iters = 5

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# get data ----

# pull data for Tier 3 species in Gulf of Alaska (1990 on)
yrs = 1990
# species = c(10110, 10130, 21720, 21740, 30060, 10261)
# species = c(10110, 10261)
# species = c(30060, 10130)
# species = c(21720)
# species = c(21740)
# species = 30420
species = c(30060, 21720)
region = 'GOA'

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv')) %>% 
  tidytable::filter(species_code %in% species)
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv')) %>% 
  tidytable::filter(species_code %in% species)
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv')) %>% 
  tidytable::filter(species_code %in% species)
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species)


# run for 2 cm bins ----
lfreq %>% 
  tidytable::mutate(length = 10 * (2 * ceiling((length / 10) / 2))) -> lfreq_2

specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(length = 10 * (2 * ceiling((length / 10) / 2))) -> specimen_2

# run adding ageing error and growth variability
srvy_iss(iters = iters, 
         lfreq_data = lfreq_2,
         specimen_data = specimen_2, 
         cpue_data = cpue, 
         strata_data = strata,
         r_t = read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE,
         al_var = TRUE,
         al_var_ann = TRUE,
         age_err = TRUE,
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'popoll_bin2')


# run for 5 cm bins ----
lfreq %>% 
  tidytable::mutate(length = 10 * (5 * ceiling((length / 10) / 5))) -> lfreq_5

specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::mutate(length = 10 * (5 * ceiling((length / 10) / 5))) -> specimen_5

srvy_iss(iters = iters, 
         lfreq_data = lfreq_5,
         specimen_data = specimen_5, 
         cpue_data = cpue, 
         strata_data = strata,
         r_t = read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE,
         al_var = TRUE,
         al_var_ann = TRUE,
         age_err = TRUE,
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'popoll_bin5')


# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}


