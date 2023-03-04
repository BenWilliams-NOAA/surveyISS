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

# get database username/password
db <- vroom::vroom(here::here("database_specs.csv"))
afsc_user = db$username[db$database == "AFSC"]
afsc_pass = db$password[db$database == "AFSC"]

# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
# iters = 500
iters = 5

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# run for gulf of alaska stocks ----

# pull data for Tier 3 species in Gulf of Alaska (1990 on)
yrs = 1990
# species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
species = c(10110, 10262, 10261)
# species = c(30060, 30420, 10180)
# species = c(21720, 10200)
# species = c(21740, 10130)

region = 'GOA'

# query_data(region,
#            species,
#            yrs, 
#            afsc_user,
#            afsc_pass)

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv')) %>% 
  tidytable::filter(species_code %in% species)
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv')) %>% 
  tidytable::filter(species_code %in% species)
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv')) %>% 
  tidytable::filter(species_code %in% species)
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select.(species_code, region, read_age, test_age) %>% 
  tidytable::rename.(age = 'read_age') %>% 
  tidytable::filter.(species_code %in% species)

# run for all species (and subsetting out special cases so we don't have two places with those results)
cpue %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .cpue
lfreq %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .lfreq
specimen %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .specimen
read_test %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .read_test

iters = iters
lfreq_data = .lfreq
specimen_data = .specimen
cpue_data = .cpue
strata_data = strata
r_t = .read_test






# run adding ageing error and growth variability
srvy_iss(iters = iters, 
         lfreq_data = .lfreq,
         specimen_data = .specimen, 
         cpue_data = .cpue, 
         strata_data = strata,
         r_t = .read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE,
         al_var = TRUE,
         age_err = TRUE,
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'spec1')

# # run for goa rougheye-blackspotted stock complex
# cpue %>% 
#   tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
# lfreq %>% 
#   tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
# specimen %>% 
#   tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs
# read_test %>% 
#   tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .read_test
# 
#
# # run for goa dusky stock (has different historical species codes)
# cpue %>% 
#   tidytable::filter.(species_code %in% c(30150, 30152)) -> .cpue_dr
# lfreq %>% 
#   tidytable::filter.(species_code %in% c(30150, 30152)) -> .lfreq_dr
# specimen %>% 
#   tidytable::filter.(species_code %in% c(30150, 30152)) -> .specimen_dr
# 
# 
# # run for goa northern/southern rock sole
# cpue %>% 
#   tidytable::filter.(species_code %in% c(10261, 10262)) -> .cpue_nsrs
# lfreq %>% 
#   tidytable::filter.(species_code %in% c(10261, 10262)) -> .lfreq_nsrs
# specimen %>% 
#   tidytable::filter.(species_code %in% c(10261, 10262)) -> .specimen_nsrs
# 
# 
# # Run for GOA rex sole
# cpue %>% 
#   tidytable::filter.(species_code %in% c(10200)) -> .cpue_rex
# lfreq %>% 
#   tidytable::filter.(species_code %in% c(10200)) -> .lfreq_rex
# specimen %>% 
#   tidytable::filter.(species_code %in% c(10200)) -> .specimen_rex
# 

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}


