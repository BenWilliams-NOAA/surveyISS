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
species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
#species = c(21740, 30060) # pollock and pop for testing

region = 'GOA'

# query_data(region,
#            species,
#            yrs, 
#            afsc_user,
#            afsc_pass)

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))
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

# hauls only
srvy_iss(iters = iters, 
         lfreq_data = .lfreq, 
         specimen_data = .specimen, 
         cpue_data = .cpue, 
         strata_data = strata, 
         r_t = .read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = FALSE, 
         boot_ages = FALSE, 
         al_var = FALSE,
         age_err = FALSE, 
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'hl')

# lengths only
srvy_iss(iters = iters, 
         lfreq_data = .lfreq, 
         specimen_data = .specimen, 
         cpue_data = .cpue, 
         strata_data = strata, 
         r_t = .read_test,
         yrs = yrs, 
         boot_hauls = FALSE, 
         boot_lengths = TRUE, 
         boot_ages = FALSE, 
         al_var = FALSE,
         age_err = FALSE, 
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'ln')

# specimen only
srvy_iss(iters = iters, 
         lfreq_data = .lfreq, 
         specimen_data = .specimen, 
         cpue_data = .cpue, 
         strata_data = strata, 
         r_t = .read_test,
         yrs = yrs, 
         boot_hauls = FALSE, 
         boot_lengths = FALSE, 
         boot_ages = TRUE, 
         al_var = FALSE,
         age_err = FALSE, 
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'sp')

# hauls & lengths
srvy_iss(iters = iters, 
         lfreq_data = .lfreq, 
         specimen_data = .specimen, 
         cpue_data = .cpue, 
         strata_data = strata, 
         r_t = .read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = FALSE, 
         al_var = FALSE,
         age_err = FALSE, 
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'hl_ln')

# all
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
         al_var = FALSE,
         age_err = FALSE, 
         region = 'goa', 
         save_interm = FALSE,
         match_orig = FALSE,
         save = 'all')


# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

