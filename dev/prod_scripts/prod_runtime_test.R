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

# run for all species (and subsetting out special cases so we don't have two places with those results)
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

## original fcns ----

# for testing run time
st_og <- Sys.time()

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
         use_gapindex = FALSE,
         by_strata = FALSE,
         region = region, 
         save_interm = FALSE, 
         save = 'og_test')

end_og <- Sys.time()


## gap fcns at regional level ----

# for testing run time
st_gap_reg <- Sys.time()

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
         region = region, 
         save_interm = FALSE, 
         save = 'gapreg_test')

end_gap_reg <- Sys.time()

## gap fcns at strata level ----

# for testing run time
st_gap_st <- Sys.time()

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
         by_strata = TRUE,
         region = region, 
         save_interm = FALSE, 
         save = 'gapst_test')

end_gap_st <- Sys.time()

# For testing run time ----

end_og - st_og
end_gap_reg - st_gap_reg
end_gap_st - st_gap_st