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

# run for eastern bering sea stocks ----

# ebs shelf
yrs = 1979
# yrs = 2010
# species = c(10110, 10130, 10210, 10261)
# species = 21720
species = 21740
region = 'BS'

cpue <- vroom::vroom(here::here('data', 'cpue_bs.csv')) %>% 
  tidytable::filter(species_code %in% species)
lfreq <- vroom::vroom(here::here('data', 'lfreq_bs.csv')) %>% 
  tidytable::filter(species_code %in% species)
strata <- vroom::vroom(here::here('data', 'strata_bs_mb.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_bs.csv')) %>% 
  tidytable::filter(species_code %in% species)
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species & region == "BS") %>% 
  tidytable::slice_sample(n = 30000)

# run adding ageing error and growth variability (pooling growth data for bin size of 1 cm)
# srvy_iss(iters = iters,
#          lfreq_data = lfreq,
#          specimen_data = specimen,
#          cpue_data = cpue,
#          strata_data = strata,
#          r_t = read_test,
#          yrs = yrs,
#          bin = 1,
#          boot_hauls = TRUE,
#          boot_lengths = TRUE,
#          boot_ages = TRUE,
#          sex_spec = TRUE,
#          al_var = TRUE,
#          al_var_ann = FALSE,
#          age_err = TRUE,
#          region = 'bs',
#          save_interm = FALSE,
#          match_orig = FALSE,
#          srvy_type = 'shelf',
#          save = "spec3")

# run adding ageing error and growth variability (annual growth data for bin size of 1 cm)
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
         sex_spec = TRUE,
         al_var = TRUE,
         al_var_ann = TRUE,
         age_err = TRUE,
         region = 'bs',
         save_interm = FALSE,
         match_orig = FALSE,
         srvy_type = 'shelf',
         save = "spec3_ann1cm")

# run adding ageing error and growth variability (annual growth data for bin size of 2 cm)
srvy_iss(iters = iters,
         lfreq_data = lfreq,
         specimen_data = specimen,
         cpue_data = cpue,
         strata_data = strata,
         r_t = read_test,
         yrs = yrs,
         bin = 2,
         boot_hauls = TRUE,
         boot_lengths = TRUE,
         boot_ages = TRUE,
         sex_spec = TRUE,
         al_var = TRUE,
         al_var_ann = TRUE,
         age_err = TRUE,
         region = 'bs',
         save_interm = FALSE,
         match_orig = FALSE,
         srvy_type = 'shelf',
         save = "spec3_ann2cm")

# run adding ageing error and growth variability (annual growth data for bin size of 5 cm)
srvy_iss(iters = iters,
         lfreq_data = lfreq,
         specimen_data = specimen,
         cpue_data = cpue,
         strata_data = strata,
         r_t = read_test,
         yrs = yrs,
         bin = 5,
         boot_hauls = TRUE,
         boot_lengths = TRUE,
         boot_ages = TRUE,
         sex_spec = TRUE,
         al_var = TRUE,
         al_var_ann = TRUE,
         age_err = TRUE,
         region = 'bs',
         save_interm = FALSE,
         match_orig = FALSE,
         srvy_type = 'shelf',
         save = "spec3_ann5cm")


# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}


