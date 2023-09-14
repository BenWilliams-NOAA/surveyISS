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
iters = 500
# iters = 5

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# run for eastern bering sea stocks ----

# ebs shelf
yrs = 1979
species = c(21720)
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
  tidytable::select.(species_code, region, read_age, test_age) %>% 
  tidytable::rename.(age = 'read_age') %>% 
  tidytable::filter.(species_code %in% species)

# run bootstrap hauls
srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata,
         r_t = read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = FALSE, 
         boot_ages = FALSE,
         al_var = FALSE,
         age_err = FALSE,
         region = 'bs', 
         save_interm = FALSE,
         match_orig = FALSE,
         srvy_type = 'shelf',
         save = 'cod_hl')

# run bootstrap hauls & lengths
srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata,
         r_t = read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = FALSE,
         al_var = FALSE,
         age_err = FALSE,
         region = 'bs', 
         save_interm = FALSE,
         match_orig = FALSE,
         srvy_type = 'shelf',
         save = 'cod_hl_len')

# run bootstrap hauls & lengths
srvy_iss(iters = iters, 
         lfreq_data = lfreq,
         specimen_data = specimen, 
         cpue_data = cpue, 
         strata_data = strata,
         r_t = read_test,
         yrs = yrs, 
         boot_hauls = TRUE, 
         boot_lengths = TRUE, 
         boot_ages = TRUE,
         al_var = FALSE,
         age_err = FALSE,
         region = 'bs', 
         save_interm = FALSE,
         match_orig = FALSE,
         srvy_type = 'shelf',
         save = 'cod_hl_len_age')

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

# compile results ----

dir.create(here::here('output', region, 'add_err', 'pcod_examp'), recursive = TRUE)

vroom::vroom(here::here('output', region, 'add_err', 'cod_hl_iss_ag_shelf.csv')) %>% 
  tidytable::rename(hl = base) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', region, 'add_err', 'cod_hl_len_iss_ag_shelf.csv')) %>% 
                         tidytable::rename(hl_len = base)) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', region, 'add_err', 'cod_hl_len_age_iss_ag_shelf.csv')) %>% 
                         tidytable::rename(hl_len_age = base)) %>% 
  vroom::vroom_write(.,
                     here::here('output', region, 'add_err', 'pcod_examp', 'ebspcod_iss_age.csv'),
                     delim = ',')

vroom::vroom(here::here('output', region, 'add_err', 'cod_hl_iss_sz_shelf.csv')) %>% 
  tidytable::rename(hl = base) %>% 
  tidytable::left_join(vroom::vroom(here::here('output', region, 'add_err', 'cod_hl_len_iss_sz_shelf.csv')) %>% 
                         tidytable::rename(hl_len = base)) %>% 
  vroom::vroom_write(.,
                     here::here('output', region, 'add_err', 'pcod_examp', 'ebspcod_iss_length.csv'),
                     delim = ',')





