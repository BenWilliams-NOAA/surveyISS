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

# run for eastern bering sea stocks ----

# ebs shelf
yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
region = 'BS'

# query_data(region,
#            species,
#            yrs, 
#            afsc_user,
#            afsc_pass,
#            nbs = FALSE,
#            bs_slope = FALSE)

cpue <- vroom::vroom(here::here('data', 'cpue_bs.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_bs.csv'))
strata <- vroom::vroom(here::here('data', 'strata_bs_mb.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_bs.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select.(species_code, region, read_age, test_age) %>% 
  tidytable::rename.(age = 'read_age') %>% 
  tidytable::filter.(species_code %in% species)

srvy_iss(iters = iters,
         lfreq_data = lfreq,
         specimen_data = specimen,
         cpue_data = cpue,
         strata_data = strata,
         r_t = .read_test,
         yrs = yrs,
         boot_hauls = TRUE,
         boot_lengths = TRUE,
         boot_ages = TRUE, 
         al_var = FALSE,
         age_err = FALSE,
         region = 'bs', 
         save_interm = TRUE,
         match_orig = TRUE,
         srvy_type = 'shelf',
         save = 'prod')

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

