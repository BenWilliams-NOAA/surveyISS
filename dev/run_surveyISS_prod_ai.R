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

# run for aleutian islands stocks ----

yrs = 1991
species = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)
region = 'AI'

# query_data(region,
#            species, 
#            yrs, 
#            afsc_user,
#            afsc_pass)

cpue <- vroom::vroom(here::here('data', 'cpue_ai.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_ai.csv'))
strata <- vroom::vroom(here::here('data', 'strata_ai.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_ai.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select.(species_code, region, read_age, test_age) %>% 
  tidytable::rename.(age = 'read_age') %>% 
  tidytable::filter.(species_code %in% species)

# Run for all species (and subsetting out REBS so we don't have two places with those results)
cpue %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052))) -> .cpue
lfreq %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052))) -> .lfreq
specimen %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052))) -> .specimen
read_test %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052))) -> .read_test

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
         region = 'ai', 
         save_interm = TRUE,
         match_orig = TRUE,
         save = 'prod')

# Run for AI REBS stock complex
cpue %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
lfreq %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
specimen %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs
read_test %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .read_test_rebs

srvy_iss_ai_rebs(iters = iters,
                 lfreq_data = .lfreq_rebs, 
                 specimen_data = .specimen_rebs, 
                 cpue_data = .cpue_rebs, 
                 strata_data = strata,
                 r_t = .read_test_rebs, 
                 yrs = yrs, 
                 boot_hauls = TRUE, 
                 boot_lengths = TRUE, 
                 boot_ages = TRUE, 
                 al_var = FALSE,
                 age_err = FALSE, 
                 region = 'ai', 
                 save_interm = TRUE,
                 match_orig = TRUE,
                 save = 'prod')

# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

