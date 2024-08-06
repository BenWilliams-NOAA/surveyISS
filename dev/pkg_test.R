# script to test package
# load surveyISS library ----
# devtools::unload('surveyISS')
# devtools::install_github("BenWilliams-NOAA/surveyISS", force = TRUE)
# library(surveyISS)

## load/source libraries/functions for testing ----
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# set iterations ----
iters = 2

# for testing stuff ----
# bin = 5
# plus_len = 20
# plus_age = 8
bin = c(4.5, 9.5, 14.5, 19.5, 24.5, 29.5, 34.5, 39.5, 44.5, 49.5, 54.5, 59.5, 64.5, 69.5, 74.5, 79.5, 84.5, 89.5, 94.5, 99.5, 104.5, 109.5, 114.5, 119.5)

# get data ----
# if query = TRUE then will run data queries, if FALSE will read previously run data
# set = TRUE if first time running, or if data has changed

data <- surveyISS::query_data_t3(query = FALSE)

data_goa <- data$data_goa

# gulf of alaska ----

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
data_goa$cpue %>% 
  tidytable::filter(species_code == 21720) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(species_code == 21720) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(species_code == 21720) -> .specimen
strata_data <- data_goa$strata

# for testing
lfreq_data = .lfreq
specimen_data = .specimen
cpue_data = .cpue
yrs = 1990
bin = bin
boot_hauls = TRUE
boot_lengths = TRUE
boot_ages = TRUE
al_var = TRUE
al_var_ann = TRUE
age_err = TRUE
len_samples = NULL
age_samples = NULL
plus_len = NULL
plus_age = NULL
use_gapindex = TRUE
by_strata = TRUE
global = FALSE

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = .lfreq,
                    specimen_data = .specimen, 
                    cpue_data = .cpue, 
                    strata_data = strata_data,
                    yrs = 1990, 
                    bin = bin,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    plus_len = plus_len,
                    plus_age = plus_age,
                    region = 'goa', 
                    save = 'prodtest')
