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

# compile afsc trawl survey iss results (across regions) ----

vroom::vroom(here::here('output', 'bs', 'add_err', 'iss_ag_shelf.csv')) %>%
  mutate.(region = 'bs_shelf') %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'iss_ag_slope.csv')) %>%
               mutate.(region = 'bs_slope')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'iss_ag.csv')) %>%
               mutate.(region = 'ai')) %>%  
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'iss_ag.csv')) %>%
               mutate.(region = 'goa')) %>%
  vroom::vroom_write(.,
                     here::here('output', 'afsc_iss_err.csv'),
                     delim = ',')



