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

# get data ----

cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', 'cpue_goa.csv')))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', 'lfreq_goa.csv')))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', 'strata_goa.csv')))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', 'specimen_goa.csv')))
gap_lpop <- tidytable::as_tidytable(vroom::vroom(here::here('data', 'lpop_goa.csv')))
gap_apop <- tidytable::as_tidytable(vroom::vroom(here::here('data', 'apop_goa.csv')))

yrs <- 1990

# get original age/length pop'n values
og <- srvy_comps(lfreq_data = lfreq, 
                 specimen_data = specimen, 
                 cpue_data = cpue, 
                 strata_data = strata,
                 r_t = NULL,
                 yrs = yrs, 
                 bin = 1,
                 boot_hauls = FALSE, 
                 boot_lengths = FALSE, 
                 boot_ages = FALSE,
                 sex_spec = TRUE,
                 al_var = FALSE,
                 age_err = FALSE)


oga <- og$age %>% 
  select(-type)
ogl <- og$length %>% 
  select(-type)


match_gap(oga, ogl, gap_apop, gap_lpop, thresh = 0.01, region = 'goa')

