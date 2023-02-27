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

# compile bs
vroom::vroom(here::here('output', 'bs', 'iss_ag_shelf.csv')) %>%
  rename.('iss_base' = iss) %>%
  left_join.(vroom::vroom(here::here('output', 'bs', 'alvar', 'iss_ag_shelf.csv'))) %>%
  rename.('iss_alvar' = iss) %>%
  left_join.(vroom::vroom(here::here('output', 'bs', 'agerr', 'iss_ag_shelf.csv'))) %>%
  rename.('iss_agerr' = iss) %>%
  left_join.(vroom::vroom(here::here('output', 'bs', 'alvar_agerr', 'iss_ag_shelf.csv'))) %>%
  rename.('iss_alvar_agerr' = iss) %>%
  mutate.(region = 'bs_shelf') %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'iss_ag_slope.csv')) %>%
               rename.('iss_base' = iss) %>%
               left_join.(vroom::vroom(here::here('output', 'bs', 'alvar', 'iss_ag_slope.csv'))) %>%
               rename.('iss_alvar' = iss) %>%
               left_join.(vroom::vroom(here::here('output', 'bs', 'agerr', 'iss_ag_slope.csv'))) %>%
               rename.('iss_agerr' = iss) %>%
               left_join.(vroom::vroom(here::here('output', 'bs', 'alvar_agerr', 'iss_ag_slope.csv'))) %>%
               rename.('iss_alvar_agerr' = iss) %>%
               mutate.(region = 'bs_slope')) -> bs

# compile ai
vroom::vroom(here::here('output', 'ai', 'iss_ag.csv')) %>%
  rename.('iss_base' = iss) %>%
  select.(-nss, -hls) %>%
  left_join.(vroom::vroom(here::here('output', 'ai', 'alvar', 'iss_ag.csv'))) %>%
  rename.('iss_alvar' = iss) %>%
  select.(-nss, -hls) %>%
  left_join.(vroom::vroom(here::here('output', 'ai', 'agerr', 'iss_ag.csv'))) %>%
  rename.('iss_agerr' = iss) %>%
  select.(-nss, -hls) %>%
  left_join.(vroom::vroom(here::here('output', 'ai', 'alvar_agerr', 'iss_ag.csv'))) %>%
  rename.('iss_alvar_agerr' = iss) %>%
  select.(-nss, -hls) %>%
  mutate.(region = 'ai') -> ai


# compile goa
vroom::vroom(here::here('output', 'goa', 'iss_ag.csv')) %>%
  rename.('iss_base' = iss) %>%
  select.(-nss, -hls) %>%
  left_join.(vroom::vroom(here::here('output', 'goa', 'alvar', 'iss_ag.csv'))) %>%
  rename.('iss_alvar' = iss) %>%
  select.(-nss, -hls) %>%
  left_join.(vroom::vroom(here::here('output', 'goa', 'agerr', 'iss_ag.csv'))) %>%
  rename.('iss_agerr' = iss) %>%
  select.(-nss, -hls) %>%
  left_join.(vroom::vroom(here::here('output', 'goa', 'alvar_agerr', 'iss_ag.csv'))) %>%
  rename.('iss_alvar_agerr' = iss) %>%
  select.(-nss, -hls) %>%
  mutate.(region = 'goa') -> goa

# compile all and write results
goa %>%
  bind_rows.(ai) %>%
  bind_rows.(bs) %>%
  vroom::vroom_write(.,
                     here::here('output', 'afsc_iss_err.csv'),
                     delim = ',')



