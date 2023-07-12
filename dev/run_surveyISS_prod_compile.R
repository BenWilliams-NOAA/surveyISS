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

# compile afsc trawl survey iss results (across regions) ----

# compile bs
vroom::vroom(here::here('output', 'bs', 'prod_iss_ag_shelf.csv')) %>% 
  tidytable::rename.('iss_age' = base) %>% 
  tidytable::mutate.(region = 'bs_shelf') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'prod_iss_ag_slope.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>% 
                          tidytable::mutate.(region = 'bs_slope')) -> bs_age

vroom::vroom(here::here('output', 'bs', 'prod_iss_sz_shelf.csv')) %>% 
  tidytable::rename.('iss_length' = base) %>% 
  tidytable::mutate.(region = 'bs_shelf') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'prod_iss_sz_slope.csv')) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'bs_slope')) -> bs_len

# compile ai
vroom::vroom(here::here('output', 'ai', 'prod_iss_ag.csv')) %>% 
  tidytable::rename.('iss_age' = base) %>% 
  tidytable::mutate.(region = 'ai') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'prod_iss_ag_rebs.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>% 
                          tidytable::mutate.(region = 'ai')) -> ai_age

vroom::vroom(here::here('output', 'ai', 'prod_iss_sz.csv')) %>% 
  tidytable::rename.('iss_length' = base) %>% 
  tidytable::mutate.(region = 'ai') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'prod_iss_sz_rebs.csv')) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'ai')) -> ai_len

# compile goa
vroom::vroom(here::here('output', 'goa', 'prod_iss_ag.csv')) %>% 
  tidytable::rename.('iss_age' = base) %>% 
  tidytable::mutate.(region = 'goa') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod2_iss_ag.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>%
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_rebs.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>%
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_dr.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>% 
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_w_c_egoa.csv')) %>% 
                          tidytable::rename.('iss_age' = base)) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_wc_egoa.csv')) %>% 
                          tidytable::rename.('iss_age' = base)) -> goa_age

vroom::vroom(here::here('output', 'goa', 'prod_iss_sz.csv')) %>% 
  tidytable::rename.('iss_length' = base) %>% 
  tidytable::mutate.(region = 'goa') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod2_iss_sz.csv')) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_rebs.csv')) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_dr.csv')) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_w_c_egoa.csv')) %>% 
                          tidytable::rename.('iss_length' = base)) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_wc_egoa.csv')) %>% 
                          tidytable::rename.('iss_length' = base)) -> goa_len

# compile all and write results
goa_age %>% 
  bind_rows.(ai_age) %>% 
  bind_rows.(bs_age) %>% 
  vroom::vroom_write(., 
                     here::here('output', 'afsc_iss_age.csv'), 
                     delim = ',')

goa_len %>% 
  bind_rows.(ai_len) %>% 
  bind_rows.(bs_len) %>% 
  vroom::vroom_write(., 
                     here::here('output', 'afsc_iss_len.csv'), 
                     delim = ',')
