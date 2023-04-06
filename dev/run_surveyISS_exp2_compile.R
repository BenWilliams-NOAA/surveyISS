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

# compile afsc trawl survey iss results (across regions) ----

# aleutians

ai_iss_ag <- vroom::vroom(here::here('output', 'ai', 'add_err', 'all_iss_ag.csv'))  %>%
  tidytable::mutate(err_src = 'all') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'hl_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'hl')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'ln_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'ln')) %>%
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'hl_ln_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'hl_ln')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'sp_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'sp'))

ai_iss_sz <- vroom::vroom(here::here('output', 'ai', 'add_err', 'all_iss_sz.csv'))  %>%
  tidytable::mutate(err_src = 'all') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'hl_iss_sz.csv'))  %>%
                          tidytable::mutate(err_src = 'hl')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'ln_iss_sz.csv'))  %>%
                          tidytable::mutate(err_src = 'ln'))

ai_iss_ag %>% 
  tidytable::mutate(comp_type = 'age') %>% 
  tidytable::rename(iss = base) %>% 
  tidytable::bind_rows(ai_iss_sz %>% 
                         tidytable::mutate(comp_type = 'length') %>% 
                         tidytable::rename(iss = base)) %>% 
  tidytable::mutate(region = 'ai') -> ai_iss

# gulf of alaska

goa_iss_ag <- vroom::vroom(here::here('output', 'goa', 'add_err', 'all_iss_ag.csv'))  %>%
  tidytable::mutate(err_src = 'all') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'hl_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'hl')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'ln_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'ln')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'hl_ln_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'hl_ln')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'sp_iss_ag.csv'))  %>%
                          tidytable::mutate(err_src = 'sp'))

goa_iss_sz <- vroom::vroom(here::here('output', 'goa', 'add_err', 'all_iss_sz.csv'))  %>%
  tidytable::mutate(err_src = 'all') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'hl_iss_sz.csv'))  %>%
                          tidytable::mutate(err_src = 'hl')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'ln_iss_sz.csv'))  %>%
                          tidytable::mutate(err_src = 'ln'))

goa_iss_ag %>% 
  tidytable::mutate(comp_type = 'age') %>% 
  tidytable::rename(iss = base) %>% 
  tidytable::bind_rows(goa_iss_sz %>% 
                         tidytable::mutate(comp_type = 'length') %>% 
                         tidytable::rename(iss = base)) %>% 
  tidytable::mutate(region = 'goa') -> goa_iss

# bering sea

bs_iss_ag <- vroom::vroom(here::here('output', 'bs', 'add_err', 'all_iss_ag_shelf.csv'))  %>%
  tidytable::mutate(err_src = 'all') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'hl_iss_ag_shelf.csv'))  %>%
                          tidytable::mutate(err_src = 'hl')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'ln_iss_ag_shelf.csv'))  %>%
                          tidytable::mutate(err_src = 'ln')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'hl_ln_iss_ag_shelf.csv'))  %>%
                          tidytable::mutate(err_src = 'hl_ln')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'sp_iss_ag_shelf.csv'))  %>%
                          tidytable::mutate(err_src = 'sp'))

bs_iss_sz <- vroom::vroom(here::here('output', 'bs', 'add_err', 'all_iss_sz_shelf.csv'))  %>%
  tidytable::mutate(err_src = 'all') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'hl_iss_sz_shelf.csv'))  %>%
                          tidytable::mutate(err_src = 'hl')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'ln_iss_sz_shelf.csv'))  %>%
                          tidytable::mutate(err_src = 'ln'))

bs_iss_ag %>% 
  tidytable::mutate(comp_type = 'age') %>% 
  tidytable::rename(iss = base) %>% 
  tidytable::bind_rows(bs_iss_sz %>% 
                         tidytable::mutate(comp_type = 'length') %>% 
                         tidytable::rename(iss = base)) %>% 
  tidytable::mutate(region = 'bs') -> bs_iss

# combine all

goa_iss %>% 
  tidytable::bind_rows(ai_iss) %>% 
  tidytable::bind_rows(bs_iss) %>%
  vroom::vroom_write(.,
                     here::here('output', 'afsc_iss_exp2.csv'),
                     delim = ',')
  
  

