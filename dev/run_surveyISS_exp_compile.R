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

ai_iss <- vroom::vroom(here::here('output', 'ai', 'add_err', 'spec1_iss_ag.csv'))  %>%
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'spec2_iss_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'spec3_iss_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'spec5_iss_ag.csv'))) %>% 
  vroom::vroom_write(.,
                     here::here('output', 'ai', 'add_err', 'iss_ag.csv'),
                     delim = ',')

ai_ess <- vroom::vroom(here::here('output', 'ai', 'add_err', 'spec1_iter_ess_ag.csv'))  %>%
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'spec2_iter_ess_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'spec3_iter_ess_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'ai', 'add_err', 'spec5_iter_ess_ag.csv'))) %>% 
  vroom::vroom_write(.,
                     here::here('output', 'ai', 'add_err', 'iter_ess_ag.csv'),
                     delim = ',')

# gulf of alaska

goa_iss <- vroom::vroom(here::here('output', 'goa', 'add_err', 'spec1_iss_ag.csv'))  %>%
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec2_iss_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec3_iss_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec4_iss_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec5_iss_ag.csv'))) %>% 
  vroom::vroom_write(.,
                     here::here('output', 'goa', 'add_err', 'iss_ag.csv'),
                     delim = ',')

goa_ess <- vroom::vroom(here::here('output', 'goa', 'add_err', 'spec1_iter_ess_ag.csv'))  %>%
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec2_iter_ess_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec3_iter_ess_ag.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec4_iter_ess_ag.csv'))) %>%  
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec5_iter_ess_ag.csv'))) %>%
  vroom::vroom_write(.,
                     here::here('output', 'goa', 'add_err', 'iter_ess_ag.csv'),
                     delim = ',')

# bering sea

bs_iss <- vroom::vroom(here::here('output', 'bs', 'add_err', 'spec1_iss_ag_shelf.csv'))  %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec2_iss_ag_shelf.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec3_iss_ag_shelf.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec4_iss_ag_shelf.csv'))) %>% 
  vroom::vroom_write(.,
                     here::here('output', 'bs', 'add_err', 'iss_ag_shelf.csv'),
                     delim = ',')

bs_ess <- vroom::vroom(here::here('output', 'bs', 'add_err', 'spec1_iter_ess_ag_shelf.csv'))  %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec2_iter_ess_ag_shelf.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec3_iter_ess_ag_shelf.csv'))) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec4_iter_ess_ag_shelf.csv'))) %>% 
  vroom::vroom_write(.,
                     here::here('output', 'bs', 'add_err', 'iter_ess_ag_shelf.csv'),
                     delim = ',')

# combine all

goa_iss %>% 
  tidytable::mutate(region = 'goa') %>% 
  tidytable::bind_rows(ai_iss %>% 
                         tidytable::mutate(region = 'ai')) %>% 
  tidytable::bind_rows(bs_iss %>% 
                         tidytable::mutate(region = 'bs')) %>%
  vroom::vroom_write(.,
                     here::here('output', 'afsc_iss_err.csv'),
                     delim = ',')
  
  

