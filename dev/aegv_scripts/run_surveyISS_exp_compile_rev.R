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

ai_iss <- vroom::vroom(here::here('output', 'ai', 'add_err', 'iss_ag.csv'))  %>%
  tidytable::mutate(bin = '1cm',
                    grwth = 'Pooled') %>% 
  bind_rows(vroom::vroom(here::here('output', 'ai', 'add_err', 'pool1cm_iss_ag.csv'))  %>%
              tidytable::mutate(bin = '1cm',
                                grwth = 'Pooled')) %>% 
  bind_rows(vroom::vroom(here::here('output', 'ai', 'add_err', 'ann1cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '1cm',
                                 grwth = 'Annual')) %>% 
    bind_rows(vroom::vroom(here::here('output', 'ai', 'add_err', 'ann2cm_iss_ag.csv'))  %>%
                tidytable::mutate(bin = '2cm',
                                  grwth = 'Annual')) %>% 
    bind_rows(vroom::vroom(here::here('output', 'ai', 'add_err', 'ann5cm_iss_ag.csv'))  %>%
                tidytable::mutate(bin = '5cm',
                                  grwth = 'Annual')) %>% vroom::vroom_write(.,
                     here::here('output', 'ai', 'add_err', 'iss_ag_rev.csv'),
                     delim = ',')

# gulf of alaska

goa_iss <- vroom::vroom(here::here('output', 'goa', 'add_err', 'iss_ag.csv'))  %>%
  tidytable::mutate(bin = '1cm',
                    grwth = 'Pooled') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec1_ann1cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '1cm',
                                 grwth = 'Annual')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec1_ann2cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '2cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec1_ann5cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '5cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec2_ann1cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '1cm',
                                 grwth = 'Annual')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec2_ann2cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '2cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec2_ann5cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '5cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec3_ann1cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '1cm',
                                 grwth = 'Annual')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec3_ann2cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '2cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'goa', 'add_err', 'spec3_ann5cm_iss_ag.csv'))  %>%
               tidytable::mutate(bin = '5cm',
                                 grwth = 'Annual')) %>%
  vroom::vroom_write(.,
                     here::here('output', 'goa', 'add_err', 'iss_ag_rev.csv'),
                     delim = ',')


# bering sea

bs_iss <- vroom::vroom(here::here('output', 'bs', 'add_err', 'iss_ag_shelf.csv'))  %>%
  tidytable::mutate(bin = '1cm',
                    grwth = 'Pooled') %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec1_ann1cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '1cm',
                                 grwth = 'Annual')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec1_ann2cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '2cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec1_ann5cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '5cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec2_ann1cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '1cm',
                                 grwth = 'Annual')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec2_ann2cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '2cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec2_ann5cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '5cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec3_ann1cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '1cm',
                                 grwth = 'Annual')) %>% 
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec3_ann2cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '2cm',
                                 grwth = 'Annual')) %>%
  bind_rows.(vroom::vroom(here::here('output', 'bs', 'add_err', 'spec3_ann5cm_iss_ag_shelf.csv'))  %>%
               tidytable::mutate(bin = '5cm',
                                 grwth = 'Annual')) %>%
  vroom::vroom_write(.,
                     here::here('output', 'bs', 'add_err', 'iss_ag_rev_shelf.csv'),
                     delim = ',')

# combine all

goa_iss %>% 
  tidytable::mutate(region = 'goa') %>% 
  tidytable::bind_rows(ai_iss %>% 
                         tidytable::mutate(region = 'ai')) %>% 
  tidytable::bind_rows(bs_iss %>% 
                         tidytable::mutate(region = 'bs')) %>%
  vroom::vroom_write(.,
                     here::here('output', 'afsc_iss_err_rev.csv'),
                     delim = ',')
  
  

