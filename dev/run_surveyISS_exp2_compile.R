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
  tidytable::rename(iss = base,
                    sex_cat = comp_type) %>%  
  tidytable::mutate(comp_type = 'age') %>% 
  
  tidytable::bind_rows(ai_iss_sz %>% 
                         tidytable::rename(iss = base,
                                           sex_cat = comp_type) %>% 
                         tidytable::mutate(comp_type = 'length')) %>% 
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
  tidytable::rename(iss = base,
                    sex_cat = comp_type) %>%  
  tidytable::mutate(comp_type = 'age') %>% 
  
  tidytable::bind_rows(goa_iss_sz %>% 
                         tidytable::rename(iss = base,
                                           sex_cat = comp_type) %>% 
                         tidytable::mutate(comp_type = 'length')) %>% 
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
  tidytable::rename(iss = base,
                    sex_cat = comp_type) %>%  
  tidytable::mutate(comp_type = 'age') %>% 
  
  tidytable::bind_rows(bs_iss_sz %>% 
                         tidytable::rename(iss = base,
                                           sex_cat = comp_type) %>% 
                         tidytable::mutate(comp_type = 'length')) %>% 
  tidytable::mutate(region = 'bs') -> bs_iss

# bind some categorical variables ----

# goa

goa_species <- vroom::vroom(here::here('data', 'species_goa.csv'))

goa_specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))

goa_lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))

goa_specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_age = max(age), .by = c(year, species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(goa_specimen %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_age = max(age), .by = c(year, species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> goa_specimen_ann

goa_specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_age = max(age), .by = c(species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(goa_specimen %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_age = max(age), .by = c(species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> goa_specimen_all

goa_lfreq %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_len = max(length), .by = c(year, species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(goa_lfreq %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_len = max(length), .by = c(year, species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> goa_lfreq_ann

goa_lfreq %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_len = max(length), .by = c(species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(goa_lfreq %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_len = max(length), .by = c(species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> goa_lfreq_all

goa_iss %>% 
  tidytable::left_join(goa_species) %>% 
  tidytable::left_join(goa_specimen_ann) %>% 
  tidytable::left_join(goa_lfreq_ann) -> goa_iss_ann 

goa_iss %>% 
  tidytable::summarize(iss = mean(iss), .by = c(species_code, sex_cat, err_src, comp_type, region)) %>% 
  tidytable::left_join(goa_species) %>% 
  tidytable::left_join(goa_specimen_all) %>% 
  tidytable::left_join(goa_lfreq_all) -> goa_iss_mu

# ai

ai_species <- vroom::vroom(here::here('data', 'species_ai.csv'))

ai_specimen <- vroom::vroom(here::here('data', 'specimen_ai.csv'))

ai_lfreq <- vroom::vroom(here::here('data', 'lfreq_ai.csv'))

ai_specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_age = max(age), .by = c(year, species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(ai_specimen %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_age = max(age), .by = c(year, species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> ai_specimen_ann

ai_specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_age = max(age), .by = c(species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(ai_specimen %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_age = max(age), .by = c(species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> ai_specimen_all

ai_lfreq %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_len = max(length), .by = c(year, species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(ai_lfreq %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_len = max(length), .by = c(year, species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> ai_lfreq_ann

ai_lfreq %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_len = max(length), .by = c(species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(ai_lfreq %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_len = max(length), .by = c(species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> ai_lfreq_all

ai_iss %>% 
  tidytable::left_join(ai_species) %>% 
  tidytable::left_join(ai_specimen_ann) %>% 
  tidytable::left_join(ai_lfreq_ann) -> ai_iss_ann 

ai_iss %>% 
  tidytable::summarize(iss = mean(iss), .by = c(species_code, sex_cat, err_src, comp_type, region)) %>% 
  tidytable::left_join(ai_species) %>% 
  tidytable::left_join(ai_specimen_all) %>% 
  tidytable::left_join(ai_lfreq_all) -> ai_iss_mu

# bs

bs_species <- vroom::vroom(here::here('data', 'species_bs.csv'))

bs_specimen <- vroom::vroom(here::here('data', 'specimen_bs.csv'))

bs_lfreq <- vroom::vroom(here::here('data', 'lfreq_bs.csv'))

bs_specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_age = max(age), .by = c(year, species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(bs_specimen %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_age = max(age), .by = c(year, species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> bs_specimen_ann

bs_specimen %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_age = max(age), .by = c(species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(bs_specimen %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_age = max(age), .by = c(species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> bs_specimen_all

bs_lfreq %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_len = max(length), .by = c(year, species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(bs_lfreq %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_len = max(length), .by = c(year, species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> bs_lfreq_ann

bs_lfreq %>% 
  tidytable::drop_na() %>% 
  tidytable::summarize(max_len = max(length), .by = c(species_code, sex)) %>% 
  tidytable::filter(sex != 3) %>% 
  tidytable::rename(sex_cat = sex) %>% 
  tidytable::mutate(sex_cat = case_when(sex_cat == 1 ~ 'male',
                                        sex_cat == 2 ~ 'female')) %>% 
  tidytable::bind_rows(bs_lfreq %>% 
                         tidytable::drop_na() %>% 
                         tidytable::summarize(max_len = max(length), .by = c(species_code)) %>% 
                         tidytable::mutate(sex_cat = 'total')) -> bs_lfreq_all

bs_iss %>% 
  tidytable::left_join(bs_species) %>% 
  tidytable::left_join(bs_specimen_ann) %>% 
  tidytable::left_join(bs_lfreq_ann) -> bs_iss_ann 

bs_iss %>% 
  tidytable::summarize(iss = mean(iss), .by = c(species_code, sex_cat, err_src, comp_type, region)) %>% 
  tidytable::left_join(bs_species) %>% 
  tidytable::left_join(bs_specimen_all) %>% 
  tidytable::left_join(bs_lfreq_all) -> bs_iss_mu

# combine all ----

goa_iss_ann %>% 
  tidytable::bind_rows(ai_iss_ann) %>% 
  tidytable::bind_rows(bs_iss_ann) %>%
  vroom::vroom_write(.,
                     here::here('output', 'afsc_iss_exp2_ann.csv'),
                     delim = ',')
  
goa_iss_mu %>% 
  tidytable::bind_rows(ai_iss_mu) %>% 
  tidytable::bind_rows(bs_iss_mu) %>%
  vroom::vroom_write(.,
                     here::here('output', 'afsc_iss_exp2_mu.csv'),
                     delim = ',')

