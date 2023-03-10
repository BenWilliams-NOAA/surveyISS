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
#iters = 500
iters = 5

# for testing run time
if(iters < 100){
  st <- Sys.time()
}

# run for gulf of alaska stocks ----

# pull data for Tier 3 species in Gulf of Alaska (1990 on)
yrs = 1990
species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
#species = c(21740, 30060) # pollock and pop for testing

region = 'GOA'

# query_data(region,
#            species,
#            yrs, 
#            afsc_user,
#            afsc_pass)

cpue <- vroom::vroom(here::here('data', 'cpue_goa.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_goa.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select.(species_code, region, read_age, test_age) %>% 
  tidytable::rename.(age = 'read_age') %>% 
  tidytable::filter.(species_code %in% species)

# run for all species (and subsetting out special cases so we don't have two places with those results)
cpue %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .cpue
lfreq %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .lfreq
specimen %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .specimen
read_test %>% 
  tidytable::filter.(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200))) -> .read_test

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
         region = 'goa', 
         save_interm = TRUE,
         match_orig = TRUE,
         save = 'prod')

# run for goa rougheye-blackspotted stock complex
cpue %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .cpue_rebs
lfreq %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .lfreq_rebs
specimen %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .specimen_rebs
read_test %>% 
  tidytable::filter.(species_code %in% c(30050, 30051, 30052)) -> .read_test_rebs

srvy_iss_goa_rebs(iters = iters,
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
                  region = 'goa', 
                  save_interm = TRUE,
                  match_orig = TRUE,
                  save = 'prod')

# run for goa dusky stock (has different historical species codes)
cpue %>% 
  tidytable::filter.(species_code %in% c(30150, 30152)) -> .cpue_dr
lfreq %>% 
  tidytable::filter.(species_code %in% c(30150, 30152)) -> .lfreq_dr
specimen %>% 
  tidytable::filter.(species_code %in% c(30150, 30152)) -> .specimen_dr
read_test %>% 
  tidytable::filter.(species_code %in% c(30150, 30152)) -> .read_test_dr

srvy_iss_goa_dr(iters = iters, 
                lfreq_data = .lfreq_dr, 
                specimen_data = .specimen_dr,
                cpue_data = .cpue_dr, 
                strata_data = strata, 
                r_t = .read_test_dr,
                yrs = yrs,
                boot_hauls = TRUE, 
                boot_lengths = TRUE, 
                boot_ages = TRUE,  
                al_var = FALSE,
                age_err = FALSE,
                region = 'goa', 
                save_interm = TRUE,
                match_orig = TRUE,
                save = 'prod')

# run for goa northern/southern rock sole
cpue %>% 
  tidytable::filter.(species_code %in% c(10261, 10262)) -> .cpue_nsrs
lfreq %>% 
  tidytable::filter.(species_code %in% c(10261, 10262)) -> .lfreq_nsrs
specimen %>% 
  tidytable::filter.(species_code %in% c(10261, 10262)) -> .specimen_nsrs
read_test %>% 
  tidytable::filter.(species_code %in% c(10261, 10262)) -> .read_test_nsrs


srvy_iss_goa_w_c_e(iters = iters, 
                   lfreq_data = .lfreq_nsrs, 
                   specimen_data = .specimen_nsrs, 
                   cpue_data = .cpue_nsrs, 
                   strata_data = strata, 
                   r_t = .read_test_nsrs,
                   yrs = yrs,
                   boot_hauls = TRUE, 
                   boot_lengths = TRUE, 
                   boot_ages = TRUE,  
                   al_var = FALSE,
                   age_err = FALSE,
                   region = 'goa', 
                   save_interm = TRUE,
                   match_orig = TRUE,
                   save = 'prod')

# Run for GOA rex sole
cpue %>% 
  tidytable::filter.(species_code %in% c(10200)) -> .cpue_rex
lfreq %>% 
  tidytable::filter.(species_code %in% c(10200)) -> .lfreq_rex
specimen %>% 
  tidytable::filter.(species_code %in% c(10200)) -> .specimen_rex
read_test %>% 
  tidytable::filter.(species_code %in% c(10200)) -> .read_test_rex

srvy_iss_goa_wc_e(iters = iters, 
                  lfreq_data = .lfreq_rex, 
                  specimen_data = .specimen_rex, 
                  cpue_data = .cpue_rex, 
                  strata_data = strata, 
                  r_t = .read_test_rex,
                  yrs = yrs,
                  boot_hauls = TRUE, 
                  boot_lengths = TRUE, 
                  boot_ages = TRUE,  
                  al_var = FALSE,
                  age_err = FALSE,
                  region = 'goa', 
                  save_interm = TRUE,
                  match_orig = TRUE,
                  save = 'prod')

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

# run for eastern bering sea stocks ----

# ebs shelf
yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
region = 'BS'

# query_data(region,
#            species,
#            yrs, 
#            afsc_user,
#            afsc_pass,
#            nbs = FALSE,
#            bs_slope = FALSE)

cpue <- vroom::vroom(here::here('data', 'cpue_bs.csv'))
lfreq <- vroom::vroom(here::here('data', 'lfreq_bs.csv'))
strata <- vroom::vroom(here::here('data', 'strata_bs_mb.csv'))
specimen <- vroom::vroom(here::here('data', 'specimen_bs.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select.(species_code, region, read_age, test_age) %>% 
  tidytable::rename.(age = 'read_age') %>% 
  tidytable::filter.(species_code %in% species)

srvy_iss(iters = iters,
         lfreq_data = lfreq,
         specimen_data = specimen,
         cpue_data = cpue,
         strata_data = strata,
         r_t = .read_test,
         yrs = yrs,
         boot_hauls = TRUE,
         boot_lengths = TRUE,
         boot_ages = TRUE, 
         al_var = FALSE,
         age_err = FALSE,
         region = 'bs', 
         save_interm = TRUE,
         match_orig = TRUE,
         srvy_type = 'shelf',
         save = 'prod')

# ebs slope

yrs = 2002
species = c(10110, 10112, 10115,30060)
region = 'BS'

# query_data(region,
#            species,
#            yrs, 
#            afsc_user,
#            afsc_pass,
#            nbs = FALSE,
#            bs_slope = TRUE)

cpue_data_s <- vroom::vroom(here::here('data', 'cpue_slope_bs.csv'))
lfreq_data_s <- vroom::vroom(here::here('data', 'lfreq_slope_bs.csv'))
strata_data_s <- vroom::vroom(here::here('data', 'strata_slope_bs_mb.csv'))
specimen_data_s <- vroom::vroom(here::here('data', 'specimen_slope_bs.csv'))
read_test <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select.(species_code, region, read_age, test_age) %>% 
  tidytable::rename.(age = 'read_age') %>% 
  tidytable::filter.(species_code %in% species)

srvy_iss(iters = iters,
         lfreq_data = lfreq_data_s,
         specimen_data = specimen_data_s,
         cpue_data = cpue_data_s,
         strata_data = strata_data_s,
         r_t = .read_test,
         yrs = yrs,
         boot_hauls = TRUE,
         boot_lengths = TRUE,
         boot_ages = TRUE, 
         al_var = FALSE,
         age_err = FALSE,
         region = 'bs', 
         save_interm = TRUE,
         match_orig = TRUE,
         srvy_type = 'slope',
         save = 'prod')


# compile afsc trawl survey iss results (across regions) ----

# compile bs
vroom::vroom(here::here('output', 'bs', 'prod_iss_ag_shelf.csv')) %>% 
  tidytable::rename.('iss_age' = base) %>% 
  tidytable::left_join.(vroom::vroom(here::here('output', 'bs', 'prod_iss_sz_shelf.csv'))) %>% 
  tidytable::rename.('iss_length' = base) %>% 
  tidytable::mutate.(region = 'bs_shelf') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'bs', 'prod_iss_ag_slope.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>% 
                          tidytable::left_join.(vroom::vroom(here::here('output', 'bs', 'prod_iss_sz_slope.csv'))) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'bs_slope')) -> bs

# compile ai
vroom::vroom(here::here('output', 'ai', 'iss_ag.csv')) %>% 
  tidytable::rename.('iss_age' = base) %>% 
  tidytable::left_join.(vroom::vroom(here::here('output', 'ai', 'iss_sz.csv'))) %>% 
  tidytable::rename.('iss_length' = base) %>% 
  tidytable::mutate.(region = 'ai') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'ai', 'prod_iss_ag_rebs.csv')) %>% 
               tidytable::rename.('iss_age' = base) %>% 
               tidytable::left_join.(vroom::vroom(here::here('output', 'ai', 'prod_iss_sz_rebs.csv'))) %>% 
               tidytable::rename.('iss_length' = base) %>% 
               tidytable::mutate.(region = 'ai')) -> ai

# compile goa
vroom::vroom(here::here('output', 'goa', 'iss_ag.csv')) %>% 
  tidytable::rename.('iss_age' = base) %>% 
  tidytable::left_join.(vroom::vroom(here::here('output', 'goa', 'iss_sz.csv'))) %>% 
  tidytable::rename.('iss_length' = base) %>% 
  tidytable::mutate.(region = 'goa') %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_rebs.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>%
                          tidytable::left_join.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_rebs.csv'))) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_dr.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>% 
                          tidytable::left_join.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_dr.csv'))) %>% 
                          tidytable::rename.('iss_length' = base) %>% 
                          tidytable::mutate.(region = 'goa')) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_w_c_egoa.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>% 
                          tidytable::left_join.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_w_c_egoa.csv'))) %>% 
                          tidytable::rename.('iss_length' = base)) %>% 
  tidytable::bind_rows.(vroom::vroom(here::here('output', 'goa', 'prod_iss_ag_wc_egoa.csv')) %>% 
                          tidytable::rename.('iss_age' = base) %>% 
                          tidytable::left_join.(vroom::vroom(here::here('output', 'goa', 'prod_iss_sz_wc_egoa.csv'))) %>% 
                          tidytable::rename.('iss_length' = base)) -> goa

# compile all and write results
goa %>% 
  bind_rows.(ai) %>% 
  bind_rows.(bs) %>% 
  vroom::vroom_write(., 
                     here::here('output', 'afsc_iss.csv'), 
                     delim = ',')



# For testing run time of 500 iterations ----
if(iters < 100){
  end <- Sys.time()
  runtime <- (end - st) / iters * 500 / 60
  runtime
}

