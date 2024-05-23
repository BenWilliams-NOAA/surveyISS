# example script to obtain age/length input sample size for production run

# load surveyISS library ----
devtools::install_github("afsc-assessments/surveyISS", force = TRUE)
library(surveyISS)

## load/source libraries/functions for testing ----
# library(purrr)
# library(tidyverse)
# library(tidytable)
# library(psych)
# library(vroom)
# library(here)
# 
# source_files <- list.files(here::here("R"), "*.R$")
# map(here::here("R", source_files), source)

# get data ----
# if query = TRUE then will run data queries, if FALSE will read previously run data
# set = TRUE if first time running, or if data has changed
query = FALSE

## gulf of alaska ----
region = 'goa'
yrs = 1990
species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
# species = c(21740, 30060) # pollock and pop for testing
survey = 47

if(isTRUE(query)){
  data_goa <- surveyISS::query_data(survey = survey,
                                    region = region,
                                    species = species,
                                    yrs = yrs)
  
  saveRDS(data_goa, file = here::here('data', region, 'data.RDS'))
} else{
  data_goa <- readRDS(file = here::here('data', region, 'data.RDS'))
}

# gulf of alaska ----

## run for all species (and subsetting out special cases so we don't have two places with those results) ----
data_goa$cpue %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .cpue
data_goa$lfreq %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .lfreq
data_goa$specimen %>% 
  tidytable::filter(!(species_code %in% c(30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200, 21740))) -> .specimen

## original fcns ----

# for testing run time
st_og <- Sys.time()

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = .lfreq,
                    specimen_data = .specimen, 
                    cpue_data = .cpue, 
                    strata_data = data_goa$strata, 
                    yrs = yrs,
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    use_gapindex = FALSE,
                    by_strata = FALSE,
                    region = region, 
                    save = 'og_test')

end_og <- Sys.time()


## gap fcns at regional level ----

# for testing run time
st_gap_reg <- Sys.time()

surveyISS::srvy_iss(iters = iters, 
                    lfreq_data = .lfreq,
                    specimen_data = .specimen, 
                    cpue_data = .cpue, 
                    strata_data = data_goa$strata, 
                    yrs = yrs,  
                    boot_hauls = TRUE, 
                    boot_lengths = TRUE, 
                    boot_ages = TRUE, 
                    al_var = TRUE, 
                    al_var_ann = TRUE, 
                    age_err = TRUE,
                    use_gapindex = TRUE,
                    by_strata = FALSE,
                    region = region, 
                    save = 'gapreg_test')

end_gap_reg <- Sys.time()

## gap fcns at strata level ----

# for testing run time
st_gap_st <- Sys.time()

srvy_iss(iters = iters,
         lfreq_data = .lfreq,
         specimen_data = .specimen,
         cpue_data = .cpue,
         strata_data = data_goa$strata,
         yrs = yrs,
         boot_hauls = TRUE,
         boot_lengths = TRUE,
         boot_ages = TRUE,
         al_var = TRUE,
         al_var_ann = TRUE,
         age_err = TRUE,
         use_gapindex = TRUE,
         by_strata = TRUE,
         region = region,
         save = 'gapst_test')

end_gap_st <- Sys.time()

# For testing run time ----

end_og - st_og
end_gap_reg - st_gap_reg
end_gap_st - st_gap_st