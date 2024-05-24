#' the following was to test run times when gapindex functionality was incorporated into the
#' surveyISS package, and includes options for running with original age/length expansion
#' functions. note that these switches are now deprecated and this script will throw errors if attempted.
#' the final test run times for 10 iterations were:
#' 3.03 mins for original method
#' 2.18 mins for gap methods at regional level
#' 2.47 mins for gap methods at strata level










# load surveyISS library ----
# devtools::unload('surveyISS')
# devtools::install_github("BenWilliams-NOAA/surveyISS", force = TRUE)
library(surveyISS)

# set iterations ----
# set number of desired bootstrap iterations (suggested here: 10 for testing, 500 for running)
# iters = 500
iters = 10

# get data ----
# if query = TRUE then will run data queries, if FALSE will read previously run data
# set = TRUE if first time running, or if data has changed
query = FALSE

## gulf of alaska ----
region = 'goa'
yrs = 1990
species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
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