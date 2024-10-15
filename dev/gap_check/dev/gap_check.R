#' example script to check expanded estimates of pop'n numbers at length and age from the surveyISS
#' package and those produced by gap (for both gap_products tables on akfin and the gapindex package - 
#' which in theory should be the same, but to check run the gap_prod_indx_test.R script)
#' two statistics are evaluated:
#' 1. maximum absolute percent difference in pop'n numbers at age and length, called mapd - gives
#' indication of how far off each estimate is
#' 2. sum of absolute difference in proportions at age and length, called sad - gives indication
#' of how much the age/length comp used in assessments is different
#' 
#' 5/23/24 note: was run a final time with switch to test original age/length expansion fcns, 
#' this switch is now deprecated, but result was that there was a mismatch between og and gap 
#' results, which was the driving force behind redeveloping expansion fcns to match gap_products 
#' and gapindex

# load/source libraries/functions ----
# devtools::unload('surveyISS')
# devtools::install_github("BenWilliams-NOAA/surveyISS", force = TRUE)
library(surveyISS)

source_files <- list.files(here::here("dev", "gap_check", "R"), "*.R$")
purrr::map(here::here("dev", "gap_check", "R", source_files), source)

# determine whether data pull is necessary ----
query_svyISS = FALSE
query_gpprod = FALSE
query_gpindx = FALSE

# check gap_products on akfin ----
# note: run surveyISS::query_data_t3 prior to this, and leave default query = FALSE

## check goa ----
reg_match_gapprod(region = 'goa',
                  query_svyISS = query_svyISS,
                  query_gpprod = query_gpprod,
                  reg_stratum = 99903,
                  survey = 47,
                  yrs = 1990)

## check ai ----
reg_match_gapprod(region = 'ai',
                  query_svyISS = query_svyISS,
                  query_gpprod = query_gpprod,
                  reg_stratum = 99904,
                  survey = 52,
                  yrs = 1991)

## check ebs ----
# note on 5/23/24: matches perfectly with gapindex and reg_stratum = 99901 (which is what sould be used)
# but need to have it set to 99900 to match with gap_products
reg_match_gapprod(region = 'ebs',
                  query_svyISS = query_svyISS,
                  query_gpprod = query_gpprod,
                  reg_stratum = 99900,
                  survey = 98,
                  yrs = 1979)

## check nbs ----
reg_match_gapprod(region = 'nbs',
                  query_svyISS = query_svyISS,
                  query_gpprod = query_gpprod,
                  reg_stratum = 99902,
                  survey = 143,
                  yrs = 1979)

## check ebs slope ----
reg_match_gapprod(region = 'ebs_slope',
                  query_svyISS = query_svyISS,
                  query_gpprod = query_gpprod,
                  reg_stratum = 99905,
                  survey = 78,
                  yrs = 2002)


# check gapindex ----
# note: only checking for a subset of species given gapindex run time

## check goa ----
species = c(10110, 21720, 21740, 30060)

reg_match_gapindex(region = 'goa',
                   query_svyISS = query_svyISS,
                   query_gpindx = query_gpindx,
                   species = species,
                   survey = 47,
                   yrs = 1990,
                   fill_NA_method = "AIGOA",
                   global = FALSE)

## check ai ----
species = c(10110, 21720, 21740, 30060)

reg_match_gapindex(region = 'ai',
                   query_svyISS = query_svyISS,
                   query_gpindx = query_gpindx,
                   species = species,
                   survey = 52,
                   yrs = 1991,
                   fill_NA_method = "AIGOA",
                   global = FALSE)

## check ebs ----
species = c(10210, 21720, 21740)

reg_match_gapindex(region = 'ebs',
                   query_svyISS = query_svyISS,
                   query_gpindx = query_gpindx,
                   species = species,
                   survey = 98,
                   yrs = 1979,
                   fill_NA_method = "BS",
                   global = FALSE)

## check nbs ----
species = c(10210, 21720, 21740)

reg_match_gapindex(region = 'nbs',
                   query_svyISS = query_svyISS,
                   query_gpindx = query_gpindx,
                   species = species,
                   survey = 143,
                   yrs = 1979,
                   fill_NA_method = "BS",
                   global = FALSE)

## check ebs slope ----
species = c(10110, 10112, 10115,30060)

reg_match_gapindex(region = 'ebs_slope',
                   query_svyISS = query_svyISS,
                   query_gpindx = query_gpindx,
                   species = species,
                   survey = 78,
                   yrs = 2002,
                   fill_NA_method = "BS",
                   global = FALSE)

