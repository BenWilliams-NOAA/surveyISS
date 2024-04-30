# example script to check expanded estimates of pop'n numbers at length and age from the surveyISS package and those produced by gap
# two statistics are evaluated:
# 1. maximum absolute percent difference in pop'n numbers at age and length, called mapd - gives indication of how far off each estimate is
# 2. sum of absolute difference in proportions at age and length, called sad - gives indication of how much the age/length comp used in assessments is different

# load/source libraries/functions
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

source_files <- list.files(here::here("dev", "gapindex", "R"), "*.R$")
map(here::here("dev", "gapindex", "R", source_files), source)

# check goa ----

reg_match_gap(region = 'goa',
              query = FALSE,
              reg_stratum = 99903,
              species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200),
              survey = 47,
              yrs = 1990)

# check ai ----

reg_match_gap(region = 'ai',
              query = FALSE,
              reg_stratum = 99904,
              species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200),
              survey = 52,
              yrs = 1991)

# check ebs ----

reg_match_gap(region = 'ebs',
              query = FALSE,
              reg_stratum = 99900,
              species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740),
              survey = 98,
              yrs = 1979)

# check nbs ----

reg_match_gap(region = 'nbs',
              query = FALSE,
              reg_stratum = 99902,
              species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740),
              survey = 143,
              yrs = 1979)

# check ebs slope ----

reg_match_gap(region = 'ebs_slope',
              query = FALSE,
              reg_stratum = 99905,
              species = c(10110, 10112, 10115,30060),
              survey = 78,
              yrs = 2002)
