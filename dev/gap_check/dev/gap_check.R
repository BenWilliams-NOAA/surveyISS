# example script to check expanded estimates of pop'n numbers at length and age from the surveyISS package and those produced by gap
# two statistics are evaluated:
# 1. maximum absolute percent difference in pop'n numbers at age and length, called mapd - gives indication of how far off each estimate is
# 2. sum of absolute difference in proportions at age and length, called sad - gives indication of how much the age/length comp used in assessments is different

# load/source libraries/functions
library(tidyverse)
library(tidytable)
library(vroom)
library(here)
library(gapindex)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

source_files <- list.files(here::here("dev", "gap_check", "R"), "*.R$")
map(here::here("dev", "gap_check", "R", source_files), source)

# check gap_products on akfin ----

## check goa ----
reg_match_gapprod(region = 'goa',
                  query = TRUE,
                  reg_stratum = 99903,
                  species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200),
                  survey = 47,
                  yrs = 1990,
                  global = FALSE)

## check ai ----
reg_match_gapprod(region = 'ai',
                  query = TRUE,
                  reg_stratum = 99904,
                  species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200),
                  survey = 52,
                  yrs = 1991,
                  global = FALSE)

## check ebs ----
reg_match_gapprod(region = 'ebs',
                  query = TRUE,
                  reg_stratum = 99900,
                  species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740),
                  survey = 98,
                  yrs = 1979,
                  global = FALSE)

## check nbs ----
reg_match_gapprod(region = 'nbs',
                  query = TRUE,
                  reg_stratum = 99902,
                  species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740),
                  survey = 143,
                  yrs = 1979,
                  global = FALSE)

## check ebs slope ----
reg_match_gapprod(region = 'ebs_slope',
                  query = TRUE,
                  reg_stratum = 99905,
                  species = c(10110, 10112, 10115,30060),
                  survey = 78,
                  yrs = 2002,
                  global = FALSE)


# check gapindex ----
# note: only checking for a subset of species given gapindex run time

## check goa ----
species1 = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
species2 = c(10110, 21720, 21740, 30060)

reg_match_gapindex(region = 'goa',
                   query = FALSE,
                   species = species2,
                   survey = 47,
                   yrs = 1990,
                   fill_NA_method = "AIGOA",
                   global = FALSE)

## check ai ----
species1 = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)
species2 = c(10110, 21720, 21740, 30060)

reg_match_gapindex(region = 'ai',
                   query = FALSE,
                   species = species2,
                   survey = 52,
                   yrs = 1991,
                   fill_NA_method = "AIGOA",
                   global = FALSE)

## check ebs ----
species1 = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
species2 = c(10210, 21720, 21740)

reg_match_gapindex(region = 'ebs',
                   query = FALSE,
                   species = species2,
                   survey = 98,
                   yrs = 1979,
                   fill_NA_method = "BS",
                   global = FALSE)

## check nbs ----
species1 = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
species2 = c(10210, 21720, 21740)

reg_match_gapindex(region = 'nbs',
                   query = FALSE,
                   species = species2,
                   survey = 143,
                   yrs = 1979,
                   fill_NA_method = "BS",
                   global = FALSE)

## check ebs slope ----
species1 = c(10110, 10112, 10115,30060)

reg_match_gapindex(region = 'ebs_slope',
                   query = FALSE,
                   species = species1,
                   survey = 78,
                   yrs = 2002,
                   fill_NA_method = "BS",
                   global = FALSE)

