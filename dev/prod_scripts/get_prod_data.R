# get data for production run of bottom trawl survey iss (all tier 3 and above species)

# load/source libraries/functions
library(purrr)
library(tidyverse)
library(tidytable)
library(psych)
library(vroom)
library(here)

source_files <- list.files(here::here("R"), "*.R$")
map(here::here("R", source_files), source)

# get goa data ----

yrs = 1990
species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
# species = c(21740, 30060) # pollock and pop for testing
survey = 47
region = 'GOA'

query_data(survey, region, species, yrs)
  
# get ai data ----

yrs = 1991
species = c(10110, 10112, 21720, 21740, 21921, 30060, 30420, 30050, 30051, 30052)
survey = 52
region = 'AI'

query_data(survey, region, species, yrs)

# get ebs slope data ----

yrs = 2002
species = c(10110, 10112, 10115,30060)
survey = 78
region = 'EBS_slope'

query_data(survey, region, species, yrs)

# get ebs data ----

yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
survey = 98
region = 'EBS'

query_data(survey, region, species, yrs)

# get nbs data ----

yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
survey = 143
region = 'NBS'

query_data(survey, region, species, yrs)

# get ebs & nbs data ----

yrs = 1979
species = c(10110, 10112, 10115, 10130, 10210, 10261, 10285, 21720, 21740)
survey = c(98, 143)
region = 'NEBS'

query_data(survey, region, species, yrs)
