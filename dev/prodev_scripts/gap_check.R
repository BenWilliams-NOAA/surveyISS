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

# check goa ----

region = 'goa'

cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))
gap_lpop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lpop_', region, '.csv'))))
gap_apop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('apop_', region, '.csv'))))

yrs <- 1990

# get original age/length pop'n values
og <- srvy_comps(lfreq_data = lfreq, 
                 specimen_data = specimen, 
                 cpue_data = cpue, 
                 strata_data = strata,
                 r_t = NULL,
                 yrs = yrs, 
                 bin = 1,
                 boot_hauls = FALSE, 
                 boot_lengths = FALSE, 
                 boot_ages = FALSE,
                 al_var = FALSE,
                 al_var_ann = FALSE,
                 age_err = FALSE)

oga <- og$age
ogl <- og$length

match <- match_gap(oga, ogl, gap_apop, gap_lpop)

# Write out results
vroom::vroom_write(match[[1]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[2]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_a_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[3]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[4]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_a_", region, ".csv")), delim = ",")


# check ai ----

region = 'ai'

cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))
gap_lpop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lpop_', region, '.csv'))))
gap_apop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('apop_', region, '.csv'))))

yrs <- 1991

# get original age/length pop'n values
og <- srvy_comps(lfreq_data = lfreq, 
                 specimen_data = specimen, 
                 cpue_data = cpue, 
                 strata_data = strata,
                 r_t = NULL,
                 yrs = yrs, 
                 bin = 1,
                 boot_hauls = FALSE, 
                 boot_lengths = FALSE, 
                 boot_ages = FALSE,
                 al_var = FALSE,
                 al_var_ann = FALSE,
                 age_err = FALSE)

oga <- og$age
ogl <- og$length

match <- match_gap(oga, ogl, gap_apop, gap_lpop)

# Write out results
vroom::vroom_write(match[[1]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[2]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_a_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[3]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[4]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_a_", region, ".csv")), delim = ",")


# check ebs ----

region = 'ebs'

cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))
gap_lpop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lpop_', region, '.csv'))))
gap_apop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('apop_', region, '.csv'))))

yrs <- 1979

# get original age/length pop'n values
og <- srvy_comps(lfreq_data = lfreq, 
                 specimen_data = specimen, 
                 cpue_data = cpue, 
                 strata_data = strata,
                 r_t = NULL,
                 yrs = yrs, 
                 bin = 1,
                 boot_hauls = FALSE, 
                 boot_lengths = FALSE, 
                 boot_ages = FALSE,
                 al_var = FALSE,
                 al_var_ann = FALSE,
                 age_err = FALSE)

oga <- og$age
ogl <- og$length

match <- match_gap(oga, ogl, gap_apop, gap_lpop)

# Write out results
vroom::vroom_write(match[[1]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[2]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_a_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[3]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[4]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_a_", region, ".csv")), delim = ",")


# check nbs ----

region = 'nbs'

cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))
gap_lpop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lpop_', region, '.csv'))))
gap_apop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('apop_', region, '.csv'))))

yrs <- 1979

# get original age/length pop'n values
og <- srvy_comps(lfreq_data = lfreq, 
                 specimen_data = specimen, 
                 cpue_data = cpue, 
                 strata_data = strata,
                 r_t = NULL,
                 yrs = yrs, 
                 bin = 1,
                 boot_hauls = FALSE, 
                 boot_lengths = FALSE, 
                 boot_ages = FALSE,
                 al_var = FALSE,
                 al_var_ann = FALSE,
                 age_err = FALSE)

oga <- og$age
ogl <- og$length

match <- match_gap(oga, ogl, gap_apop, gap_lpop)

# Write out results
vroom::vroom_write(match[[1]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[2]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_a_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[3]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[4]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_a_", region, ".csv")), delim = ",")


# check nebs ----

region = 'nebs'

cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))
gap_lpop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lpop_', region, '.csv'))))
gap_apop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('apop_', region, '.csv'))))

yrs <- 1979

# get original age/length pop'n values
og <- srvy_comps(lfreq_data = lfreq, 
                 specimen_data = specimen, 
                 cpue_data = cpue, 
                 strata_data = strata,
                 r_t = NULL,
                 yrs = yrs, 
                 bin = 1,
                 boot_hauls = FALSE, 
                 boot_lengths = FALSE, 
                 boot_ages = FALSE,
                 al_var = FALSE,
                 al_var_ann = FALSE,
                 age_err = FALSE)

oga <- og$age
ogl <- og$length

match <- match_gap(oga, ogl, gap_apop, gap_lpop)

# Write out results
vroom::vroom_write(match[[1]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[2]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_a_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[3]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[4]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_a_", region, ".csv")), delim = ",")


# check ebs slope ----

region = 'ebs_slope'

cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))
gap_lpop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lpop_', region, '.csv'))))
gap_apop <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('apop_', region, '.csv'))))

yrs <- 2002

# get original age/length pop'n values
og <- srvy_comps(lfreq_data = lfreq, 
                 specimen_data = specimen, 
                 cpue_data = cpue, 
                 strata_data = strata,
                 r_t = NULL,
                 yrs = yrs, 
                 bin = 1,
                 boot_hauls = FALSE, 
                 boot_lengths = FALSE, 
                 boot_ages = FALSE,
                 al_var = FALSE,
                 al_var_ann = FALSE,
                 age_err = FALSE)

oga <- og$age
ogl <- og$length

match <- match_gap(oga, ogl, gap_apop, gap_lpop)

# Write out results
vroom::vroom_write(match[[1]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[2]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("mapd_a_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[3]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_l_", region, ".csv")), delim = ",")
vroom::vroom_write(match[[4]], file = here::here("dev", "prodev_scripts", "gap_check_output", paste0("sad_a_", region, ".csv")), delim = ",")

