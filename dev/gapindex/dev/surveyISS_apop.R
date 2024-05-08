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

# get survey ISS output

region = 'goa'

cpue_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))

yrs <- 1990

# complete cases by length/sex/strata for all years
lfreq_data %>%
  tidytable::filter(year >= yrs) %>% 
  tidytable::distinct(year, species_code, length) %>% 
  tidytable::expand(year, length, .by = species_code) -> lngs


############# put this in srvy_comps

# complete cases by year/length/sex/strata for all years
tidytable::expand_grid(year = unique(lfreq_data$year),
                       species_code = unique(specimen_data$species_code),
                       sex = unique(specimen_data$sex),
                       length = seq(from = min(lfreq_data$length, na.rm = TRUE), 
                                    to = max(lfreq_data$length, na.rm = TRUE), 
                                    by = 10),
                       age = seq(from = min(specimen_data$age, na.rm = TRUE), 
                                 to = max(specimen_data$age, na.rm = TRUE),
                                 by = 1)) -> lngs

###############

# first pass of filtering
data.table::setDT(cpue_data) %>%
  tidytable::filter(year >= yrs) %>% 
  tidytable::left_join(strata_data) -> cpue

data.table::setDT(lfreq_data) %>%
  tidytable::filter(year >= yrs & !is.na(frequency)) -> lfreq

lfreq %>% 
  tidytable::uncount(frequency) -> lfreq_un

lfreq_un %>% 
  tidytable::bind_rows(lfreq_un %>% 
                         tidytable::mutate(sex = 0)) -> lfreq_un

data.table::setDT(specimen_data) %>%
  tidytable::filter(year >= yrs) %>% 
  tidytable::drop_na() -> agedat

# get length pop'n
lpop_gap(lfreq_un, cpue) -> lpop

# add combined sex to agedat
agedat %>% 
  tidytable::bind_rows(agedat %>% 
                         tidytable::mutate(sex = 0)) -> agedat



# start of apop fcn ----


# compute female/male age pop'n


# Calculate distribution of age proportions for a given length, `p_yklm`. This is the non-global age-length key.
# female/male/unsexed
lngs %>% 
  tidytable::left_join(agedat %>%
                         tidytable::filter(!(sex %in% c(0, 3))) %>%
                         tidytable::bind_rows(agedat %>%
                                                tidytable::filter(sex != 0) %>%
                                                tidytable::mutate(sex = 3)) %>% 
                         tidytable::summarise(age_num = .N,
                                              .by = c(year, species_code, sex, length, age)) %>%
                         tidytable::mutate(age_frac = age_num/sum(age_num), 
                                           .by = c(year, species_code, sex, length))) -> .p_yklm
# combined sex categories
lngs %>% 
  tidytable::filter(sex == 1) %>% 
  tidytable::mutate(sex = 0) %>% 
  tidytable::left_join(agedat %>%
                         tidytable::filter(sex == 0) %>%
                         tidytable::summarise(age_num = .N,
                                              .by = c(year, species_code, sex, length, age)) %>%
                         tidytable::mutate(age_frac = age_num/sum(age_num), 
                                           .by = c(year, species_code, sex, length))) -> .p_yklm_comb

# Append the globally-filled lengths with the the non-global `p_yklm` alk to get a now global alk. 
# female/male/unsexed
lngs %>% 
  tidytable::left_join(.p_yklm %>% 
                         tidytable::filter(!is.na(age_frac)) %>% 
                         tidytable::bind_rows(.p_yklm %>% 
                                                # Determine missing lengths
                                                tidytable::summarise(age_frac = sum(age_frac, na.rm = TRUE), .by = c(year, species_code, sex, length)) %>% 
                                                tidytable::filter(age_frac == 0) %>% 
                                                tidytable::select(-age_frac) %>% 
                                                # for missing lengths, merge age probabilities from golbal ALK
                                                tidytable::left_join(agedat %>%
                                                                       # Aggregate specimen information over years to calculate a global ALK,
                                                                       tidytable::filter(sex != 0) %>%
                                                                       tidytable::summarise(age_num = .N,
                                                                                            .by = c(species_code, sex, length, age)) %>%
                                                                       tidytable::mutate(age_frac = age_num/sum(age_num), 
                                                                                         .by = c(species_code, sex, length))) %>% 
                                                tidytable::filter(!is.na(age_frac)))) %>% 
  tidytable::select(-age_num) %>% 
  tidytable::replace_na(list(age_frac = 0)) -> p_yklm
# combined sex categories
lngs %>% 
  tidytable::filter(sex == 1) %>% 
  tidytable::mutate(sex = 0) %>% 
  tidytable::left_join(.p_yklm_comb %>% 
                         tidytable::filter(!is.na(age_frac)) %>% 
                         tidytable::bind_rows(.p_yklm_comb %>% 
                                                # Determine missing lengths
                                                tidytable::summarise(age_frac = sum(age_frac, na.rm = TRUE), .by = c(year, species_code, sex, length)) %>% 
                                                tidytable::filter(age_frac == 0) %>% 
                                                tidytable::select(-age_frac) %>% 
                                                # for missing lengths, merge age probabilities from golbal ALK
                                                tidytable::left_join(agedat %>%
                                                                       # Aggregate specimen information over years to calculate a global ALK,
                                                                       tidytable::filter(sex == 0) %>%
                                                                       tidytable::summarise(age_num = .N,
                                                                                            .by = c(species_code, sex, length, age)) %>%
                                                                       tidytable::mutate(age_frac = age_num/sum(age_num), 
                                                                                         .by = c(species_code, sex, length))) %>% 
                                                tidytable::filter(!is.na(age_frac)))) %>% 
  tidytable::select(-age_num) %>% 
  tidytable::replace_na(list(age_frac = 0)) -> p_yklm_comb


# Calculate numbers at age as the product of the age_frac and the numbers at length
# female/male/unsexed
lpop %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::left_join(p_yklm) %>% 
  tidytable::replace_na(list(age = -9)) %>% 
  tidytable::replace_na(list(age_frac = 1)) %>% 
  tidytable::mutate(agepop = abund * age_frac) %>% 
  tidytable::select(-age_frac, -abund) %>% 
  # summarize numbers at age across length, and compute mean length
  tidytable::summarise(agepop = round(sum(agepop)),
                       mean_length = round(sum(length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2),
                       .by = c(year, species_code, sex, age)) %>% 
  tidytable::filter(agepop > 0 & age > 0) %>% 
  # combined sex categories
  tidytable::bind_rows(lpop %>% 
                         tidytable::filter(sex == 0) %>% 
                         tidytable::left_join(p_yklm_comb) %>% 
                         tidytable::replace_na(list(age = -9)) %>% 
                         tidytable::replace_na(list(age_frac = 1)) %>% 
                         tidytable::mutate(agepop = abund * age_frac) %>% 
                         tidytable::select(-age_frac, -abund) %>% 
                         # summarize numbers at age across length, and compute mean length
                         tidytable::summarise(agepop = round(sum(agepop)),
                                              mean_length = round(sum(length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2),
                                              .by = c(year, species_code, sex, age)) %>% 
                         tidytable::filter(agepop > 0 & age > 0)) -> age_comp











gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))


cruise <- gapdata$cruise
haul <- gapdata$haul
cpue <- gapindex::calc_cpue(gapdata)
size <- gapdata$size
racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                        cpue)
size_comp <- gapindex::calc_sizecomp_stratum(gapdata,
                                             cpue,
                                             racebase_stratum_popn) 
alk <- gapindex::calc_alk(gapdata,
                          unsex = "all")




age_comp_gap <- 
  merge(x = subset(x = size_comp, 
                   subset = SPECIES_CODE %in% unique(x = alk$SPECIES_CODE)),
        y = alk,
        by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "LENGTH_MM"),
        all.x = TRUE)
age_comp_gap$AGE[is.na(x = age_comp_gap$AGE)] <- -9
age_comp_gap$AGE_FRAC[is.na(x = age_comp_gap$AGE_FRAC)] <- 1
age_comp_gap$AGEPOP <- age_comp_gap$AGE_FRAC * age_comp_gap$POPULATION_COUNT




lpop %>% 
  tidytable::filter(sex != 0) %>% 
  tidytable::left_join(p_yklm) %>% 
  tidytable::replace_na(list(age = -9)) %>% 
  tidytable::replace_na(list(age_frac = 1)) %>% 
  tidytable::mutate(agepop = round(abund * age_frac)) %>% 
  tidytable::select(-age_frac, -abund) %>% 
  tidytable::rename(agepop_srv = 'agepop') -> age_comp



age_comp_gap %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::rename(length = 'length_mm') %>% 
  tidytable::summarise(agepop = round(sum(agepop)), .by = c(year, species_code, sex, length, age)) %>% 
  tidytable::left_join(age_comp) %>% 
  tidytable::mutate(diff = agepop - agepop_srv) %>% 
  tidytable::filter(diff != 0) %>% 
  print(n = 100)







# test for arrowtooth


gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))


cruise <- gapdata$cruise
haul <- gapdata$haul
gapdata$catch %>% 
  tidytable::filter(SPECIES_CODE == 10110) -> gapdata$catch
gapdata$size %>% 
  tidytable::filter(SPECIES_CODE == 10110) -> gapdata$size
gapdata$specimen %>% 
  tidytable::filter(SPECIES_CODE == 10110) -> gapdata$specimen
gapdata$species %>% 
  tidytable::filter(SPECIES_CODE == 10110) -> gapdata$species

cpue <- gapindex::calc_cpue(gapdata)

racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                        cpue)
size_comp <- gapindex::calc_sizecomp_stratum(gapdata,
                                             cpue,
                                             racebase_stratum_popn) 
alk <- gapindex::calc_alk(gapdata,
                          unsex = "all")

gap_age_comp_st <- gapindex::calc_agecomp_stratum(gapdata,
                                                  alk,
                                                  size_comp)

gap_age_comp <- gapindex::calc_agecomp_region(gapdata,
                                              gap_age_comp_st)





gap_apop <- vroom::vroom(here::here("dev", "gapindex", "data", paste0("gap_apop_full_", region, ".csv"))) %>% 
  tidytable::filter(species_code == 10110 & stratum == 99903 & age > 0) %>% 
  select(-survey, -stratum)

gap_age_comp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(year, species_code, sex, age, agepop_gapindex = population_count) %>% 
  tidytable::filter(age > 0) %>% 
  tidytable::left_join(gap_apop) %>% 
  tidytable::mutate(diff = agepop_gapindex - population_count) %>% 
  tidytable::filter(diff != 0)




age_comp %>% 
  tidytable::summarise(agepop_srv = sum(agepop_srv), .by = c(year, species_code, sex, age)) %>% 
  tidytable::filter(sex != 0 & age > 0 & species_code == 10110 & agepop_srv > 0) %>% 
  tidytable::left_join(gap_age_comp %>% 
                         dplyr::rename_all(tolower) %>% 
                         tidytable::select(year, species_code, sex, age, agepop_gapindex = population_count)) %>% 
  tidytable::mutate(diff = agepop_gapindex - agepop_srv)





nrow(p_yklm)
nrow(alk)

alk %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::rename(length = 'length_mm') %>% 
  tidytable::select(-survey) %>% 
  tidytable::left_join(p_yklm %>% 
                         tidytable::rename(age_frac_srv = 'age_frac')) %>% 
  tidytable::mutate(diff = age_frac - age_frac_srv) %>% 
  tidytable::filter(diff != 0)






cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))

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


gap_lpop <- vroom::vroom(here::here("dev", "gapindex", "data", paste0("gap_lpop_full_", region, ".csv"))) %>% 
  tidytable::filter(stratum == reg_stratum & length > 0)
gap_apop <- vroom::vroom(here::here("dev", "gapindex", "data", paste0("gap_apop_full_", region, ".csv"))) %>% 
  tidytable::filter(stratum == reg_stratum & age > 0)


oga %>% 
  filter(sex != 0) %>% 
  arrange(year, species_code, sex, age)


gap_apop %>% 
  filter(year < 2025) %>% 
  arrange(year, species_code, sex, age)



gap_apop %>% 
  tidytable::select(-stratum) %>% 
  tidytable::rename(abund_gap = population_count) %>% 
  tidytable::left_join(oga) %>% 
  filter(is.na(agepop)) %>% 
  filter(sex == 3) %>% 
  distinct(year)
tidytable::drop_na() %>% 
  tidytable::mutate(match_num = abs(agepop - abund_gap)/abund_gap) %>% 
  tidytable::summarize(match_mapd_a = sum(match_num, na.rm = TRUE),
                       .by = c(year, species_code, sex)) %>% 
  tidytable::mutate(test_mapd_a = tidytable::case_when(match_mapd_a < thresh_mapd ~ 'Y',
                                                       match_mapd_a > thresh_mapd ~ 'N'))






























p_yklm_comb %>% 
  filter(age_frac > 0)

age_comp %>% 
  filter(sex == 0)



age_comp %>% 
  tidytable::summarise(agepop = round(sum(agepop)),
                       mean_length = round(sum(length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2),
                       .by = c(year, species_code, sex, age)) %>% 
  tidytable::filter(agepop > 0) %>% 
  tidytable::arrange(year, species_code, sex, age) %>% 
  print(n = 100)















  tidytable::arrange(year, species_code, sex, length)






%>% 
  tidytable::left_join(.lpop_long) %>%
  tidytable::drop_na() %>% 
  tidytable::mutate(agepop = age_frac * sizepop, 
                    .by = c(year, species_code, sex, length)) %>%
  tidytable::summarize(agepop = sum(agepop), 
                       .by = c(year, species_code, sex, age)) %>%
  tidytable::filter(sex != 3) -> .agepop_mf

.agepop_mf %>% 
  print(n = 50)


# determine magnitude of unsex samples
agedat %>%
  tidytable::drop_na() %>%  
  tidytable::summarise(n = .N,
                       .by = c(year, species_code, sex)) %>% 
  tidytable::filter(sex == 3) %>%
  tidytable::select(year, species_code, n) -> .sex_cnt_ag
# if unsexed samples exist, compute unsexed age pop'n
if(length(.sex_cnt_ag$n)>0){
  .lpop_long %>%
    tidytable::filter(sex == 3) -> .lpop_long_un
  agedat %>%
    tidytable::left_join(.sex_cnt_ag) %>%
    tidytable::filter(n > 0) %>%
    tidytable::summarise(age_num = .N,
                         .by = c(year, species_code, length, age)) %>%
    tidytable::mutate(age_frac = age_num/sum(age_num), 
                      .by = c(year, species_code, length)) %>%
    tidytable::left_join(.lpop_long_un) %>%
    tidytable::drop_na() %>% 
    tidytable::mutate(agepop = age_frac * sizepop, 
                      .by = c(year, species_code, length)) %>%
    tidytable::summarise(agepop = sum(agepop), 
                         .by = c(year, species_code, sex, age)) %>% 
    # add female/male age pop'n
    tidytable::bind_rows(.agepop_mf) %>% 
    # compute and add total (combined sex) age pop'n
    tidytable::bind_rows(agedat %>%
                           tidytable::filter(sex == 0) %>% 
                           tidytable::summarise(age_num = .N,
                                                .by = c(year, species_code, length, age)) %>%
                           tidytable::mutate(age_frac = age_num/sum(age_num), 
                                             .by = c(year, species_code, length)) %>%
                           tidytable::left_join(.lpop_long %>%
                                                  tidytable::filter(sex == 0)) %>%
                           tidytable::drop_na() %>% 
                           tidytable::mutate(agepop = age_frac * sizepop, 
                                             .by = c(year, species_code, length)) %>%
                           tidytable::summarise(agepop = sum(agepop), 
                                                .by = c(year, species_code, sex, age)))
} else {
  # female/male age pop'n
  .agepop_mf %>% 
    # compute and add total (combined sex) age pop'n
    tidytable::bind_rows(agedat %>%
                           tidytable::filter(sex == 0) %>% 
                           tidytable::summarise(age_num = .N,
                                                .by = c(year, species_code, length, age)) %>% 
                           tidytable::mutate(age_frac = age_num/sum(age_num), 
                                             .by = c(year, species_code, length)) %>%
                           tidytable::left_join(.lpop_long %>%
                                                  tidytable::filter(sex == 0)) %>%
                           tidytable::drop_na() %>% 
                           tidytable::mutate(agepop = age_frac * sizepop, 
                                             .by = c(year, species_code, length)) %>%
                           tidytable::summarise(agepop = sum(agepop), 
                                                .by = c(year, species_code, sex, age)))
}



