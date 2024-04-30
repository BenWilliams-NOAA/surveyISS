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

region = 'nbs'

cpue_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
lfreq_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
strata_data <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))

yrs <- 1979

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


# male/female/unsexed
lfreq_un %>%
  tidytable::filter(sex != 0) %>% 
  tidytable::summarise(frequency = .N, 
                       .by = c(year, species_code, stratum, hauljoin, sex, length)) %>% 
  tidytable::inner_join(lfreq_un %>%
                          tidytable::filter(sex != 0) %>% 
                          tidytable::summarise(s_ijk = .N, 
                                               .by = c(year, species_code, stratum, hauljoin))) %>% 
  tidytable::inner_join(cpue) %>% 
  ## The size CPUE (S_ijklm)
  tidytable::mutate(S_ijklm = frequency / s_ijk * numcpue) -> size

# combined sex
lfreq_un %>%
  tidytable::filter(sex == 0) %>% 
  tidytable::summarise(frequency = .N, 
                       .by = c(year, species_code, stratum, hauljoin, sex, length)) %>% 
  tidytable::inner_join(lfreq_un %>%
                          tidytable::filter(sex == 0) %>% 
                          tidytable::summarise(s_ijk = .N, 
                                               .by = c(year, species_code, stratum, hauljoin))) %>% 
  tidytable::inner_join(cpue) %>% 
  ## The size CPUE (S_ijklm)
  tidytable::mutate(S_ijklm = frequency / s_ijk * numcpue) -> size_tot

# id hauls without lengths
# see issue #35 for reasoning 
#
# note from gapindex - 
# For AI and GOA survey  region, the size composition for hauls with
# positive counts but missing size data is imputted by averaging the
# size composition from the hauls in that same stratum and year.

if(unique(cpue$survey) %in% c(47, 52)){
  # male/female/unsexed
  size <- size %>% 
    tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm) %>% 
    ## Append the `missing_hauljoins` to size.
    tidytable::bind_rows(cpue %>%
                           tidytable::filter(numcpue > 0) %>%
                           tidytable::distinct(hauljoin, species_code)  %>% 
                           ## ID hauls with positive counts but have no records in `size`
                           tidytable::anti_join(size %>%
                                                  tidytable::summarise(hauljoin = unique(hauljoin),
                                                                       .by = c(species_code, stratum))) %>% 
                           tidytable::inner_join(cpue) %>%
                           ## Calculate mean S_ijklm of individuals of species-k with sex-m and 
                           ## length-l among the hauls within stratum-i. Technically, this would be
                           ## something like S_hat_iklm. 
                           tidytable::inner_join(size %>% 
                                                   tidytable::mutate(p_ijklm = frequency / s_ijk) %>% 
                                                   ## First we sum all the proportions within a stratum...
                                                   tidytable::summarise(p_ijklm = sum(p_ijklm), .by = c(year, stratum, species_code, sex, length)) %>% 
                                                   ## Then we calculate the total number of unique hauls within a stratum...
                                                   tidytable::inner_join(lfreq_un %>%
                                                                           tidytable::filter(sex != 0) %>% 
                                                                           tidytable::summarise(nhauls = data.table::uniqueN(hauljoin),
                                                                                                .by = c(year, species_code, stratum))) %>% 
                                                   ## Then we calculate the average proportion.
                                                   tidytable::mutate(p_ijklm = p_ijklm / nhauls), .by = c("year", "stratum", "species_code")) %>% 
                           ## Recalculate S_ijklm using the imputted size composition. 
                           tidytable::mutate(S_ijklm = p_ijklm * numcpue) %>% 
                           tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm))
  
  # combined sex
  size_tot <- size_tot %>% 
    tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm) %>% 
    ## Append the `missing_hauljoins` to size.
    tidytable::bind_rows(cpue %>%
                           tidytable::filter(numcpue > 0) %>%
                           tidytable::distinct(hauljoin, species_code)  %>% 
                           ## ID hauls with positive counts but have no records in `size`
                           tidytable::anti_join(size_tot %>%
                                                  tidytable::summarise(hauljoin = unique(hauljoin),
                                                                       .by = c(species_code, stratum))) %>% 
                           tidytable::inner_join(cpue) %>%
                           ## Calculate mean S_ijklm of individuals of species-k with sex-m and 
                           ## length-l among the hauls within stratum-i. Technically, this would be
                           ## something like S_hat_iklm. 
                           tidytable::inner_join(size_tot %>% 
                                                   tidytable::mutate(p_ijklm = frequency / s_ijk) %>% 
                                                   ## First we sum all the proportions within a stratum...
                                                   tidytable::summarise(p_ijklm = sum(p_ijklm), .by = c(year, stratum, species_code, sex, length)) %>% 
                                                   ## Then we calculate the total number of unique hauls within a stratum...
                                                   tidytable::inner_join(lfreq_un %>%
                                                                           tidytable::filter(sex == 0) %>% 
                                                                           tidytable::summarise(nhauls = data.table::uniqueN(hauljoin),
                                                                                                .by = c(year, species_code, stratum))) %>% 
                                                   ## Then we calculate the average proportion.
                                                   tidytable::mutate(p_ijklm = p_ijklm / nhauls), .by = c("year", "stratum", "species_code")) %>% 
                           ## Recalculate S_ijklm using the imputted size composition. 
                           tidytable::mutate(S_ijklm = p_ijklm * numcpue) %>% 
                           tidytable::select(year, stratum, hauljoin, species_code, length, sex, numcpue, S_ijklm))
}

# note from gapindex - 
# Continuing on with Wakabayashi et al.
#   1985 Equation 17: The number of individuals (population) of species-k
#   of sex-m and length-l in stratum-i is noted as P_iklm.
#
# S_ijklm is summed over hauls-j, creating S_iklm and this is divided
#    by S_ik, which is S_ijklm summed over hauls-j, sex-m, and length-l.
# S_iklm / S_ik is the proportion of the stratum population attributed
#    to sex-m and length-l for species-k in stratum-i. When multiplied by
#    P_ik, we get P_iklm.
#
# P_ik (racebase_stratum_popn$pop) and S_ik are merged into the S_iklm
#    df via the year, stratum, and species_code values. Wakabayashi et al.
#    1985 assumes one year of data so there is no year index in their
#    paper but this function has the capability of calculating size comps
#    for multiple years of data.

# male/female/unsexed
size %>% 
  ## Aggregate S_ijklm across stratum, species_code, length bin, and sex
  tidytable::summarise(S_iklm = sum(S_ijklm), .by = c(year, stratum, species_code, length, sex)) %>% 
  ## Aggregate S_ijklm across stratum and species_code
  tidytable::inner_join(size %>% 
                          tidytable::summarise(S_ik = sum(S_ijklm),
                                               .by = c(year, stratum, species_code))) %>% 
  # join pop'n estimates
  tidytable::inner_join(cpue %>%
                          tidytable::filter(numcpue >= 0) %>%
                          tidytable::mutate(st_num = mean(numcpue) * area,
                                            tot = sum(numcpue), 
                                            .by = c(year, species_code, stratum)) %>%
                          tidytable::summarise(abund = mean(numcpue) / tot * st_num,
                                               .by = c(year, species_code, stratum, hauljoin)) %>% 
                          tidytable::summarise(abund = sum(abund, na.rm = TRUE), 
                                               .by = c(year, species_code, stratum))) %>% 
  filter(species_code == 21740 & year == 2021 & stratum == 71) %>% 
  arrange(-length, sex) %>% 
  print(n = 150)
  















%>% 
  ## There are some strata with no length data. In these cases, the length
  ## is coded as -9 and sex = 3. S_ik and S_ik are set to 1 to ease calculations
  tidytable::replace_na(list(length = -9)) %>% 
  tidytable::replace_na(list(sex = 3)) %>% 
  tidytable::replace_na(list(S_iklm = 1)) %>% 
  tidytable::replace_na(list(S_ik = 1)) %>% 
  ## Stratum-level population calculation
  tidytable::mutate(number = round(abund * S_iklm / S_ik)) %>% 
  tidytable::filter(year == 2023 & stratum == 41 & species_code == 21740) %>% 
  arrange(-length, sex) %>% 
  print(n = 150)


%>% 
  ## Remove zero-records
  tidytable::filter(number > 0) %>% 
  tidytable::select(-S_iklm, - S_ik, -abund) %>% 
  # rename
  tidytable::rename(abund = 'number') %>% 
  











%>% 
  # combined sex
  tidytable::bind_rows(size_tot %>% 
                         ## Aggregate S_ijklm across stratum, species_code, length bin, and sex
                         tidytable::summarise(S_iklm = sum(S_ijklm), .by = c(year, stratum, species_code, length, sex)) %>% 
                         ## Aggregate S_ijklm across stratum and species_code
                         tidytable::inner_join(size_tot %>% 
                                                 tidytable::summarise(S_ik = sum(S_ijklm),
                                                                      .by = c(year, stratum, species_code))) %>% 
                         # join pop'n estimates
                         tidytable::inner_join(cpue %>%
                                                 tidytable::filter(numcpue >= 0) %>%
                                                 tidytable::mutate(st_num = mean(numcpue) * area,
                                                                   tot = sum(numcpue), 
                                                                   .by = c(year, species_code, stratum)) %>%
                                                 tidytable::summarise(abund = mean(numcpue) / tot * st_num,
                                                                      .by = c(year, species_code, stratum, hauljoin)) %>% 
                                                 tidytable::summarise(abund = sum(abund, na.rm = TRUE), 
                                                                      .by = c(year, species_code, stratum))) %>% 
                         ## There are some strata with no length data. In these cases, the length
                         ## is coded as -9 and sex = 3. S_ik and S_ik are set to 1 to ease calculations
                         tidytable::replace_na(list(length = -9)) %>% 
                         tidytable::replace_na(list(sex = 3)) %>% 
                         tidytable::replace_na(list(S_iklm = 1)) %>% 
                         tidytable::replace_na(list(S_ik = 1)) %>% 
                         ## Stratum-level population calculation
                         tidytable::mutate(number = round(abund * S_iklm / S_ik)) %>% 
                         ## Remove zero-records
                         tidytable::filter(number > 0) %>% 
                         tidytable::select(-S_iklm, - S_ik, -abund) %>% 
                         # rename
                         tidytable::rename(abund = 'number')) 











tidytable::filter(year == 2023 & stratum == 41 & species_code == 21740) %>% 
  arrange(-length, sex)























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
ogl <- og$length %>% 
  tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length, sex))
