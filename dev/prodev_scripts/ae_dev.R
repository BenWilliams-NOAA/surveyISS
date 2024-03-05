# example script to obtain age/length input sample size for experimental runs

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

# get data ----

age_dat <- vroom::vroom(here::here('data', 'specimen_goa.csv')) %>% 
  tidytable::select(-weight) %>% 
  tidytable::mutate(id = .I)

age_dat %>% 
  tidytable::distinct(species_code) -> species

r_t <- vroom::vroom(here::here('data', 'reader_tester.csv')) %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::select(species_code, region, read_age, test_age) %>% 
  tidytable::rename(age = 'read_age') %>% 
  tidytable::filter(species_code %in% species$species_code)

# subset to most common reater-tester ages for cod and pop

r_t %>% 
  tidytable::filter(species_code == 21740 & age == 5) %>% 
  tidytable::bind_rows(
    r_t %>% 
      tidytable::filter(species_code == 30060 & age == 10)) -> r_t_sub

age_dat %>% 
  tidytable::filter(species_code == 21740 & age == 5 & year == 1984) %>% 
  tidytable::bind_rows(
    age_dat %>% 
      tidytable::filter(species_code == 30060 & age == 10 & year == 1987)) -> age_dat_sub



# resampling example fcn ----

nonpar <- function(age_dat, r_t){
  # sample the age data from reader-tester results
  age_dat %>% 
    tidytable::inner_join(
      r_t %>% 
        tidytable::filter(.N >= 10, 
                          .by = c(age, species_code)) %>% 
        tidytable::mutate(new_age = sample(test_age, .N, replace = TRUE), 
                          .by = c(age, species_code))
    ) %>% 
    tidytable::slice_sample(n = 1, .by = id) -> agerr
  
  # remove the old ages, replace with new ones and bind back with samples that were not tested
  agerr %>% 
    tidytable::select(-age, -test_age, -region, age = new_age) %>% 
    tidytable::bind_rows(anti_join(age_dat, agerr, by = "id")) %>% 
    tidytable::select(-id)
}

# multinomial example fcn ----

multi <- function(age_dat, r_t){
  # sample the age data from reader-tester results
  age_dat %>% 
    tidytable::drop_na() %>% 
    tidytable::left_join(age_dat %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(aged = .N, .by = c(species_code, year, age)) %>% 
                           tidytable::left_join(r_t %>% 
                                                  tidytable::summarise(count = .N, .by = c(species_code, age, test_age)) %>%
                                                  tidytable::mutate(p_a = count / sum(count), .by = c(species_code, age))) %>% 
                           tidytable::drop_na() %>% 
                           dplyr::group_by(species_code, year, age) %>% 
                           dplyr::mutate(new_age = rmultinom(1, aged, p_a)) %>% 
                           tidytable::filter(new_age[,1] != 0) %>% 
                           tidytable::select(species_code, year, age, test_age, new_age) %>% 
                           tidytable::uncount(., new_age)) %>% 
    tidytable::slice_sample(n = 1, .by = id) -> agerr
  
  # remove the old ages, replace with new ones and bind back with samples that were not tested
  agerr %>% 
    tidytable::select(-age, age = test_age) %>% 
    tidytable::bind_rows(anti_join(age_dat, agerr, by = "id")) %>% 
    tidytable::select(-id)
}


# run test ----

# set number of desired bootstrap iterations
# iters = 100
iters = 250

# multinomial
# for testing run time
st <- Sys.time()

rr_multi <- purrr::map(1:iters, ~ age_error(age_dat = age_dat_sub, r_t = r_t_sub))

# For testing run time of 500 iterations
end <- Sys.time()
runtime_multi <- (end - st)


# resampling
# for testing run time
st <- Sys.time()

rr_nonpar <- purrr::map(1:iters, ~ age_error(age_dat = age_dat_sub, r_t = r_t_sub, nonpar = TRUE))

# For testing run time of 500 iterations
end <- Sys.time()
runtime_nonpar <- (end - st)


# analyze results

r_t_sub %>% 
  tidytable::summarise(count = .N, .by = c(species_code, test_age)) %>% 
  tidytable::mutate(p_rt = count / sum(count), .by = c(species_code)) %>% 
  tidytable::select(species_code, age = test_age, p_rt) %>% 
  tidytable::left_join(rr_multi %>% 
                         tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                         tidytable::summarise(count = .N, .by = c(species_code, sim, age)) %>% 
                         tidytable::summarise(avg_count = mean(count), .by = c(species_code, age)) %>% 
                         tidytable::mutate(p_multi = avg_count / sum(avg_count), .by = c(species_code)) %>% 
                         tidytable::select(species_code, age, p_multi)) %>% 
  tidytable::left_join(rr_nonpar %>% 
                         tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                         tidytable::summarise(count = .N, .by = c(species_code, sim, age)) %>% 
                         tidytable::summarise(avg_count = mean(count), .by = c(species_code, age)) %>% 
                         tidytable::mutate(p_nonpar = avg_count / sum(avg_count), .by = c(species_code)) %>% 
                         tidytable::select(species_code, age, p_nonpar)) -> samp_test


ggplot(data = samp_test, aes(x = age, y = p_rt)) +
  geom_bar(stat = "identity", fill = "darkblue") +
  facet_wrap(~species_code, ncol = 1) +
  geom_line(aes(x = age, y = p_multi), color = "orange") +
  geom_point(aes(x = age, y = p_multi), color = "orange") +
  geom_line(aes(x = age, y = p_nonpar), color = "green") +
  geom_point(aes(x = age, y = p_nonpar), color = "green")

runtime_multi
runtime_nonpar
