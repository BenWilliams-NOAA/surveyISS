#' Resample reader/tester data to implement ageing error.
#'
#' @param age_dat age-length specimen input dataframe 
#' @param r_t reader/tester agreement data 
#'
#' @export
#'
age_error <- function(age_dat, 
                      r_t) {

  # add id
  age_dat %>% 
    tidytable::mutate(id = .I) -> age_dat

  # sample the age data from reader-tester results
  age_dat %>% 
    tidytable::drop_na() %>% 
    tidytable::left_join(age_dat %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(aged = .N,
                                                .by = c(species_code, year, age)) %>% 
                           tidytable::left_join(r_t %>% 
                                                  tidytable::summarise(count = .N, .by = c(species_code, age, test_age)) %>%
                                                  tidytable::mutate(p_a = count / sum(count), .by = c(species_code, age))) %>% 
                           tidytable::drop_na() %>% 
                           dplyr::group_by(species_code, year, age) %>% 
                           dplyr::mutate(new_age = rmultinom(1, aged, p_a)) %>% 
                           dplyr::ungroup() %>% 
                           # note the following throws an error
                           # tidytable::mutate(new_age = rmultinom(1, aged, p_a), .by = c(species_code, year, age)) %>% 
                           tidytable::filter(new_age[,1] != 0) %>% 
                           tidytable::select(species_code, year, age, test_age, new_age) %>% 
                           tidytable::uncount(., new_age)) %>% 
    tidytable::slice_sample(n = 1, .by = c(id)) -> agerr
  
  # remove the old ages, replace with new ones and bind back with samples that were not tested
  agerr %>% 
    tidytable::mutate(age = tidytable::case_when(!is.na(test_age) ~ test_age,
                                                 .default = age)) %>% 
    tidytable::select(-test_age) %>% 
    tidytable::bind_rows(tidytable::anti_join(age_dat, agerr, by = "id")) %>% 
    tidytable::select(-id)

}
