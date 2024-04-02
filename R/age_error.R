#' resample reader/tester data to implement ageing error
#'
#' @param age_dat age specimen data 
#' @param r_t reader/tester data 
#' @param nonpar use either sampling with replacement or multinomial for reader-tester data (default = FALSE)
#'
#' @return
#' @export
#'
#' @examples
age_error <- function(age_dat, 
                      r_t, 
                      nonpar = FALSE) {

  # add id
  age_dat %>% 
    tidytable::mutate(id = .I) -> age_dat

  # non-parametric (with replacement)
  if(isTRUE(nonpar)){
    # sample the age data from reader-tester results
    age_dat %>% 
      tidytable::inner_join(
        r_t %>% 
          tidytable::filter(.N >= 10, 
                            .by = c(age, species_code)) %>% 
          group_by(age, species_code) %>% 
          tidytable::mutate(new_age = sample(test_age, .N, replace = TRUE)) %>% 
          ungroup
      ) %>% 
      group_by(id) %>% 
      tidytable::slice_sample(n = 1) %>% 
      ungroup -> agerr
    
    # remove the old ages, replace with new ones and bind back with samples that were not tested
    agerr %>% 
      tidytable::select(-age, -test_age, -region, age = new_age) %>% 
      tidytable::bind_rows(anti_join(age_dat, agerr, by = "id")) %>% 
      tidytable::select(-id)
  } else {
    # parametric (using multinomial)
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
                             ungroup %>% 
                             tidytable::filter(new_age[,1] != 0) %>% 
                             tidytable::select(species_code, year, age, test_age, new_age) %>% 
                             tidytable::uncount(., new_age)) %>% 
      group_by(id) %>% 
      tidytable::slice_sample(n = 1) %>% 
      ungroup -> agerr
    
    # remove the old ages, replace with new ones and bind back with samples that were not tested
    agerr %>% 
      tidytable::select(-age, age = test_age) %>% 
      tidytable::bind_rows(anti_join(age_dat, agerr, by = "id")) %>% 
      tidytable::select(-id)
  }

}
