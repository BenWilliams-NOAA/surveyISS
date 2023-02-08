#' resample reader/tester data to implement ageing error
#'
#' @param age_dat age specimen data 
#' @param r_t reader/tester data 
#'
#' @return
#' @export
#'
#' @examples
age_error <- function(age_dat, r_t) {

  # add id
  age_dat %>% 
    tidytable::mutate.(id = .I) -> age_dat
  
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
