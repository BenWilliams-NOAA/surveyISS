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

  age_dat %>% 
    tidytable::mutate.(id = .I) -> age_dat

  age_dat %>% 
    tidytable::count.(age, species_code) -> .sample_sz
  
  r_t %>% 
    tidytable::count.(age, species_code) %>% 
    filter(n < 10) -> r_t_rem # filter to at least 10 reader-tester replicates
  
  r_t %>% 
    anti_join(r_t_rem) -> .r_t

  .r_t %>% 
    tidytable::left_join.(.sample_sz) %>% 
    tidytable::drop_na.() %>% 
    tidytable::select(species_code, age, test_age, n) %>%
    dplyr::group_by(age, species_code) %>% 
    dplyr::summarise(new_age = sample(test_age, size = mean(n), replace = T)) %>% 
    tidytable::left_join.(age_dat) %>% 
    dplyr::group_by(id) %>% 
    dplyr::slice_sample(n = 1) -> agerr
  
  age_dat %>% 
    tidytable::anti_join.(agerr) %>% 
    tidytable::select.(-id) -> no_match # ages in specimen data that haven't been tested
    
  agerr %>% 
    tidytable::select.(-age, -id) %>% 
    tidytable::rename.('age' = new_age) %>% 
    tidytable::bind_rows.(no_match)
   
}
