#' resample age-length data to implement growth variability
#'
#' @param age_dat age specimen data 
#'
#' @return
#' @export
#'
#' @examples
al_variab <- function(age_dat) {
  
  age_dat %>% 
    tidytable::mutate.(id = .I) -> age_dat
  
  age_dat %>% 
    tidytable::count.(age, species_code) -> .sample_sz
  
  age_dat %>% 
    tidytable::left_join.(.sample_sz) %>% 
    tidytable::select(species_code, age, length, n) %>%
    dplyr::group_by(age, species_code) %>% 
    dplyr::summarise(new_length = sample(length, size = mean(n), replace = T)) %>% 
    tidytable::left_join.(age_dat) %>% 
    dplyr::group_by(id) %>% 
    dplyr::slice_sample(n = 1) %>% 
    tidytable::select.(-length, -id) %>% 
    tidytable::rename.('length' = new_length)

}
