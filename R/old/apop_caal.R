#' conditional age-at-length
#'
#' @param agedat age dataframe
#' @param sex_spec determine whether computing for sex-specific or total comps
#'
#' @return
#' @export apop_caal
#'
#' @examples
apop_caal <- function(agedat, sex_spec = TRUE){

  # compute conditional age-at-length for females & males
  if(isTRUE(sex_spec)){
    agedat %>%
      tidytable::drop_na() %>%
      tidytable::filter(sex != 3) %>%
      tidytable::summarise(age_num = .N, .by = c(year, species_code, sex, length, age, type)) %>%
      tidytable::mutate(age_frac = age_num/sum(age_num), 
                        .by = c(year, species_code, sex, length, type)) %>% 
      tidytable::select(-age_num) %>% 
      tidytable::pivot_wider(names_from = sex, values_from = age_frac, values_fill = 0) %>%
      tidytable::rename(males = '1', females = '2') %>% 
    tidytable::full_join(agedat %>%
                           tidytable::drop_na() %>% 
                           tidytable::filter(sex != 3) %>%
                           tidytable::summarise(age_num = .N, .by = c(year, species_code, length, age, type)) %>%  
                           tidytable::mutate(age_frac = age_num/sum(age_num), 
                                             .by = c(year, species_code, length, type)) %>% 
                           tidytable::select(-age_num) %>% 
                           tidytable::rename(total = 'age_frac'))
  } else{
    agedat %>%
      tidytable::drop_na() %>%
      tidytable::summarise(age_num = .N, .by = c(year, species_code, sex, length, age, type)) %>% 
      tidytable::mutate(age_frac = age_num/sum(age_num), 
                        .by = c(year, species_code, sex, length, type)) %>% 
      tidytable::select(-age_num) %>% 
      tidytable::pivot_wider(names_from = sex, values_from = age_frac, values_fill = 0) %>%
      tidytable::rename(total = '1')
  }
  
}