#' Resample age-length data to implement growth variability in age-length key.
#'
#' @param age_dat age-length specimen input dataframe 
#' @param annual resample age-length data by years or pooled across years
#' 
#' @export
#'
al_variab <- function(age_dat, 
                      annual = FALSE) {

  if(isTRUE(annual)){
    age_dat %>% 
      tidytable::drop_na() %>% 
      tidytable::mutate(id = .I) %>% 
      tidytable::left_join(age_dat %>% 
                             tidytable::drop_na() %>% 
                             tidytable::summarise(lengthed = .N, 
                                                  .by = c(species_code, year, sex, age, length)) %>% 
                             tidytable::mutate(p_l = lengthed / sum(lengthed), 
                                               .by = c(species_code, year, sex, age)) %>% 
                             tidytable::drop_na() %>% 
                             dplyr::group_by(species_code, year, sex, age) %>% 
                             tidytable::mutate(samp_length = rmultinom(1, sum(lengthed), p_l)) %>% 
                             dplyr::ungroup() %>% 
                             # note the following throws an error
                             # tidytable::mutate(samp_length = rmultinom(1, sum(lengthed), p_l), .by = c(species_code, year, sex, age)) %>% 
                             tidytable::filter(samp_length[,1] != 0) %>% 
                             tidytable::select(species_code, year, sex, age, length, samp_length) %>% 
                             tidytable::uncount(., samp_length) %>% 
                             tidytable::rename(new_length = length)) %>% 
      tidytable::slice_sample(n = 1, .by = c(id)) %>% 
      tidytable::select(-length, length = new_length) %>% 
      tidytable::select(-id)
  } else{
    age_dat %>% 
      tidytable::drop_na() %>% 
      tidytable::mutate(id = .I) %>% 
      tidytable::left_join(age_dat %>% 
                             tidytable::drop_na() %>% 
                             tidytable::summarise(lengthed = .N,
                                                  .by = c(species_code, sex, age, length)) %>% 
                             tidytable::mutate(p_l = lengthed / sum(lengthed), 
                                               .by = c(species_code, sex, age)) %>% 
                             tidytable::drop_na() %>% 
                             dplyr::group_by(species_code, sex, age) %>% 
                             tidytable::mutate(samp_length = rmultinom(1, sum(lengthed), p_l)) %>% 
                             dplyr::ungroup() %>% 
                             # note the following throws an error
                             # tidytable::mutate(samp_length = rmultinom(1, sum(lengthed), p_l), .by = c(species_code, sex, age)) %>% 
                             tidytable::filter(samp_length[,1] != 0) %>% 
                             tidytable::select(species_code, sex, age, length, samp_length) %>% 
                             tidytable::uncount(., samp_length) %>% 
                             tidytable::rename(new_length = length)) %>% 
      tidytable::slice_sample(n = 1, .by = c(id)) %>% 
      tidytable::select(-length, length = new_length) %>% 
      tidytable::select(-id)
  }
  
}
