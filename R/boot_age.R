#' resample age data w/replacement
#'
#' @param age_dat age specimen data 
#'
#' @return
#' @export
#'
#' @examples
boot_age <- function(age_dat) {

  # combine sex-length-age to common id - bootstrap based on year, species, haul then split back apart
  
  age_dat %>%
    tidytable::mutate(sex_ln_ag = paste0(sex, "-", length, "-", age)) %>%
    tidytable::mutate(sex_ln_ag = sample(sex_ln_ag, .N, replace = TRUE),
                      .by = c(year, species_code, hauljoin)) %>%
    tidytable::separate(sex_ln_ag, c('sex', 'length', "age"), sep = '-', convert = TRUE) -> .age_dat
  
  .age_dat %>% 
    tidytable::bind_rows(.agedat %>% 
                           tidytable::mutate(sex = 0))
  
  # age_dat %>%
  #   tidytable::mutate(ln_ag = paste0(length, "-", age)) %>%
  #   tidytable::mutate(ln_ag = sample(ln_ag, .N, replace = TRUE), 
  #                     .by = c(year, species_code, hauljoin, sex)) %>%
  #   tidytable::separate(ln_ag, c('length', 'age'), sep = '-', convert = TRUE) 
}