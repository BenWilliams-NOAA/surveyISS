#' resample age data w/replacement
#'
#' @param age_dat age specimen data 
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{boot_age(age_data)}
boot_age <- function(age_dat) {

  # combine sex-length-age to common id - bootstrap based on year, species, haul then split back apart
  age_dat %>%
    tidytable::mutate(sex_ln_ag = paste0(sex, "-", length, "-", age)) %>%
    tidytable::mutate(sex_ln_ag = sample(sex_ln_ag, .N, replace = TRUE), .by = c(year, species_code, hauljoin)) %>%
    tidytable::separate(sex_ln_ag, c('sex', 'length', "age"), sep = '-', convert = TRUE) -> .age_dat
  # add combined sex resampled data
  .age_dat %>% 
    tidytable::bind_rows(.age_dat %>% 
                           tidytable::mutate(sex = 0))

}