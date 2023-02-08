#' resample age data w/replacement
#'
#' @param age_dat age specimen data 
#'
#' @return
#' @export
#'
#' @examples
boot_age <- function(age_dat) {

  age_dat %>%
    tidytable::mutate.(sex_ln_ag = paste0(sex, "-", length, "-", age)) %>%
    tidytable::mutate(sex_ln_ag = sample(sex_ln_ag, .N, replace = TRUE), 
                      .by = c(year, species_code, hauljoin)) %>%
    tidytable::separate(sex_ln_ag, c('sex', 'length', "age"), sep = '-', convert = TRUE)   
}