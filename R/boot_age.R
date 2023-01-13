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
    tidytable:: mutate.(sex_ln_ag = paste0(sex, "-", length, "-", age)) %>%
    .[, sex_ln_ag := sex_ln_ag[sample.int(.N, .N, replace = TRUE)],
      by = c('year', 'species_code', 'hauljoin') ] %>%
    tidytable::separate.(sex_ln_ag, c('sex_res', 'len_res', "age_res"), sep = '-') %>%
    tidytable::mutate.(sex = as.numeric(sex_res), 
                       length = as.numeric(len_res), 
                       age = as.numeric(age_res)) %>%
    tidytable::select.(-sex_res, -len_res, -age_res)   
}