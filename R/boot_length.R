#' resample length data w/replacement
#'
#' @param lfreq_un expanded length frequency data 
#'
#' @return
#' @export
#'
#' @examples
boot_length <- function(lfreq_un) {
  
  # combine sex-length to common id - bootstrap based on year, species, haul then split back apart
  lfreq_un %>%
    tidytable::mutate(sex_ln = paste0(sex, "-", length)) %>%
    group_by(year, species_code, hauljoin) %>% 
    tidytable::mutate(sex_ln = sample(sex_ln, .N, replace = TRUE)) %>%
    tidytable::separate(sex_ln, c('sex', 'length'), sep = '-', convert = TRUE) -> .lfreq_un
  # add combined sex resampled data
  .lfreq_un %>% 
    tidytable::bind_rows(.lfreq_un %>% 
                           tidytable::mutate(sex = 0))

}