#' Resample length frequency data w/replacement.
#'
#' @param lfreq_un expanded length frequency data (one row per sample)
#' 
#' @return dataframe of resampled length-sex pairs by year, species, and haul
#'
#' @export
#'
boot_length <- function(lfreq_un) {
  
  # combine sex-length to common id - bootstrap based on year, species, haul then split back apart
  lfreq_un %>%
    tidytable::mutate(sex_ln = paste0(sex, "-", length)) %>%
    tidytable::mutate(sex_ln = base::sample(sex_ln, .N, replace = TRUE), .by= c(year, species_code, hauljoin)) %>%
    tidytable::separate(sex_ln, c('sex', 'length'), sep = '-', convert = TRUE) -> .lfreq_un
  # add combined sex resampled data
  .lfreq_un %>% 
    tidytable::bind_rows(.lfreq_un %>% 
                           tidytable::mutate(sex = 0))

}