#' resample length data w/replacement
#'
#' @param lfreq_un expanded length frequency data 
#'
#' @return
#' @export
#'
#' @examples
boot_length <- function(lfreq_un) {
  
  lfreq_un %>%
    tidytable::mutate(sex_ln = paste0(sex, "-", length)) %>% 
    tidytable::mutate(sex_ln = sample(sex_ln, .N, replace = TRUE), 
                      .by = c(year, species_code, hauljoin)) %>% 
    tidytable::separate(sex_ln, c('sex', 'length'), sep = '-', convert = TRUE)   
}