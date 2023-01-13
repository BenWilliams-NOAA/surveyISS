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
    tidytable::mutate.(sex_ln = paste0(sex, "-", length)) %>%
    .[, sex_ln := sex_ln[sample.int(.N, .N, replace = TRUE)],
      by = c('year', 'species_code', 'hauljoin') ]  %>%
    tidytable::separate.(., sex_ln, c('sex_res', 'len_res'), sep = '-') %>%
    tidytable::mutate.(sex = as.numeric(sex_res), 
                       length = as.numeric(len_res))  %>%
    tidytable::select.(-sex_res, -len_res)
}