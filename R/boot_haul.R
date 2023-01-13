#' resample hauls w/replacement
#'
#' @param cpue_data cpue dataframe
#'
#' @return
#' @export
#'
#' @examples
boot_haul <- function(cpue_data) {
  
  cpue_data %>%
    dplyr::group_by(year, species_code) %>%
    dplyr::distinct(hauljoin) %>%
    tidytable::as_tidytable(.) %>%
    .[, hauljoin := hauljoin[sample.int(.N, .N, replace = TRUE)],
      by = c('year', 'species_code')]
}