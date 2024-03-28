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
    tidytable::select(year, species_code, hauljoin) %>% 
    tidytable::distinct() %>% 
    dplyr::group_by(year, species_code) %>% 
    tidytable::mutate(hauljoin = sample(hauljoin, .N, replace = TRUE))
  
}