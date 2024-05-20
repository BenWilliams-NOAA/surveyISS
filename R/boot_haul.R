#' Resample hauls w/replacement.
#'
#' @param cpue_data catch-per-unit effort input dataframe
#'
#' @return dataframe of resampled hauls by year and species
#' 
#' @export
#'
boot_haul <- function(cpue_data) {
  
  cpue_data %>% 
    tidytable::select(year, species_code, hauljoin) %>% 
    tidytable::distinct() %>% 
    tidytable::mutate(hauljoin = sample(hauljoin, .N, replace = TRUE), .by = c(year, species_code)) 
  
}