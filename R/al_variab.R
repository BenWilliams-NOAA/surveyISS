#' resample age-length data to implement growth variability
#'
#' @param age_dat age specimen data 
#' @param annual resample age data annually or pooled across years
#'
#' @return
#' @export
#'
#' @examples
al_variab <- function(age_dat, 
                      annual = FALSE) {

  if(isTRUE(annual)){
    age_dat %>% 
      dplyr::group_by(age, species_code, sex, year) %>% 
      tidytable::mutate(length = sample(length, .N, replace = TRUE))
  } else{
    age_dat %>% 
      dplyr::group_by(age, species_code, sex) %>% 
      tidytable::mutate(length = sample(length, .N, replace = TRUE))
  }
  
}
