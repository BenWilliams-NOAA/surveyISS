#' resample age-length data to implement growth variability
#'
#' @param age_dat age specimen data 
#' @param annual resample age data annually or pooled across years
#'
#' @return
#' @export
#'
#' @examples
al_variab <- function(age_dat, annual = FALSE) {
  
  if(isTRUE(annual)){
    age_dat %>% 
      tidytable::mutate(length = sample(length, .N, replace = TRUE), 
                        .by = c(age, species_code, sex, year))}
  else{
    age_dat %>% 
      tidytable::mutate(length = sample(length, .N, replace = TRUE), 
                        .by = c(age, species_code, sex))
  }
  
}
