#' resample age-length data to implement growth variability
#'
#' @param age_dat age specimen data 
#'
#' @return
#' @export
#'
#' @examples
al_variab <- function(age_dat) {
  
  age_dat %>% 
    tidytable::mutate(length = sample(length, .N, replace = TRUE), 
                        .by = c(age, species_code, sex))

}
