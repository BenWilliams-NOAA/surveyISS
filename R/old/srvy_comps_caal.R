#' primary survey age/length pop'n numbers function
#'
#' @param specimen_data age-length specimen data
#' @param cpue_data abundance by length data 
#' @param r_t reader/tester ageing data 
#' @param yrs age filter returns years >= (default = NULL)
#' @param boot_hauls switch for resampling hauls (default = FALSE)
#' @param boot_ages switch for resampling ages (default = FALSE)
#' @param sex_spec determine whether to do sex specific or total comps (default = TRUE)
#' @param al_var switch for including age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err switch for including ageing error (default = FALSE)
#'
#' @return
#' @export srvy_comps
#'
#' @examples
#' 

srvy_comps_caal <- function(specimen_data, cpue_data, r_t, yrs, 
                            boot_hauls, boot_ages, sex_spec, al_var, al_var_ann, age_err) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----

  # first pass of filtering
  data.table::setDT(cpue_data) %>%
    tidytable::filter(year >= yrs) -> .cpue
  
  data.table::setDT(specimen_data) %>%
    tidytable::filter(year >= yrs) %>% 
    tidytable::drop_na() -> .agedat
  
  # randomize hauls ----  
  if(isTRUE(boot_hauls)) {
    boot_haul(.cpue) %>% 
      tidytable::mutate(hauljoin_unq = .I) -> .hls

    .hls %>% 
      tidytable::left_join(.agedat) %>% 
      tidytable::drop_na() %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .agedat
    
  } 
  
  # randomize age ----
  if(isTRUE(boot_ages)) {
    boot_age(.agedat) %>% 
      tidytable::mutate(type = 'base') -> .agedat
  } else{
    .agedat %>% 
      tidytable::mutate(type = 'base') -> .agedat
  }
  
  # add age-length variability ----
  if(isTRUE(al_var)) {
    al_variab(.agedat, annual = al_var_ann)  %>% 
      tidytable::mutate(type = 'al') -> .agedat_al
  }
  
  # add ageing error ----
  if(isTRUE(age_err)) {
    age_error(.agedat, r_t)  %>% 
      tidytable::mutate(type = 'ae') -> .agedat_ae
  }
  
  # with age-length and ageing error ----
  if(isTRUE(al_var) & isTRUE(age_err)) {
    age_error(.agedat_al, r_t)  %>% 
      tidytable::mutate(type = 'ae_al') %>% 
      tidytable::bind_rows(.agedat_al) %>% 
      tidytable::bind_rows(.agedat_ae) %>% 
      tidytable::bind_rows(.agedat) -> .agedat
  } else if(isTRUE(al_var) & !isTRUE(age_err)){
    .agedat %>% 
      tidytable::bind_rows(.agedat_al) -> .agedat
  } else if(!isTRUE(al_var) & isTRUE(age_err)){
    .agedat %>% 
      tidytable::bind_rows(.agedat_ae) -> .agedat
  }
  
  # age population ----
  apop_caal(.agedat, sex_spec = sex_spec) -> .apop
  
  list(age = .apop)
  
}