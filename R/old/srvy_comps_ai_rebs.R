#' primary survey age/length pop'n numbers function (customized for ai rebs complex)
#'
#' @param lfreq_data length frequency data
#' @param specimen_data age-length specimen data
#' @param cpue_data abundance by length data 
#' @param strata_data strata and associated area 
#' @param r_t reader/tester ageing data 
#' @param yrs age filter returns years >= (default = NULL)
#' @param boot_hauls switch for resampling hauls (default = FALSE)
#' @param boot_lengths switch for resampling lengths (default = FALSE)
#' @param boot_ages switch for resampling ages (default = FALSE)
#' @param al_var switch for including age-length variability (default = FALSE)
#' @param age_err switch for including ageing error (default = FALSE)
#'
#' @return
#' @export srvy_comps_ai_rebs
#'
#' @examples
srvy_comps_ai_rebs <- function(lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs, 
                               boot_hauls, boot_lengths, boot_ages, al_var, age_err) {
  
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # complete cases by length/sex/strata for all years
  
  lfreq_data %>%
      tidytable::filter(year >= yrs) %>% 
      tidytable::distinct(year, species_code, length) %>% 
      tidytable::expand(year, length, .by = species_code) -> .lngs
  

  # first pass of filtering
  data.table::setDT(cpue_data) %>%
    tidytable::filter(year >= yrs) %>% 
    tidytable::left_join(strata_data) -> .cpue
  
  data.table::setDT(lfreq_data) %>%
    tidytable::filter(year >= yrs) %>% 
    tidytable::drop_na() -> .lfreq
  
  .lfreq %>% 
    tidytable::uncount(frequency) -> .lfreq_un
  
  data.table::setDT(specimen_data) %>%
    tidytable::filter(year >= yrs) %>% 
    tidytable::drop_na() -> .agedat
  
  # randomize hauls ----  
  if(isTRUE(boot_hauls)) {
    boot_haul(.cpue) %>% 
      tidytable::mutate(hauljoin_unq = .I) -> .hls
    
    .hls %>% 
      tidytable::left_join(.cpue) %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .cpue
    .hls %>% 
      tidytable::left_join(.lfreq) %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .lfreq
    .hls %>% 
      tidytable::left_join(.lfreq_un) %>% 
      tidytable::drop_na() %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .lfreq_un
    .hls %>% 
      tidytable::left_join(.agedat) %>% 
      tidytable::drop_na() %>% 
      tidytable::rename(hauljoin_orig = 'hauljoin',
                        hauljoin = 'hauljoin_unq') -> .agedat
    
  } 
  
  # randomize lengths ----
  if(isTRUE(boot_lengths)) {
    boot_length(.lfreq_un) %>% 
      tidytable::mutate(type = 'base') -> .lfreq_un
  } else{
    .lfreq_un %>% 
      tidytable::mutate(type = 'base') -> .lfreq_un
  }
  
  # length comp ----
  lcomp(.lfreq_un) -> .lcomp
  
  # length population ----
  lpop(.lcomp, .cpue, .lngs) %>% 
    tidytable::mutate(species_code = 3005012) -> .lpop
  
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
    al_variab(.agedat)  %>% 
      tidytable::mutate(type = 'al') -> .agedat_al
    .agedat_al %>% 
      tidytable::bind_rows(.agedat) -> .agedat
  }
  
  # add ageing error ----
  if(isTRUE(age_err)) {
    age_error(.agedat, r_t)  %>% 
      tidytable::mutate(type = 'ae') %>% 
      tidytable::bind_rows(.agedat) -> .agedat
  }
  
  # with age-length and ageing error ----
  if(isTRUE(al_var) & isTRUE(age_err)) {
    age_error(.agedat_al, r_t)  %>% 
      tidytable::mutate(type = 'ae_al') %>% 
      tidytable::bind_rows(.agedat) -> .agedat
  }
  
  .agedat %>% 
    tidytable::mutate(species_code = 3005012) -> .agedat
  
  # age population ----
  apop(.lpop, .agedat) -> .apop
  
  list(age = .apop, length = .lpop)
  
}