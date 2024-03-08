#' primary survey age/length pop'n numbers function
#'
#' @param lfreq_data length frequency data
#' @param specimen_data age-length specimen data
#' @param cpue_data abundance by length data 
#' @param strata_data strata and associated area 
#' @param r_t reader/tester ageing data 
#' @param yrs age filter returns years >= (default = NULL)
#' @param bin length bin size (default = 1 cm)
#' @param boot_hauls switch for resampling hauls (default = FALSE)
#' @param boot_lengths switch for resampling lengths (default = FALSE)
#' @param boot_ages switch for resampling ages (default = FALSE)
#' @param al_var switch for including age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err switch for including ageing error (default = FALSE)
#'
#' @return
#' @export srvy_comps
#'
#' @examples
#' 

srvy_comps <- function(lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs, bin,
                       boot_hauls, boot_lengths, boot_ages, al_var, al_var_ann, age_err) {
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
  
  # bin length data ---- # note that this automatically converts from mm to cm
  .lfreq_un %>% 
    tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .lfreq_un
  
  # length comp ----
  lcomp(.lfreq_un) -> .lcomp
  
  # length population ----
  lpop(.lcomp, .cpue, .lngs) -> .lpop
  
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
  
  .agedat %>% 
    tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .agedat
  
  # age population ----
  apop(.lpop, .agedat) -> .apop
  
  list(age = .apop, length = .lpop)
  
}