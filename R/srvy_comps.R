#' Resample data and compute length and age population expansion
#'
#' @description
#' Following Fig. 1 in Hulson and Williams 2024 this function resamples cpue, length frequency,
#' and age specimen data, adds ageing error and growth variability, and expands these data to population at length and age.
#' 
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe, included in surveyISS package data
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm), also can use custom length bins through defining vector of upper length bin limits, i.e., c(5, 10, 20, 35, 60), plus length group will automatically be populated and denoted with the largest defined bin + 1 (i.e., 61 for provided example)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key? (default = FALSE)
#' 
#' @return List with dataframes of population numbers at length (.lpop) and age (.apop).
#' 
#' @export
#'
srvy_comps <- function(lfreq_data, 
                       specimen_data, 
                       cpue_data, 
                       strata_data, 
                       r_t, 
                       yrs = NULL, 
                       bin = 1,
                       boot_hauls = FALSE, 
                       boot_lengths = FALSE, 
                       boot_ages = FALSE, 
                       al_var = FALSE,
                       al_var_ann = FALSE,
                       age_err = FALSE,
                       len_samples = NULL,
                       age_samples = NULL,
                       by_strata = FALSE,
                       global = FALSE) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # complete cases by year/length/sex/strata for all years (following gapindex)
  tidytable::expand_grid(year = unique(lfreq_data$year),
                         species_code = unique(specimen_data$species_code),
                         sex = unique(specimen_data$sex),
                         length = seq(from = min(lfreq_data$length, na.rm = TRUE), 
                                      to = max(lfreq_data$length, na.rm = TRUE), 
                                      by = 10),
                         age = seq(from = min(specimen_data$age, na.rm = TRUE), 
                                   to = max(specimen_data$age, na.rm = TRUE),
                                   by = 1)) -> .lngs
  # bin by cm blocks
  if(length(bin) == 1){
    .lngs %>% 
      tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) %>% 
      tidytable::distinct(year, species_code, sex, length, age) -> .lngs
  } else{ # custom length bins
    # set up bin bounds
    c(0, bin) %>% 
      tidytable::bind_cols(c(bin, bin[length(bin)] + 1)) %>% 
      tidytable::rename(lwr = '...1', upr = '...2') -> bin_bnds
    # determine which bin length is in, and define new length as upper bin
    # note, plus bin is denoted as max length bin + 1
    .lngs %>% 
      tidytable::distinct(length) %>% 
      tidytable::mutate(new_length = bin_bnds$upr[max(which(bin_bnds$lwr < length / 10))], 
                        .by = c(length)) -> new_lengths
    # replace lengths in length frequency data with new binned lengths
    .lngs %>% 
      tidytable::left_join(new_lengths) %>% 
      tidytable::select(-length, length = new_length) %>% 
      tidytable::distinct(year, species_code, sex, length, age) -> .lngs
  }
  
  # first pass of filtering
  data.table::setDT(cpue_data) %>%
    tidytable::filter(year >= yrs) %>% 
    tidytable::left_join(strata_data) -> .cpue
  
  data.table::setDT(lfreq_data) %>%
    tidytable::filter(year >= yrs & !is.na(frequency)) -> .lfreq
  
  .lfreq %>% 
    tidytable::uncount(frequency) -> .lfreq_un
  
  data.table::setDT(specimen_data) %>%
    tidytable::filter(year >= yrs) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(length = round(length / 10) * 10) -> .agedat
  
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
  
  # randomize lengths (and add sex = 0 for sex-combined (total) comp calculations) ----
  if(isTRUE(boot_lengths)) {
    boot_length(.lfreq_un) -> .lfreq_un
  } else{
    .lfreq_un %>% 
      tidytable::bind_rows(.lfreq_un %>% 
                             tidytable::mutate(sex = 0)) -> .lfreq_un
  }
  
  # if desired, reduce length frequency sample sizes
  if(!is.null(len_samples)) {
    red_len_samples(.lfreq_un, samples = length_samples) -> .lfreq_un
  }
  
  # bin length data, note that this automatically converts from mm to cm
  # bin by cm blocks
  if(length(bin) == 1){
    .lfreq_un %>% 
      tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .lfreq_un
  } else{ # custom length bins
    # set up bin bounds
    c(0, bin) %>% 
      tidytable::bind_cols(c(bin, bin[length(bin)] + 1)) %>% 
      tidytable::rename(lwr = '...1', upr = '...2') -> bin_bnds
    # determine which bin length is in, and define new length as upper bin
    # note, plus bin is denoted as max length bin + 1
    .lfreq_un %>% 
      tidytable::distinct(length) %>% 
      tidytable::mutate(new_length = bin_bnds$upr[max(which(bin_bnds$lwr < length / 10))], 
                        .by = c(length)) -> new_lengths
    # replace lengths in length frequency data with new binned lengths
    .lfreq_un %>% 
      tidytable::left_join(new_lengths) %>% 
      tidytable::select(-length, length = new_length) -> .lfreq_un
  }
  
  # length population ----
  lpop(.lfreq_un, .cpue, by_strata = by_strata) -> .lpop
  
  
  # randomize age  (and add sex = 0 for sex-combined (total) comp calculations) ----
  if(isTRUE(boot_ages)) {
    boot_age(.agedat) -> .agedat
  } else{
    .agedat %>% 
      tidytable::bind_rows(.agedat %>% 
                             tidytable::mutate(sex = 0)) -> .agedat
  }
  
  # with age-length and ageing error
  if(isTRUE(al_var) & isTRUE(age_err)) {
    al_variab(.agedat, annual = al_var_ann) -> .agedat_al
    age_error(.agedat_al, r_t) -> .agedat
  } else if(isTRUE(al_var) & !isTRUE(age_err)){
    al_variab(.agedat, annual = al_var_ann) -> .agedat
  } else if(!isTRUE(al_var) & isTRUE(age_err)){
    age_error(.agedat, r_t) -> .agedat
  }
  
  # if desired, adjust age sample sizes
  if(!is.null(age_samples)) {
    adj_age_samples(.agedat, rate = age_samples) -> .agedat
  }
  
  # bin lengths in age data, note that this automatically converts from mm to cm
  # bin by cm blocks
  if(length(bin) == 1){
    .agedat %>% 
      tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .agedat
  } else{ # custom length bins
    # set up bin bounds
    c(0, bin) %>% 
      tidytable::bind_cols(c(bin, bin[length(bin)] + 1)) %>% 
      tidytable::rename(lwr = '...1', upr = '...2') -> bin_bnds
    # determine which bin length is in, and define new length as upper bin
    # note, plus bin is denoted as max length bin + 1
    .agedat %>% 
      tidytable::distinct(length) %>% 
      tidytable::mutate(new_length = bin_bnds$upr[max(which(bin_bnds$lwr < length / 10))], 
                        .by = c(length)) -> new_lengths
    # replace lengths in length frequency data with new binned lengths
    .agedat %>% 
      tidytable::left_join(new_lengths) %>% 
      tidytable::select(-length, length = new_length) -> .agedat
  }
  
  # age population ----
  apop(.lpop, .agedat, .lngs, by_strata = by_strata, global = global) -> .apop
  
  # return age and length pop'n estimates
  if(isTRUE(by_strata)){
    .lpop %>% 
      tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length, sex)) -> .lpop
  }
  
  list(age = .apop, length = .lpop)
  
}

#' Resample data and compute length and age population expansion
#'
#' @description
#' Function to resample data and compute length and age expansion following Fig. 1 in Hulson and Williams 2024. 
#' Customized for ai complexes (i.e., blackspotted-rougheye rockfish) where population at length are expanded at species 
#' level, but age specimen data are combined and population at age are expanded at complex level
#'
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe, included in surveyISS package data
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm), also can use custom length bins through defining vector of upper length bin limits, i.e., c(5, 10, 20, 35, 60), plus length group will automatically be populated and denoted with the largest defined bin + 1 (i.e., 61 for provided example)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param cmplx_code Numeric value to replace the individual species codes with a complex code shared across species. (default = 3005012)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key? (default = FALSE)
#' 
#' @return List with dataframes of population numbers at length (.lpop) and age (.apop).
#' 
#' @export
#'
srvy_comps_ai_cmplx <- function(lfreq_data, 
                                specimen_data, 
                                cpue_data, 
                                strata_data, 
                                r_t, 
                                yrs = NULL, 
                                bin = 1,
                                boot_hauls = FALSE, 
                                boot_lengths = FALSE, 
                                boot_ages = FALSE, 
                                al_var = FALSE,
                                al_var_ann = FALSE,
                                age_err = FALSE,
                                len_samples = NULL,
                                age_samples = NULL,
                                cmplx_code = NULL,
                                by_strata = FALSE,
                                global = FALSE) {
  # globals ----
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # prep data ----
  # complete cases by year/length/sex/strata for all years (following gapindex)
  tidytable::expand_grid(year = unique(lfreq_data$year),
                         species_code = unique(specimen_data$species_code),
                         sex = unique(specimen_data$sex),
                         length = seq(from = min(lfreq_data$length, na.rm = TRUE), 
                                      to = max(lfreq_data$length, na.rm = TRUE), 
                                      by = 10),
                         age = seq(from = min(specimen_data$age, na.rm = TRUE), 
                                   to = max(specimen_data$age, na.rm = TRUE),
                                   by = 1)) -> .lngs
  # bin by cm blocks
  if(length(bin) == 1){
    .lngs %>% 
      tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) %>% 
      tidytable::distinct(year, species_code, sex, length, age) -> .lngs
  } else{ # custom length bins
    # set up bin bounds
    c(0, bin) %>% 
      tidytable::bind_cols(c(bin, bin[length(bin)] + 1)) %>% 
      tidytable::rename(lwr = '...1', upr = '...2') -> bin_bnds
    # determine which bin length is in, and define new length as upper bin
    # note, plus bin is denoted as max length bin + 1
    .lngs %>% 
      tidytable::distinct(length) %>% 
      tidytable::mutate(new_length = bin_bnds$upr[max(which(bin_bnds$lwr < length / 10))], 
                        .by = c(length)) -> new_lengths
    # replace lengths in length frequency data with new binned lengths
    .lngs %>% 
      tidytable::left_join(new_lengths) %>% 
      tidytable::select(-length, length = new_length) %>% 
      tidytable::distinct(year, species_code, sex, length, age) -> .lngs
  }
  
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
    tidytable::drop_na() %>%
    tidytable::mutate(species_code = cmplx_code) -> .agedat
  
  data.table::setDT(r_t) %>%
    tidytable::mutate(species_code = cmplx_code) -> r_t
  
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
    boot_length(.lfreq_un) -> .lfreq_un
  } else{
    .lfreq_un %>% 
      tidytable::bind_rows(.lfreq_un %>% 
                             tidytable::mutate(sex = 0)) -> .lfreq_un
  }
  
  # if desired, reduce length frequency sample sizes
  if(!is.null(len_samples)) {
    red_len_samples(.lfreq_un, samples = length_samples) -> .lfreq_un
  }
  
  # bin length data, note that this automatically converts from mm to cm
  # bin by cm blocks
  if(length(bin) == 1){
    .lfreq_un %>% 
      tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .lfreq_un
  } else{ # custom length bins
    # set up bin bounds
    c(0, bin) %>% 
      tidytable::bind_cols(c(bin, bin[length(bin)] + 1)) %>% 
      tidytable::rename(lwr = '...1', upr = '...2') -> bin_bnds
    # determine which bin length is in, and define new length as upper bin
    # note, plus bin is denoted as max length bin + 1
    .lfreq_un %>% 
      tidytable::distinct(length) %>% 
      tidytable::mutate(new_length = bin_bnds$upr[max(which(bin_bnds$lwr < length / 10))], 
                        .by = c(length)) -> new_lengths
    # replace lengths in length frequency data with new binned lengths
    .lfreq_un %>% 
      tidytable::left_join(new_lengths) %>% 
      tidytable::select(-length, length = new_length) -> .lfreq_un
  }
  
  # length population ----
  lpop(.lfreq_un, .cpue, by_strata = by_strata) %>% 
    tidytable::summarise(abund = sum(abund), .by = c(year, length, sex)) %>% 
    tidytable::mutate(species_code = cmplx_code) -> .lpop
  
  # randomize age  (and add sex = 0 for sex-combined (total) comp calculations) ----
  if(isTRUE(boot_ages)) {
    boot_age(.agedat) -> .agedat
  } else{
    .agedat %>% 
      tidytable::bind_rows(.agedat %>% 
                             tidytable::mutate(sex = 0)) -> .agedat
  }
  
  # add age-length variability
  if(isTRUE(al_var)) {
    al_variab(.agedat, annual = al_var_ann) -> .agedat_al
  }
  
  # add ageing error
  if(isTRUE(age_err)) {
    age_error(.agedat, r_t) -> .agedat_ae
  }
  
  # with age-length and ageing error
  if(isTRUE(al_var) & isTRUE(age_err)) {
    age_error(.agedat_al, r_t) -> .agedat
  } else if(isTRUE(al_var) & !isTRUE(age_err)){
    .agedat_al -> .agedat
  } else if(!isTRUE(al_var) & isTRUE(age_err)){
    .agedat_ae -> .agedat
  }
  
  # if desired, adjust age sample sizes
  if(!is.null(age_samples)) {
    adj_age_samples(.agedat, rate = age_samples) -> .agedat
  }
  
  # bin lengths in age data, note that this automatically converts from mm to cm
  # bin by cm blocks
  if(length(bin) == 1){
    .agedat %>% 
      tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .agedat
  } else{ # custom length bins
    # set up bin bounds
    c(0, bin) %>% 
      tidytable::bind_cols(c(bin, bin[length(bin)] + 1)) %>% 
      tidytable::rename(lwr = '...1', upr = '...2') -> bin_bnds
    # determine which bin length is in, and define new length as upper bin
    # note, plus bin is denoted as max length bin + 1
    .agedat %>% 
      tidytable::distinct(length) %>% 
      tidytable::mutate(new_length = bin_bnds$upr[max(which(bin_bnds$lwr < length / 10))], 
                        .by = c(length)) -> new_lengths
    # replace lengths in length frequency data with new binned lengths
    .agedat %>% 
      tidytable::left_join(new_lengths) %>% 
      tidytable::select(-length, length = new_length) -> .agedat
  }
  
  # age population ----
  apop(.lpop, .agedat, .lngs, by_strata = by_strata, global = global) -> .apop
  
  list(age = .apop, length = .lpop)
  
}

#' Resample data and compute conditional age-at-length
#'
#' @description
#' This function resamples cpue and age specimen data, adds ageing error and growth variability,
#' and then computes conditional age-at-length.
#' 
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param r_t age reader-tester input dataframe, included in surveyISS package data
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm), also can use custom length bins through defining vector of upper length bin limits, i.e., c(5, 10, 20, 35, 60), plus length group will automatically be populated and denoted with the largest defined bin + 1 (i.e., 61 for provided example)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' 
#' @return List with dataframe of conditional age-at-length (.caal).
#' 
#' @export
#'
srvy_comps_caal <- function(specimen_data, 
                            cpue_data,
                            r_t, 
                            yrs = NULL,  
                            bin = 1,
                            boot_hauls = FALSE, 
                            boot_ages = FALSE,
                            al_var = FALSE,
                            al_var_ann = FALSE,
                            age_err = FALSE,
                            age_samples = NULL) {
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
    boot_age(.agedat) -> .agedat
  } else{
    .agedat %>% 
      tidytable::bind_rows(.agedat %>% 
                             tidytable::mutate(sex = 0)) -> .agedat
  }
  
  # add age-length variability
  if(isTRUE(al_var)) {
    al_variab(.agedat, annual = al_var_ann) -> .agedat_al
  }
  
  # add ageing error
  if(isTRUE(age_err)) {
    age_error(.agedat, r_t) -> .agedat_ae
  }
  
  # with age-length and ageing error
  if(isTRUE(al_var) & isTRUE(age_err)) {
    age_error(.agedat_al, r_t) -> .agedat
  } else if(isTRUE(al_var) & !isTRUE(age_err)){
    .agedat_al -> .agedat
  } else if(!isTRUE(al_var) & isTRUE(age_err)){
    .agedat_ae -> .agedat
  }
  
  # if desired, adjust age sample sizes
  if(!is.null(age_samples)) {
    adj_age_samples(.agedat, rate = age_samples) -> .agedat
  }
  
  # bin lengths in age data, note that this automatically converts from mm to cm
  # bin by cm blocks
  if(length(bin) == 1){
    .agedat %>% 
      tidytable::mutate(length = 10 * (bin * ceiling((length / 10) / bin))) -> .agedat
  } else{ # custom length bins
    # set up bin bounds
    c(0, bin) %>% 
      tidytable::bind_cols(c(bin, bin[length(bin)] + 1)) %>% 
      tidytable::rename(lwr = '...1', upr = '...2') -> bin_bnds
    # determine which bin length is in, and define new length as upper bin
    # note, plus bin is denoted as max length bin + 1
    .agedat %>% 
      tidytable::distinct(length) %>% 
      tidytable::mutate(new_length = bin_bnds$upr[max(which(bin_bnds$lwr < length / 10))], 
                        .by = c(length)) -> new_lengths
    # replace lengths in length frequency data with new binned lengths
    .agedat %>% 
      tidytable::left_join(new_lengths) %>% 
      tidytable::select(-length, length = new_length) -> .agedat
  }
  
  
  # caal ----
  apop_caal(.agedat) -> .caal
  
  list(caal = .caal)
  
}


