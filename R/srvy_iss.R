#' Survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl survey age and length composition
#' for computation of input sample size
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param use_gapindex Boolean. Use functions derived from gapindex package? (default = TRUE)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level in gap fcns? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key in gap fcns? (default = FALSE)
#' @param region Region will create a folder and place results in said folder. (default = NULL)
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, species (using RACE species codes), sex (0 - combined sex 
#' with age/length data combined prior to expansion, 1 - males, 2 - females, 3 - unsexed, 12 - female/male
#' compositions that sum to one across both sexes combined, 4 - combined sex after summing sex-specific age/length
#' composition after expansion; all with short description IDs in sex_desc column) for age composition (output saved with
#' 'iss_age' in file name) and length composition (output saved with 'iss_ln' in file name). For comparison, 
#' nominal sample size ('nss' - the number of age/length samples actually taken) and the number of 
#' sampled hauls for age/length ('nhls') are included. Will also produce other dataframes if desired 
#' (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss <- function(iters = 1, 
                     lfreq_data,
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
                     use_gapindex = TRUE,
                     by_strata = FALSE,
                     global = FALSE,
                     region = NULL, 
                     save_interm = FALSE, 
                     save_stats = FALSE,
                     save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # get original age/length pop'n values ----
  og <- srvy_comps(lfreq_data = lfreq_data, 
                   specimen_data = specimen_data, 
                   cpue_data = cpue_data, 
                   strata_data = strata_data,
                   r_t = r_t,
                   yrs = yrs, 
                   bin = bin,
                   boot_hauls = FALSE, 
                   boot_lengths = FALSE, 
                   boot_ages = FALSE,
                   al_var = FALSE,
                   al_var_ann = FALSE,
                   age_err = FALSE,
                   len_samples = NULL,
                   age_samples = NULL,
                   use_gapindex = use_gapindex,
                   by_strata = by_strata,
                   global = global)
  oga <- og$age
  ogl <- og$length
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = lfreq_data, 
                                         specimen_data = specimen_data, 
                                         cpue_data = cpue_data, 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         bin = bin,
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         al_var_ann = al_var_ann,
                                         age_err = age_err,
                                         len_samples = len_samples,
                                         age_samples = age_samples,
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim")
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim")
  
  # compute statistics ----
  out_stats <- comp_stats(r_age, oga, r_length, ogl, specimen_data, lfreq_data)
  
  # write results ----
  # input sample size
  vroom::vroom_write(out_stats$iss_length, here::here("output", region, paste0(save, "_iss_ln.csv")), delim = ",")    
  vroom::vroom_write(out_stats$iss_age, here::here("output", region, paste0(save, "_iss_ag.csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base age & length pop'n
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age.csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length.csv")), delim = ",")
    # bias in age & length pop'n
    vroom::vroom_write(out_stats$bias_age, file = here::here("output", region, paste0(save, "_bias_age.csv")), delim = ",")
    vroom::vroom_write(out_stats$bias_length, file = here::here("output", region, paste0(save, "_bias_length.csv")), delim = ",")
    # mean length-at-age and sd (if using gap fcns)
    if("mean_length" %in% names(r_age)){
      vroom::vroom_write(out_stats$mean_length, file = here::here("output", region, paste0(save, "_mean_length.csv")), delim = ",")
    }
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_length, here::here("output", region, paste0(save, "_resampled_length.csv")), delim = ",")
    vroom::vroom_write(r_age, here::here("output", region, paste0(save, "_resampled_age.csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_length, here::here("output", region, paste0(save, "_iter_rss_ln.csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_age, here::here("output", region, paste0(save, "_iter_rss_ag.csv")), delim = ",")
  }
  
}

#' Stock complex survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl survey age and length composition
#' for computation of input sample size tailored to AI stock complexes (where length composition
#' is expanded for each individual species in the complex, but age-length specimen data is combined 
#' across species in the complex prior to age composition expansion)
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param cmplx_code Numeric value to replace the individual species codes with a complex code shared across species. (default = 3005012)
#' @param use_gapindex Boolean. Use functions derived from gapindex package? (default = TRUE)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level in gap fcns? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key in gap fcns? (default = FALSE)
#' @param cmplx Complex name for saving results. (default = 'bsre')
#' @param region Region will create a folder and place results in said folder. (default = 'ai')
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, complex (designated with cmplx_code), sex (0 - combined sex 
#' with age/length data combined prior to expansion, 1 - males, 2 - females, 3 - unsexed, 12 - female/male
#' compositions that sum to one across both sexes combined, 4 - combined sex after summing sex-specific age/length
#' composition after expansion; all with short description IDs in sex_desc column) for age composition (output saved with
#' 'iss_age' in file name) and length composition (output saved as 'iss_ln' in file name). For comparison, nominal 
#' sample size ('nss' - the number of age/length samples actually taken) and the number of sampled hauls for 
#' age/length ('nhls') are included. AI complex output will be denoted with whatever is defined in cmplx argument
#'  within the file name. Will also produce other dataframes if desired (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss_ai_cmplx <- function(iters = 1, 
                              lfreq_data, 
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
                              cmplx_code = 3005012,
                              use_gapindex = TRUE,
                              by_strata = FALSE,
                              global = FALSE,
                              cmplx = 'bsre',
                              region = 'ai', 
                              save_interm = FALSE, 
                              save_stats = FALSE, 
                              save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # get original age/length pop'n values ----
  og <- srvy_comps_ai_cmplx(lfreq_data = lfreq_data, 
                            specimen_data = specimen_data, 
                            cpue_data = cpue_data, 
                            strata_data = strata_data,
                            r_t = r_t,
                            yrs = yrs, 
                            bin = bin,
                            boot_hauls = FALSE, 
                            boot_lengths = FALSE, 
                            boot_ages = FALSE,
                            al_var = FALSE,
                            al_var_ann = FALSE,
                            age_err = FALSE,
                            len_samples = NULL,
                            age_samples = NULL,
                            cmplx_code = cmplx_code,
                            use_gapindex = use_gapindex,
                            by_strata = by_strata,
                            global = global)
  oga <- og$age
  ogl <- og$length
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ srvy_comps_ai_cmplx(lfreq_data = lfreq_data, 
                                                  specimen_data = specimen_data, 
                                                  cpue_data = cpue_data, 
                                                  strata_data = strata_data,
                                                  r_t = r_t,
                                                  yrs = yrs, 
                                                  bin = bin,
                                                  boot_hauls = boot_hauls, 
                                                  boot_lengths = boot_lengths, 
                                                  boot_ages = boot_ages,
                                                  al_var = al_var,
                                                  al_var_ann = al_var_ann,
                                                  age_err = age_err,
                                                  len_samples = len_samples,
                                                  age_samples = age_samples,
                                                  cmplx_code = cmplx_code,
                                                  use_gapindex = use_gapindex,
                                                  by_strata = by_strata,
                                                  global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim")
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim")
  
  # compute statistics ----
  out_stats <- comp_stats(r_age, oga, r_length, ogl, 
                          specimen_data %>% 
                            tidytable::mutate(species_code = cmplx_code), 
                          lfreq_data %>% 
                            tidytable::mutate(species_code = cmplx_code))
  
  # write results ----
  # input sample size
  vroom::vroom_write(out_stats$iss_length, here::here("output", region, paste0(save, "_iss_ln_", cmplx, ".csv")), delim = ",")    
  vroom::vroom_write(out_stats$iss_age, here::here("output", region, paste0(save, "_iss_ag_", cmplx, ".csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base age & length pop'n
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_", cmplx, ".csv")), delim = ",")
    # bias in age & length pop'n
    vroom::vroom_write(out_stats$bias_age, file = here::here("output", region, paste0(save, "_bias_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(out_stats$bias_length, file = here::here("output", region, paste0(save, "_bias_length_", cmplx, ".csv")), delim = ",")
    # mean length-at-age and sd (if using gap fcns)
    if("mean_length" %in% names(r_age)){
      vroom::vroom_write(out_stats$mean_length, file = here::here("output", region, paste0(save, "_mean_length_", cmplx, ".csv")), delim = ",")
    }
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_length, here::here("output", region, paste0(save, "_resampled_length_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(r_age, here::here("output", region, paste0(save, "_resampled_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_", cmplx, ".csv")), delim = ",")
  }
  
}

#' Stock complex survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl survey age and length composition
#' for computation of input sample size tailored to GOA stock complexes (i.e., rougheye-blackspotted and
#' dusky rockfish; where length and age composition are expanded for each individual species in the complex,
#' and combined post expansion)
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param cmplx_code Numeric value to replace the individual species codes with a complex code shared across species. (default = 3005012)
#' @param use_gapindex Boolean. Use functions derived from gapindex package? (default = TRUE)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level in gap fcns? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key in gap fcns? (default = FALSE)
#' @param cmplx Complex name for saving results. (default = 'rebs')
#' @param region Region will create a folder and place results in said folder. (default = 'goa')
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, complex (designated with cmplx_code), sex (0 - combined sex 
#' with age/length data combined prior to expansion, 1 - males, 2 - females, 3 - unsexed, 12 - female/male
#' compositions that sum to one across both sexes combined, 4 - combined sex after summing sex-specific age/length
#' composition after expansion; all with short description IDs in sex_desc column) for age composition (output saved with
#' 'iss_age' in file name) and length composition (output saved with 'iss_ln' in file name). For comparison, nominal 
#' sample size ('nss' - the number of age/length samples actually taken) and the number of sampled hauls for 
#' age/length ('nhls') are included. GOA complex output will be denoted with whatever is defined in cmplx argument 
#' within the file name. Will also produce other dataframes if desired (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss_goa_cmplx <- function(iters = 1, 
                               lfreq_data, 
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
                               cmplx_code = 3005012,
                               use_gapindex = TRUE,
                               by_strata = FALSE,
                               global = FALSE,
                               cmplx = 'rebs',
                               region = 'goa', 
                               save_interm = FALSE,  
                               save_stats = FALSE,
                               save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # get original age/length pop'n values ----
  og <- srvy_comps(lfreq_data = lfreq_data, 
                   specimen_data = specimen_data, 
                   cpue_data = cpue_data, 
                   strata_data = strata_data,
                   r_t = r_t,
                   yrs = yrs, 
                   bin = bin,
                   boot_hauls = FALSE, 
                   boot_lengths = FALSE, 
                   boot_ages = FALSE,
                   al_var = FALSE,
                   al_var_ann = FALSE,
                   age_err = FALSE,
                   len_samples = NULL,
                   age_samples = NULL,
                   use_gapindex = use_gapindex,
                   by_strata = by_strata,
                   global = global)
  
  og$age %>% 
    tidytable::summarize(agepop = sum(agepop),
                         mean_length = agepop * mean_length / sum(agepop),
                         sd_length = agepop * sd_length / sum(agepop),
                         .by = c(year, sex, age)) %>% 
    tidytable::mutate(species_code = cmplx_code) -> oga
  og$length %>% 
    tidytable::summarize(abund = sum(abund), .by = c(year, sex, length)) %>% 
    tidytable::mutate(species_code = cmplx_code) -> ogl
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = lfreq_data, 
                                         specimen_data = specimen_data, 
                                         cpue_data = cpue_data, 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         bin = bin,
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         al_var_ann = al_var_ann,
                                         age_err = age_err,
                                         len_samples = len_samples,
                                         age_samples = age_samples,
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(agepop = sum(agepop), 
                         mean_length = agepop * mean_length / sum(agepop),
                         sd_length = agepop * sd_length / sum(agepop),
                         .by = c(sim, year, sex, age)) %>% 
    tidytable::mutate(species_code = cmplx_code)
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(abund = sum(abund), .by = c(sim, year, sex, length)) %>% 
    tidytable::mutate(species_code = cmplx_code)
  
  # compute statistics ----
  out_stats <- comp_stats(r_age, oga, r_length, ogl, 
                          specimen_data %>% 
                            tidytable::mutate(species_code = cmplx_code), 
                          lfreq_data %>% 
                            tidytable::mutate(species_code = cmplx_code))
  
  # write results ----
  # input sample size
  vroom::vroom_write(out_stats$iss_length, here::here("output", region, paste0(save, "_iss_ln_", cmplx, ".csv")), delim = ",")    
  vroom::vroom_write(out_stats$iss_age, here::here("output", region, paste0(save, "_iss_ag_", cmplx, ".csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base age & length pop'n
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_", cmplx, ".csv")), delim = ",")
    # bias in age & length pop'n
    vroom::vroom_write(out_stats$bias_age, file = here::here("output", region, paste0(save, "_bias_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(out_stats$bias_length, file = here::here("output", region, paste0(save, "_bias_length_", cmplx, ".csv")), delim = ",")
    # mean length-at-age and sd (if using gap fcns)
    if("mean_length" %in% names(r_age)){
      vroom::vroom_write(out_stats$mean_length, file = here::here("output", region, paste0(save, "_mean_length_", cmplx, ".csv")), delim = ",")
    }
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_length, here::here("output", region, paste0(save, "_resampled_length_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(r_age, here::here("output", region, paste0(save, "_resampled_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_", cmplx, ".csv")), delim = ",")
  }
}

#' Spatially-explicit survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl survey age and length composition
#' for computation of input sample size by western, central and eastern GOA subregions
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param use_gapindex Boolean. Use functions derived from gapindex package? (default = TRUE)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level in gap fcns? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key in gap fcns? (default = FALSE)
#' @param region Region will create a folder and place results in said folder. (default = NULL)
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, region (western - 'wgoa', central - 'cgoa', eastern - 'egoa',
#' and combined across subregions after subregion expansion - 'goa'), species (using RACE species codes), sex (0 - combined sex 
#' with age/length data combined prior to expansion, 1 - males, 2 - females, 3 - unsexed, 12 - female/male
#' compositions that sum to one across both sexes combined, 4 - combined sex after summing sex-specific age/length
#' composition after expansion; all with short description IDs in sex_desc column) for age composition (output saved with
#' 'iss_age' in file name) and length composition (output saved with 'iss_ln' in file name). 
#' For comparison, nominal sample size ('nss' - the number of age/length samples actually taken) and the number of 
#' sampled hauls for age/length ('nhls') are included. GOA subregion output will be denoted with w_c_egoa in
#' output filename. Will also produce other dataframes if desired (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss_goa_w_c_e <- function(iters = 1, 
                               lfreq_data, 
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
                               use_gapindex = TRUE,
                               by_strata = FALSE,
                               global = FALSE,
                               region = NULL,
                               save_interm = FALSE, 
                               save_stats = FALSE,
                               save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # define data by subregion 
  lfreq_data %>%
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id == 803 ~ 'cgoa',
                                         area_id == 804 ~ 'egoa',
                                         area_id == 805 ~ 'wgoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id == 803 ~ 'cgoa',
                                         area_id == 804 ~ 'egoa',
                                         area_id == 805 ~ 'wgoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name) -> .specimen_data
  
  cpue_data %>%  
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id == 803 ~ 'cgoa',
                                         area_id == 804 ~ 'egoa',
                                         area_id == 805 ~ 'wgoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name)  -> .cpue_data
  
  # get original age/length pop'n values ----
  subregion = c('wgoa', 'cgoa', 'egoa')
  og <- purrr::map(1:length(subregion), ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == subregion[.]), 
                                                     specimen_data = subset(.specimen_data, .specimen_data$region == subregion[.]), 
                                                     cpue_data = subset(.cpue_data, .cpue_data$region == subregion[.]), 
                                                     strata_data = strata_data,
                                                     r_t = r_t,
                                                     yrs = yrs, 
                                                     bin = bin,
                                                     boot_hauls = FALSE, 
                                                     boot_lengths = FALSE, 
                                                     boot_ages = FALSE,
                                                     al_var = FALSE,
                                                     al_var_ann = FALSE,
                                                     age_err = FALSE,
                                                     len_samples = NULL,
                                                     age_samples = NULL,
                                                     use_gapindex = use_gapindex,
                                                     by_strata = by_strata,
                                                     global = global))
  
  oga <- do.call(mapply, c(list, og, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
    tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                         region == 2 ~ subregion[2],
                                         region == 3 ~ subregion[3])) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, og, SIMPLIFY = FALSE))$age %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                           tidytable::summarize(agepop = sum(agepop),
                                                mean_length = mean_length * agepop / sum(agepop),
                                                sd_length = sd_length * agepop / sum(agepop),
                                                .by = c(year, species_code, sex, age)) %>% 
                           tidytable::mutate(region = 'goa'))
  
  ogl <- do.call(mapply, c(list, og, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
    tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                         region == 2 ~ subregion[2],
                                         region == 3 ~ subregion[3])) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, og, SIMPLIFY = FALSE))$length %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                           tidytable::summarize(abund = sum(abund),
                                                .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(region = 'goa'))
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ purrr::map(1:length(subregion), ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == subregion[.]), 
                                                                           specimen_data = subset(.specimen_data, .specimen_data$region == subregion[.]), 
                                                                           cpue_data = subset(.cpue_data, .cpue_data$region == subregion[.]), 
                                                                           strata_data = strata_data,
                                                                           r_t = r_t,
                                                                           yrs = yrs, 
                                                                           bin = bin,
                                                                           boot_hauls = boot_hauls, 
                                                                           boot_lengths = boot_lengths, 
                                                                           boot_ages = boot_ages,
                                                                           al_var = al_var,
                                                                           al_var_ann = al_var_ann,
                                                                           age_err = age_err,
                                                                           len_samples = len_samples,
                                                                           age_samples = age_samples,
                                                                           use_gapindex = use_gapindex,
                                                                           by_strata = by_strata,
                                                                           global = global)))
  
  # get resampled age pop'n
  r_age <- purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$age %>% 
                                   tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                   tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                        region == 2 ~ subregion[2],
                                                                        region == 3 ~ subregion[3])))) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    # sum across subregions to get region age pop'n
    tidytable::bind_rows(purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$age %>% 
                                                 tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                                 tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                                      region == 2 ~ subregion[2],
                                                                                      region == 3 ~ subregion[3])))) %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::summarise(agepop = sum(agepop),
                                                mean_length = sum(mean_length * agepop) / sum(agepop),
                                                sd_length = sum(sd_length * agepop) / sum(agepop),
                                                .by = c(sim, year, species_code, sex, age)) %>% 
                           tidytable::mutate(region = 'goa'))
  # get resampled length pop'n
  r_length <- purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$length %>% 
                                      tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                      tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                           region == 2 ~ subregion[2],
                                                                           region == 3 ~ subregion[3])))) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    # sum across subregions to get region length pop'n
    tidytable::bind_rows(purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$length %>% 
                                                 tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                                 tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                                      region == 2 ~ subregion[2],
                                                                                      region == 3 ~ subregion[3])))) %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::summarise(abund = sum(abund),
                                                .by = c(sim, year, species_code, sex, length)) %>% 
                           tidytable::mutate(region = 'goa')) 
  
  # compute statistics ----
  out_stats <- comp_stats(r_age, oga, r_length, ogl, .specimen_data, .lfreq_data, region)
  
  # write results ----
  # input sample size
  vroom::vroom_write(out_stats$iss_length, here::here("output", region, paste0(save, "_iss_ln_w_c_egoa.csv")), delim = ",")    
  vroom::vroom_write(out_stats$iss_age, here::here("output", region, paste0(save, "_iss_ag_w_c_egoa.csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base age & length pop'n
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_w_c_egoa.csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_w_c_egoa.csv")), delim = ",")
    # bias in age & length pop'n
    vroom::vroom_write(out_stats$bias_age, file = here::here("output", region, paste0(save, "_bias_age_w_c_egoa.csv")), delim = ",")
    vroom::vroom_write(out_stats$bias_length, file = here::here("output", region, paste0(save, "_bias_length_w_c_egoa.csv")), delim = ",")
    # mean length-at-age and sd (if using gap fcns)
    if("mean_length" %in% names(r_age)){
      vroom::vroom_write(out_stats$mean_length, file = here::here("output", region, paste0(save, "_mean_length_w_c_egoa.csv")), delim = ",")
    }
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_length, here::here("output", region, paste0(save, "_resampled_length_w_c_egoa.csv")), delim = ",")
    vroom::vroom_write(r_age, here::here("output", region, paste0(save, "_resampled_age_w_c_egoa.csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_length, here::here("output", region, paste0(save, "_iter_rss_ln._w_c_egoacsv")), delim = ",")
    vroom::vroom_write(out_stats$rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_w_c_egoa.csv")), delim = ",")
  }
  
}

#' Spatially-explicit survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl survey age and length composition
#' for computation of input sample size by western-central combined and eastern GOA subregions
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param use_gapindex Boolean. Use functions derived from gapindex package? (default = TRUE)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level in gap fcns? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key in gap fcns? (default = FALSE)
#' @param region Region will create a folder and place results in said folder. (default = NULL)
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, region (western-central combined - 'wcgoa', eastern - 'egoa',
#' and combined across subregions after subregion expansion - 'goa'), species (using RACE species codes), sex (0 - combined sex 
#' with age/length data combined prior to expansion, 1 - males, 2 - females, 3 - unsexed, 12 - female/male
#' compositions that sum to one across both sexes combined, 4 - combined sex after summing sex-specific age/length
#' composition after expansion; all with short description IDs in sex_desc column) for age composition (output saved as '
#' iss_age.csv' at end of file name) and length composition (output saved as 'iss_ln,csv' at end of file name). 
#' For comparison, nominal sample size ('nss' - the number of age/length samples actually taken) and the number of 
#' sampled hauls for age/length ('nhls') are included. GOA subregion output will be denoted with wc_egoa in
#' output filename. Will also produce other dataframes if desired (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss_goa_wc_e <- function(iters = 1, 
                              lfreq_data, 
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
                              use_gapindex = TRUE,
                              by_strata = FALSE,
                              global = FALSE,
                              region = NULL,
                              save_interm = FALSE, 
                              save_stats = FALSE,
                              save){
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # define data by subregion 
  lfreq_data %>%
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id %in% c(803, 805) ~ 'wcgoa',
                                         area_id == 804 ~ 'egoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id %in% c(803, 805) ~ 'wcgoa',
                                         area_id == 804 ~ 'egoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name) -> .specimen_data
  
  cpue_data %>%  
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id %in% c(803, 805) ~ 'wcgoa',
                                         area_id == 804 ~ 'egoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name)  -> .cpue_data
  
  # get original age/length pop'n values ----
  subregion = c('wcgoa', 'egoa')
  og <- purrr::map(1:length(subregion), ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == subregion[.]), 
                                                     specimen_data = subset(.specimen_data, .specimen_data$region == subregion[.]), 
                                                     cpue_data = subset(.cpue_data, .cpue_data$region == subregion[.]), 
                                                     strata_data = strata_data,
                                                     r_t = r_t,
                                                     yrs = yrs, 
                                                     bin = bin,
                                                     boot_hauls = FALSE, 
                                                     boot_lengths = FALSE, 
                                                     boot_ages = FALSE,
                                                     al_var = FALSE,
                                                     al_var_ann = FALSE,
                                                     age_err = FALSE,
                                                     len_samples = NULL,
                                                     age_samples = NULL,
                                                     use_gapindex = use_gapindex,
                                                     by_strata = by_strata,
                                                     global = global))
  
  oga <- do.call(mapply, c(list, og, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
    tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                         region == 2 ~ subregion[2])) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, og, SIMPLIFY = FALSE))$age %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                           tidytable::summarize(agepop = sum(agepop),
                                                mean_length = mean_length * agepop / sum(agepop),
                                                sd_length = sd_length * agepop / sum(agepop),
                                                .by = c(year, species_code, sex, age)) %>% 
                           tidytable::mutate(region = 'goa'))
  
  ogl <- do.call(mapply, c(list, og, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
    tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                         region == 2 ~ subregion[2])) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, og, SIMPLIFY = FALSE))$length %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                           tidytable::summarize(abund = sum(abund),
                                                .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(region = 'goa'))
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ purrr::map(1:length(subregion), ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == subregion[.]), 
                                                                           specimen_data = subset(.specimen_data, .specimen_data$region == subregion[.]), 
                                                                           cpue_data = subset(.cpue_data, .cpue_data$region == subregion[.]), 
                                                                           strata_data = strata_data,
                                                                           r_t = r_t,
                                                                           yrs = yrs, 
                                                                           bin = bin,
                                                                           boot_hauls = boot_hauls, 
                                                                           boot_lengths = boot_lengths, 
                                                                           boot_ages = boot_ages,
                                                                           al_var = al_var,
                                                                           al_var_ann = al_var_ann,
                                                                           age_err = age_err,
                                                                           len_samples = len_samples,
                                                                           age_samples = age_samples,
                                                                           use_gapindex = use_gapindex,
                                                                           by_strata = by_strata,
                                                                           global = global)))
  
  # get resampled age pop'n
  r_age <- purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$age %>% 
                                   tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                   tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                        region == 2 ~ subregion[2],
                                                                        region == 3 ~ subregion[3])))) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    # sum across subregions to get region age pop'n
    tidytable::bind_rows(purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$age %>% 
                                                 tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                                 tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                                      region == 2 ~ subregion[2],
                                                                                      region == 3 ~ subregion[3])))) %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::summarise(agepop = sum(agepop),
                                                mean_length = sum(mean_length * agepop) / sum(agepop),
                                                sd_length = sum(sd_length * agepop) / sum(agepop),
                                                .by = c(sim, year, species_code, sex, age)) %>% 
                           tidytable::mutate(region = 'goa'))
  # get resampled length pop'n
  r_length <- purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$length %>% 
                                      tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                      tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                           region == 2 ~ subregion[2],
                                                                           region == 3 ~ subregion[3])))) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    # sum across subregions to get region length pop'n
    tidytable::bind_rows(purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$length %>% 
                                                 tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                                 tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                                      region == 2 ~ subregion[2],
                                                                                      region == 3 ~ subregion[3])))) %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::summarise(abund = sum(abund),
                                                .by = c(sim, year, species_code, sex, length)) %>% 
                           tidytable::mutate(region = 'goa')) 
  
  # compute statistics ----
  out_stats <- comp_stats(r_age, oga, r_length, ogl, .specimen_data, .lfreq_data, region)
  
  # write results ----
  # input sample size
  vroom::vroom_write(out_stats$iss_length, here::here("output", region, paste0(save, "_iss_ln_wc_egoa.csv")), delim = ",")    
  vroom::vroom_write(out_stats$iss_age, here::here("output", region, paste0(save, "_iss_ag_wc_egoa.csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base age & length pop'n
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_wc_egoa.csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_wc_egoa.csv")), delim = ",")
    # bias in age & length pop'n
    vroom::vroom_write(out_stats$bias_age, file = here::here("output", region, paste0(save, "_bias_age_wc_egoa.csv")), delim = ",")
    vroom::vroom_write(out_stats$bias_length, file = here::here("output", region, paste0(save, "_bias_length_wc_egoa.csv")), delim = ",")
    # mean length-at-age and sd (if using gap fcns)
    if("mean_length" %in% names(r_age)){
      vroom::vroom_write(out_stats$mean_length, file = here::here("output", region, paste0(save, "_mean_length_wc_egoa.csv")), delim = ",")
    }
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_length, here::here("output", region, paste0(save, "_resampled_length_wc_egoa.csv")), delim = ",")
    vroom::vroom_write(r_age, here::here("output", region, paste0(save, "_resampled_age_wc_egoa.csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_length, here::here("output", region, paste0(save, "_iter_rss_ln._wc_egoacsv")), delim = ",")
    vroom::vroom_write(out_stats$rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_wc_egoa.csv")), delim = ",")
  }
  
}

#' Survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl survey age and length composition
#' for computation of input sample size for goa west of 140 (i.e., for goa pollock stock)
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param use_gapindex Boolean. Use functions derived from gapindex package? (default = TRUE)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level in gap fcns? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key in gap fcns? (default = FALSE)
#' @param region Region will create a folder and place results in said folder. (default = NULL)
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, species (using RACE species codes), sex (0 - combined sex 
#' with age/length data combined prior to expansion, 1 - males, 2 - females, 3 - unsexed, 12 - female/male
#' compositions that sum to one across both sexes combined, 4 - combined sex after summing sex-specific age/length
#' composition after expansion; all with short description IDs in sex_desc column) for age composition (output saved with
#' 'iss_age' in file name) and length composition (output saved with 'iss_ln' in file name). For comparison, 
#' nominal sample size ('nss' - the number of age/length samples actually taken) and the number of 
#' sampled hauls for age/length ('nhls') are included.  Output for west of 140 will be designated with 'w140' in filename.
#' Will also produce other dataframes if desired (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss_w140 <- function(iters = 1, 
                          lfreq_data,
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
                          use_gapindex = TRUE,
                          by_strata = FALSE,
                          global = FALSE,
                          region = NULL, 
                          save_interm = FALSE, 
                          save_stats = FALSE, 
                          save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # subset data to be west of 140 and reclassify stratum 142 west of 140 to now be 141
  lfreq_data %>% 
    tidytable::filter(long_mid < -140) %>% 
    tidytable::mutate(stratum = case_when(stratum == 142 ~ 141,
                                          stratum != 142 ~ stratum)) -> lfreq_data
  specimen_data %>% 
    tidytable::filter(long_mid < -140) %>% 
    tidytable::mutate(stratum = case_when(stratum == 142 ~ 141,
                                          stratum != 142 ~ stratum)) -> specimen_data
  cpue_data %>% 
    tidytable::filter(long_mid < -140) %>% 
    tidytable::mutate(stratum = case_when(stratum == 142 ~ 141,
                                          stratum != 142 ~ stratum)) -> cpue_data
  
  # update stratum areas (from zack oyafuso provided in issue #88 in afsc-ga-products/data-requests)
  updated_stratum_area <- 
    data.table::data.table(
      survey = 47, 
      design_year = 1984,
      stratum = c(40, 41, 140, 141, 240, 
                  241, 340, 341, 440, 540),
      area = c(4980.0055, 6714.745, 7346.035, 9993.9158, 2286.1398, 
               1503.6357, 751.2782, 1296.7165, 1252.9542, 1609.551),
      area_id = NA, subarea_name = NA, description = NA)
  
  strata_data <- tidytable::bind_rows(strata_data %>% 
                                        tidytable::filter(!(stratum %in% c(updated_stratum_area$stratum,
                                                                           142, 143, 
                                                                           50, 150, 151, 250, 251, 
                                                                           350, 351, 450, 550))),
                                      updated_stratum_area)
  
  # get original age/length pop'n values ----
  og <- srvy_comps(lfreq_data = lfreq_data, 
                   specimen_data = specimen_data, 
                   cpue_data = cpue_data, 
                   strata_data = strata_data,
                   r_t = r_t,
                   yrs = yrs, 
                   bin = bin,
                   boot_hauls = FALSE, 
                   boot_lengths = FALSE, 
                   boot_ages = FALSE,
                   al_var = FALSE,
                   al_var_ann = FALSE,
                   age_err = FALSE,
                   len_samples = NULL,
                   age_samples = NULL,
                   use_gapindex = use_gapindex,
                   by_strata = by_strata,
                   global = global)
  oga <- og$age
  ogl <- og$length
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = lfreq_data, 
                                         specimen_data = specimen_data, 
                                         cpue_data = cpue_data, 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         bin = bin,
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         al_var_ann = al_var_ann,
                                         age_err = age_err,
                                         len_samples = len_samples,
                                         age_samples = age_samples,
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim")
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim")
  
  # compute statistics ----
  out_stats <- comp_stats(r_age, oga, r_length, ogl, specimen_data, lfreq_data)
  
  # write results ----
  # input sample size
  vroom::vroom_write(out_stats$iss_length, here::here("output", region, paste0(save, "_iss_ln_w140.csv")), delim = ",")    
  vroom::vroom_write(out_stats$iss_age, here::here("output", region, paste0(save, "_iss_ag_w140.csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base age & length pop'n
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_w140.csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_w140.csv")), delim = ",")
    # bias in age & length pop'n
    vroom::vroom_write(out_stats$bias_age, file = here::here("output", region, paste0(save, "_bias_age_w140.csv")), delim = ",")
    vroom::vroom_write(out_stats$bias_length, file = here::here("output", region, paste0(save, "_bias_length_w140.csv")), delim = ",")
    # mean length-at-age and sd (if using gap fcns)
    if("mean_length" %in% names(r_age)){
      vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length_w140.csv")), delim = ",")
    }
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_length, here::here("output", region, paste0(save, "_resampled_length_w140.csv")), delim = ",")
    vroom::vroom_write(r_age, here::here("output", region, paste0(save, "_resampled_age_w140.csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_w140.csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_w140.csv")), delim = ",")
  }
  
}

#' Spatially-explicit survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl survey age and length composition
#' for computation of spatially-explicit input sample size by AI subregions
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param lfreq_data  length frequency input dataframe
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param strata_data strata id and area size input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_lengths Boolean. Resample length frequency w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param len_samples If set at a value, tests reductions in haul-level length sampling. To test, set this value at some smaller level than current sampling rate, i.e., 25 (default = NULL)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param use_gapindex Boolean. Use functions derived from gapindex package? (default = TRUE)
#' @param by_strata Boolean. Should length/age pop'n values be computed at stratum level in gap fcns? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-lenth key in gap fcns? (default = FALSE)
#' @param region Region will create a folder and place results in said folder. (default = NULL)
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, region (western - 'wai', central - 'cai', eastern - 'egoa',
#' southern bering sea - 'sbs', and combined across subregions after subregion expansion - 'ai'), species 
#' (using RACE species codes), sex (0 - combined sex with age/length data combined prior to expansion, 
#' 1 - males, 2 - females, 3 - unsexed, 12 - female/male compositions that sum to one across both sexes combined,
#' 4 - combined sex after summing sex-specific age/length composition after expansion; all with short description 
#' IDs in sex_desc column) for age composition (output saved with 'iss_age' in file name) and length composition 
#' (output saved with 'iss_ln' in file name). For comparison, nominal sample size ('nss' - the number of age/length 
#' samples actually taken) and the number of sampled hauls for age/length ('nhls') are included. AI subregion 
#' output will be denoted with 'ai_subreg' in output filename. Will also produce other dataframes if desired 
#' (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss_ai_subreg <- function(iters = 1,
                               lfreq_data, 
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
                               use_gapindex = TRUE,
                               by_strata = FALSE,
                               global = FALSE,
                               region = NULL, 
                               save_interm = FALSE, 
                               save_stats = FALSE, 
                               save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # define data by subregion 
  lfreq_data %>%
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(stratum %in% 
                                           c(211, 212, 213, 214, 221, 222, 223) ~ 'wai',
                                         stratum %in% 
                                           c(311, 312, 313, 314, 321, 322, 323, 324, 411, 412, 413, 414, 421, 422, 423, 424) ~ 'cai',
                                         stratum %in% 
                                           c(511, 512, 513, 521, 522, 523, 594, 611, 612, 613, 614, 621, 622, 623, 624) ~ 'eai',
                                         stratum %in% 
                                           c(711, 712, 721, 722, 793, 794) ~ 'sbs')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(stratum %in% 
                                           c(211, 212, 213, 214, 221, 222, 223) ~ 'wai',
                                         stratum %in% 
                                           c(311, 312, 313, 314, 321, 322, 323, 324, 411, 412, 413, 414, 421, 422, 423, 424) ~ 'cai',
                                         stratum %in% 
                                           c(511, 512, 513, 521, 522, 523, 594, 611, 612, 613, 614, 621, 622, 623, 624) ~ 'eai',
                                         stratum %in% 
                                           c(711, 712, 721, 722, 793, 794) ~ 'sbs')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name) -> .specimen_data
  
  cpue_data %>%  
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(stratum %in% 
                                           c(211, 212, 213, 214, 221, 222, 223) ~ 'wai',
                                         stratum %in% 
                                           c(311, 312, 313, 314, 321, 322, 323, 324, 411, 412, 413, 414, 421, 422, 423, 424) ~ 'cai',
                                         stratum %in% 
                                           c(511, 512, 513, 521, 522, 523, 594, 611, 612, 613, 614, 621, 622, 623, 624) ~ 'eai',
                                         stratum %in% 
                                           c(711, 712, 721, 722, 793, 794) ~ 'sbs')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name)  -> .cpue_data
  
  # get original age/length pop'n values ----
  subregion = c('wai', 'cai', 'eai', 'sbs')
  og <- purrr::map(1:length(subregion), ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == subregion[.]), 
                                                     specimen_data = subset(.specimen_data, .specimen_data$region == subregion[.]), 
                                                     cpue_data = subset(.cpue_data, .cpue_data$region == subregion[.]), 
                                                     strata_data = strata_data,
                                                     r_t = r_t,
                                                     yrs = yrs, 
                                                     bin = bin,
                                                     boot_hauls = FALSE, 
                                                     boot_lengths = FALSE, 
                                                     boot_ages = FALSE,
                                                     al_var = FALSE,
                                                     al_var_ann = FALSE,
                                                     age_err = FALSE,
                                                     len_samples = NULL,
                                                     age_samples = NULL,
                                                     use_gapindex = use_gapindex,
                                                     by_strata = by_strata,
                                                     global = global))
  
  oga <- do.call(mapply, c(list, og, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
    tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                         region == 2 ~ subregion[2],
                                         region == 3 ~ subregion[3],
                                         region == 4 ~ subregion[4])) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, og, SIMPLIFY = FALSE))$age %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                           tidytable::summarize(agepop = sum(agepop),
                                                mean_length = mean_length * agepop / sum(agepop),
                                                sd_length = sd_length * agepop / sum(agepop),
                                                .by = c(year, species_code, sex, age)) %>% 
                           tidytable::mutate(region = 'ai'))
  
  ogl <- do.call(mapply, c(list, og, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
    tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                         region == 2 ~ subregion[2],
                                         region == 3 ~ subregion[3],
                                         region == 4 ~ subregion[4])) %>% 
    tidytable::bind_rows(do.call(mapply, c(list, og, SIMPLIFY = FALSE))$length %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                           tidytable::summarize(abund = sum(abund),
                                                .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(region = 'ai'))
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ purrr::map(1:length(subregion), ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == subregion[.]), 
                                                                           specimen_data = subset(.specimen_data, .specimen_data$region == subregion[.]), 
                                                                           cpue_data = subset(.cpue_data, .cpue_data$region == subregion[.]), 
                                                                           strata_data = strata_data,
                                                                           r_t = r_t,
                                                                           yrs = yrs, 
                                                                           bin = bin,
                                                                           boot_hauls = boot_hauls, 
                                                                           boot_lengths = boot_lengths, 
                                                                           boot_ages = boot_ages,
                                                                           al_var = al_var,
                                                                           al_var_ann = al_var_ann,
                                                                           age_err = age_err,
                                                                           len_samples = len_samples,
                                                                           age_samples = age_samples,
                                                                           use_gapindex = use_gapindex,
                                                                           by_strata = by_strata,
                                                                           global = global)))
  
  # get resampled age pop'n
  r_age <- purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$age %>% 
                                   tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                   tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                        region == 2 ~ subregion[2],
                                                                        region == 3 ~ subregion[3],
                                                                        region == 4 ~ subregion[4])))) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    # sum across subregions to get region age pop'n
    tidytable::bind_rows(purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$age %>% 
                                                 tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                                 tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                                      region == 2 ~ subregion[2],
                                                                                      region == 3 ~ subregion[3],
                                                                                      region == 4 ~ subregion[4])))) %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::summarise(agepop = sum(agepop),
                                                mean_length = sum(mean_length * agepop) / sum(agepop),
                                                sd_length = sum(sd_length * agepop) / sum(agepop),
                                                .by = c(sim, year, species_code, sex, age)) %>% 
                           tidytable::mutate(region = 'ai'))
  # get resampled length pop'n
  r_length <- purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$length %>% 
                                      tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                      tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                           region == 2 ~ subregion[2],
                                                                           region == 3 ~ subregion[3],
                                                                           region == 4 ~ subregion[4])))) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    # sum across subregions to get region length pop'n
    tidytable::bind_rows(purrr::map(1:iters, ~(do.call(mapply, c(list, rr[[.]], SIMPLIFY = FALSE))$length %>% 
                                                 tidytable::map_df(., ~as.data.frame(.x), .id = "region") %>% 
                                                 tidytable::mutate(region = case_when(region == 1 ~ subregion[1],
                                                                                      region == 2 ~ subregion[2],
                                                                                      region == 3 ~ subregion[3],
                                                                                      region == 4 ~ subregion[4])))) %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::summarise(abund = sum(abund),
                                                .by = c(sim, year, species_code, sex, length)) %>% 
                           tidytable::mutate(region = 'ai')) 
  
  # compute statistics ----
  out_stats <- comp_stats(r_age, oga, r_length, ogl, .specimen_data, .lfreq_data, region)
  
  # write results ----
  # input sample size
  vroom::vroom_write(out_stats$iss_length, here::here("output", region, paste0(save, "_iss_ln_ai_subreg.csv")), delim = ",")    
  vroom::vroom_write(out_stats$iss_age, here::here("output", region, paste0(save, "_iss_ag_ai_subreg.csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base age & length pop'n
    vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_ai_subreg.csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_ai_subreg.csv")), delim = ",")
    # bias in age & length pop'n
    vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age_ai_subreg.csv")), delim = ",")
    vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length_ai_subreg.csv")), delim = ",")
    # mean length-at-age and sd (if using gap fcns)
    if("mean_length" %in% names(r_age)){
      vroom::vroom_write(out_stats$mean_length, file = here::here("output", region, paste0(save, "_mean_length_ai_subreg.csv")), delim = ",")
    }
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_length, here::here("output", region, paste0(save, "_resampled_length_ai_subreg.csv")), delim = ",")
    vroom::vroom_write(r_age, here::here("output", region, paste0(save, "_resampled_age_ai_subreg.csv")), delim = ",")
    vroom::vroom_write(out_stats$rss_length, here::here("output", region, paste0(save, "_iter_rss_ln._ai_subregcsv")), delim = ",")
    vroom::vroom_write(out_stats$rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_ai_subreg.csv")), delim = ",")
  }
  
}

#' Survey input sample size function
#' 
#' @description
#' Bootstrap data sources to replicate bottom trawl conditional age-at-length.
#' 
#' @param iters number of iterations (min of 500 recommended for full run)
#' @param specimen_data age-length specimen input dataframe
#' @param cpue_data catch-per-unit effort input dataframe
#' @param r_t age reader-tester input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls Boolean. Resample hauls w/replacement? (default = FALSE)
#' @param boot_ages Boolean. Resample ages w/replacement? (default = FALSE)
#' @param al_var Boolean. Include age-length variability in resampled age data? (default = FALSE)
#' @param al_var_ann Boolean. Resample age-length variability annually or pooled across years? (default = FALSE)
#' @param age_err Boolean. Include ageing error in resampled age data? (default = FALSE)
#' @param age_samples If set at a value, tests reductions (and increases) in survey-level number of ages collected. To test, set at a proportion of ages collected, i.e., 0.8 or 1.2 (default = NULL)
#' @param region Region will create a folder and place results in said folder. (default = NULL)
#' @param save_interm Boolean. Save the intermediate results: resampled age/length comps and realized sample size per iteration? (default = FALSE)
#' @param save_stats Boolean. Save other statistics: base age/length comps without resampling, mean length-at-age, bootstrap bias? (default = FALSE)
#' @param save Name to attach to and identify output files. 
#' 
#' @return Dataframe of input sample size by year, species (using RACE species codes), sex (0 - combined sex,
#' 1 - males, 2 - females; all with short description IDs in sex_desc column), and length for conditional age-at-length
#'  (output saved with 'iss_caal' in file name). For comparison, nominal sample size ('nss' - the number of age-length samples
#'   actually taken) is included. Will also produce other dataframes if desired (see save_intern and save_stats argument descriptions).
#' 
#' @export
#'
srvy_iss_caal <- function(iters = 1, 
                          specimen_data, 
                          cpue_data, 
                          r_t, 
                          yrs = NULL,  
                          bin = 1, 
                          boot_hauls = FALSE, 
                          boot_ages = FALSE,
                          al_var = FALSE, 
                          al_var_ann = FALSE, 
                          age_err = FALSE,
                          age_samples = NULL,
                          region = NULL, 
                          save_interm = FALSE, 
                          save_stats = FALSE,
                          save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # restructure data
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  
  # get original caal values ----
  og <- srvy_comps_caal(specimen_data = specimen_data, 
                        cpue_data = cpue_data, 
                        r_t = r_t,
                        yrs = yrs, 
                        bin = bin,
                        boot_hauls = FALSE, 
                        boot_ages = FALSE,
                        al_var = FALSE,
                        al_var_ann = FALSE,
                        age_err = FALSE,
                        age_samples = NULL)
  ogcaal <- og$caal
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ srvy_comps_caal(specimen_data = specimen_data, 
                                              cpue_data = cpue_data, 
                                              r_t = r_t,
                                              yrs = yrs, 
                                              bin = bin,
                                              boot_hauls = boot_hauls, 
                                              boot_ages = boot_ages,
                                              al_var = al_var,
                                              al_var_ann = al_var_ann,
                                              age_err = age_err,
                                              age_samples = age_samples))
  
  r_caal <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$caal %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim")
  
  # compute statistics ----
  out_stats <- comp_stats_caal(r_caal, ogcaal, specimen_data)
  
  # write results ----
  # input sample size   
  vroom::vroom_write(out_stats$iss_caal, here::here("output", region, paste0(save, "_iss_caal.csv")), delim = ",")
  
  # if desired, write out additional statistics
  if(isTRUE(save_stats)){
    # base conditional age-at-length
    vroom::vroom_write(ogcaal, file = here::here("output", region, "base_caal.csv"), delim = ",")
    # bootstrap bias in conditional age-at-length
    vroom::vroom_write(out_stats$bias_caal, file = here::here("output", region, "base_caal.csv"), delim = ",")
  }
  
  # if desired, write out bootstrapped conditional age-at-length and realized sample sizes
  if(isTRUE(save_interm)) {
    vroom::vroom_write(r_caal, here::here("output", region, "resampled_caal.csv"), delim = ",")
    vroom::vroom_write(out_stats$rss_caal, here::here("output", region, paste0(save, "_iter_rss_caal.csv")), delim = ",")
  }
}



