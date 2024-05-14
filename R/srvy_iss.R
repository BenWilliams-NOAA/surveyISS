#' replicate survey input sample size function for production run
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param use_gapindex use functions derived from gapindex package (default = TRUE)
#' @param by_strata should length/age pop'n values be computed at stratum level in gap fcns (default = FALSE)
#' @param global fills in missing length bins with global alk in gap fcns (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss
#'
#' @examples
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
                     use_gapindex = TRUE,
                     by_strata = FALSE,
                     global = FALSE,
                     region = NULL, 
                     save_interm = FALSE, 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_length

  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age(.rss_age, specimen_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    .mean_length <- grwth_stats(r_age, oga)
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length(.rss_length, lfreq_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length(r_length, ogl)
  
  # write results ----
  # input sample size
  vroom::vroom_write(.iss_length, here::here("output", region, paste0(save, "_iss_ln.csv")), delim = ",")    
  vroom::vroom_write(.iss_age, here::here("output", region, paste0(save, "_iss_ag.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age.csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length.csv")), delim = ",")
  # bias in age & length pop'n
  vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age.csv")), delim = ",")
  vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length.csv")), delim = ",")
  # mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length.csv")), delim = ",")
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_length.csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_age.csv")), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag.csv")), delim = ",")
  }

}

#' replicate survey input sample size function for production run for ai stock complexes (e.g., blackspotted-rougheye)
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param cmplx_code numeric value to replace the individual species codes with a complex code (default = NULL)
#' @param use_gapindex use functions derived from gapindex package (default = TRUE)
#' @param by_strata should length/age pop'n values be computed at stratum level in gap fcns (default = FALSE)
#' @param global fills in missing length bins with global alk in gap fcns (default = FALSE)
#' @param cmplx complex name for saving results (default = NULL)
#' @param region region will create a folder and place results in said folder (default = 'ai')
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_ai_cmplx
#'
#' @examples
#' 
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
                              cmplx_code = NULL,
                              use_gapindex = TRUE,
                              by_strata = FALSE,
                              global = FALSE,
                              cmplx = NULL,
                              region = 'ai', 
                              save_interm = FALSE, 
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
                                                  cmplx_code = cmplx_code,
                                                  use_gapindex = use_gapindex,
                                                  by_strata = by_strata,
                                                  global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age(.rss_age, specimen_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    .mean_length <- grwth_stats(r_age, oga)
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length(.rss_length, lfreq_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length(r_length, ogl)
  
  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln_", cmplx, ".csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag_", cmplx, ".csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_", cmplx, ".csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_", cmplx, ".csv")), delim = ",")
  # bias in age & length pop'n
  vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age_", cmplx, ".csv")), delim = ",")
  vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length_", cmplx, ".csv")), delim = ",")
  # mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length_", cmplx, ".csv")), delim = ",")
  }
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_length_", cmplx, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_", cmplx, ".csv")), delim = ",")
  }
  
}

#' replicate survey input sample size function for production run for goa stock complexes (e.g., rougheye-blackpostted and dusky rockfish)
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param cmplx_code numeric value to replace the individual species codes with a complex code (default = NULL)
#' @param use_gapindex use functions derived from gapindex package (default = TRUE)
#' @param by_strata should length/age pop'n values be computed at stratum level in gap fcns (default = FALSE)
#' @param global fills in missing length bins with global alk in gap fcns (default = FALSE)
#' @param cmplx complex name for saving results (default = NULL)
#' @param region region will create a folder and place results in said folder (default = 'goa')
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_goa_cmplx
#'
#' @examples
#' 
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
                               cmplx_code = NULL,
                               use_gapindex = TRUE,
                               by_strata = FALSE,
                               global = FALSE,
                               cmplx = NULL,
                               region = 'goa', 
                               save_interm = FALSE, 
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
                   use_gapindex = use_gapindex,
                   by_strata = by_strata,
                   global = global)
  
  og$age %>% 
    tidytable::summarize(agepop = sum(agepop), .by = c(year, sex, age)) %>% 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(agepop = sum(agepop), .by = c(sim, year, sex, age)) %>% 
    tidytable::mutate(species_code = cmplx_code) %>% 
    split(., .[,'sim'])
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(abund = sum(abund), .by = c(sim, year, sex, length)) %>% 
    tidytable::mutate(species_code = cmplx_code) %>% 
    split(., .[,'sim'])
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age(.rss_age, specimen_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    .mean_length <- grwth_stats(r_age, oga)
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length(.rss_length, lfreq_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length(r_length, ogl)
  
  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln_", cmplx, ".csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag_", cmplx, ".csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_", cmplx, ".csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_", cmplx, ".csv")), delim = ",")
  # bias in age & length pop'n
  vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age_", cmplx, ".csv")), delim = ",")
  vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length_", cmplx, ".csv")), delim = ",")
  # mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length_", cmplx, ".csv")), delim = ",")
  }
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_length_", cmplx, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_age_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_", cmplx, ".csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_", cmplx, ".csv")), delim = ",")
  }
}

#' replicate srvy_iss function for spatially-explicit input sample size for western, central and eastern gulf subregions
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param use_gapindex use functions derived from gapindex package (default = TRUE)
#' @param by_strata should length/age pop'n values be computed at stratum level in gap fcns (default = FALSE)
#' @param global fills in missing length bins with global alk in gap fcns (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_goa_w_c_e
#'
#' @examples
#' 
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
                               use_gapindex = TRUE,
                               by_strata = FALSE,
                               global = FALSE,
                               region = NULL,
                               save_interm = FALSE,
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
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id == 803 ~ 'cgoa',
                                         area_id == 804 ~ 'egoa',
                                         area_id == 805 ~ 'wgoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description) -> .specimen_data
  
  cpue_data %>%  
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id == 803 ~ 'cgoa',
                                         area_id == 804 ~ 'egoa',
                                         area_id == 805 ~ 'wgoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description)  -> .cpue_data
  
  # get original age/length pop'n values ----
  
  # western goa
  og_w <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wgoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "wgoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "wgoa"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)

  oga_w <- og_w$age
  ogl_w <- og_w$length

  # central goa
  og_c <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "cgoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "cgoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "cgoa"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_c <- og_c$age
  ogl_c <- og_c$length
  
  # eastern goa
  og_e <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_e <- og_e$age
  ogl_e <- og_e$length
  
  # compile og results 
  oga <- oga_w %>% 
    tidytable::mutate(region = "wgoa") %>% 
    tidytable::bind_rows(oga_c %>% 
                           tidytable::mutate(region = "cgoa")) %>% 
    tidytable::bind_rows(oga_e %>% 
                           tidytable::mutate(region = "egoa")) %>% 
    tidytable::bind_rows(oga_w %>% 
                           tidytable::rename(agepop_w = agepop) %>% 
                           tidytable::full_join(oga_c %>% 
                                                  tidytable::rename(agepop_c = agepop)) %>% 
                           tidytable::full_join(oga_e %>% 
                                                  tidytable::rename(agepop_e = agepop)) %>% 
                           tidytable::replace_na(list(agepop_w = 0)) %>% 
                           tidytable::replace_na(list(agepop_c = 0)) %>% 
                           tidytable::replace_na(list(agepop_e = 0)) %>% 
                           tidytable::mutate(agepop = agepop_w + agepop_c + agepop_e,
                                             region = 'goa') %>% 
                           tidytable::select(year, species_code, sex, age, agepop, region))
  
  ogl <- ogl_w %>% 
    tidytable::mutate(region = "wgoa") %>% 
    tidytable::bind_rows(ogl_c %>% 
                           tidytable::mutate(region = "cgoa")) %>% 
    tidytable::bind_rows(ogl_e %>% 
                           tidytable::mutate(region = "egoa")) %>% 
    tidytable::bind_rows(ogl_w %>% 
                           tidytable::rename(abund_w = abund) %>% 
                           tidytable::full_join(ogl_c %>% 
                                                  tidytable::rename(abund_c = abund)) %>% 
                           tidytable::full_join(ogl_e %>% 
                                                  tidytable::rename(abund_e = abund)) %>% 
                           tidytable::replace_na(list(abund_w = 0)) %>% 
                           tidytable::replace_na(list(abund_c = 0)) %>% 
                           tidytable::replace_na(list(abund_e = 0)) %>% 
                           tidytable::mutate(abund = abund_w + abund_c + abund_e,
                                             region = 'goa') %>% 
                           tidytable::select(year, species_code, sex, length, abund, region))

  # run resampling iterations ----
  # western goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wgoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "wgoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "wgoa"), 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age_w <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_w <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length

  # central goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "cgoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "cgoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "cgoa"), 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age_c <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_c <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # eastern goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"), 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # combine subregions with goa-wide
  # age comp
  r_age <- r_age_w %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(region = 'wgoa') %>% 
    tidytable::bind_rows(r_age_c %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'cgoa')) %>% 
    tidytable::bind_rows(r_age_e %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'egoa')) %>% 
    tidytable::bind_rows(r_age_w %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::rename(agepop_w = agepop) %>% 
                           tidytable::full_join(r_age_c %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(agepop_c = agepop)) %>% 
                           tidytable::full_join(r_age_e %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(agepop_e = agepop)) %>% 
                           tidytable::replace_na(list(agepop_w = 0)) %>% 
                           tidytable::replace_na(list(agepop_c = 0)) %>% 
                           tidytable::replace_na(list(agepop_e = 0)) %>% 
                           tidytable::mutate(agepop = agepop_w + agepop_c + agepop_e,
                                             region = 'goa') %>% 
                           tidytable::select(sim, year, species_code, sex, age, agepop, region)) %>% 
    split(., .[,'sim'])
  # length comp
  r_length <- r_length_w %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(region = 'wgoa') %>% 
    tidytable::bind_rows(r_length_c %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'cgoa')) %>% 
    tidytable::bind_rows(r_length_e %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'egoa')) %>% 
    tidytable::bind_rows(r_length_w %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::rename(abund_w = abund) %>% 
                           tidytable::full_join(r_length_c %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(abund_c = abund)) %>% 
                           tidytable::full_join(r_length_e %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(abund_e = abund)) %>% 
                           tidytable::replace_na(list(abund_w = 0)) %>% 
                           tidytable::replace_na(list(abund_c = 0)) %>% 
                           tidytable::replace_na(list(abund_e = 0)) %>% 
                           tidytable::mutate(abund = abund_w + abund_c + abund_e,
                                             region = 'goa') %>% 
                           tidytable::select(sim, year, species_code, sex, length, abund, region)) %>% 
    split(., .[,'sim'])

  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age_reg(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  r_length %>%
    tidytable::map(., ~rss_length_reg(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age_reg(.rss_age, specimen_data, region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age_reg(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    .mean_length <- grwth_stats_reg(r_age, oga)
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length_reg(.rss_length, lfreq_data, region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length_reg(r_length, ogl)

  # write results ----
  # input sample size
  vroom::vroom_write(.iss_length, here::here("output", region, paste0(save, "_iss_ln_w_c_egoa.csv")), delim = ",")    
  vroom::vroom_write(.iss_age, here::here("output", region, paste0(save, "_iss_ag_w_c_egoa.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_w_c_egoa.csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_w_c_egoa.csv")), delim = ",")
  # bias in age & length pop'n
  vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age_w_c_egoa.csv")), delim = ",")
  vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length_w_c_egoa.csv")), delim = ",")
  # mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length_w_c_egoa.csv")), delim = ",")
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_length_w_c_egoa.csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_age_w_c_egoa.csv")), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln._w_c_egoacsv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_w_c_egoa.csv")), delim = ",")
  }

}

#' replicate srvy_iss function for spatially-explicit input sample size for western-central and eastern gulf subregions
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param use_gapindex use functions derived from gapindex package (default = TRUE)
#' @param by_strata should length/age pop'n values be computed at stratum level in gap fcns (default = FALSE)
#' @param global fills in missing length bins with global alk in gap fcns (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_goa_wc_e
#'
#' @examples
#' 
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
                              use_gapindex = TRUE,
                              by_strata = FALSE,
                              global = FALSE,
                              region = NULL,
                              save_interm = FALSE,
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
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id %in% c(803, 805) ~ 'wcgoa',
                                         area_id == 804 ~ 'egoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description) -> .specimen_data
  
  cpue_data %>%  
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = case_when(area_id %in% c(803, 805) ~ 'wcgoa',
                                         area_id == 804 ~ 'egoa')) %>% 
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description)  -> .cpue_data
  
  # get original age/length pop'n values ----

  # western & central goa
  og_c <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wcgoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "wcgoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "wcgoa"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_wc <- og_c$age
  ogl_wc <- og_c$length
  
  # eastern goa
  og_e <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_e <- og_e$age
  ogl_e <- og_e$length

  # compile og results 
  oga <- oga_wc %>% 
    tidytable::mutate(region = "wcgoa") %>% 
    tidytable::bind_rows(oga_e %>% 
                           tidytable::mutate(region = "egoa")) %>% 
    tidytable::bind_rows(oga_wc %>% 
                           tidytable::rename(agepop_wc = agepop) %>% 
                           tidytable::full_join(oga_e %>% 
                                                  tidytable::rename(agepop_e = agepop)) %>% 
                           tidytable::replace_na(list(agepop_wc = 0)) %>% 
                           tidytable::replace_na(list(agepop_e = 0)) %>% 
                           tidytable::mutate(agepop = agepop_wc + agepop_e,
                                             region = 'goa') %>% 
                           tidytable::select(year, species_code, sex, age, agepop, region))
  
  ogl <- ogl_wc %>% 
    tidytable::mutate(region = "wcgoa") %>% 
    tidytable::bind_rows(ogl_e %>% 
                           tidytable::mutate(region = "egoa")) %>% 
    tidytable::bind_rows(ogl_wc %>% 
                           tidytable::rename(abund_wc = abund) %>% 
                           tidytable::full_join(ogl_e %>% 
                                                  tidytable::rename(abund_e = abund)) %>% 
                           tidytable::replace_na(list(abund_wc = 0)) %>% 
                           tidytable::replace_na(list(abund_e = 0)) %>% 
                           tidytable::mutate(abund = abund_wc + abund_e,
                                             region = 'goa') %>% 
                           tidytable::select(year, species_code, sex, length, abund, region))
  
  # run resampling iterations ----
  # western & central goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wcgoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "wcgoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "wcgoa"), 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age_wc <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_wc <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length

  # eastern goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"), 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # combine subregions with goa-wide
  r_age <- r_age_wc %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(region = 'wcgoa') %>% 
    tidytable::bind_rows(r_age_e %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'egoa')) %>% 
    tidytable::bind_rows(r_age_wc %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::rename(agepop_wc = agepop) %>% 
                           tidytable::full_join(r_age_e %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(agepop_e = agepop)) %>% 
                           tidytable::replace_na(list(agepop_wc = 0)) %>% 
                           tidytable::replace_na(list(agepop_e = 0)) %>% 
                           tidytable::mutate(agepop = agepop_wc + agepop_e,
                                             region = 'goa') %>% 
                           tidytable::select(sim, year, species_code, sex, age, agepop, region)) %>% 
    split(., .[,'sim'])
  
  r_length <- r_length_wc %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(region = 'wcgoa') %>% 
    tidytable::bind_rows(r_length_e %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'egoa')) %>% 
    tidytable::bind_rows(r_length_wc %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::rename(abund_wc = abund) %>% 
                           tidytable::full_join(r_length_e %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(abund_e = abund)) %>% 
                           tidytable::replace_na(list(abund_wc = 0)) %>% 
                           tidytable::replace_na(list(abund_e = 0)) %>% 
                           tidytable::mutate(abund = abund_wc + abund_e,
                                             region = 'goa') %>% 
                           tidytable::select(sim, year, species_code, sex, length, abund, region)) %>% 
    split(., .[,'sim'])
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age_reg(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  r_length %>%
    tidytable::map(., ~rss_length_reg(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age_reg(.rss_age, specimen_data, region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age_reg(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    .mean_length <- grwth_stats_reg(r_age, oga)
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length_reg(.rss_length, lfreq_data, region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length_reg(r_length, ogl)
  
  # write results ----
  # input sample size
  vroom::vroom_write(.iss_length, here::here("output", region, paste0(save, "_iss_ln_wc_egoa.csv")), delim = ",")    
  vroom::vroom_write(.iss_age, here::here("output", region, paste0(save, "_iss_ag_wc_egoa.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_wc_egoa.csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_wc_egoa.csv")), delim = ",")
  # bias in age & length pop'n
  vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age_wc_egoa.csv")), delim = ",")
  vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length_wc_egoa.csv")), delim = ",")
  # mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length_wc_egoa.csv")), delim = ",")
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_length_wc_egoa.csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_age_wc_egoa.csv")), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln._wc_egoacsv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_wc_egoa.csv")), delim = ",")
  }

}

#' replicate survey input sample size function for goa west of 140
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param use_gapindex use functions derived from gapindex package (default = TRUE)
#' @param by_strata should length/age pop'n values be computed at stratum level in gap fcns (default = FALSE)
#' @param global fills in missing length bins with global alk in gap fcns (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_w140
#'
#' @examples
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
                          use_gapindex = TRUE,
                          by_strata = FALSE,
                          global = FALSE,
                          region = NULL, 
                          save_interm = FALSE, 
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
                                         use_gapindex = use_gapindex,
                                         by_strata = by_strata,
                                         global = global))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age(.rss_age, specimen_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    .mean_length <- grwth_stats(r_age, oga)
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length(.rss_length, lfreq_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length(r_length, ogl)
  
  # write results ----
  # input sample size
  vroom::vroom_write(.iss_length, here::here("output", region, paste0(save, "_iss_ln_w140.csv")), delim = ",")    
  vroom::vroom_write(.iss_age, here::here("output", region, paste0(save, "_iss_ag_w140.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_w140.csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_w140.csv")), delim = ",")
  # bias in age & length pop'n
  vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age_w140.csv")), delim = ",")
  vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length_w140.csv")), delim = ",")
  # mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length_w140.csv")), delim = ",")
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_length_w140.csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_age_w140.csv")), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_w140.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_w140.csv")), delim = ",")
  }
  
}


#' replicate srvy_iss function for spatially-explicit input sample size for aleutian islands subregions
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size (default = 1 cm)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param use_gapindex use functions derived from gapindex package (default = TRUE)
#' @param by_strata should length/age pop'n values be computed at stratum level in gap fcns (default = FALSE)
#' @param global fills in missing length bins with global alk in gap fcns (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_ai_subreg
#'
#' @examples
#' 
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
                               use_gapindex = TRUE,
                               by_strata = FALSE,
                               global = FALSE,
                               region = NULL, 
                               save_interm = FALSE, 
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
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description) -> .lfreq_data
  
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
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description) -> .specimen_data
  
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
    tidytable::select(-design_year, -area, -area_id, -subarea_name, -description)  -> .cpue_data
  
  # get original age/length pop'n values ----
  
  # western ai
  og_w <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wai"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "wai"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "wai"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_w <- og_w$age
  ogl_w <- og_w$length
  
  # central ai
  og_c <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "cai"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "cai"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "cai"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_c <- og_c$age
  ogl_c <- og_c$length
  
  # eastern ai
  og_e <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "eai"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "eai"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "eai"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_e <- og_e$age
  ogl_e <- og_e$length
  
  # southern bering sea
  og_s <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "sbs"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "sbs"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "sbs"), 
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
                     use_gapindex = use_gapindex,
                     by_strata = by_strata,
                     global = global)
  
  oga_s <- og_s$age
  ogl_s <- og_s$length
  
  # compile og results 
  oga <- oga_w %>% 
    tidytable::mutate(region = "wai") %>% 
    tidytable::bind_rows(oga_c %>% 
                           tidytable::mutate(region = "cai")) %>% 
    tidytable::bind_rows(oga_e %>% 
                           tidytable::mutate(region = "eai")) %>% 
    tidytable::bind_rows(oga_s %>% 
                           tidytable::mutate(region = "sbs")) %>% 
    tidytable::bind_rows(oga_w %>% 
                           tidytable::rename(agepop_w = agepop) %>% 
                           tidytable::full_join(oga_c %>% 
                                                  tidytable::rename(agepop_c = agepop)) %>% 
                           tidytable::full_join(oga_e %>% 
                                                  tidytable::rename(agepop_e = agepop)) %>% 
                           tidytable::full_join(oga_s %>% 
                                                  tidytable::rename(agepop_s = agepop)) %>% 
                           tidytable::replace_na(list(agepop_w = 0)) %>% 
                           tidytable::replace_na(list(agepop_c = 0)) %>% 
                           tidytable::replace_na(list(agepop_e = 0)) %>% 
                           tidytable::replace_na(list(agepop_s = 0)) %>%
                           tidytable::mutate(agepop = agepop_w + agepop_c + agepop_e + agepop_s,
                                             region = 'ai') %>% 
                           tidytable::select(year, species_code, sex, age, agepop, region))
  
  ogl <- ogl_w %>% 
    tidytable::mutate(region = "wai") %>% 
    tidytable::bind_rows(ogl_c %>% 
                           tidytable::mutate(region = "cai")) %>% 
    tidytable::bind_rows(ogl_e %>% 
                           tidytable::mutate(region = "eai")) %>% 
    tidytable::bind_rows(ogl_s %>% 
                           tidytable::mutate(region = "sbs")) %>% 
    tidytable::bind_rows(ogl_w %>% 
                           tidytable::rename(abund_w = abund) %>% 
                           tidytable::full_join(ogl_c %>% 
                                                  tidytable::rename(abund_c = abund)) %>% 
                           tidytable::full_join(ogl_e %>% 
                                                  tidytable::rename(abund_e = abund)) %>% 
                           tidytable::full_join(ogl_s %>% 
                                                  tidytable::rename(abund_s = abund)) %>% 
                           tidytable::replace_na(list(abund_w = 0)) %>% 
                           tidytable::replace_na(list(abund_c = 0)) %>% 
                           tidytable::replace_na(list(abund_e = 0)) %>% 
                           tidytable::replace_na(list(abund_s = 0)) %>% 
                           tidytable::mutate(abund = abund_w + abund_c + abund_e + abund_s,
                                             region = 'ai') %>% 
                           tidytable::select(year, species_code, sex, length, abund, region))
  
  # run resampling iterations ----
  # western ai
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wai"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "wai"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "wai"), 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         bin = bin,
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         al_var_ann = al_var_ann,
                                         age_err = age_err))
  
  r_age_w <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_w <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # central ai
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "cai"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "cai"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "cai"), 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         bin = bin,
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         al_var_ann = al_var_ann,
                                         age_err = age_err))
  
  r_age_c <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_c <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # eastern ai
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "eai"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "eai"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "eai"), 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         bin = bin,
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         al_var_ann = al_var_ann,
                                         age_err = age_err))
  
  r_age_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # southern bering sea
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "sbs"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "sbs"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "sbs"), 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         bin = bin,
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         al_var_ann = al_var_ann,
                                         age_err = age_err))
  
  r_age_s <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_s <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # combine subregions with ai-wide
  r_age <- r_age_w %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(region = 'wai') %>% 
    tidytable::bind_rows(r_age_c %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'cai')) %>% 
    tidytable::bind_rows(r_age_e %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'eai')) %>% 
    tidytable::bind_rows(r_age_s %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'sbs')) %>% 
    tidytable::bind_rows(r_age_w %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::rename(agepop_w = agepop) %>% 
                           tidytable::full_join(r_age_c %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(agepop_c = agepop)) %>% 
                           tidytable::full_join(r_age_e %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(agepop_e = agepop)) %>% 
                           tidytable::full_join(r_age_s %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(agepop_s = agepop)) %>% 
                           tidytable::replace_na(list(agepop_w = 0)) %>% 
                           tidytable::replace_na(list(agepop_c = 0)) %>% 
                           tidytable::replace_na(list(agepop_e = 0)) %>% 
                           tidytable::replace_na(list(agepop_s = 0)) %>% 
                           tidytable::mutate(agepop = agepop_w + agepop_c + agepop_e + agepop_s,
                                             region = 'ai') %>% 
                           tidytable::select(sim, year, species_code, sex, age, agepop, region)) %>% 
    split(., .[,'sim'])
  
  r_length <- r_length_w %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(region = 'wai') %>% 
    tidytable::bind_rows(r_length_c %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'cai')) %>% 
    tidytable::bind_rows(r_length_e %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'eai')) %>% 
    tidytable::bind_rows(r_length_s %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::mutate(region = 'sbs')) %>% 
    tidytable::bind_rows(r_length_w %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::rename(abund_w = abund) %>% 
                           tidytable::full_join(r_length_c %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(abund_c = abund)) %>% 
                           tidytable::full_join(r_length_e %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(abund_e = abund)) %>% 
                           tidytable::full_join(r_length_s %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::rename(abund_s = abund)) %>% 
                           tidytable::replace_na(list(abund_w = 0)) %>% 
                           tidytable::replace_na(list(abund_c = 0)) %>% 
                           tidytable::replace_na(list(abund_e = 0)) %>% 
                           tidytable::replace_na(list(abund_s = 0)) %>% 
                           tidytable::mutate(abund = abund_w + abund_c + abund_e + abund_s,
                                             region = 'ai') %>% 
                           tidytable::select(sim, year, species_code, sex, length, abund, region)) %>% 
    split(., .[,'sim'])
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age_reg(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  r_length %>%
    tidytable::map(., ~rss_length_reg(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 12 ~ 'female_male',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age_reg(.rss_age, specimen_data, region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age_reg(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    .mean_length <- grwth_stats_reg(r_age, oga)
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length_reg(.rss_length, lfreq_data, region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length_reg(r_length, ogl)
  
  # write results ----
  # input sample size
  vroom::vroom_write(.iss_length, here::here("output", region, paste0(save, "_iss_ln_ai_subreg.csv")), delim = ",")    
  vroom::vroom_write(.iss_age, here::here("output", region, paste0(save, "_iss_ag_ai_subreg.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, paste0(save, "_base_age_ai_subreg.csv")), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, paste0(save, "_base_length_ai_subreg.csv")), delim = ",")
  # bias in age & length pop'n
  vroom::vroom_write(.bias_age, file = here::here("output", region, paste0(save, "_bias_age_ai_subreg.csv")), delim = ",")
  vroom::vroom_write(.bias_length, file = here::here("output", region, paste0(save, "_bias_length_ai_subreg.csv")), delim = ",")
  # mean length-at-age and sd (if using gap fcns)
  if(isTRUE(use_gapindex)){
    vroom::vroom_write(.mean_length, file = here::here("output", region, paste0(save, "_mean_length_ai_subreg.csv")), delim = ",")
  }
  
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_length_ai_subreg.csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0(save, "_resampled_age_ai_subreg.csv")), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln._ai_subregcsv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_ai_subreg.csv")), delim = ",")
  }

}

#' replicate survey input sample size function for production run for conditional age-at-length data
#'
#' @param iters number of iterations (500 recommended)
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param bin bin size for length data
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_caal
#'
#' @examples

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
                          region = NULL, 
                          save_interm = FALSE,
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
                        age_err = FALSE)
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
                                              age_err = age_err))
  
  r_caal <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$caal

  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_caal %>%
    tidytable::map(., ~rss_caal(sim_data = .x, og_data = ogcaal)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female')) -> .rss_caal

  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .rss_caal %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE, zero = FALSE),
                         .by = c(year, species_code, sex, sex_desc, length)) %>% 
    # compute average relative bias in pop'n estimates (avg relative bias across age or length)
    tidytable::left_join(r_caal %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::left_join(ogcaal %>% 
                                                  tidytable::rename(og_caal = caal)) %>% 
                           tidytable::mutate(bias = (caal - og_caal)) %>% 
                           tidytable::drop_na() %>%
                           tidytable::summarise(bias = mean(bias, na.rm = TRUE), .by = c(year, species_code, sex, length))) %>% 
    # add nominal sample size (nss)
    tidytable::left_join(specimen_data %>% 
                           tidytable::drop_na(age) %>% 
                           tidytable::filter(sex != 3) %>% 
                           tidytable::summarise(nss = .N, .by = c(year, species_code, sex, length)) %>% 
                           tidytable::bind_rows(specimen_data %>% 
                                                  tidytable::drop_na(age) %>% 
                                                  tidytable::summarise(nss = .N, .by = c(year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 0)) %>% 
                           tidytable::bind_rows(specimen_data %>% 
                                                  tidytable::drop_na(age) %>% 
                                                  tidytable::summarise(nss = .N, .by = c(year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::bind_rows(specimen_data %>% 
                                                  tidytable::drop_na(age) %>% 
                                                  tidytable::filter(sex != 3) %>% 
                                                  tidytable::summarise(nss = .N, .by = c(year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 12))) -> iss_caal

  # write results ----
  # input sample size   
  vroom::vroom_write(iss_caal, here::here("output", region, paste0(save, "_iss_caal.csv")), delim = ",")
  # base conditional age-at-length
  vroom::vroom_write(ogcaal, file = here::here("output", region, "base_caal.csv"), delim = ",")
  # if desired, write out bootstrapped conditional age-at-length and realized sample sizes
  if(isTRUE(save_interm)) {
    r_caal %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_caal.csv"), delim = ",")
    vroom::vroom_write(.rss_caal, here::here("output", region, paste0(save, "_iter_rss_caal.csv")), delim = ",")
  }
}



