#' replicate survey input sample size function for production run
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param al_var include age-length variability (default = FALSE)
#' @param age_err include ageing error (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param match_orig match the computed values to gap output (default = FALSE)
#' @param srvy_type only for bering sea survey, denotes whether it's the shelf or slope survey (default = NULL)
#'
#' @return
#' @export srvy_iss
#'
#' @examples

srvy_iss <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, age_err = FALSE,
                     region = NULL, save_interm = FALSE, match_orig = FALSE, srvy_type = NULL){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  # create storage location
  if(!dir.exists(here::here('output', region, 'add_err')) & (isTRUE(al_var) | isTRUE(age_err))){
    dir.create(here::here('output', region, 'add_err'), recursive = TRUE)
  }
  
  # restructure data
  lfreq_data <- tidytable::as_tidytable(lfreq_data) 
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # get original age/length pop'n values
  og <- srvy_comps(lfreq_data = lfreq_data, 
                   specimen_data = specimen_data, 
                   cpue_data = cpue_data, 
                   strata_data = strata_data,
                   r_t = r_t,
                   yrs = yrs, 
                   boot_hauls = FALSE, 
                   boot_lengths = FALSE, 
                   boot_ages = FALSE,
                   al_var = FALSE,
                   age_err = FALSE)
  oga <- og$age %>% 
    select(-type)
  ogl <- og$length %>% 
    select(-type)
  
  # if desired, match original age/length pop'n values with GAP output
  if(isTRUE(match_orig) & region != 'bs'){
    gap_lpop <- vroom::vroom(here::here('data', paste0('race_lpop_', tolower(region), '.csv')))
    gap_apop <- vroom::vroom(here::here('data', paste0('race_apop_', tolower(region), '.csv')))
    match_gap(oga, ogl, gap_apop, gap_lpop, thresh = 0.01, region = region)
  }
  if(isTRUE(match_orig) & region == 'bs'){
    if(isTRUE(srvy_type == 'shelf')){
      gap_lpop <- vroom::vroom(here::here('data', paste0('race_lpop_', tolower(region), '.csv')))
      match_gap_bs(ogl, gap_lpop, thresh = 0.01, region = region, save = srvy_type)} else{
        gap_lpop <- vroom::vroom(here::here('data', paste0('race_lpop_slope_', tolower(region), '.csv')))
        match_gap_bs(ogl, gap_lpop, thresh = 0.01, region = region, save = srvy_type)
      }
  }
  
  # run resampling iterations
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = lfreq_data, 
                                         specimen_data = specimen_data, 
                                         cpue_data = cpue_data, 
                                         strata_data = strata_data,
                                         r_t = r_t,
                                         yrs = yrs, 
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # if desired, write out intermediate results
  if(isTRUE(save_interm) & region != 'bs') {
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length.csv"), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size.csv"), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age.csv"), delim = ",")
  }
  if(isTRUE(save_interm) & region == 'bs') {
    vroom::vroom_write(oga, file = here::here("output", region, paste0("orig_age_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0("orig_length_", srvy_type, ".csv")), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  }
  
  # compute effective sample size of bootstrapped age/length
  r_age %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_age
  r_length %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_size

  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  ess_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(value, na.rm=T),
                         .by = c(year, species_code, comp_type, type)) %>% 
    tidytable::filter.(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_age
  
  ess_age %>%
    tidytable::pivot_wider(names_from = type, values_from = value) -> ess_age
  
  ess_size %>% 
    tidytable::summarise(iss = psych::harmonic.mean(value, na.rm=T),
                         .by = c(year, species_code, comp_type, type)) %>% 
    tidytable::filter.(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_size
  
  ess_size %>%
    tidytable::pivot_wider(names_from = type, values_from = value) -> ess_size
  
  # write input sample size results
  
  if(region != 'bs'){
    vroom::vroom_write(ess_size, here::here("output", region, "iter_ess_sz.csv"), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "iter_ess_ag.csv"), delim = ",")
    vroom::vroom_write(iss_size, here::here("output", region, "iss_sz.csv"), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "iss_ag.csv"), delim = ",")
  } else{
    vroom::vroom_write(ess_size, here::here("output", region, paste0("iter_ess_sz_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, paste0("iter_ess_ag_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(iss_size, here::here("output", region, paste0("iss_sz_", srvy_type, ".csv")), delim = ",") 
    vroom::vroom_write(iss_age, here::here("output", region, paste0("iss_ag_", srvy_type, ".csv")), delim = ",")
  }
  
}