#' replicate survey input sample size function for experimental runs
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

srvy_iss_exp <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, age_err = FALSE,
                     region = NULL, save_interm = FALSE, match_orig = FALSE, srvy_type = NULL){
  
  # create storage location
  if(isTRUE(age_err) & !dir.exists(here::here('output', region, 'agerr'))){
    dir.create(here::here('output', region, 'agerr'), recursive = TRUE)
  }
  if(isTRUE(al_var) & !dir.exists(here::here('output', region, 'alvar'))){
    dir.create(here::here('output', region, 'alvar'), recursive = TRUE)
  }
  if(isTRUE(al_var) & isTRUE(age_err) & !dir.exists(here::here('output', region, 'alvar_agerr'))){
    dir.create(here::here('output', region, 'alvar_agerr'), recursive = TRUE)
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
  oga <- og$age
  ogl <- og$length
  
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
  if(isTRUE(save_interm) & region != 'bs' & isTRUE(al_var) & !isTRUE(age_err)) {
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar", "resampled_size.csv"), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar", "resampled_age.csv"), delim = ",")
  } else if(isTRUE(save_interm) & region != 'bs' & !isTRUE(al_var) & isTRUE(age_err)) {
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "agerr", "resampled_size.csv"), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "agerr", "resampled_age.csv"), delim = ",")
  } else if(isTRUE(save_interm) & region != 'bs' & isTRUE(al_var) & isTRUE(age_err)) {
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar_agerr", "resampled_size.csv"), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar_agerr", "resampled_age.csv"), delim = ",")
  }
  
  if(isTRUE(save_interm) & region == 'bs' & isTRUE(al_var) & !isTRUE(age_err)) {
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar", paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar", paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  } else if(isTRUE(save_interm) & region == 'bs' & !isTRUE(al_var) & isTRUE(age_err)) {
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "agerr", paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "agerr", paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  } else if(isTRUE(save_interm) & region == 'bs' & isTRUE(al_var) & isTRUE(age_err)) {
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar_agerr", paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "alvar_agerr", paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  }
  
  # compute effective sample size of bootstrapped age/length
  r_age %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age
  r_length %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size
  
  # Write iterated effective sample size results
  if(region != 'bs' & isTRUE(al_var) & !isTRUE(age_err)) {
    vroom::vroom_write(ess_size, here::here("output", region, "alvar", "iter_ess_sz.csv"), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "alvar", "iter_ess_ag.csv"), delim = ",")
  } else if(region != 'bs' & !isTRUE(al_var) & isTRUE(age_err)) {
    vroom::vroom_write(ess_size, here::here("output", region, "agerr", "iter_ess_sz.csv"), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "agerr", "iter_ess_ag.csv"), delim = ",")
  } else if(region != 'bs' & isTRUE(al_var) & isTRUE(age_err)) {
    vroom::vroom_write(ess_size, here::here("output", region, "alvar_agerr", "iter_ess_sz.csv"), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "alvar_agerr", "iter_ess_ag.csv"), delim = ",")
  }
  if(region == 'bs' & isTRUE(al_var) & !isTRUE(age_err)) {
    vroom::vroom_write(ess_size, here::here("output", region, "alvar", paste0("iter_ess_sz_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "alvar", paste0("iter_ess_ag_", srvy_type, ".csv")), delim = ",")
  } else if(region == 'bs' & !isTRUE(al_var) & isTRUE(age_err)) {
    vroom::vroom_write(ess_size, here::here("output", region, "agerr", paste0("iter_ess_sz_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "agerr", paste0("iter_ess_ag_", srvy_type, ".csv")), delim = ",")
  } else if(region == 'bs' & isTRUE(al_var) & isTRUE(age_err)) {
    vroom::vroom_write(ess_size, here::here("output", region, "alvar_agerr", paste0("iter_ess_sz_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "alvar_agerr", paste0("iter_ess_ag_", srvy_type, ".csv")), delim = ",")
  }
  
  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  #  also add nominal sample size (nss) and number of hauls (hls) as column (for goa and ai)
  
  if(region != 'bs'){
    lfreq_data %>% 
      tidytable::summarise.(nss = sum(frequency),
                            .by = c(year, species_code, sex)) %>% 
      tidytable::pivot_wider.(names_from = sex,
                              values_from = nss) %>% 
      tidytable::rename.(male = '1',
                         female = '2',
                         unsexed = '3') %>% 
      tidytable::mutate.(unsexed = case_when(is.na(unsexed) ~ 0,
                                             !is.na(unsexed) ~ unsexed),
                         total = male + female + unsexed) %>% 
      tidytable::select.(-unsexed) %>% 
      tidytable::pivot_longer.(cols = c(male, female, total),
                               names_to = 'comp_type',
                               values_to = 'nss') -> nss_size
    
    lfreq_data %>% 
      tidytable::summarise.(total = length(unique(hauljoin)),
                            .by = c(year, species_code)) -> tot_hls
    
    lfreq_data %>% 
      tidytable::summarise.(hls = length(unique(hauljoin)),
                            .by = c(year, species_code, sex)) %>% 
      tidytable::pivot_wider.(names_from = sex,
                              values_from = hls) %>% 
      tidytable::rename.(male = '1',
                         female = '2',
                         unsexed = '3') %>%
      tidytable::select.(-unsexed) %>% 
      tidytable::left_join.(tot_hls) %>% 
      tidytable::pivot_longer.(cols = c(male, female, total),
                               names_to = 'comp_type',
                               values_to = 'hls') -> hls_size
    
    ess_size %>% 
      tidytable::mutate.(iss = psych::harmonic.mean(value), 
                         .by = c(year, species_code, ess)) %>%
      dplyr::distinct(year, species_code, ess, iss) %>% 
      tidytable::drop_na.() %>% 
      tidytable::filter.(iss > 0) %>% 
      tidytable::rename.(comp_type = ess) %>% 
      tidytable::mutate.(comp_type = case_when(comp_type == 'ess_f' ~ 'female',
                                               comp_type == 'ess_m' ~ 'male',
                                               comp_type == 'ess_t' ~ 'total')) %>% 
      tidytable::left_join.(nss_size) %>% 
      tidytable::left_join.(hls_size) -> iss_size
    
    specimen_data %>% 
      tidytable::filter.(!is.na(age)) %>% 
      tidytable::summarise.(nss = length(age),
                            .by = c(year, species_code, sex)) %>% 
      tidytable::pivot_wider.(names_from = sex,
                              values_from = nss) %>% 
      tidytable::rename.(male = '1',
                         female = '2',
                         unsexed = '3') %>% 
      tidytable::mutate.(unsexed = case_when(is.na(unsexed) ~ 0,
                                             !is.na(unsexed) ~ unsexed),
                         total = male + female + unsexed) %>% 
      tidytable::select.(-unsexed) %>% 
      tidytable::pivot_longer.(cols = c(male, female, total),
                               names_to = 'comp_type',
                               values_to = 'nss') -> nss_age
    
    specimen_data %>% 
      tidytable::filter.(!is.na(age)) %>%
      tidytable::summarise.(total = length(unique(hauljoin)),
                            .by = c(year, species_code)) -> tot_hls
    
    specimen_data %>% 
      tidytable::filter.(!is.na(age)) %>% 
      tidytable::summarise.(hls = length(unique(hauljoin)),
                            .by = c(year, species_code, sex)) %>% 
      tidytable::pivot_wider.(names_from = sex,
                              values_from = hls) %>% 
      tidytable::rename.(male = '1',
                         female = '2',
                         unsexed = '3') %>%
      tidytable::select.(-unsexed) %>% 
      tidytable::left_join.(tot_hls) %>% 
      tidytable::pivot_longer.(cols = c(male, female, total),
                               names_to = 'comp_type',
                               values_to = 'hls') -> hls_age
    
    ess_age %>% 
      tidytable::mutate.(iss = psych::harmonic.mean(value), 
                         .by = c(year, species_code, ess)) %>%
      dplyr::distinct(year, species_code, ess, iss) %>% 
      tidytable::drop_na.() %>% 
      tidytable::filter.(iss > 0) %>% 
      tidytable::rename.(comp_type = ess) %>% 
      tidytable::mutate.(comp_type = case_when(comp_type == 'ess_f' ~ 'female',
                                               comp_type == 'ess_m' ~ 'male',
                                               comp_type == 'ess_t' ~ 'total')) %>% 
      tidytable::left_join.(nss_age) %>% 
      tidytable::left_join.(hls_age) -> iss_age
    
  } else{
    
    ess_size %>% 
      tidytable::mutate.(iss = psych::harmonic.mean(value), 
                         .by = c(year, species_code, ess)) %>%
      dplyr::distinct(year, species_code, ess, iss) %>% 
      tidytable::drop_na.() %>% 
      tidytable::filter.(iss > 0) %>% 
      tidytable::rename.(comp_type = ess) %>% 
      tidytable::mutate.(comp_type = case_when(comp_type == 'ess_f' ~ 'female',
                                               comp_type == 'ess_m' ~ 'male',
                                               comp_type == 'ess_t' ~ 'total')) -> iss_size
    
    ess_age %>% 
      tidytable::mutate.(iss = psych::harmonic.mean(value), 
                         .by = c(year, species_code, ess)) %>%
      dplyr::distinct(year, species_code, ess, iss) %>% 
      tidytable::drop_na.() %>% 
      tidytable::filter.(iss > 0) %>% 
      tidytable::rename.(comp_type = ess) %>% 
      tidytable::mutate.(comp_type = case_when(comp_type == 'ess_f' ~ 'female',
                                               comp_type == 'ess_m' ~ 'male',
                                               comp_type == 'ess_t' ~ 'total')) -> iss_age
  }
  
  # write input sample size results
  if(region != 'bs' & isTRUE(al_var) & !isTRUE(age_err)){
    vroom::vroom_write(iss_size, here::here("output", region, "alvar", "iss_sz.csv"), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "alvar", "iss_ag.csv"), delim = ",")
  } else if(region != 'bs' & !isTRUE(al_var) & isTRUE(age_err)){
    vroom::vroom_write(iss_size, here::here("output", region, "agerr", "iss_sz.csv"), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "agerr", "iss_ag.csv"), delim = ",")
  } else if(region != 'bs' & isTRUE(al_var) & isTRUE(age_err)){
    vroom::vroom_write(iss_size, here::here("output", region, "alvar_agerr", "iss_sz.csv"), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "alvar_agerr", "iss_ag.csv"), delim = ",")
  }
  
  if(region == 'bs' & isTRUE(al_var) & !isTRUE(age_err)){
    vroom::vroom_write(iss_size, here::here("output", region, "alvar", paste0("iss_sz_", srvy_type, ".csv")), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "alvar", paste0("iss_ag_", srvy_type, ".csv")), delim = ",")
  } else if(region == 'bs' & !isTRUE(al_var) & isTRUE(age_err)){
    vroom::vroom_write(iss_size, here::here("output", region, "agerr", paste0("iss_sz_", srvy_type, ".csv")), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "agerr", paste0("iss_ag_", srvy_type, ".csv")), delim = ",")
  } else if(region == 'bs' & isTRUE(al_var) & isTRUE(age_err)){
    vroom::vroom_write(iss_size, here::here("output", region, "alvar_agerr", paste0("iss_sz_", srvy_type, ".csv")), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "alvar_agerr", paste0("iss_ag_", srvy_type, ".csv")), delim = ",")
  }
  
}