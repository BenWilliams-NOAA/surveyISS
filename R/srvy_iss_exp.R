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
  if(!dir.exists(here::here('output', region, 'add_err'))){
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
  oga <- og$age
  ogl <- og$length
  
  # run resampling iterations
  rr <- purrr::map(1:iters, ~ srvy_comps_exp(lfreq_data = lfreq_data, 
                                             specimen_data = specimen_data, 
                                             cpue_data = cpue_data, 
                                             strata_data = strata_data,
                                             r_t = r_t,
                                             yrs = yrs, 
                                             boot_hauls = boot_hauls, 
                                             boot_lengths = boot_lengths, 
                                             boot_ages = boot_ages,
                                             al_var = TRUE,
                                             age_err = TRUE))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_age_al <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age_al
  r_age_ae <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age_ae
  r_age_al_ae <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age_al_ae
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # if desired, write out intermediate results - would need to add a bunch of if() statements here
  # if(isTRUE(save_interm) & region != 'bs' & isTRUE(al_var) & !isTRUE(age_err)) {
  #   r_length %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar", "resampled_size.csv"), delim = ",")
  #   r_age %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar", "resampled_age.csv"), delim = ",")
  # } else if(isTRUE(save_interm) & region != 'bs' & !isTRUE(al_var) & isTRUE(age_err)) {
  #   r_length %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "agerr", "resampled_size.csv"), delim = ",")
  #   r_age %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "agerr", "resampled_age.csv"), delim = ",")
  # } else if(isTRUE(save_interm) & region != 'bs' & isTRUE(al_var) & isTRUE(age_err)) {
  #   r_length %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar_agerr", "resampled_size.csv"), delim = ",")
  #   r_age %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar_agerr", "resampled_age.csv"), delim = ",")
  # }
  # 
  # if(isTRUE(save_interm) & region == 'bs' & isTRUE(al_var) & !isTRUE(age_err)) {
  #   r_length %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar", paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
  #   r_age %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar", paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  # } else if(isTRUE(save_interm) & region == 'bs' & !isTRUE(al_var) & isTRUE(age_err)) {
  #   r_length %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "agerr", paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
  #   r_age %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "agerr", paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  # } else if(isTRUE(save_interm) & region == 'bs' & isTRUE(al_var) & isTRUE(age_err)) {
  #   r_length %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar_agerr", paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
  #   r_age %>%
  #     tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
  #     vroom::vroom_write(here::here("output", region, "alvar_agerr", paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  # }
  
  # compute effective sample size of bootstrapped age/length
  r_age %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(ess == 'ess_f' ~ 'female',
                                                        ess == 'ess_m' ~ 'male',
                                                        ess == 'ess_t' ~ 'total')) %>% 
    tidytable::rename(ess_base = 'value') %>% 
    tidytable::select(sim, year, species_code, comp_type, ess_base) -> ess_ag
  r_age_al %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(ess == 'ess_f' ~ 'female',
                                                        ess == 'ess_m' ~ 'male',
                                                        ess == 'ess_t' ~ 'total')) %>% 
    tidytable::rename(ess_al = 'value') %>% 
    tidytable::select(sim, year, species_code, comp_type, ess_al) -> ess_age_al
  r_age_ae %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(ess == 'ess_f' ~ 'female',
                                                        ess == 'ess_m' ~ 'male',
                                                        ess == 'ess_t' ~ 'total')) %>% 
    tidytable::rename(ess_ae = 'value') %>% 
    tidytable::select(sim, year, species_code, comp_type, ess_ae) -> ess_age_ae
  r_age_al_ae %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate.(comp_type = tidytable::case_when(ess == 'ess_f' ~ 'female',
                                                        ess == 'ess_m' ~ 'male',
                                                        ess == 'ess_t' ~ 'total')) %>% 
    tidytable::rename(ess_al_ae = 'value') %>% 
    tidytable::select(sim, year, species_code, comp_type, ess_al_ae) -> ess_age_al_ae
  r_length %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size
  
  ess_ag %>% 
    tidytable::left_join(ess_age_al) %>% 
    tidytable::left_join(ess_age_ae) %>% 
    tidytable::left_join(ess_age_al_ae) -> ess_age
  
  
  # Write iterated effective sample size results
  if(region != 'bs') {
    vroom::vroom_write(ess_size, here::here("output", region, "add_err", "iter_ess_sz.csv"), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "add_err", "iter_ess_ag.csv"), delim = ",")
  }
  if(region == 'bs') {
    vroom::vroom_write(ess_size, here::here("output", region, "add_err", paste0("iter_ess_sz_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ess_age, here::here("output", region, "add_err", paste0("iter_ess_ag_", srvy_type, ".csv")), delim = ",")
  }
  
  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  
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
      tidytable::summarise.(iss_base = psych::harmonic.mean(ess_base),
                            iss_al = psych::harmonic.mean(ess_al),
                            iss_ae = psych::harmonic.mean(ess_ae),
                            iss_al_ae = psych::harmonic.mean(ess_al_ae),
                            .by = c(year, species_code, comp_type)) -> iss_age
  
  # write input sample size results
  if(region != 'bs'){
    vroom::vroom_write(iss_size, here::here("output", region, "add_err", "iss_sz.csv"), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "add_err", "iss_ag.csv"), delim = ",")
  }
  
  if(region == 'bs'){
    vroom::vroom_write(iss_size, here::here("output", region, "add_err", paste0("iss_sz_", srvy_type, ".csv")), delim = ",")    
    vroom::vroom_write(iss_age, here::here("output", region, "add_err", paste0("iss_ag_", srvy_type, ".csv")), delim = ",")
  }
  
}