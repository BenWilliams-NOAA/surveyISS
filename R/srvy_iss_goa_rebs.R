#' replicate srvy_iss function for goa rebs stock complex
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
#'
#' @return
#' @export srvy_iss_goa_rebs
#'
#' @examples
#' 
#'
srvy_iss_goa_rebs <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, 
                              boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, age_err = FALSE,
                              region = NULL, save_interm = FALSE, match_orig = FALSE){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # get original values
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
  og$age %>% 
    select(-type) %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age)) %>% 
    tidytable::mutate.(species_code = 3005012) -> oga
  og$length %>% 
    select(-type) %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length)) %>% 
    tidytable::mutate.(species_code = 3005012) -> ogl
  
  # currently, due to the complex nature of how we do our complexes we do not match with gap output
  
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
  
  do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, age, type)) %>% 
    tidytable::mutate.(species_code = 3005012) %>% 
    split(., .[,'sim']) -> r_age
  
  do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, type)) %>% 
    tidytable::mutate.(species_code = 3005012) %>% 
    split(., .[,'sim']) -> r_length
  
  # if desired, write out intermediate results
  if(isTRUE(save_interm)) {
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age_rebs.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length_rebs.csv"), delim = ",")
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_rebs.csv"), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size_rebs.csv"), delim = ",")
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
  vroom::vroom_write(ess_age, 
                     here::here("output", region, "iter_ess_ag_rebs.csv"), 
                     delim = ",")
  vroom::vroom_write(ess_size, 
                     here::here("output", region, "iter_ess_sz_rebs.csv"), 
                     delim = ",")
  vroom::vroom_write(iss_age, 
                     here::here("output", region, "iss_ag_rebs.csv"), 
                     delim = ",")
  vroom::vroom_write(iss_size, 
                     here::here("output", region, "iss_sz_rebs.csv"), 
                     delim = ",")
  
}