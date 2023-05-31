#' replicate srvy_iss function for spatially-explicit input sample size for western, central and eastern gulf subregions
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
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_goa_wc_e
#'
#' @examples
#' 
#'
srvy_iss_goa_wc_e <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, 
                              boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, age_err = FALSE,
                              region = NULL, save_interm = FALSE, match_orig = FALSE, save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # define data by subregion (swap out 'stratum' column with subregion) 
  lfreq_data %>%
    tidytable::left_join(strata_data)  %>% 
    tidytable::mutate(region = ifelse(summary_area %in% c(919, 929, 939), "wcgoa",
                                      ifelse(summary_area %in% c(949, 959), "egoa", NA))) %>% 
    tidytable::select(-survey, -area, -summary_area) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = ifelse(summary_area %in% c(919, 929, 939), "wcgoa",
                                       ifelse(summary_area %in% c(949, 959), "egoa", NA))) %>%  
    tidytable::select(-survey, -area, -summary_area) -> .specimen_data
  
  cpue_data %>%  
    tidytable::left_join(strata_data) %>% 
    tidytable::mutate(region = ifelse(summary_area %in% c(919, 929, 939), "wcgoa",
                                       ifelse(summary_area %in% c(949, 959), "egoa", NA))) %>% 
    tidytable::select(-survey, -area, -summary_area)  -> .cpue_data
  
  
  # get original values for western & central goa
  og_wc <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wcgoa"), 
                      specimen_data = subset(.specimen_data, .specimen_data$region == "wcgoa"), 
                      cpue_data = subset(.cpue_data, .cpue_data$region == "wcgoa"),
                      strata_data = strata_data, 
                      r_t = r_t,
                      yrs = yrs, 
                      boot_hauls = FALSE, 
                      boot_lengths = FALSE, 
                      boot_ages = FALSE,
                      al_var = FALSE,
                      age_err = FALSE)
  
  oga <- og_wc$age %>% 
    select(-type)
  oga %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_wc
  ogl <- og_wc$length %>% 
    select(-type)
  ogl %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_wc
  
  # get original values for eastern goa
  og_e <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"),
                     strata_data = strata_data, 
                     r_t = r_t,
                     yrs = yrs, 
                     boot_hauls = FALSE, 
                     boot_lengths = FALSE, 
                     boot_ages = FALSE,
                     al_var = FALSE,
                     age_err = FALSE)
  
  oga <- og_e$age %>% 
    select(-type)
  oga %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_e
  ogl <- og_e$length %>% 
    select(-type)
  ogl %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_e
  
  # compile og results 
  oga_wc %>% 
    tidytable::mutate(region = "wcgoa") -> .oga_wc
  oga_e %>% 
    tidytable::mutate(region = "egoa") %>% 
    tidytable::bind_rows(.oga_wc) -> oga
  
  ogl_wc %>% 
    tidytable::mutate(region = "wcgoa") -> .ogl_wc
  ogl_e %>% 
    tidytable::mutate(region = "egoa") %>% 
    tidytable::bind_rows(.ogl_wc) -> ogl
  
  # run iterations for western & central goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wcgoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "wcgoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "wcgoa"), 
                                         strata_data = strata_data, 
                                         r_t = r_t,
                                         yrs = yrs, 
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, age, species_code, type)) %>% 
    split(., .[,'sim']) -> r_age_wc
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code, type)) %>% 
    split(., .[,'sim']) -> r_length_wc
  
  # run iterations for eastern goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"), 
                                         strata_data = strata_data, 
                                         r_t = r_t,
                                         yrs = yrs, 
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages,
                                         al_var = al_var,
                                         age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, age, species_code, type)) %>% 
    split(., .[,'sim']) -> r_age_e
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code, type)) %>% 
    split(., .[,'sim']) -> r_length_e
  
  # if desired, write out intermediate results
  if(isTRUE(save_interm)) {
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age_w_c_egoa.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length_w_c_egoa.csv"), delim = ",")
    r_age_wc %>%
      tidytable::map_df(., ~as.data.frame(.x)) %>% 
      tidytable::mutate(region = "wcgoa") -> .r_age_wc
    r_age_e %>%
      tidytable::map_df(., ~as.data.frame(.x)) %>% 
      tidytable::mutate(region = "egoa") %>% 
      tidytable::bind_rows(.r_age_wc) %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_wc_egoa.csv"), delim = ",")
    r_length_wc %>%
      tidytable::map_df(., ~as.data.frame(.x)) %>% 
      tidytable::mutate(region = "wcgoa") -> .r_length_wc
    r_length_e %>%
      tidytable::map_df(., ~as.data.frame(.x)) %>% 
      tidytable::mutate(region = "egoa") %>% 
      tidytable::bind_rows(.r_length_wc) %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size_wc_egoa.csv"), delim = ",") 
  }
  
  # compute effective sample size of bootstrapped age/length by subregion
  r_age_wc %>%
    tidytable::map(., ~ess_age(sim_data = .x, og_data = oga_wc)) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_age_wc
  
  r_length_wc %>%
    tidytable::map(., ~ess_size(sim_data = .x, og_data = ogl_wc)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_size_wc
  
  r_age_e %>%
    tidytable::map(., ~ess_age(sim_data = .x, og_data = oga_e)) %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_age_e
  
  r_length_e %>%
    tidytable::map(., ~ess_size(sim_data = .x, og_data = ogl_e)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_size_e
  
  ess_age_wc %>% 
    tidytable::mutate(region = "wcgoa") -> .ess_age_wc
  ess_age_e %>% 
    tidytable::mutate(region = "egoa") %>% 
    tidytable::bind_rows(.ess_age_wc) -> ess_age
  
  ess_size_wc %>% 
    tidytable::mutate(region = "wcgoa") -> .ess_size_wc
  ess_size_e %>% 
    tidytable::mutate(region = "egoa") %>% 
    tidytable::bind_rows(.ess_size_wc) -> ess_size
  
  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  ess_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(value, na.rm=T),
                         .by = c(year, species_code, comp_type, type, region)) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_age
  
  ess_age %>%
    tidytable::pivot_wider(names_from = type, values_from = value) -> ess_age
  
  ess_size %>% 
    tidytable::summarise(iss = psych::harmonic.mean(value, na.rm=T),
                         .by = c(year, species_code, comp_type, type, region)) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) -> iss_size
  
  ess_size %>%
    tidytable::pivot_wider(names_from = type, values_from = value) -> ess_size
  
  # write input sample size results
  if(save == 'prod'){
    vroom::vroom_write(ess_age, 
                       here::here("output", region, paste0(save, "_iter_ess_ag_wc_egoa.csv")), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, paste0(save, "_iter_ess_sz_wc_egoa.csv")), 
                       delim = ",")
    vroom::vroom_write(iss_age, 
                       here::here("output", region, paste0(save, "_iss_ag_wc_egoa.csv")), 
                       delim = ",")
    vroom::vroom_write(iss_size, 
                       here::here("output", region, paste0(save, "_iss_sz_wc_egoa.csv")), 
                       delim = ",")
  } else{
    vroom::vroom_write(ess_age, 
                       here::here("output", region, "add_err", paste0(save, "_iter_ess_ag_wc_egoa.csv")), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, "add_err", paste0(save, "_iter_ess_sz_wc_egoa.csv")), 
                       delim = ",")
    vroom::vroom_write(iss_age, 
                       here::here("output", region, "add_err", paste0(save, "_iss_ag_wc_egoa.csv")), 
                       delim = ",")
    vroom::vroom_write(iss_size, 
                       here::here("output", region, "add_err", paste0(save, "_iss_sz_wc_egoa.csv")), 
                       delim = ",")
  }
  
}