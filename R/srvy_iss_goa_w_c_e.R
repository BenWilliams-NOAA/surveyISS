#' replicate srvy_iss function for spatially-explicit input sample size for western, central and eastern gulf subregions
#'
#' @param iters number of iterations (500 recommended)
#' @param lfreq_data  input dataframe
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param strata_data input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_lengths resample lengths w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_orig save the original comps (default = FALSE)
#' @param save_comps save the resampled comps (default = FALSE)
#' @param save_ess save the iterated effective sample sizes (default = FALSE)
#' @param match_gap match the computed values to gap output (default = FALSE)
#'
#' @return
#' @export srvy_iss_goa_w_c_e
#'
#' @examples
#' 
#'
srvy_iss_goa_w_c_e <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, yrs = NULL, 
                               boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, 
                               region = NULL, save_orig = FALSE, save_comps = FALSE, save_ess = FALSE, match_orig = FALSE){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  
  # define data by subregion (swap out 'stratum' column with subregion) 
  lfreq_data %>%
    tidytable::left_join.(strata_data)  %>% 
    tidytable::mutate.(region = ifelse(summary_area %in% c(919), "wgoa",
                                       ifelse(summary_area %in% c(929, 939), "cgoa",
                                              ifelse(summary_area %in% c(949, 959), "egoa", NA)))) %>% 
    tidytable::select.(-survey, -area, -summary_area) -> .lfreq_data
  
  specimen_data %>% 
    tidytable::left_join.(strata_data) %>% 
    tidytable::mutate.(region = ifelse(summary_area %in% c(919), "wgoa",
                                       ifelse(summary_area %in% c(929, 939), "cgoa",
                                              ifelse(summary_area %in% c(949, 959), "egoa", NA)))) %>%  
    tidytable::select.(-survey, -area, -summary_area) -> .specimen_data
  
  cpue_data %>%  
    tidytable::left_join.(strata_data) %>% 
    tidytable::mutate.(region = ifelse(summary_area %in% c(919), "wgoa",
                                       ifelse(summary_area %in% c(929, 939), "cgoa",
                                              ifelse(summary_area %in% c(949, 959), "egoa", NA)))) %>% 
    tidytable::select.(-survey, -area, -summary_area)  -> .cpue_data
  
  
  # get original values for western goa
  og_w <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wgoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "wgoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "wgoa"),
                     strata_data = strata_data, yrs = yrs, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE)
  
  oga <- og_w$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_w
  ogl <- og_w$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_w
  
  # get original values for central goa
  og_c <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "cgoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "cgoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "cgoa"),
                     strata_data = strata_data, yrs = yrs, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE)
  
  oga <- og_c$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_c
  ogl <- og_c$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_c
  
  # get original values for eastern goa
  og_e <- srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                     specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                     cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"),
                     strata_data = strata_data, yrs = yrs, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE)
  
  oga <- og_e$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age, species_code)) -> oga_e
  ogl <- og_e$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length, species_code)) -> ogl_e
  
  # compile og results 
  oga_w %>% 
    tidytable::mutate.(region = "wgoa") -> .oga_w
  oga_c %>% 
    tidytable::mutate.(region = "cgoa") -> .oga_c
  oga_e %>% 
    tidytable::mutate.(region = "egoa") %>% 
    tidytable::bind_rows.(.oga_w, .oga_c) -> oga
  
  ogl_w %>% 
    tidytable::mutate.(region = "wgoa") -> .ogl_w
  ogl_c %>% 
    tidytable::mutate.(region = "cgoa") -> .ogl_c
  ogl_e %>% 
    tidytable::mutate.(region = "egoa") %>% 
    tidytable::bind_rows.(.ogl_w, .ogl_c) -> ogl
  
  # if desired, write original age/length pop'n estimates
  if(isTRUE(save_orig)){
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age_w_c_egoa.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length_w_c_egoa.csv"), delim = ",")
  }
  
  # run iterations for western goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "wgoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "wgoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "wgoa"), 
                                         strata_data = strata_data, 
                                         yrs = yrs, 
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_age %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, age, species_code)) %>% 
    split(., .[,'sim']) -> r_age_w
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code)) %>% 
    split(., .[,'sim']) -> r_length_w
  
  # run iterations for central goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "cgoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "cgoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "cgoa"), 
                                         strata_data = strata_data, 
                                         yrs = yrs, 
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_age %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, age, species_code)) %>% 
    split(., .[,'sim']) -> r_age_c
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code)) %>% 
    split(., .[,'sim']) -> r_length_c
  
  # run iterations for eastern goa
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = subset(.lfreq_data, .lfreq_data$region == "egoa"), 
                                         specimen_data = subset(.specimen_data, .specimen_data$region == "egoa"), 
                                         cpue_data = subset(.cpue_data, .cpue_data$region == "egoa"), 
                                         strata_data = strata_data, 
                                         yrs = yrs, 
                                         boot_hauls = boot_hauls, 
                                         boot_lengths = boot_lengths, 
                                         boot_ages = boot_ages))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_age %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, age, species_code)) %>% 
    split(., .[,'sim']) -> r_age_e
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length, species_code)) %>% 
    split(., .[,'sim']) -> r_length_e
  
  # if desired, write out resampled comp data
  if(isTRUE(save_comps)) {
    r_age_w %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "wgoa") %>% 
      tidytable::select(-sim...2) %>% 
      tidytable::rename(sim = 'sim...1') -> .r_age_w
    r_age_c %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "cgoa") %>% 
      tidytable::select(-sim...2) %>% 
      tidytable::rename(sim = 'sim...1') -> .r_age_c
    r_age_e %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "egoa") %>% 
      tidytable::select(-sim...2) %>% 
      tidytable::rename(sim = 'sim...1') %>% 
      tidytable::bind_rows.(.r_age_w, .r_age_c) %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_w_c_egoa.csv"), delim = ",")
    r_length_w %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "wgoa") %>% 
      tidytable::select(-sim...2) %>% 
      tidytable::rename(sim = 'sim...1') -> .r_length_w
    r_length_c %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "cgoa") %>% 
      tidytable::select(-sim...2) %>% 
      tidytable::rename(sim = 'sim...1') -> .r_length_c
    r_length_e %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      tidytable::mutate.(region = "egoa") %>% 
      tidytable::select(-sim...2) %>% 
      tidytable::rename(sim = 'sim...1') %>% 
      tidytable::bind_rows.(.r_length_w, .r_length_c) %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size_w_c_egoa.csv"), delim = ",") 
  }
  
  # compute effective sample size of bootstrapped age/length by subregion
  r_age_w %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_w)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age_w
  
  r_length_w %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_w)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size_w
  
  r_age_c %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_c)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age_c
  
  r_length_c %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_c)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size_c
  
  r_age_e %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_e)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age_e
  
  r_length_e %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_e)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size_e
  
  ess_age_w %>% 
    tidytable::mutate.(region = "wgoa") -> .ess_age_w
  ess_age_c %>% 
    tidytable::mutate.(region = "cgoa") -> .ess_age_c
  ess_age_e %>% 
    tidytable::mutate.(region = "egoa") %>% 
    tidytable::bind_rows.(.ess_age_w, .ess_age_c) -> ess_age
  
  ess_size_w %>% 
    tidytable::mutate.(region = "wgoa") -> .ess_size_w
  ess_size_c %>% 
    tidytable::mutate.(region = "cgoa") -> .ess_size_c
  ess_size_e %>% 
    tidytable::mutate.(region = "egoa") %>% 
    tidytable::bind_rows.(.ess_size_w, .ess_size_c) -> ess_size
  
  # if desired, Write iterated effective sample size results
  if(isTRUE(save_ess)) {
    vroom::vroom_write(ess_age, 
                       here::here("output", region, "iter_ess_ag_w_c_egoa.csv"), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, "iter_ess_sz_w_c_egoa.csv"), 
                       delim = ",")
  }
  
  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  #  also add nominal sample size (nss) and number of hauls (hls) as column
  
  .lfreq_data %>% 
    tidytable::summarise.(nss = sum(frequency),
                          .by = c(year, species_code, sex, region)) %>% 
    tidytable::pivot_wider.(names_from = sex,
                            values_from = nss) %>% 
    tidytable::rename.(male = '1',
                       female = '2',
                       unsexed = '3') %>% 
    tidytable::mutate.(unsexed = case_when(is.na(unsexed) ~ 0,
                                           !is.na(unsexed) ~ unsexed),
                       male = case_when(is.na(male) ~ 0,
                                        !is.na(male) ~ male),
                       female = case_when(is.na(female) ~ 0,
                                          !is.na(female) ~ female),
                       total = male + female + unsexed) %>% 
    tidytable::select.(-unsexed) %>% 
    tidytable::pivot_longer.(cols = c(male, female, total),
                             names_to = 'comp_type',
                             values_to = 'nss') -> nss_size
  
  .lfreq_data %>% 
    tidytable::summarise.(total = length(unique(hauljoin)),
                          .by = c(year, species_code, region)) -> tot_hls
  
  .lfreq_data %>% 
    tidytable::summarise.(hls = length(unique(hauljoin)),
                          .by = c(year, species_code, sex, region)) %>% 
    tidytable::pivot_wider.(names_from = sex,
                            values_from = hls) %>% 
    tidytable::rename.(male = '1',
                       female = '2',
                       unsexed = '3') %>%
    tidytable::select.(-unsexed) %>% 
    tidytable::mutate.(male = case_when(is.na(male) ~ 0,
                                        !is.na(male) ~ male),
                       female = case_when(is.na(female) ~ 0,
                                          !is.na(female) ~ female)) %>% 
    tidytable::left_join.(tot_hls) %>% 
    tidytable::pivot_longer.(cols = c(male, female, total),
                             names_to = 'comp_type',
                             values_to = 'hls') -> hls_size
  
  ess_size %>% 
    tidytable::mutate.(iss = psych::harmonic.mean(value), 
                       .by = c(year, species_code, ess, region)) %>%
    dplyr::distinct(year, species_code, region, ess, iss) %>% 
    tidytable::drop_na.() %>% 
    tidytable::filter.(iss > 0) %>% 
    tidytable::rename.(comp_type = ess) %>% 
    tidytable::mutate.(comp_type = case_when(comp_type == 'ess_f' ~ 'female',
                                             comp_type == 'ess_m' ~ 'male',
                                             comp_type == 'ess_t' ~ 'total')) %>% 
    tidytable::left_join.(nss_size) %>% 
    tidytable::left_join.(hls_size) -> iss_size
  
  .specimen_data %>% 
    tidytable::filter.(!is.na(age)) %>% 
    tidytable::summarise.(nss = length(age),
                          .by = c(year, species_code, sex, region)) %>% 
    tidytable::pivot_wider.(names_from = sex,
                            values_from = nss) %>% 
    tidytable::rename.(male = '1',
                       female = '2',
                       unsexed = '3') %>% 
    tidytable::mutate.(unsexed = case_when(is.na(unsexed) ~ 0,
                                           !is.na(unsexed) ~ unsexed),
                       male = case_when(is.na(male) ~ 0,
                                        !is.na(male) ~ male),
                       female = case_when(is.na(female) ~ 0,
                                          !is.na(female) ~ female),
                       total = male + female + unsexed) %>% 
    tidytable::select.(-unsexed) %>% 
    tidytable::pivot_longer.(cols = c(male, female, total),
                             names_to = 'comp_type',
                             values_to = 'nss') -> nss_age
  
  .specimen_data %>% 
    tidytable::filter.(!is.na(age)) %>%
    tidytable::summarise.(total = length(unique(hauljoin)),
                          .by = c(year, species_code, region)) -> tot_hls
  
  .specimen_data %>% 
    tidytable::filter.(!is.na(age)) %>% 
    tidytable::summarise.(hls = length(unique(hauljoin)),
                          .by = c(year, species_code, sex, region)) %>% 
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
                       .by = c(year, species_code, ess, region)) %>%
    dplyr::distinct(year, species_code, region, ess, iss) %>% 
    tidytable::drop_na.() %>% 
    tidytable::filter.(iss > 0) %>% 
    tidytable::rename.(comp_type = ess) %>% 
    tidytable::mutate.(comp_type = case_when(comp_type == 'ess_f' ~ 'female',
                                             comp_type == 'ess_m' ~ 'male',
                                             comp_type == 'ess_t' ~ 'total')) %>% 
    tidytable::left_join.(nss_age) %>% 
    tidytable::left_join.(hls_age) -> iss_age
  
  # write input sample size results
  vroom::vroom_write(iss_age, 
                     here::here("output", region, "iss_ag_w_c_egoa.csv"), 
                     delim = ",")
  vroom::vroom_write(iss_size, 
                     here::here("output", region, "iss_sz_w_c_egoa.csv"), 
                     delim = ",")
  
}