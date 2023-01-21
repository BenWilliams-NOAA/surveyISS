#' replicate srvy_iss function for goa dusky rockfish
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
#' @param match_orig match the computed values to gap output (default = FALSE)
#'
#' @return
#' @export srvy_iss_goa_dr
#'
#' @examples
#' 
#'
srvy_iss_goa_dr <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, yrs = NULL, 
                            boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, 
                            region = NULL, save_orig = FALSE, save_comps = FALSE, save_ess = FALSE, match_orig = FALSE){
  
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
                   yrs = yrs, 
                   boot_hauls = FALSE, 
                   boot_lengths = FALSE, 
                   boot_ages = FALSE)
  oga <- og$age
  oga %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, age)) %>% 
    tidytable::mutate.(species_code = 301502) -> oga_c
  ogl <- og$length
  ogl %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(year, length)) %>% 
    tidytable::mutate.(species_code = 301502) -> ogl_c
  
  # if desired, write original age/length pop'n estimates
  if(isTRUE(save_orig)){
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age_dr.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length_dr.csv"), delim = ",")
  }
  
  # currently, due to the complex nature of how we do our complexes we do not match with gap output
  
  # run resampling iterations
  rr <- purrr::map(1:iters, ~ srvy_comps(lfreq_data = lfreq_data, 
                                         specimen_data = specimen_data, 
                                         cpue_data = cpue_data, 
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
                          .by = c(sim, year, age)) %>% 
    tidytable::mutate.(species_code = 301502) %>% 
    split(., .[,'sim']) -> r_age_cmplx
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  r_length %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize.(males = sum(males),
                          females = sum(females),
                          unsexed = sum(unsexed),
                          .by = c(sim, year, length)) %>% 
    tidytable::mutate.(species_code = 301502) %>% 
    split(., .[,'sim']) -> r_length_cmplx
  
  # if desired, write out resampled comp data
  if(isTRUE(save_comps)) {
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_dr.csv"), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size_dr.csv"), delim = ",")
  }
  
  # compute effective sample size of bootstrapped age/length (for complex as whole)
  r_age_cmplx %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga_c)) %>% 
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age
  
  r_length_cmplx %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl_c)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size
  
  # if desired, Write iterated effective sample size results
  if(isTRUE(save_ess)) {
    vroom::vroom_write(ess_age, 
                       here::here("output", region, "iter_ess_ag_dr.csv"), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, "iter_ess_sz_dr.csv"), 
                       delim = ",")
  }
  
  # compute harmonic mean of iterated effective sample size, which is the input sample size
  #  also add nominal sample size (nss) and number of hauls (hls) as column
  
  lfreq_data %>% 
    tidytable::summarise.(nss = sum(frequency),
                          .by = c(year, sex)) %>% 
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
                          .by = c(year)) -> tot_hls
  
  lfreq_data %>% 
    tidytable::summarise.(hls = length(unique(hauljoin)),
                          .by = c(year, sex)) %>% 
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
    tidytable::left_join.(hls_size)  -> iss_size
  
  specimen_data %>% 
    tidytable::filter.(!is.na(age)) %>% 
    tidytable::summarise.(nss = length(age),
                          .by = c(year, sex)) %>% 
    tidytable::pivot_wider.(names_from = sex,
                            values_from = nss) %>% 
    tidytable::rename.(male = '1',
                       female = '2') %>% 
    tidytable::mutate.(total = male + female) %>% 
    tidytable::pivot_longer.(cols = c(male, female, total),
                             names_to = 'comp_type',
                             values_to = 'nss') -> nss_age
  
  specimen_data %>% 
    tidytable::filter.(!is.na(age)) %>%
    tidytable::summarise.(total = length(unique(hauljoin)),
                          .by = c(year)) -> tot_hls
  
  specimen_data %>% 
    tidytable::filter.(!is.na(age)) %>% 
    tidytable::summarise.(hls = length(unique(hauljoin)),
                          .by = c(year, sex)) %>% 
    tidytable::pivot_wider.(names_from = sex,
                            values_from = hls) %>% 
    tidytable::rename.(male = '1',
                       female = '2') %>% 
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
  
  # write input sample size results
  vroom::vroom_write(iss_age, 
                     here::here("output", region, "iss_ag_dr.csv"), 
                     delim = ",")
  vroom::vroom_write(iss_size, 
                     here::here("output", region, "iss_sz_dr.csv"), 
                     delim = ",")
  
}