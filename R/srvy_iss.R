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
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss
#'
#' @examples
#'
srvy_iss <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, bin = 1, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, al_var_ann = FALSE, age_err = FALSE,
                     region = NULL, save_interm = FALSE, save){
  
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
                   age_err = FALSE)
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
                                         age_err = age_err))
  
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
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  #   and compute average relative bias in pop'n estimates (avg relative bias across age or length)
  .rss_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_age %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::bind_rows(r_age %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(agepop = mean(agepop), .by = c(year, species_code, sex, age)) %>% 
                           tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-agepop) %>% 
                           tidytable::left_join(oga %>% 
                                                  tidytable::bind_rows(oga %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-agepop)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) -> iss_age

  .rss_length %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_length %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::bind_rows(r_length %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(abund = mean(abund), .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-abund) %>% 
                           tidytable::left_join(ogl %>% 
                                                  tidytable::bind_rows(ogl %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-abund)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) -> iss_length

  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln.csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, "base_age.csv"), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, "base_length.csv"), delim = ",")
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_length.csv"), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age.csv"), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag.csv")), delim = ",")
  }

}

#' rreplicate survey input sample size function for production run for ai rebs stock complex
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
#' @param region region will create a folder and place results in said folder (default = 'ai')
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_ai_rebs
#'
#' @examples
#' 
#'
srvy_iss_ai_rebs <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, bin = 1, 
                             boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, al_var_ann = FALSE, age_err = FALSE,
                             region = 'ai', save_interm = FALSE, save){
  
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
  og <- srvy_comps_ai_rebs(lfreq_data = lfreq_data, 
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
                           age_err = FALSE)
  oga <- og$age
  ogl <- og$length
  
  # run resampling iterations ----
  rr <- purrr::map(1:iters, ~ srvy_comps_ai_rebs(lfreq_data = lfreq_data, 
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
                                                 age_err = age_err))
  
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
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  #   and compute average relative bias in pop'n estimates (avg relative bias across age or length)
  .rss_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_age %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::bind_rows(r_age %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(agepop = mean(agepop), .by = c(year, species_code, sex, age)) %>% 
                           tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-agepop) %>% 
                           tidytable::left_join(oga %>% 
                                                  tidytable::bind_rows(oga %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-agepop)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) -> iss_age
  
  .rss_length %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_length %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::bind_rows(r_length %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(abund = mean(abund), .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-abund) %>% 
                           tidytable::left_join(ogl %>% 
                                                  tidytable::bind_rows(ogl %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-abund)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) -> iss_length

  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln_rebs.csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag_rebs.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, "base_age_rebs.csv"), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, "base_length_rebs.csv"), delim = ",")
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_length_rebs.csv"), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_rebs.csv"), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_rebs.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_rebs_ag.csv")), delim = ",")
  }
  
}

#' replicate survey input sample size function for production run for goa dusky rockfish
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
#' @param region region will create a folder and place results in said folder (default = 'goa')
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_goa_dr
#'
#' @examples
#' 
#'
srvy_iss_goa_dr <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, bin = 1, 
                            boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, al_var_ann = FALSE, age_err = FALSE,
                            region = 'goa', save_interm = FALSE, save){
  
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
                   age_err = FALSE)
  
  og$age %>% 
    tidytable::summarize(agepop = sum(agepop), .by = c(year, sex, age)) %>% 
    tidytable::mutate(species_code = 301502) -> oga
  og$length %>% 
    tidytable::summarize(abund = sum(abund), .by = c(year, sex, length)) %>% 
    tidytable::mutate(species_code = 301502) -> ogl
  
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
                                         age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(agepop = sum(agepop), .by = c(sim, year, sex, age)) %>% 
    tidytable::mutate(species_code = 301502) %>% 
    split(., .[,'sim'])
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(abund = sum(abund), .by = c(sim, year, sex, length)) %>% 
    tidytable::mutate(species_code = 301502) %>% 
    split(., .[,'sim'])
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  #   and compute average relative bias in pop'n estimates (avg relative bias across age or length)
  .rss_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_age %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::select(-sim...2) %>% 
                           tidytable::rename(sim = sim...1) %>% 
                           tidytable::bind_rows(r_age %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::select(-sim...2) %>% 
                                                  tidytable::rename(sim = sim...1) %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(agepop = mean(agepop), .by = c(year, species_code, sex, age)) %>% 
                           tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-agepop) %>% 
                           tidytable::left_join(oga %>% 
                                                  tidytable::bind_rows(oga %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-agepop)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::arrange(year, species_code, sex) -> iss_age
  
  .rss_length %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_length %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::select(-sim...2) %>% 
                           tidytable::rename(sim = sim...1) %>% 
                           tidytable::bind_rows(r_length %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::select(-sim...2) %>% 
                                                  tidytable::rename(sim = sim...1) %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(abund = mean(abund), .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-abund) %>% 
                           tidytable::left_join(ogl %>% 
                                                  tidytable::bind_rows(ogl %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-abund)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) -> iss_length
  
  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln_dr.csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag_dr.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, "base_age_dr.csv"), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, "base_length_dr.csv"), delim = ",")
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_length_dr.csv"), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_dr.csv"), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_dr.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_dr.csv")), delim = ",")
  }
  
}

#' replicate survey input sample size function for production run for goa rebs stock complex
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
#' @param region region will create a folder and place results in said folder (default = 'goa')
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_goa_rebs
#'
#' @examples
#'
srvy_iss_goa_rebs <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, bin = 1, 
                              boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, al_var_ann = FALSE, age_err = FALSE,
                              region = 'goa', save_interm = FALSE, save){
  
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
                   age_err = FALSE)
  
  og$age %>% 
    tidytable::summarize(agepop = sum(agepop), .by = c(year, sex, age)) %>% 
    tidytable::mutate(species_code = 3005012) -> oga
  og$length %>% 
    tidytable::summarize(abund = sum(abund), .by = c(year, sex, length)) %>% 
    tidytable::mutate(species_code = 3005012) -> ogl
  
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
                                         age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(agepop = sum(agepop), .by = c(sim, year, sex, age)) %>% 
    tidytable::mutate(species_code = 3005012) %>% 
    split(., .[,'sim'])
  
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length %>% 
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::summarize(abund = sum(abund), .by = c(sim, year, sex, length)) %>% 
    tidytable::mutate(species_code = 3005012) %>% 
    split(., .[,'sim'])
  
  # compute statistics ----
  # compute realized sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~rss_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .rss_age
  
  
  r_length %>%
    tidytable::map(., ~rss_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  #   and compute average relative bias in pop'n estimates (avg relative bias across age or length)
  .rss_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_age %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::select(-sim...2) %>% 
                           tidytable::rename(sim = sim...1) %>% 
                           tidytable::bind_rows(r_age %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::select(-sim...2) %>% 
                                                  tidytable::rename(sim = sim...1) %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(agepop = mean(agepop), .by = c(year, species_code, sex, age)) %>% 
                           tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-agepop) %>% 
                           tidytable::left_join(oga %>% 
                                                  tidytable::bind_rows(oga %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-agepop)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::arrange(year, species_code, sex) -> iss_age
  
  .rss_length %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_length %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::select(-sim...2) %>% 
                           tidytable::rename(sim = sim...1) %>% 
                           tidytable::bind_rows(r_length %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::select(-sim...2) %>% 
                                                  tidytable::rename(sim = sim...1) %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(abund = mean(abund), .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-abund) %>% 
                           tidytable::left_join(ogl %>% 
                                                  tidytable::bind_rows(ogl %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                                                  tidytable::select(-abund)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))) %>% 
    tidytable::filter(iss > 0) -> iss_length
  
  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln_rebs.csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag_rebs.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, "base_age_rebs.csv"), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, "base_length_rebs.csv"), delim = ",")
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_length_rebs.csv"), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_rebs.csv"), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_rebs.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_rebs.csv")), delim = ",")
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
#' @export srvy_iss_goa_w_c_e
#'
#' @examples
#' 
#'
srvy_iss_goa_w_c_e <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, 
                               boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, age_err = FALSE,
                               region = NULL, save_interm = FALSE, match_orig = FALSE, save){
  
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
                   age_err = FALSE)

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
                     age_err = FALSE)
  
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
                     age_err = FALSE)
  
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
                                         age_err = age_err))
  
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
                                         age_err = age_err))
  
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
                                         age_err = age_err))
  
  r_age_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length_e <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # combine subregions with goa-wide
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
                                           sex == 4 ~ 'total_post')) -> .rss_age

  r_length %>%
    tidytable::map(., ~rss_length_reg(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .rss_length
  
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  #   and compute average relative bias in pop'n estimates (avg relative bias across age or length)
  .rss_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, region, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_age %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::select(-sim...2) %>% 
                           tidytable::rename(sim = sim...1) %>% 
                           tidytable::bind_rows(r_age %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::select(-sim...2) %>% 
                                                  tidytable::rename(sim = sim...1) %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(agepop = sum(agepop), 
                                                                       .by = c(sim, year, region, species_code, age)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(agepop = mean(agepop), 
                                                .by = c(year, region, species_code, sex, age)) %>% 
                           tidytable::mutate(p_sim = agepop / sum(agepop), 
                                             .by = c(year, region, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-agepop) %>% 
                           tidytable::left_join(oga %>% 
                                                  tidytable::bind_rows(oga %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(agepop = sum(agepop), 
                                                                                              .by = c(year, region, species_code, age)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = agepop / sum(agepop), 
                                                                    .by = c(year, region, species_code, sex)) %>% 
                                                  tidytable::select(-agepop)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), 
                                                .by = c(year, region, species_code, sex))) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::arrange(year, species_code, sex) -> iss_age
  
  .rss_length %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE),
                         .by = c(year, region, species_code, sex, sex_desc)) %>% 
    tidytable::left_join(r_length %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::select(-sim...2) %>% 
                           tidytable::rename(sim = sim...1) %>% 
                           tidytable::bind_rows(r_length %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::select(-sim...2) %>% 
                                                  tidytable::rename(sim = sim...1) %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(abund = sum(abund), 
                                                                       .by = c(sim, year, region, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(abund = mean(abund), 
                                                .by = c(year, region, species_code, sex, length)) %>% 
                           tidytable::mutate(p_sim = abund / sum(abund), 
                                             .by = c(year, region, species_code, sex)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::select(-abund) %>% 
                           tidytable::left_join(ogl %>% 
                                                  tidytable::bind_rows(ogl %>% 
                                                                         tidytable::filter(sex != 0) %>% 
                                                                         tidytable::summarise(abund = sum(abund), 
                                                                                              .by = c(year, region, species_code, length)) %>% 
                                                                         tidytable::mutate(sex = 4)) %>% 
                                                  tidytable::mutate(p_og = abund / sum(abund), 
                                                                    .by = c(year, region, species_code, sex)) %>% 
                                                  tidytable::select(-abund)) %>% 
                           tidytable::mutate(bias = (p_sim - p_og)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::summarise(bias = mean(bias), 
                                                .by = c(year, region, species_code, sex))) %>% 
    tidytable::filter(iss > 0) -> iss_length
  
  # write results ----
  # input sample size
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln_w_c_egoa.csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag_w_c_egoa.csv")), delim = ",")
  # base age & length pop'n
  vroom::vroom_write(oga, file = here::here("output", region, "base_age_w_c_egoa.csv"), delim = ",")
  vroom::vroom_write(ogl, file = here::here("output", region, "base_length_w_c_egoa.csv"), delim = ",")
  # if desired, write out bootstrapped age & length pop'n and realized sample sizes
  if(isTRUE(save_interm)) {
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_length_w_c_egoa.csv"), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age_w_c_egoa.csv"), delim = ",")
    vroom::vroom_write(.rss_length, here::here("output", region, paste0(save, "_iter_rss_ln_w_c_egoa.csv")), delim = ",")
    vroom::vroom_write(.rss_age, here::here("output", region, paste0(save, "_iter_rss_ag_w_c_egoa.csv")), delim = ",")
  }

}
