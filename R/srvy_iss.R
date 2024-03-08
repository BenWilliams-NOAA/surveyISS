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
#' @param srvy_type only for bering sea survey, denotes whether it's the shelf or slope survey (default = NULL)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss
#'
#' @examples

srvy_iss <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, r_t, yrs = NULL, bin = 1, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, al_var = FALSE, al_var_ann = FALSE, age_err = FALSE,
                     region = NULL, save_interm = FALSE, srvy_type = NULL, save){
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here('output', region))){
    dir.create(here::here('output', region), recursive = TRUE)
  }
  # create storage location
  if(!dir.exists(here::here('output', region, 'add_err')) & (isTRUE(al_var) | isTRUE(age_err))){
    dir.create(here::here('output', region, 'add_err'), recursive = TRUE)
  }
  
  # restructure data (and add sex = 0 for sex-combined (total) comp calculations)
  lfreq_data <- tidytable::as_tidytable(lfreq_data %>% 
                                          tidytable::bind_rows(lfreq_data %>% 
                                                                 tidytable::mutate(sex = 0))) 
  specimen_data <- tidytable::as_tidytable(specimen_data %>% 
                                             tidytable::bind_rows(specimen_data %>% 
                                                                    tidytable::mutate(sex = 0))) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 
  strata_data <- tidytable::as_tidytable(strata_data) 
  
  # get original age/length pop'n values
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
                   age_err = FALSE)
  oga <- og$age %>% 
    select(-type)
  ogl <- og$length %>% 
    select(-type)
  
  # run resampling iterations
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
  
  # if desired, write out intermediate results
  if(isTRUE(save_interm) & region != 'bs') {
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length.csv"), delim = ",")
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size.csv"), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_age.csv"), delim = ",")
  }
  if(isTRUE(save_interm) & region == 'bs') {
    vroom::vroom_write(oga, file = here::here("output", region, paste0("orig_age_", srvy_type, ".csv")), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, paste0("orig_length_", srvy_type, ".csv")), delim = ",")
    r_length %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0("resampled_size_", srvy_type, ".csv")), delim = ",")
    r_age %>%
      tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, paste0("resampled_age_", srvy_type, ".csv")), delim = ",")
  }
  
  # compute effective sample size of bootstrapped age/length
  r_age %>%
    tidytable::map(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .ess_age
  r_length %>%
    tidytable::map(., ~ess_length(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::mutate(sex_desc = case_when(sex == 0 ~ 'total_pre',
                                           sex == 1 ~ 'male',
                                           sex == 2 ~ 'female',
                                           sex == 4 ~ 'total_post')) -> .ess_length

  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)
  .ess_age %>% 
    tidytable::summarise(iss = psych::harmonic.mean(ess, na.rm = TRUE),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::filter(iss > 0) -> iss_age

  .ess_length %>% 
    tidytable::summarise(iss = psych::harmonic.mean(ess, na.rm=T),
                         .by = c(year, species_code, sex, sex_desc)) %>% 
    tidytable::filter(iss > 0) -> iss_length

  # write input/effective sample size results
  vroom::vroom_write(ess_length, here::here("output", region, paste0(save, "_iter_ess_ln.csv")), delim = ",")
  vroom::vroom_write(ess_age, here::here("output", region, paste0(save, "_iter_ess_ag.csv")), delim = ",")
  vroom::vroom_write(iss_length, here::here("output", region, paste0(save, "_iss_ln.csv")), delim = ",")    
  vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag.csv")), delim = ",")

}