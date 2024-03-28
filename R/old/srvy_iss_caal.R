#' replicate survey input sample size function for production run
#'
#' @param iters number of iterations (500 recommended)
#' @param specimen_data input dataframe
#' @param cpue_data input dataframe
#' @param r_t input dataframe
#' @param yrs any year filter >= (default = NULL)
#' @param boot_hauls resample hauls w/replacement (default = FALSE)
#' @param boot_ages resample ages w/replacement (default = FALSE)
#' @param sex_spec determine whether to do sex specific or total comps (default = TRUE)
#' @param al_var include age-length variability (default = FALSE)
#' @param al_var_ann resample age-length annually or pooled across years
#' @param age_err include ageing error (default = FALSE)
#' @param region region will create a folder and place results in said folder
#' @param save_interm save the intermediate results: original comps, resampled comps (default = FALSE)
#' @param match_orig match the computed values to gap output (default = FALSE)
#' @param srvy_type only for bering sea survey, denotes whether it's the shelf or slope survey (default = NULL)
#' @param save name to save output
#'
#' @return
#' @export srvy_iss_caal
#'
#' @examples

srvy_iss_caal <- function(iters = 1, specimen_data, cpue_data, r_t, yrs = NULL, 
                          boot_hauls = FALSE, boot_ages = FALSE, sex_spec = TRUE, al_var = FALSE, al_var_ann = FALSE, age_err = FALSE,
                          region = NULL, save_interm = FALSE, match_orig = FALSE, srvy_type = NULL, save){
  
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
  specimen_data <- tidytable::as_tidytable(specimen_data) 
  cpue_data <- tidytable::as_tidytable(cpue_data) 

  # get original age/length pop'n values
  og <- srvy_comps_caal(specimen_data = specimen_data, 
                        cpue_data = cpue_data, 
                        r_t = r_t,
                        yrs = yrs, 
                        boot_hauls = FALSE, 
                        boot_ages = FALSE,
                        sex_spec = sex_spec,
                        al_var = FALSE,
                        age_err = FALSE)
  oga <- og$age %>% 
    select(-type)

  # run resampling iterations
  rr <- purrr::map(1:iters, ~ srvy_comps_caal(specimen_data = specimen_data, 
                                              cpue_data = cpue_data, 
                                              r_t = r_t,
                                              yrs = yrs, 
                                              boot_hauls = boot_hauls, 
                                              boot_ages = boot_ages,
                                              sex_spec = sex_spec,
                                              al_var = al_var,
                                              al_var_ann = al_var_ann,
                                              age_err = age_err))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age

  # compute effective sample size of bootstrapped caal
  r_age %>%
    tidytable::map(., ~ess_caal(sim_data = .x, og_data = oga, sex_spec = sex_spec)) %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::rename(comp_type = ess) %>% 
    tidytable::mutate(comp_type = tidytable::case_when(comp_type == 'ess_f' ~ 'female',
                                                        comp_type == 'ess_m' ~ 'male',
                                                        comp_type == 'ess_t' ~ 'total')) -> ess_age1

  # compute harmonic mean of iterated effective sample size, which is the input sample size (iss)

  ess_age1 %>% 
    tidytable::summarise(iss = psych::harmonic.mean(value, na.rm = TRUE),
                         .by = c(year, species_code, comp_type, type, length)) %>% 
    tidytable::filter(iss > 0) %>% 
    tidytable::pivot_wider(names_from = type, values_from = iss) %>% 
    tidytable::drop_na() -> iss_age

  ess_age1 %>%
    tidytable::pivot_wider(names_from = type, values_from = value) -> ess_age1
  
  # write input sample size results
  vroom::vroom_write(iss_age, here::here("output", region, 'add_err', paste0(save, "_iss_ag.csv")), delim = ",")

  # if(region != 'bs' & save == 'prod'){
  #   vroom::vroom_write(ess_size, here::here("output", region, paste0(save, "_iter_ess_sz.csv")), delim = ",")
  #   vroom::vroom_write(ess_age, here::here("output", region, paste0(save, "_iter_ess_ag.csv")), delim = ",")
  #   vroom::vroom_write(iss_size, here::here("output", region, paste0(save, "_iss_sz.csv")), delim = ",")    
  #   vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag.csv")), delim = ",")
  # } else if(region != 'bs' & save != 'prod'){
  #   vroom::vroom_write(ess_size, here::here("output", region, 'add_err', paste0(save, "_iter_ess_sz.csv")), delim = ",")
  #   vroom::vroom_write(ess_age, here::here("output", region, 'add_err', paste0(save, "_iter_ess_ag.csv")), delim = ",")
  #   vroom::vroom_write(iss_size, here::here("output", region, 'add_err', paste0(save, "_iss_sz.csv")), delim = ",")    
  #   vroom::vroom_write(iss_age, here::here("output", region, 'add_err', paste0(save, "_iss_ag.csv")), delim = ",")
  # }
  # if(region == 'bs' & save == 'prod'){
  #   vroom::vroom_write(ess_size, here::here("output", region, paste0(save, "_iter_ess_sz_", srvy_type, ".csv")), delim = ",")
  #   vroom::vroom_write(ess_age, here::here("output", region, paste0(save, "_iter_ess_ag_", srvy_type, ".csv")), delim = ",")
  #   vroom::vroom_write(iss_size, here::here("output", region, paste0(save, "_iss_sz_", srvy_type, ".csv")), delim = ",") 
  #   vroom::vroom_write(iss_age, here::here("output", region, paste0(save, "_iss_ag_", srvy_type, ".csv")), delim = ",")
  # } else if(region == 'bs' & save != 'prod'){
  #   vroom::vroom_write(ess_size, here::here("output", region, 'add_err', paste0(save, "_iter_ess_sz_", srvy_type, ".csv")), delim = ",")
  #   vroom::vroom_write(ess_age, here::here("output", region, 'add_err', paste0(save, "_iter_ess_ag_", srvy_type, ".csv")), delim = ",")
  #   vroom::vroom_write(iss_size, here::here("output", region, 'add_err', paste0(save, "_iss_sz_", srvy_type, ".csv")), delim = ",") 
  #   vroom::vroom_write(iss_age, here::here("output", region, 'add_err', paste0(save, "_iss_ag_", srvy_type, ".csv")), delim = ",")
  # }
}