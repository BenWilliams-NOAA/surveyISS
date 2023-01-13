#' replicate survey input sample size function
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
#'
#' @return
#' @export srvy_iss
#'
#' @examples
srvy_iss <- function(iters = 1, lfreq_data, specimen_data, cpue_data, strata_data, yrs = NULL, 
                     boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE, 
                     write_comp = FALSE, region = NULL, save_orig = FALSE, save = FALSE, save_ess = FALSE){
    
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
  
  # get original age/length pop'n values (these should match GAP output)
  og <- srvy_comps(lfreq_data = lfreq_data, specimen_data = specimen_data, 
                   cpue_data = cpue_data, strata_data = strata_data, yrs = yrs, 
                   boot_hauls = FALSE, boot_lengths = FALSE, boot_ages = FALSE)
  oga <- og$age
  ogl <- og$length
  
  # if desired, write original age/length pop'n estimates
  if(isTRUE(save_orig)){
    vroom::vroom_write(oga, file = here::here("output", region, "orig_age.csv"), delim = ",")
    vroom::vroom_write(ogl, file = here::here("output", region, "orig_length.csv"), delim = ",")
  }
  
  # run resampling iterations
  rr <- purrr::rerun(iters, srvy_comps(lfreq_data = lfreq_data, 
                                       specimen_data = specimen_data, 
                                       cpue_data = cpue_data, 
                                       strata_data = strata_data, 
                                       yrs = yrs, 
                                       boot_hauls = boot_hauls, 
                                       boot_lengths = boot_lengths, 
                                       boot_ages = boot_ages))
  
  r_age <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$age
  r_length <- do.call(mapply, c(list, rr, SIMPLIFY = FALSE))$length
  
  # if desired, write out resampled comp data
  if(isTRUE(save)) {
    r_age %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
    vroom::vroom_write(here::here("output", region, "resampled_age.csv"), delim = ",")
    r_length %>%
      tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") %>% 
      vroom::vroom_write(here::here("output", region, "resampled_size.csv"), delim = ",")
  }
  
  # compute effective sample size of bootstrapped age/length
  r_age %>%
    tidytable::map.(., ~ess_age(sim_data = .x, og_data = oga)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_age
  r_length %>%
    tidytable::map.(., ~ess_size(sim_data = .x, og_data = ogl)) %>%
    tidytable::map_df.(., ~as.data.frame(.x), .id = "sim") -> ess_size
  
  # if desired, Write iterated effective sample size results
  if(isTRUE(save_ess)) {
    vroom::vroom_write(ess_age, 
                       here::here("output", region, "iter_ess_ag.csv"), 
                       delim = ",")
    vroom::vroom_write(ess_size, 
                       here::here("output", region, "iter_ess_sz.csv"), 
                       delim = ",")
  }
  
  # compute harmonic mean of iterated effective sample size, which is the input sample size
  ess_size %>% 
    mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
            .by = c(year, species_code, ess)) %>% 
    group_by(year, species_code, ess) %>%
    dplyr::distinct(iss) %>% 
    tidytable::drop_na.() %>% 
    tidytable::filter.(iss > 0) -> iss_size
  
  ess_age %>% 
    mutate.(iss = length(unique(sim))/sum(value^(-1),na.rm=TRUE), 
            .by = c(year, species_code, ess)) %>% 
    group_by(year, species_code, ess) %>%
    dplyr::distinct(iss) %>% 
    tidytable::drop_na.() %>% 
    tidytable::filter.(iss > 0) -> iss_age
  
  # write input sample size results
  vroom::vroom_write(iss_age, 
                     here::here("output", region, "iss_ag.csv"), 
                     delim = ",")
  vroom::vroom_write(iss_size, 
                     here::here("output", region, "iss_sz.csv"), 
                     delim = ",")
  
}