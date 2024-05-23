#' compare with gap estimates of pop'n at age and length
#'
#' @param oga surveyISS computed pop'n at age
#' @param ogl surveyISS computed pop'n at length
#' @param gap_apop gap computed pop'n at age
#' @param gap_lpop gap computed pop'n at length
#' @param thresh_mapd threshold for desired max abs percent difference between gap and surveyISS pop'n numbers (default is 1 percent difference)
#' @param thresh_sad threshold for desired sum of absolute difference between gap and surveyISS proportions (default is 0.1)
#' 
#' @return
#' @export match_gap
#'
#' @examples

match_gap <- function(oga, 
                      ogl, 
                      gap_apop, 
                      gap_lpop, 
                      thresh_mapd = 1, 
                      thresh_sad = 0.1){

  # Compare between surveyISS and GAP estimates of pop'n at length
  
  gap_lpop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(ogl) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(match_num = abs(abund - abund_gap)/abund_gap * 100) %>% 
    tidytable::summarize(match_mapd_l = max(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_mapd_l = tidytable::case_when(match_mapd_l < thresh_mapd ~ 'Y',
                                                         match_mapd_l > thresh_mapd ~ 'N')) -> gap_match_len_n
  
  # Compare between surveyISS and GAP estimates of proportions at length
  
  gap_lpop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(ogl) %>% 
    tidytable::filter(sex != 3) %>% 
    tidytable::mutate(p_gap = abund_gap / sum(abund_gap, na.rm = TRUE),
                      p = abund / sum(abund, na.rm = TRUE),
                      .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(match_num = abs(p_gap - p)) %>% 
    tidytable::summarize(match_sad_l = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_sad_l = tidytable::case_when(match_sad_l < thresh_sad ~ 'Y',
                                                        match_sad_l > thresh_sad ~ 'N')) -> gap_match_len_p
  
  # Compare between surveyISS and GAP estimates of pop'n at age
  
  gap_apop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(oga) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(match_num = abs(agepop - abund_gap)/abund_gap) %>% 
    tidytable::summarize(match_mapd_a = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_mapd_a = tidytable::case_when(match_mapd_a < thresh_mapd ~ 'Y',
                                                         match_mapd_a > thresh_mapd ~ 'N')) -> gap_match_age_n

  # Compare between surveyISS and GAP estimates of proportions at age
  
  gap_apop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(oga) %>% 
    tidytable::filter(sex != 3 & age > 0) %>% 
    tidytable::mutate(p_gap = abund_gap / sum(abund_gap, na.rm = TRUE),
                      p = agepop / sum(agepop, na.rm = TRUE),
                      .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(match_num = abs(p_gap - p)) %>% 
    tidytable::summarize(match_sad_a = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_sad_a = tidytable::case_when(match_sad_a < thresh_sad ~ 'Y',
                                                        match_sad_a > thresh_sad ~ 'N')) -> gap_match_age_p
  
  list(gap_match_len_n, gap_match_age_n, gap_match_len_p, gap_match_age_p)
}



#' wrapper to compare with gap estimates of pop'n at age and length by survey region (from gap_products on akfin)
#'
#' @param region survey region
#' @param query_srvyISS switch to query or just read data for surveyISS package
#' @param query_gpprod switch to query or just read gap_products data
#' @param reg_stratum region-wide stratum number
#' @param survey survey number
#' @param yrs survey start year
#' @param global fills in missing length bins with global alk (default = FALSE)
#' 
#' @return
#' @export reg_match_gapprod
#'
#' @examples

reg_match_gapprod <- function(region = 'goa',
                              query_svyISS = FALSE,
                              query_gpprod = FALSE,
                              reg_stratum = 99903,
                              survey = 47,
                              yrs = 1990){
  
  # get survey ISS output ----
  
  ## get data ----
  
  data <- surveyISS::query_data_t3(query = query_svyISS)
  
  if(region == 'goa'){
    data <- data$data_goa
  }
  if(region == 'ai'){
    data <- data$data_ai
  }
  if(region == 'ebs'){
    data <- data$data_ebs
  }
  if(region == 'ebs_slope'){
    data <- data$data_ebss
  }
  if(region == 'nbs'){
    data <- data$data_nbs
  }

  # determine species tested
  data$lfreq %>% 
    tidytable::distinct(species_code) -> species
  species <- species$species_code
  
  ## get original age/length pop'n values ----
  og <- surveyISS::srvy_comps(lfreq_data = data$lfreq, 
                              specimen_data = data$specimen, 
                              cpue_data = data$cpue, 
                              strata_data = data$strata,
                              r_t = NULL,
                              yrs = yrs,
                              boot_hauls = FALSE, 
                              boot_lengths = FALSE, 
                              boot_ages = FALSE,
                              al_var = FALSE,
                              al_var_ann = FALSE,
                              age_err = FALSE,
                              use_gapindex = FALSE)
  
  oga_og <- og$age
  ogl_og <- og$length
  
  ## get age/length pop'n values based on gapindex at region level ----
  og_gap <- surveyISS::srvy_comps(lfreq_data = data$lfreq, 
                                  specimen_data = data$specimen, 
                                  cpue_data = data$cpue, 
                                  strata_data = data$strata,
                                  r_t = NULL,
                                  yrs = yrs,
                                  boot_hauls = FALSE, 
                                  boot_lengths = FALSE, 
                                  boot_ages = FALSE,
                                  al_var = FALSE,
                                  al_var_ann = FALSE,
                                  age_err = FALSE,
                                  use_gapindex = TRUE)
  
  oga_gap <- og_gap$age
  ogl_gap <- og_gap$length
  
  # get age/length pop'n values based on gapindex at stratum level
  og_gap_st <- surveyISS::srvy_comps(lfreq_data = data$lfreq, 
                                     specimen_data = data$specimen, 
                                     cpue_data = data$cpue, 
                                     strata_data = data$strata,
                                     r_t = NULL,
                                     yrs = yrs,
                                     boot_hauls = FALSE, 
                                     boot_lengths = FALSE, 
                                     boot_ages = FALSE,
                                     al_var = FALSE,
                                     al_var_ann = FALSE,
                                     age_err = FALSE,
                                     use_gapindex = TRUE,
                                     by_strata = TRUE)
  
  oga_gap_st <- og_gap_st$age
  ogl_gap_st <- og_gap_st$length
  
  # get gap_products output ----
  
  if(isTRUE(query_gpprod)){
    
    # create data folder
    if (!dir.exists(here::here("dev", "gap_check", "data", region))) {
      dir.create(here::here("dev", "gap_check", "data", region), recursive = TRUE)
      }
    
    # get connected to akfin
    db = 'akfin'
    conn = afscdata::connect(db)
    
    # pull akfin_sizecomp table
    dplyr::tbl(conn, dplyr::sql('gap_products.akfin_sizecomp')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::filter(survey_definition_id %in% survey,
                    species_code %in% species,
                    year >= yrs) %>% 
      dplyr::select(survey = survey_definition_id, year, stratum = area_id, species_code, sex, length = length_mm, population_count) %>% 
      dplyr::collect() -> gap_lpop_full
    
    vroom::vroom_write(gap_lpop_full, file = here::here("dev", "gap_check", "data", region, paste0("gap_lpop_full_", region, ".csv")), delim = ",")
    
    gap_lpop_full %>% 
      tidytable::filter(stratum == reg_stratum & length > 0) -> gap_lpop
    
    # pull akfin_agecomp table
    dplyr::tbl(conn, dplyr::sql('gap_products.akfin_agecomp')) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::filter(survey_definition_id %in% survey,
                    species_code %in% species,
                    year >= yrs) %>% 
      dplyr::select(survey = survey_definition_id, year, stratum = area_id, species_code, sex, age, population_count) %>% 
      dplyr::collect() -> gap_apop_full
    
    vroom::vroom_write(gap_apop_full, file = here::here("dev", "gap_check", "data", region, paste0("gap_apop_full_", region, ".csv")), delim = ",")
    
    gap_apop_full %>% 
      tidytable::filter(stratum == reg_stratum & age > 0) -> gap_apop

  } else{
    gap_lpop <- vroom::vroom(here::here("dev", "gap_check", "data", region, paste0("gap_lpop_full_", region, ".csv"))) %>% 
      tidytable::filter(stratum == reg_stratum & length > 0)
    gap_apop <- vroom::vroom(here::here("dev", "gap_check", "data", region, paste0("gap_apop_full_", region, ".csv"))) %>% 
      tidytable::filter(stratum == reg_stratum & age > 0)
  }
  
  # match with gap ----
  og <- match_gap(oga_og, ogl_og, gap_apop, gap_lpop)
  gap <- match_gap(oga_gap, ogl_gap, gap_apop, gap_lpop)
  gap_st <- match_gap(oga_gap_st, ogl_gap_st, gap_apop, gap_lpop)
  
  # get/write results ----
  # length mapd
  og[[1]] %>% 
    tidytable::select(-test_mapd_l, og_mapd = match_mapd_l) %>% 
    tidytable::left_join(gap[[1]] %>% 
                           tidytable::select(-test_mapd_l, gap_mapd = match_mapd_l)) %>% 
    tidytable::left_join(gap_st[[1]] %>% 
                           tidytable::select(-test_mapd_l, gap_st_mapd = match_mapd_l)) -> mapd_l
  # age mapd
  og[[2]] %>% 
    tidytable::select(-test_mapd_a, og_mapd = match_mapd_a) %>% 
    tidytable::left_join(gap[[2]] %>% 
                           tidytable::select(-test_mapd_a, gap_mapd = match_mapd_a)) %>% 
    tidytable::left_join(gap_st[[2]] %>% 
                           tidytable::select(-test_mapd_a, gap_st_mapd = match_mapd_a)) -> mapd_a
  # length sad
  og[[3]] %>% 
    tidytable::select(-test_sad_l, og_sad = match_sad_l) %>% 
    tidytable::left_join(gap[[3]] %>% 
                           tidytable::select(-test_sad_l, gap_sad = match_sad_l)) %>% 
    tidytable::left_join(gap_st[[3]] %>% 
                           tidytable::select(-test_sad_l, gap_st_sad = match_sad_l)) -> sad_l
  # age sad
  og[[4]] %>% 
    tidytable::select(-test_sad_a, og_sad = match_sad_a) %>% 
    tidytable::left_join(gap[[4]] %>% 
                           tidytable::select(-test_sad_a, gap_sad = match_sad_a)) %>% 
    tidytable::left_join(gap_st[[4]] %>% 
                           tidytable::select(-test_sad_a, gap_st_sad = match_sad_a)) -> sad_a
  
  # create storage location
  if(!dir.exists(here::here("dev", "gap_check", 'output', region))){
    dir.create(here::here("dev", "gap_check", 'output', region), recursive = TRUE)
  }
  
  # Write out results
  vroom::vroom_write(mapd_l, file = here::here("dev", "gap_check", "output", region, "gapprod_mapd_len.csv"), delim = ",")
  vroom::vroom_write(mapd_a, file = here::here("dev", "gap_check", "output", region, "gapprod_mapd_age.csv"), delim = ",")
  vroom::vroom_write(sad_l, file = here::here("dev", "gap_check", "output", region, "gapprod_sad_len.csv"), delim = ",")
  vroom::vroom_write(sad_a, file = here::here("dev", "gap_check", "output", region, "gapprod_sad_age.csv"), delim = ",")

}

#' wrapper to compare with gap estimates of pop'n at age and length by survey region from gapindex
#'
#' @param region survey region
#' @param query_srvyISS switch to query or just read data for surveyISS package
#' @param query_gpindx switch to query or just read data for gapindex package
#' @param species species set to query gap data
#' @param suvey survey number
#' @param yrs survey start year
#' @param fill_NA_method method to fill NAs when no size data (either GOA/AI or EBS)
#' @param global fills in missing length bins with global alk (default = FALSE)
#' 
#' @return
#' @export reg_match_gapindex
#'
#' @examples

reg_match_gapindex <- function(region = 'goa',
                               query_svyISS = FALSE,
                               query_gpindx = FALSE,
                               species = NULL,
                               survey = 47,
                               yrs = 1990,
                               fill_NA_method = NULL,
                               global = FALSE){
  
  # get survey ISS output ----
  
  ## get data ----
  
  data <- surveyISS::query_data_t3(query = query_svyISS)
  
  if(region == 'goa'){
    data <- data$data_goa
  }
  if(region == 'ai'){
    data <- data$data_ai
  }
  if(region == 'ebs'){
    data <- data$data_ebs
  }
  if(region == 'ebs_slope'){
    data <- data$data_ebss
  }
  if(region == 'nbs'){
    data <- data$data_nbs
  }
  
  # filter data to species tested
  data$lfreq %>% 
    tidytable::filter(species_code %in% species) -> lfreq_data
  data$specimen %>% 
    tidytable::filter(species_code %in% species) -> specimen_data
  data$cpue %>% 
    tidytable::filter(species_code %in% species) -> cpue_data
  
  ## get original age/length pop'n values ----
  og <- surveyISS::srvy_comps(lfreq_data, 
                              specimen_data, 
                              cpue_data, 
                              strata_data = data$strata,
                              r_t = NULL,
                              yrs = yrs,
                              boot_hauls = FALSE, 
                              boot_lengths = FALSE, 
                              boot_ages = FALSE,
                              al_var = FALSE,
                              al_var_ann = FALSE,
                              age_err = FALSE,
                              use_gapindex = FALSE)
  
  oga_og <- og$age
  ogl_og <- og$length
  
  ## get age/length pop'n values based on gapindex at region level ----
  og_gap <- surveyISS::srvy_comps(lfreq_data, 
                                  specimen_data, 
                                  cpue_data,
                                  strata_data = data$strata,
                                  r_t = NULL,
                                  yrs = yrs,
                                  boot_hauls = FALSE, 
                                  boot_lengths = FALSE, 
                                  boot_ages = FALSE,
                                  al_var = FALSE,
                                  al_var_ann = FALSE,
                                  age_err = FALSE,
                                  use_gapindex = TRUE)
  
  oga_gap <- og_gap$age
  ogl_gap <- og_gap$length
  
  # get age/length pop'n values based on gapindex at stratum level
  og_gap_st <- surveyISS::srvy_comps(lfreq_data, 
                                     specimen_data, 
                                     cpue_data, 
                                     strata_data = data$strata,
                                     r_t = NULL,
                                     yrs = yrs,
                                     boot_hauls = FALSE, 
                                     boot_lengths = FALSE, 
                                     boot_ages = FALSE,
                                     al_var = FALSE,
                                     al_var_ann = FALSE,
                                     age_err = FALSE,
                                     use_gapindex = TRUE,
                                     by_strata = TRUE)
  
  oga_gap_st <- og_gap_st$age
  ogl_gap_st <- og_gap_st$length
  
  # get gapindex output ----
  
  ## get data ----
  if(isTRUE(query_gpindx)){
    year_set = seq(yrs, as.numeric(format(Sys.Date(), '%Y')))
    survey_set = toupper(region)
    spp_codes = species
    
    gapdata <- gapindex::get_data(year_set = year_set,
                                  survey_set = survey_set,
                                  spp_codes = spp_codes,
                                  pull_lengths = TRUE)
    
    saveRDS(gapdata, here::here('dev', 'gap_check', 'data', region, 'gapdata.RDS'))
  } else{
    gapdata <- readRDS(here::here('dev', 'gap_check', 'data', region, 'gapdata.RDS'))
  }
  
  # subset data to species desired to be tested
  cruise <- gapdata$cruise
  haul <- gapdata$haul
  gapdata$catch %>% 
    tidytable::filter(SPECIES_CODE %in% species) -> gapdata$catch
  gapdata$size %>% 
    tidytable::filter(SPECIES_CODE %in% species) -> gapdata$size
  gapdata$specimen %>% 
    tidytable::filter(SPECIES_CODE %in% species) -> gapdata$specimen
  gapdata$species %>% 
    tidytable::filter(SPECIES_CODE %in% species) -> gapdata$species
  
  ## run gapindex ----
  # get cpue
  cpue <- gapindex::calc_cpue(gapdata)
  
  # get stratum pop'n
  racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                          cpue)
  
  # get pop'n at length
  gap_lc <- gapindex::calc_sizecomp_stratum(gapdata,
                                            cpue,
                                            racebase_stratum_popn,
                                            fill_NA_method = fill_NA_method) 
  
  # get age-length key
  alk <- gapindex::calc_alk(gapdata,
                            unsex = "all",
                            global = global)
  
  # get stratum-level age pop'n
  gap_age_comp_st <- gapindex::calc_agecomp_stratum(gapdata,
                                                    alk,
                                                    gap_lc)
  
  # get regional level age pop'n
  gap_ac <- gapindex::calc_agecomp_region(gapdata,
                                          gap_age_comp_st)
  
  # filter for ebs to only have standard
  if(region == 'ebs'){
    gap_ac %>% 
      tidytable::filter(AREA_ID == 99901) -> gap_ac
  }

  # rename and summarise
  gap_lc %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(length_mm > 0) %>% 
    tidytable::summarise(population_count = sum(population_count), .by = c(year, species_code, sex, length_mm)) %>% 
    tidytable::mutate(stratum = 1) %>% 
    tidytable::rename(length = 'length_mm') -> gap_lpop
  
  gap_ac %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(age > 0) %>% 
    tidytable::summarise(population_count = sum(population_count), .by = c(year, species_code, sex, age)) %>% 
    tidytable::mutate(stratum = 1) -> gap_apop
  
  # match with gap ----
  og <- match_gap(oga_og, ogl_og, gap_apop, gap_lpop)
  gap <- match_gap(oga_gap, ogl_gap, gap_apop, gap_lpop)
  gap_st <- match_gap(oga_gap_st, ogl_gap_st, gap_apop, gap_lpop)
  
  # get/write results ----
  # length mapd
  og[[1]] %>% 
    tidytable::select(-test_mapd_l, og_mapd = match_mapd_l) %>% 
    tidytable::left_join(gap[[1]] %>% 
                           tidytable::select(-test_mapd_l, gap_mapd = match_mapd_l)) %>% 
    tidytable::left_join(gap_st[[1]] %>% 
                           tidytable::select(-test_mapd_l, gap_st_mapd = match_mapd_l)) -> mapd_l
  # age mapd
  og[[2]] %>% 
    tidytable::select(-test_mapd_a, og_mapd = match_mapd_a) %>% 
    tidytable::left_join(gap[[2]] %>% 
                           tidytable::select(-test_mapd_a, gap_mapd = match_mapd_a)) %>% 
    tidytable::left_join(gap_st[[2]] %>% 
                           tidytable::select(-test_mapd_a, gap_st_mapd = match_mapd_a)) -> mapd_a
  # length sad
  og[[3]] %>% 
    tidytable::select(-test_sad_l, og_sad = match_sad_l) %>% 
    tidytable::left_join(gap[[3]] %>% 
                           tidytable::select(-test_sad_l, gap_sad = match_sad_l)) %>% 
    tidytable::left_join(gap_st[[3]] %>% 
                           tidytable::select(-test_sad_l, gap_st_sad = match_sad_l)) -> sad_l
  # age sad
  og[[4]] %>% 
    tidytable::select(-test_sad_a, og_sad = match_sad_a) %>% 
    tidytable::left_join(gap[[4]] %>% 
                           tidytable::select(-test_sad_a, gap_sad = match_sad_a)) %>% 
    tidytable::left_join(gap_st[[4]] %>% 
                           tidytable::select(-test_sad_a, gap_st_sad = match_sad_a)) -> sad_a
  
  # create storage location
  region = tolower(region)
  if(!dir.exists(here::here("dev", "gap_check", 'output', region))){
    dir.create(here::here("dev", "gap_check", 'output', region), recursive = TRUE)
  }
  
  # Write out results
  vroom::vroom_write(mapd_l, file = here::here("dev", "gap_check", "output", region, "gapindx_mapd_len.csv"), delim = ",")
  vroom::vroom_write(mapd_a, file = here::here("dev", "gap_check", "output", region, "gapindx_mapd_age.csv"), delim = ",")
  vroom::vroom_write(sad_l, file = here::here("dev", "gap_check", "output", region, "gapindx_sad_len.csv"), delim = ",")
  vroom::vroom_write(sad_a, file = here::here("dev", "gap_check", "output", region, "gapindx_sad_age.csv"), delim = ",")

}


