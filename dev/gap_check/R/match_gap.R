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
#' @param query switch to query or just read data
#' @param reg_stratum region-wide stratum number
#' @param species species set to query gap data
#' @param suvey survey number
#' @param yrs survey start year
#' 
#' @return
#' @export reg_match_gapprod
#'
#' @examples

reg_match_gapprod <- function(region = 'goa',
                              query = FALSE,
                              reg_stratum = 99903,
                              species = NULL,
                              survey = 47,
                              yrs = 1990){
  
  # get survey ISS output ----
  
  cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv'))))
  lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv'))))
  strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
  specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv'))))

  # get original age/length pop'n values
  og <- srvy_comps(lfreq_data = lfreq, 
                   specimen_data = specimen, 
                   cpue_data = cpue, 
                   strata_data = strata,
                   r_t = NULL,
                   yrs = yrs, 
                   bin = 1,
                   boot_hauls = FALSE, 
                   boot_lengths = FALSE, 
                   boot_ages = FALSE,
                   al_var = FALSE,
                   al_var_ann = FALSE,
                   age_err = FALSE,
                   use_gapindex = FALSE,
                   by_strata = FALSE)
  
  oga_og <- og$age
  ogl_og <- og$length
  
  # get age/length pop'n values based on gapindex at region level
  og_gap <- srvy_comps(lfreq_data = lfreq, 
                       specimen_data = specimen, 
                       cpue_data = cpue, 
                       strata_data = strata,
                       r_t = NULL,
                       yrs = yrs, 
                       bin = 1,
                       boot_hauls = FALSE, 
                       boot_lengths = FALSE, 
                       boot_ages = FALSE,
                       al_var = FALSE,
                       al_var_ann = FALSE,
                       age_err = FALSE,
                       use_gapindex = TRUE,
                       by_strata = FALSE)
  
  oga_gap <- og_gap$age
  ogl_gap <- og_gap$length
  
  # get age/length pop'n values based on gapindex at stratum level
  og_gap_st <- srvy_comps(lfreq_data = lfreq, 
                          specimen_data = specimen, 
                          cpue_data = cpue, 
                          strata_data = strata,
                          r_t = NULL,
                          yrs = yrs, 
                          bin = 1,
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
  
  if(isTRUE(query)){
    
    # get database username/password (ai = 52, goa = 47, ebs = 98, nbs = 143, ebs slope = 78)
    db <- vroom::vroom(here::here("database_specs.csv"))
    username = db$username[db$database == "AKFIN"]
    password = db$password[db$database == "AKFIN"]
    database = 'akfin'
    
    # connect to database
    conn = DBI::dbConnect(odbc::odbc(), database,
                          UID = username, PWD = password)
    
    # sizecomp
    lpop = readLines(here::here('inst', 'sql', 'gap_products', 'lpop_gap.sql'))
    lpop = sql_filter(sql_precode = "IN", x = survey, sql_code = lpop, flag = '-- insert survey')
    lpop = sql_filter(sql_precode = "IN", x = species, sql_code = lpop, flag = '-- insert species')
    lpop = sql_filter(sql_precode = ">=", x = yrs, sql_code = lpop, flag = '-- insert year')
    
    gap_lpop_full <- sql_run(conn, lpop) %>% 
      dplyr::rename_all(tolower)
    
    vroom::vroom_write(gap_lpop_full, file = here::here("dev", "gap_check", "data", paste0("gap_lpop_full_", region, ".csv")), delim = ",")
    
    gap_lpop_full %>% 
      tidytable::filter(stratum == reg_stratum & length > 0) -> gap_lpop
    
    # agecomp
    apop = readLines(here::here('inst', 'sql', 'gap_products', 'apop_gap.sql'))
    apop = sql_filter(sql_precode = "IN", x = survey, sql_code = apop, flag = '-- insert survey')
    apop = sql_filter(sql_precode = "IN", x = species, sql_code = apop, flag = '-- insert species')
    apop = sql_filter(sql_precode = ">=", x = yrs, sql_code = apop, flag = '-- insert year')
    
    gap_apop_full <- sql_run(conn, apop) %>% 
      dplyr::rename_all(tolower)
    
    vroom::vroom_write(gap_apop_full, file = here::here("dev", "gap_check", "data", paste0("gap_apop_full_", region, ".csv")), delim = ",")
    
    gap_apop_full %>% 
      filter(stratum == reg_stratum & age > 0) -> gap_apop
  } else{
    gap_lpop <- vroom::vroom(here::here("dev", "gap_check", "data", paste0("gap_lpop_full_", region, ".csv"))) %>% 
      tidytable::filter(stratum == reg_stratum & length > 0)
    gap_apop <- vroom::vroom(here::here("dev", "gap_check", "data", paste0("gap_apop_full_", region, ".csv"))) %>% 
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
  
  # Write out results
  vroom::vroom_write(mapd_l, file = here::here("dev", "gap_check", "output", paste0("gapprod_mapd_l_", region, ".csv")), delim = ",")
  vroom::vroom_write(mapd_a, file = here::here("dev", "gap_check", "output", paste0("gapprod_mapd_a_", region, ".csv")), delim = ",")
  vroom::vroom_write(sad_l, file = here::here("dev", "gap_check", "output", paste0("gapprod_sad_l_", region, ".csv")), delim = ",")
  vroom::vroom_write(sad_a, file = here::here("dev", "gap_check", "output", paste0("gapprod_sad_a_", region, ".csv")), delim = ",")

}




#' wrapper to compare with gap estimates of pop'n at age and length by survey region from gapindex
#'
#' @param region survey region
#' @param query switch to query or just read data
#' @param species species set to query gap data
#' @param suvey survey number
#' @param yrs survey start year
#' @param fill_NA_method method to fill NAs when no size data (either GOA/AI or EBS)
#' 
#' @return
#' @export reg_match_gapindex
#'
#' @examples

reg_match_gapindex <- function(region = 'goa',
                               query = FALSE,
                               species = NULL,
                               survey = 47,
                               yrs = 1990,
                               fill_NA_method = NULL){
  
  # get survey ISS output ----
  
  cpue <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('cpue_', region, '.csv')))) %>% 
    tidytable::filter(species_code %in% species)
  lfreq <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('lfreq_', region, '.csv')))) %>% 
    tidytable::filter(species_code %in% species)
  strata <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('strata_', region, '.csv'))))
  specimen <- tidytable::as_tidytable(vroom::vroom(here::here('data', paste0('specimen_', region, '.csv')))) %>% 
    tidytable::filter(species_code %in% species)
  
  # get original age/length pop'n values
  og <- srvy_comps(lfreq_data = lfreq, 
                   specimen_data = specimen, 
                   cpue_data = cpue, 
                   strata_data = strata,
                   r_t = NULL,
                   yrs = yrs, 
                   bin = 1,
                   boot_hauls = FALSE, 
                   boot_lengths = FALSE, 
                   boot_ages = FALSE,
                   al_var = FALSE,
                   al_var_ann = FALSE,
                   age_err = FALSE,
                   use_gapindex = FALSE,
                   by_strata = FALSE)
  
  oga_og <- og$age
  ogl_og <- og$length
  
  # get age/length pop'n values based on gapindex at region level
  og_gap <- srvy_comps(lfreq_data = lfreq, 
                       specimen_data = specimen, 
                       cpue_data = cpue, 
                       strata_data = strata,
                       r_t = NULL,
                       yrs = yrs, 
                       bin = 1,
                       boot_hauls = FALSE, 
                       boot_lengths = FALSE, 
                       boot_ages = FALSE,
                       al_var = FALSE,
                       al_var_ann = FALSE,
                       age_err = FALSE,
                       use_gapindex = TRUE,
                       by_strata = FALSE)
  
  oga_gap <- og_gap$age
  ogl_gap <- og_gap$length
  
  # get age/length pop'n values based on gapindex at stratum level
  og_gap_st <- srvy_comps(lfreq_data = lfreq, 
                          specimen_data = specimen, 
                          cpue_data = cpue, 
                          strata_data = strata,
                          r_t = NULL,
                          yrs = yrs, 
                          bin = 1,
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
  if(isTRUE(query)){
    year_set = seq(yrs, as.numeric(format(Sys.Date(), '%Y')))
    survey_set = toupper(region)
    spp_codes = species
    
    gapdata <- gapindex::get_data(year_set = year_set,
                                  survey_set = survey_set,
                                  spp_codes = spp_codes,
                                  pull_lengths = TRUE)
    
    saveRDS(gapdata, here::here('dev', 'gap_check', 'data', paste0('gapdata_', region, '.RDS')))
  } else{
    gapdata <- readRDS(here::here('dev', 'gap_check', 'data', paste0('gapdata_', region, '.RDS')))
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
                            unsex = "all")
  
  # get stratum-level age pop'n
  gap_age_comp_st <- gapindex::calc_agecomp_stratum(gapdata,
                                                    alk,
                                                    gap_lc)
  
  # get regional level age pop'n
  gap_ac <- gapindex::calc_agecomp_region(gapdata,
                                          gap_age_comp_st)


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
  
  # Write out results
  vroom::vroom_write(mapd_l, file = here::here("dev", "gap_check", "output", paste0("gapindx_mapd_l_", region, ".csv")), delim = ",")
  vroom::vroom_write(mapd_a, file = here::here("dev", "gap_check", "output", paste0("gapindx_mapd_a_", region, ".csv")), delim = ",")
  vroom::vroom_write(sad_l, file = here::here("dev", "gap_check", "output", paste0("gapindx_sad_l_", region, ".csv")), delim = ",")
  vroom::vroom_write(sad_a, file = here::here("dev", "gap_check", "output", paste0("gapindx_sad_a_", region, ".csv")), delim = ",")
  
}

