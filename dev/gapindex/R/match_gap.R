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



#' wrapper to compare with gap estimates of pop'n at age and length by survey region
#'
#' @param region survey region
#' @param query switch to query or just read data
#' @param reg_stratum region-wide stratum number
#' @param species species set to query gap data
#' @param suvey survey number
#' @param yrs survey start year
#' 
#' @return
#' @export reg_match_gap
#'
#' @examples

reg_match_gap <- function(region = 'goa',
                          query = FALSE,
                          reg_stratum = 99903,
                          species = NULL,
                          survey = 47,
                          yrs = 1990){
  
  # get survey ISS output
  
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
                   age_err = FALSE)
  
  oga <- og$age
  ogl <- og$length
  
  # get gap output
  
  if(isTRUE(query)){
    species = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)
    survey = 47
    
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
    
    vroom::vroom_write(gap_lpop_full, file = here::here("dev", "gapindex", "data", paste0("gap_lpop_full_", region, ".csv")), delim = ",")
    
    gap_lpop_full %>% 
      tidytable::filter(stratum == reg_stratum & length > 0) -> gap_lpop
    
    # agecomp
    apop = readLines(here::here('inst', 'sql', 'gap_products', 'apop_gap.sql'))
    apop = sql_filter(sql_precode = "IN", x = survey, sql_code = apop, flag = '-- insert survey')
    apop = sql_filter(sql_precode = "IN", x = species, sql_code = apop, flag = '-- insert species')
    apop = sql_filter(sql_precode = ">=", x = yrs, sql_code = apop, flag = '-- insert year')
    
    gap_apop_full <- sql_run(conn, apop) %>% 
      dplyr::rename_all(tolower)
    
    vroom::vroom_write(gap_apop_full, file = here::here("dev", "gapindex", "data", paste0("gap_apop_full_", region, ".csv")), delim = ",")
    
    gap_apop_full %>% 
      filter(stratum == reg_stratum & age > 0) -> gap_apop
  } else{
    gap_lpop <- vroom::vroom(here::here("dev", "gapindex", "data", paste0("gap_lpop_full_", region, ".csv"))) %>% 
      tidytable::filter(stratum == reg_stratum & length > 0)
    gap_apop <- vroom::vroom(here::here("dev", "gapindex", "data", paste0("gap_apop_full_", region, ".csv"))) %>% 
      tidytable::filter(stratum == reg_stratum & age > 0)
  }
  
  # match with gap
  match <- match_gap(oga, ogl, gap_apop, gap_lpop)
  
  # Write out results
  vroom::vroom_write(match[[1]], file = here::here("dev", "gapindex", "gap_check_output", paste0("mapd_l_", region, ".csv")), delim = ",")
  vroom::vroom_write(match[[2]], file = here::here("dev", "gapindex", "gap_check_output", paste0("mapd_a_", region, ".csv")), delim = ",")
  vroom::vroom_write(match[[3]], file = here::here("dev", "gapindex", "gap_check_output", paste0("sad_l_", region, ".csv")), delim = ",")
  vroom::vroom_write(match[[4]], file = here::here("dev", "gapindex", "gap_check_output", paste0("sad_a_", region, ".csv")), delim = ",")
  
}







