#' query gap_products
#'
#' @param survey survey number for gap_products survey_definition_id (ai = 52, goa = 47, ebs = 98, nbs = 143, ebs slope = 78
#' @param region region description for output data file
#' @param species species_codes e.g., c(10110, 21740)
#' @param yrs minimum year to consider (default: NULL)
#' @param database database from which to pull data
#' @param username database username
#' @param password database password
#'
#' @return
#' @export query_data
#'
#' @examples
#'            
query_data_gap <- function(survey, region, species, yrs = NULL, database, username, password) {
  
  # for development
  library(purrr)
  library(tidyverse)
  library(tidytable)
  library(psych)
  library(vroom)
  library(here)
  
  source_files <- list.files(here::here("R"), "*.R$")
  map(here::here("R", source_files), source)
  
  akfin_user = 'phulson'
  akfin_pwd = '$blwins1'
  
  afsc_user = 'hulsonp'
  afsc_pwd = 'Bri3+Fin2+Liam1'
  
  # connect to akfin
  akfin = DBI::dbConnect(odbc::odbc(), "akfin",
                        UID = akfin_user, PWD = akfin_pwd)
  # connect to afsc
  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                        UID = afsc_user, PWD = afsc_pwd)
  
  
  database = "akfin"
  username = 'phulson'
  password = '$blwins1'
  
  
  species = 21720
  yrs = 2016
  survey = 47


  # apop = sql_read('apop_gap.sql')
  apop = readLines(here::here('inst', 'sql', 'apop_gap.sql'))
  apop = sql_filter(sql_precode = "IN", x = survey, sql_code = apop, flag = '-- insert survey')
  apop = sql_filter(sql_precode = "IN", x = species, sql_code = apop, flag = '-- insert species')
  apop = sql_filter(sql_precode = ">=", x = yrs, sql_code = apop, flag = '-- insert year')
  
  sql_run(conn, apop) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(here::here('data', paste0("apop_", tolower(region), ".csv")), 
                       delim = ',')
  
  
  
  
  
  # survey desc
  # 52 - AI
  # 98 - EBS
  # 143 - NBS
  # 47 - GOA
  # 78 - EBS slope
  
  
  pcod <- sql_run(akfin, lfreq)
  
  
  # get goa and ai data
  
  survey = 98
  
  # st = sql_read('strata_gap.sql')
  st = readLines(here::here('inst', 'sql', 'strata_gap.sql'))
  st = sql_filter(sql_precode = "IN", x = survey, sql_code = st, flag = '-- insert survey')
  
  sql_run(conn, st) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(design_year == max(design_year), .by = stratum) %>% 
    tidytable::arrange(stratum) %>% 
    vroom::vroom_write(here::here('data', paste0("strata_", tolower(region), ".csv")), 
                       delim = ',')
  
  
  strata1 <- c("SELECT ", "*", "FROM gap_products.akfin_area", "WHERE ", "gap_products.akfin_area.survey_definition_id", "IN ('52')")
  
  sql_run(conn, strata1)
  
  
  survey = 98
  
  # lpop = sql_read('lpop_gap.sql')
  lpop = readLines(here::here('inst', 'sql', 'lpop_gap.sql'))
  lpop = sql_filter(sql_precode = "IN", x = survey, sql_code = lpop, flag = '-- insert survey')
  lpop = sql_filter(sql_precode = "IN", x = species, sql_code = lpop, flag = '-- insert species')
  lpop = sql_filter(sql_precode = ">=", x = yrs, sql_code = lpop, flag = '-- insert year')

  tt <- sql_run(conn, lpop)
  
  tt %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::summarize(popn = sum(population_count), .by = c(year,stratum)) %>% 
    print(n = 95)
  
  
  
  
  
  pop %>% 
    # dplyr::rename_all(tolower) %>%
    vroom::vroom_write(here::here('data', paste0("lfreq_gap_test.csv")), 
                       delim = ',')
  
  test <- vroom::vroom(here::here('data', 'lfreq_gap_test.csv'), delim = ",")
  
  pop %>% 
    distinct(SURVEY_DEFINITION_ID, SURVEY_NAME)
  
  test %>% 
    dplyr::rename_all(tolower) %>% 
    distinct(year)
  
  
  
  
  
  
  
  
  
  
  
  
  # create folder
  if (!dir.exists("data")) {dir.create("data")}

  # connect to database
  conn = DBI::dbConnect(odbc::odbc(), database,
                         UID = username, PWD = password)
  
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # length frequency data ----
  # lfreq = sql_read('length_freq_gap.sql')
  lfreq = readLines(here::here('inst', 'sql', 'length_freq_gap.sql'))
  lfreq = sql_filter(sql_precode = "IN", x = survey, sql_code = lfreq, flag = '-- insert survey')
  lfreq = sql_filter(sql_precode = "IN", x = species, sql_code = lfreq, flag = '-- insert species')
  lfreq = sql_filter(sql_precode = ">=", x = yrs, sql_code = lfreq, flag = '-- insert year')
  
  sql_run(conn, lfreq) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(here::here('data', paste0("lfreq_", tolower(region), ".csv")), 
                       delim = ',')
  
  # specimen data ----
  # sp = sql_read('specimen_gap.sql')
  sp = readLines(here::here('inst', 'sql', 'specimen_gap.sql'))
  sp = sql_filter(sql_precode = "IN", x = survey, sql_code = sp, flag = '-- insert survey')
  sp = sql_filter(sql_precode = "IN", x = species, sql_code = sp, flag = '-- insert species')
  sp = sql_filter(sql_precode = ">=", x = yrs, sql_code = sp, flag = '-- insert year')
  
  sql_run(conn, sp) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(., 
                       here::here('data', paste0("specimen_", tolower(region), ".csv")), 
                       delim = ',')
  
  # cpue data ----
  # cp = sql_read('cpue_gap.sql')
  cp = readLines(here::here('inst', 'sql', 'cpue_gap.sql'))
  cp = sql_filter(sql_precode = "IN", x = survey, sql_code = cp, flag = '-- insert survey')
  cp = sql_filter(sql_precode = "IN", x = species, sql_code = cp, flag = '-- insert species')
  cp = sql_filter(sql_precode = ">=", x = yrs, sql_code = cp, flag = '-- insert year')
  
  sql_run(conn, cp) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(here::here('data', paste0("cpue_", tolower(region), ".csv")), 
                       delim = ',')

  # strata data ----
  # st = sql_read('strata_gap.sql')
  st = readLines(here::here('inst', 'sql', 'strata_gap.sql'))
  st = sql_filter(sql_precode = "IN", x = survey, sql_code = st, flag = '-- insert survey')
  
  sql_run(conn, st) %>% 
    dplyr::rename_all(tolower) %>% 
    tidytable::filter(design_year == max(design_year), .by = stratum) %>% 
    tidytable::arrange(stratum) %>% 
    vroom::vroom_write(here::here('data', paste0("strata_", tolower(region), ".csv")), 
                       delim = ',')

  
  # sizecomp ----
  # lpop = sql_read('lpop_gap.sql')
  lpop = readLines(here::here('inst', 'sql', 'lpop_gap.sql'))
  lpop = sql_filter(sql_precode = "IN", x = survey, sql_code = lpop, flag = '-- insert survey')
  lpop = sql_filter(sql_precode = "IN", x = species, sql_code = lpop, flag = '-- insert species')
  lpop = sql_filter(sql_precode = ">=", x = yrs, sql_code = lpop, flag = '-- insert year')
  
  sql_run(conn, lpop) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(here::here('data', paste0("lpop_", tolower(region), ".csv")), 
                       delim = ',')
  
  # agecomp ----
  # apop = sql_read('apop_gap.sql')
  apop = readLines(here::here('inst', 'sql', 'apop_gap.sql'))
  apop = sql_filter(sql_precode = "IN", x = survey, sql_code = apop, flag = '-- insert survey')
  apop = sql_filter(sql_precode = "IN", x = species, sql_code = apop, flag = '-- insert species')
  apop = sql_filter(sql_precode = ">=", x = yrs, sql_code = apop, flag = '-- insert year')
  
  sql_run(conn, apop) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(here::here('data', paste0("apop_", tolower(region), ".csv")), 
                       delim = ',')
  
  # species names ----
  
  # .s = sql_read('species_gap.sql')
  .s = readLines(here::here('inst', 'sql', 'species_gap.sql'))
  .s = sql_filter(sql_precode = "IN", x = species, sql_code = .s, flag = '-- insert species')
  sql_run(conn, .s) %>% 
    dplyr::rename_all(tolower) %>% 
    vroom::vroom_write(here::here('data', paste0("species_", tolower(region), ".csv")), 

  DBI::dbDisconnect(conn)
  
}


