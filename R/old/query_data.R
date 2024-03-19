#' query racebase
#'
#' @param region specific region for data ('GOA', 'AI')
#' @param species species_codes e.g., c(10110, 21740)
#' @param yrs minimum year to consider (default: NULL)
#' @param afsc_user afsc database username
#' @param afsc_pwd afsc database password
#' @param nbs switch to include northern Bering Sea data (default: FALSE)
#' @param bs_slope switch to query data from bs slope survey (default: FALSE)
#'
#' @return
#' @export query_data
#'
#' @examples
#' query_date(region = 'AI', 
#'            species = c(30060,21740,10110,30420,21921,21720,10130,30020),
#'            yrs = 2015,
#'            afsc_user = 'your_username',
#'            afsc_pwd = 'your_password')
#'            
query_data <- function(region, species, yrs = NULL, afsc_user, afsc_pwd, nbs = FALSE, bs_slope = FALSE) {
  
  # create folder
  if (!dir.exists("data")) {dir.create("data")}
  
  if(region == 'BS' & (isFALSE(nbs))){
    message("you are excluding the northern Bering Sea data, change to nbs = TRUE if needed")
  }
  
  # connect to database
  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                        UID = afsc_user, PWD = afsc_pwd)
  
  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # length frequency data 
  if (region != 'BS'){
    
    # get goa and ai data
    lfreq = readLines(here::here('inst', 'sql', 'length_freq.sql'))
    # lfreq = sql_read('length_freq.sql')
    lfreq = sql_filter(x = region, sql_code = lfreq, flag = '-- insert region')
    lfreq = sql_filter(sql_precode = "IN", x = species, sql_code = lfreq, flag = '-- insert species')
    lfreq = sql_filter(sql_precode = ">=", x = yrs, sql_code = lfreq, flag = '-- insert year')
    
    sql_run(afsc, lfreq) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("lfreq_", tolower(region), ".csv")), 
                         delim = ',')
  }
  
  if(region == 'BS' & isFALSE(nbs) & isFALSE(bs_slope)) {
    
    # get bs shelf data without nbs
    # bs = sql_read('length_freq_bs.sql')
    bs = readLines(here::here('inst', 'sql', 'length_freq_bs.sql'))
    bs = sql_filter(x = region, sql_code = bs, flag = '-- insert region')
    bs = sql_filter(sql_precode = "IN", x = species, sql_code = bs, flag = '-- insert species')
    bs = sql_filter(sql_precode = ">=", x = yrs, sql_code = bs, flag = '-- insert year')
    
    sql_run(afsc, bs) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("lfreq_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(nbs)){
    
    # get bs shelf data with nbs
    bs = sql_read('length_freq_bs.sql')
    bs = sql_filter(x = region, sql_code = bs, flag = '-- insert region')
    bs = sql_filter(sql_precode = "IN", x = species, sql_code = bs, flag = '-- insert species')
    bs = sql_filter(sql_precode = ">=", x = yrs, sql_code = bs, flag = '-- insert year')
    
    nbs = sql_read('length_freq_nbs.sql')
    nbs = sql_filter(x = region, sql_code = nbs, flag = '-- insert region')
    nbs = sql_filter(sql_precode = "IN", x = species, sql_code = nbs, flag = '-- insert species')
    nbs = sql_filter(sql_precode = ">=", x = yrs, sql_code = nbs, flag = '-- insert year')
    
    sql_run(afsc, bs) %>% 
      tidytable::bind_rows(sql_run(afsc, nbs)) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("lfreq_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(bs_slope)){
    
    # get bs slope data
    # bss = sql_read('length_freq_bss.sql')
    bss = readLines(here::here('inst', 'sql', 'length_freq_bss.sql')) # 2010 survey was ommitted, readLines for now until pacakage developed
    bss = sql_filter(x = region, sql_code = bss, flag = '-- insert region')
    bss = sql_filter(sql_precode = "IN", x = species, sql_code = bss, flag = '-- insert species')
    
    sql_run(afsc, bss) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::mutate(year=as.numeric(substr(as.character(cruise), 1, 4))) %>% 
      vroom::vroom_write(here::here('data', paste0("lfreq_slope_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  # specimen data 
  if (region!='BS'){ 
    
    # get goa and ai data
    # sp = sql_read('specimen.sql')
    sp = readLines(here::here('inst', 'sql', 'specimen.sql'))
    sp = sql_filter(x = region, sql_code = sp, flag = '-- insert region')
    sp = sql_filter(sql_precode = "IN", x = species, sql_code = sp, flag = '-- insert species')
    sp = sql_filter(sql_precode = ">=", x = yrs, sql_code = sp, flag = '-- insert year')
    
    sql_run(afsc, sp) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(., 
                         here::here('data', paste0("specimen_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & isFALSE(nbs) & isFALSE(bs_slope)) {
    
    # get bs shelf data without nbs
    spbs = readLines(here::here('inst', 'sql', 'specimen_bs.sql'))
    # spbs = sql_read('specimen_bs.sql')
    spbs = sql_filter(x = region, sql_code = spbs, flag = '-- insert region')
    spbs = sql_filter(sql_precode = "IN", x = species, sql_code = spbs, flag = '-- insert species')
    spbs = sql_filter(sql_precode = ">=", x = yrs, sql_code = spbs, flag = '-- insert year')
    sql_run(afsc, spbs) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("specimen_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(nbs)) {
    
    # get bs shelf data with nbs
    spbs = sql_read('specimen_bs.sql')
    spbs = sql_filter(x = region, sql_code = spbs, flag = '-- insert region')
    spbs = sql_filter(sql_precode = "IN", x = species, sql_code = spbs, flag = '-- insert species')
    spbs = sql_filter(sql_precode = ">=", x = yrs, sql_code = spbs, flag = '-- insert year')
    
    
    spnbs = sql_read('specimen_nbs.sql')
    spnbs = sql_filter(x = region, sql_code = spnbs, flag = '-- insert region')
    spnbs = sql_filter(sql_precode = "IN", x = species, sql_code = spnbs, flag = '-- insert species')
    spnbs = sql_filter(sql_precode = ">=", x = yrs, sql_code = spnbs, flag = '-- insert year')
    
    sql_run(afsc, spbs) %>% 
      tidytable::bind_rows(sql_run(afsc, spnbs)) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("specimen_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(bs_slope)) {
    
    # get bs slope data
    # spbss = sql_read('specimen_bss.sql')
    spbss = readLines(here::here('inst', 'sql', 'specimen_bss.sql')) # 2010 survey was ommitted, readLines for now until pacakage developed
    spbss = sql_filter(x = region, sql_code = spbss, flag = '-- insert region')
    spbss = sql_filter(sql_precode = "IN", x = species, sql_code = spbss, flag = '-- insert species')
    
    sql_run(afsc, spbss) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr::mutate(year=as.numeric(substr(as.character(cruise), 1, 4))) %>% 
      vroom::vroom_write(here::here('data', paste0("specimen_slope_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  # cpue data 
  if (region!='BS') {
    # cp = sql_read('cpue.sql')
    cp = readLines(here::here('inst', 'sql', 'cpue.sql'))
    cp = sql_add(paste0(region, '.cpue'), cp)
    cp = sql_filter(x = region, sql_code = cp, flag = '-- insert region')
    cp = sql_filter(sql_precode = "IN", x = species, sql_code = cp, flag = '-- insert species')
    cp = sql_filter(sql_precode = ">=", x = yrs, sql_code = cp, flag = '-- insert year')
    
    sql_run(afsc, cp) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("cpue_", tolower(region), ".csv")), 
                         delim = ',')
  }
  
  if(region == 'BS' & isFALSE(nbs) & isFALSE(bs_slope)) {
    
    # get bs shelf data without nbs
    cpbs = readLines(here::here('inst', 'sql', 'cpue_BS.sql'))
    # cpbs = sql_read('cpue_BS.sql')
    cpbs = sql_add(paste0('HAEHNR', '.EBSSHELF_CPUE'), cpbs)
    cpbs = sql_filter(sql_precode = "IN", x = species, sql_code = cpbs, flag = '-- insert species')
    cpbs = sql_filter(sql_precode = ">=", x = yrs, sql_code = cpbs, flag = '-- insert year')
    
    sql_run(afsc, cpbs) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(., 
                         here::here('data', paste0("cpue_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(nbs)) {
    
    # get bs shelf data with nbs
    cpbs = sql_read('cpue_BS.sql')
    cpbs = sql_add(paste0('HAEHNR', '.EBSSHELF_CPUE'), cpbs)
    cpbs = sql_filter(sql_precode = "IN", x = species, sql_code = cpbs, flag = '-- insert species')
    cpbs = sql_filter(sql_precode = ">=", x = yrs, sql_code = cpbs, flag = '-- insert year')
    
    cpnbs = sql_read('cpue_BS.sql')
    cpnbs = sql_add(paste0('HAEHNR', '.NBS_CPUE'), cpnbs)
    cpnbs = sql_filter(sql_precode = "IN", x = species, sql_code = cpnbs, flag = '-- insert species')
    cpnbs = sql_filter(sql_precode = ">=", x = yrs, sql_code = cpnbs, flag = '-- insert year')
    
    sql_run(afsc, cpbs) %>% 
      tidytable::bind_rows(sql_run(afsc, cpnbs)) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(., 
                         here::here('data', paste0("cpue_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(bs_slope)) {
    
    # get bs slope data
    cpbss = sql_read('cpue_BS.sql')
    cpbss = sql_add(paste0('HOFFJ', '.CPUE_EBSSLOPE'), cpbss)
    cpbss = sql_filter(sql_precode = "IN", x = species, sql_code = cpbss, flag = '-- insert species')
    cpbss = sql_filter(sql_precode = ">=", x = yrs, sql_code = cpbss, flag = '-- insert year')
    
    sql_run(afsc, cpbss) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(., 
                         here::here('data', paste0("cpue_slope_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  # strata 
  if(region!='BS') {
    
    # get goa and ai data
    st = readLines(here::here('inst', 'sql', 'strata.sql'))
    # st = sql_read('strata.sql')
    st = sql_filter(x = region, sql_code = st, flag = '-- insert region')
    sql_run(afsc, st) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("strata_", tolower(region), ".csv")), 
                         delim = ',')
  }
  
  if(region == 'BS' & isFALSE(nbs) & isFALSE(bs_slope)) {
    
    # get bs shelf data without nbs
    
    # for now shutting this down until we can resolve where the bs strata data should be coming from and using meaghan's strata tables
    
    # # old call to haehner table, not public facing so switching to stratum table in racebase
    # # stbs = sql_read('strata_bs.sql')
    # 
    # # new call to racebase.stratum, note: when package is set up change from 'readLines' function to 'sql_run'
    # stbs = readLines(here::here('inst', 'sql', 'strata_bs_new.sql'))
    # # stbs = sql_read('strata_bs_new.sql')  # this would be the call when the package is set up
    # stbs = sql_filter(x = region, sql_code = stbs, flag = '-- insert region')
    # #stbs = sql_filter(x = 2022, sql_code = stbs, flag = '-- insert year') #hardwired to 2022, looks like strata sizes changed
    # 
    # sql_run(afsc, stbs) %>%
    #   dplyr::rename_all(tolower) %>%
    #   vroom::vroom_write(here::here('data', paste0("strata_test_", tolower(region), ".csv")),
    #                      delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(nbs)) {
    
    # get bs shelf data with nbs
    stbs = sql_read('strata_bs.sql')
    stnbs = sql_read('strata_nbs.sql')
    
    sql_run(afsc, stbs) %>% 
      tidytable::bind_rows(sql_run(afsc, stnbs)) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("strata_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(bs_slope)) {
    
    # get bs slope data
    # this is here as placeholder till we resolve the above comment

  }
  
  
  # race pop
  if(region!='BS') {
    
    # get goa and ai data
    rp = sql_read('race_pop.sql')
    # length pop
    rpl = sql_add(paste0(region, '.SIZECOMP_TOTAL'), rp)
    rpl = sql_filter(sql_precode = 'IN', sql_code = rpl,
                     x = species, 
                     flag = '-- insert species')
    rpl = sql_filter(sql_precode = ">=", x = yrs, 
                     sql_code = rpl, flag = '-- insert year')
    
    
    sql_run(afsc, rpl) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("race_lpop_", tolower(region), ".csv")), 
                         delim = ',')
    
    # age pop
    rpa = sql_read('race_apop.sql')
    rpa = sql_add(paste0(region, '.AGECOMP_TOTAL'), rpa)
    rpa = sql_filter(sql_precode = 'IN', sql_code = rpa,
                     x = species, 
                     flag = '-- insert species')
    rpa = sql_filter(sql_precode = ">=", x = yrs, 
                     sql_code = rpa, flag = '-- insert year')
    
    
    sql_run(afsc, rpa) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("race_apop_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & isFALSE(nbs) & isFALSE(bs_slope)) {
    
    # get bs shelf data without nbs
    rpbs = sql_read('race_pop_bs.sql')
    rpbs = sql_filter(sql_precode = 'IN', sql_code = rpbs,
                      x = species, 
                      flag = '-- insert species')
    rpbs = sql_filter(sql_precode = ">=", x = yrs, 
                      sql_code = rpbs, flag = '-- insert year')
    
    sql_run(afsc, rpbs) %>%
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("race_lpop_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(nbs)) {
    
    # get bs shelf data with nbs
    rpnbs = sql_read('race_pop_nbs.sql')
    rpnbs = sql_filter(sql_precode = 'IN', sql_code = rpnbs,
                       x = species, 
                       flag = '-- insert species')
    rpnbs = sql_filter(sql_precode = ">=", x = yrs, 
                       sql_code = rpnbs, flag = '-- insert year')
    
    rpbs = sql_read('race_pop_bs.sql')
    rpbs = sql_filter(sql_precode = 'IN', sql_code = rpbs,
                      x = species, 
                      flag = '-- insert species')
    rpbs = sql_filter(sql_precode = ">=", x = yrs, 
                      sql_code = rpbs, flag = '-- insert year')
    
    sql_run(afsc, rpnbs) %>% 
      tidytable::bind_rows(sql_run(afsc, rpbs)) %>%
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("race_pop_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(bs_slope)) {
    
    # get bs slope data
    rpbss = sql_read('race_pop_bss.sql')
    rpbss = sql_filter(x = region, sql_code = rpbss, flag = '-- insert region')
    rpbss = sql_filter(sql_precode = "IN", x = species, sql_code = rpbss, flag = '-- insert species')
    rpbss = sql_filter(sql_precode = ">=", x = yrs, sql_code = rpbss, flag = '-- insert year')
    
    sql_run(afsc, rpbss) %>% 
      dplyr::rename_all(tolower) %>% 
      dplyr:: group_by(year,species_code,length) %>% 
      dplyr::summarise(males=sum(males),females=sum(females),unsexed=sum(unsexed),total=sum(total)) %>%
      vroom::vroom_write(here::here('data', paste0("race_lpop_slope_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  # species common name 
  if(region!='BS') {
  
    # goa and ai species
    .s = sql_read('species.sql')
    .s = sql_filter(sql_precode = "IN", x = species, sql_code = .s, flag = '-- insert species')
    sql_run(afsc, .s) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("species_", tolower(region), ".csv")), 
                         delim = ',')
  
  }
  
  if(region == 'BS' & isFALSE(nbs) & isFALSE(bs_slope)) {
    
    # bs shelf species without nbs
    .s = sql_read('species.sql')
    .s = sql_filter(sql_precode = "IN", x = species, sql_code = .s, flag = '-- insert species')
    sql_run(afsc, .s) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("species_", tolower(region), ".csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(nbs)) {
    
    # bs shelf species with nbs
    .s = sql_read('species.sql')
    .s = sql_filter(sql_precode = "IN", x = species, sql_code = .s, flag = '-- insert species')
    sql_run(afsc, .s) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("species_", tolower(region), "_nbs.csv")), 
                         delim = ',')
    
  }
  
  if(region == 'BS' & !isFALSE(bs_slope)) {
    
    # bs slope species
    .s = sql_read('species.sql')
    .s = sql_filter(sql_precode = "IN", x = species, sql_code = .s, flag = '-- insert species')
    sql_run(afsc, .s) %>% 
      dplyr::rename_all(tolower) %>% 
      vroom::vroom_write(here::here('data', paste0("species_", tolower(region), "_slope.csv")), 
                         delim = ',')
    
  }
  
  DBI::dbDisconnect(afsc)
  
}


