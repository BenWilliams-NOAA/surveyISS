#' population at age
#'
#' @param lpop length population data
#' @param agedat age dataframe
#'
#' @return
#' @export
#'
#' @examples
apop <- function(lpop, agedat, strata){
  
  # reformat size pop'n data
  lpop %>%
    tidytable::pivot_longer.(cols = c(males, females, unsexed), names_to = "sex") %>%
    tidytable::mutate.(sex = tidytable::case_when.(sex == 'males' ~ 1,
                                                   sex == 'females' ~ 2,
                                                   TRUE ~ 3)) %>%
    tidytable::rename.(sizepop = value) -> .lpop_long

  # compute resampled age pop'n for females & males
  if(isTRUE(strata)) {
    agedat %>%
      tidytable::drop_na.() %>%
      tidytable::summarise.(age_num = .N, 
                            .by = c(year, species_code, stratum, sex, length, age)) %>% 
      tidytable::mutate.(age_frac = age_num/sum(age_num), 
                         .by = c(year, species_code, stratum, sex, length)) %>% 
      tidytable::left_join.(.lpop_long) %>%
      tidytable::drop_na.() %>% 
      tidytable::mutate.(agepop = age_frac * sizepop, 
                         .by = c(year, species_code, stratum, sex, length)) %>%
      tidytable::mutate.(agepop = sum(agepop), 
                         .by = c(year, species_code, stratum, sex, age)) %>%
      tidytable::select.(year, species_code, sex, stratum, age, agepop) %>%
      unique(., by = c('year', 'species_code', 'sex', 'age', 'agepop')) %>% 
      tidytable::filter.(sex != 3) -> .agepop_mf
    
    # compute resampled age pop'n for unsexed (og rule is if you have a year with unsexed specimen data you use all the specimen data)
    agedat %>%
      tidytable::drop_na.() %>% 
      tidytable::summarise.(n = .N, 
                            .by = c('year', 'species_code', 'sex')) %>%
      tidytable::filter.(sex == 3) %>%
      tidytable::select.(year, species_code, n) -> .sex_cnt_ag
    
    if(length(.sex_cnt_ag$n)>0){
      .lpop_long %>%
        tidytable::filter.(sex == 3) -> .lpop_long_un
      
      agedat %>%
        tidytable::left_join.(.sex_cnt_ag) %>%
        tidytable::filter.(n > 0) %>%
        tidytable::summarise.(age_num = .N, .by = c(year, species_code, stratum, length, age)) %>% 
        tidytable::mutate.(age_frac = age_num/sum(age_num), 
                           .by = c(year, species_code, stratum, length)) %>%
        tidytable::left_join.(.lpop_long_un) %>%
        tidytable::drop_na.() %>% 
        tidytable::mutate.(agepop = age_frac * sizepop, 
                           .by = c(year, species_code, stratum, length)) %>%
        tidytable::mutate.(agepop = sum(agepop), 
                           .by = c(year, species_code, stratum, age)) %>%
        tidytable::select.(year, species_code, stratum, sex, age, agepop) %>% 
        unique(., by = c('year', 'species_code', 'stratum', 'sex', 'age', 'agepop')) %>%
        tidytable::select.(-c(length, age_num, age_frac, sizepop)) %>% 
        tidytable::bind_rows.(.agepop_mf) %>%
        tidytable::pivot_wider.(names_from = sex, values_from = agepop, values_fill = 0) %>%
        tidytable::rename.(unsexed = '3', males = '1', females = '2') 
    } else{
      .agepop_mf %>%
        tidytable::pivot_wider.(names_from = sex, values_from = agepop, values_fill = 0) %>%
        tidytable::rename.(males = '1', females = '2') %>%
        tidytable::mutate.(unsexed = 0) 
    }
  } else {
  agedat %>%
    tidytable::drop_na.() %>%
    tidytable::summarise.(age_num = .N, .by = c(year, species_code, sex, length, age)) %>% 
    tidytable::mutate.(age_frac = age_num/sum(age_num), 
                       .by = c(year, species_code, sex, length)) %>% 
    tidytable::left_join.(.lpop_long) %>%
    tidytable::drop_na.() %>% 
    tidytable::mutate.(agepop = age_frac * sizepop, 
                       .by = c(year, species_code, sex, length)) %>%
    tidytable::mutate.(agepop = sum(agepop), 
                       .by = c(year, species_code, sex, age)) %>%
    tidytable::select.(year, species_code, sex, age, agepop) %>%
    unique(., by = c('year', 'species_code', 'sex', 'age', 'agepop')) %>% 
    tidytable::filter.(sex != 3) -> .agepop_mf
  
  # compute resampled age pop'n for unsexed (og rule is if you have a year with unsexed specimen data you use all the specimen data)
  agedat %>%
    tidytable::drop_na.() %>% 
    tidytable::summarise.(n = .N, .by = c('year', 'species_code', 'sex')) %>%
    tidytable::filter.(sex == 3) %>%
    tidytable::select.(year, species_code, n) -> .sex_cnt_ag
  
  if(length(.sex_cnt_ag$n)>0){
    .lpop_long %>%
      tidytable::filter.(sex == 3) -> .lpop_long_un
    
    agedat %>%
      tidytable::left_join.(.sex_cnt_ag) %>%
      tidytable::filter.(n > 0) %>%
      tidytable::summarise.(age_num = .N, .by = c(year, species_code, length, age)) %>% 
      tidytable::mutate.(age_frac = age_num/sum(age_num), 
                         .by = c(year, species_code, length)) %>%
      tidytable::left_join.(.lpop_long_un) %>%
      tidytable::drop_na.() %>% 
      tidytable::mutate.(agepop = age_frac * sizepop, 
                         .by = c(year, species_code, length)) %>%
      tidytable::mutate.(agepop = sum(agepop), 
                         .by = c(year, species_code, age)) %>%
      unique(., by = c('year', 'species_code', 'sex', 'age', 'agepop')) %>%
      tidytable::select.(-c(length, age_num, age_frac, sizepop)) %>% 
      tidytable::bind_rows.(.agepop_mf) %>%
      tidytable::pivot_wider.(names_from = sex, values_from = agepop, values_fill = 0) %>%
      tidytable::rename.(unsexed = '3', males = '1', females = '2') 
  } else{
    .agepop_mf %>%
      tidytable::pivot_wider.(names_from = sex, values_from = agepop, values_fill = 0) %>%
      tidytable::rename.(males = '1', females = '2') %>%
      tidytable::mutate.(unsexed = 0) 
  }
  
  }
}