#' population at age
#'
#' @param lpop length population data
#' @param agedat age dataframe
#'
#' @return
#' @export
#'
#' @examples
apop <- function(lpop, 
                 agedat){
  
  # reformat length pop'n data
  lpop %>%
    tidytable::rename(sizepop = abund) -> .lpop_long
  
  # compute female/male age pop'n
  agedat %>%
    tidytable::filter(sex != 0) %>% 
    tidytable::drop_na() %>%
    group_by(year, species_code, sex, length, age) %>% 
    tidytable::summarise(age_num = .N) %>% 
    tidytable::mutate(age_frac = age_num/sum(age_num), 
                      .by = c(year, species_code, sex, length)) %>% 
    tidytable::left_join(.lpop_long) %>%
    tidytable::drop_na() %>% 
    tidytable::mutate(agepop = age_frac * sizepop, 
                      .by = c(year, species_code, sex, length)) %>%
    tidytable::summarize(agepop = sum(agepop), 
                         .by = c(year, species_code, sex, age)) %>%
    tidytable::filter(sex != 3) -> .agepop_mf
  # determine magnitude of unsex samples
  agedat %>%
    tidytable::drop_na() %>% 
    group_by(year, species_code, sex) %>% 
    tidytable::summarise(n = .N) %>%
    tidytable::filter(sex == 3) %>%
    tidytable::select(year, species_code, n) -> .sex_cnt_ag
  # if unsexed samples exist, compute unsexed age pop'n
  if(length(.sex_cnt_ag$n)>0){
    .lpop_long %>%
      tidytable::filter(sex == 3) -> .lpop_long_un
    agedat %>%
      tidytable::left_join(.sex_cnt_ag) %>%
      tidytable::filter(n > 0) %>%
      group_by(year, species_code, length, age) %>% 
      tidytable::summarise(age_num = .N) %>% 
      tidytable::mutate(age_frac = age_num/sum(age_num), 
                        .by = c(year, species_code, length)) %>%
      tidytable::left_join(.lpop_long_un) %>%
      tidytable::drop_na() %>% 
      tidytable::mutate(agepop = age_frac * sizepop, 
                        .by = c(year, species_code, length)) %>%
      tidytable::summarise(agepop = sum(agepop), 
                           .by = c(year, species_code, sex, age)) %>% 
      # add female/male age pop'n
      tidytable::bind_rows(.agepop_mf) %>% 
      # compute and add total (combined sex) age pop'n
      tidytable::bind_rows(agedat %>%
                             tidytable::filter(sex == 0) %>% 
                             group_by(year, species_code, length, age) %>% 
                             tidytable::summarise(age_num = .N) %>% 
                             tidytable::mutate(age_frac = age_num/sum(age_num), 
                                               .by = c(year, species_code, length)) %>%
                             tidytable::left_join(.lpop_long %>%
                                                    tidytable::filter(sex == 0)) %>%
                             tidytable::drop_na() %>% 
                             tidytable::mutate(agepop = age_frac * sizepop, 
                                               .by = c(year, species_code, length)) %>%
                             tidytable::summarise(agepop = sum(agepop), 
                                                  .by = c(year, species_code, sex, age)))
  } else {
    # female/male age pop'n
    .agepop_mf %>% 
      # compute and add total (combined sex) age pop'n
      tidytable::bind_rows(agedat %>%
                             tidytable::filter(sex == 0) %>% 
                             group_by(year, species_code, length, age) %>% 
                             tidytable::summarise(age_num = .N) %>% 
                             tidytable::mutate(age_frac = age_num/sum(age_num), 
                                               .by = c(year, species_code, length)) %>%
                             tidytable::left_join(.lpop_long %>%
                                                    tidytable::filter(sex == 0)) %>%
                             tidytable::drop_na() %>% 
                             tidytable::mutate(agepop = age_frac * sizepop, 
                                               .by = c(year, species_code, length)) %>%
                             tidytable::summarise(agepop = sum(agepop), 
                                                  .by = c(year, species_code, sex, age)))
  }
}

#' conditional age-at-length
#'
#' @param agedat age dataframe
#' 
#' @return
#' @export apop_caal
#'
#' @examples
apop_caal <- function(agedat){
  
  # compute conditional age-at-length for females & males
  agedat %>%
    tidytable::drop_na() %>%
    tidytable::filter(sex %in% c(1, 2)) %>%
    group_by(year, species_code, sex, length, age) %>% 
    tidytable::summarise(age_num = .N) %>%
    tidytable::mutate(caal = age_num/sum(age_num), 
                      .by = c(year, species_code, sex, length)) %>% 
    tidytable::select(-age_num) %>% 
    tidytable::bind_rows(agedat %>%
                           tidytable::drop_na() %>%
                           tidytable::filter(sex == 0) %>%
                           group_by(year, species_code, sex, length, age) %>% 
                           tidytable::summarise(age_num = .N) %>%
                           tidytable::mutate(caal = age_num/sum(age_num), 
                                             .by = c(year, species_code, sex, length)) %>% 
                           tidytable::select(-age_num)) %>% 
    ungroup
  
}
