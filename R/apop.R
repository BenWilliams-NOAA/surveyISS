#' population at age
#'
#' @param lpop length population data
#' @param agedat age dataframe
#' @param sex_spec determine whether computing for sex-specific or total comps
#'
#' @return
#' @export
#'
#' @examples
apop <- function(lpop, agedat, sex_spec = TRUE){
  
  # compute age pop'n for either sex-specific or sex-combined (total) data
  if(isTRUE(sex_spec)){ # for sex-specific data
    # reformat length pop'n data
    lpop %>%
      tidytable::rename(sizepop = abund) %>% 
      tidytable::select(-type) -> .lpop_long
    # compute female/male age pop'n
    agedat %>%
      tidytable::drop_na() %>%
      tidytable::summarise(age_num = .N, .by = c(year, species_code, sex, length, age, type)) %>% 
      tidytable::mutate(age_frac = age_num/sum(age_num), 
                        .by = c(year, species_code, sex, length, type)) %>% 
      tidytable::left_join(.lpop_long) %>%
      tidytable::drop_na() %>% 
      tidytable::mutate(agepop = age_frac * sizepop, 
                        .by = c(year, species_code, sex, length, type)) %>%
      tidytable::summarize(agepop = sum(agepop), 
                           .by = c(year, species_code, type, sex, age)) %>%
      tidytable::filter(sex != 3) -> .agepop_mf
    # determine magnitude of unsex samples
    agedat %>%
      tidytable::drop_na() %>% 
      tidytable::summarise(n = .N, .by = c('year', 'species_code', 'sex', 'type')) %>%
      tidytable::filter(sex == 3) %>%
      tidytable::select(year, species_code, n) -> .sex_cnt_ag
    # if unsexed samples exist, compute unsexed age pop'n
    if(length(.sex_cnt_ag$n)>0){
      .lpop_long %>%
        tidytable::filter(sex == 3) -> .lpop_long_un
      agedat %>%
        tidytable::left_join(.sex_cnt_ag) %>%
        tidytable::filter(n > 0) %>%
        tidytable::summarise(age_num = .N, .by = c(year, species_code, length, age, type)) %>% 
        tidytable::mutate(age_frac = age_num/sum(age_num), 
                          .by = c(year, species_code, length, type)) %>%
        tidytable::left_join(.lpop_long_un) %>%
        tidytable::drop_na() %>% 
        tidytable::mutate(agepop = age_frac * sizepop, 
                          .by = c(year, species_code, length, type)) %>%
        tidytable::summarise(agepop = sum(agepop), 
                             .by = c(year, species_code, type, sex, age, type)) %>% 
        tidytable::bind_rows(.agepop_mf)
    } else {
      .agepop_mf
    }
  } else{ # for combined-sex (total) data
    .lpop %>%
      tidytable::summarize(sizepop = sum(abund), .by = c(species_code, year, length)) %>% 
      tidytable::mutate(sex = 4) -> .lpop_long
    .agedat %>%
      tidytable::summarise(age_num = .N, .by = c(year, species_code, length, age, type)) %>% 
      tidytable::mutate(age_frac = age_num/sum(age_num), 
                        .by = c(year, species_code, length, type)) %>%
      tidytable::left_join(.lpop_long) %>%
      tidytable::drop_na() %>% 
      tidytable::mutate(agepop = age_frac * sizepop, 
                        .by = c(year, species_code, length, type)) %>%
      tidytable::summarise(agepop = sum(agepop), 
                           .by = c(year, species_code, type, sex, age))
  }

}