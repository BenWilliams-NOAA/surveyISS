#' population at age
#'
#' @param lpop length population data
#' @param agedat age dataframe
#'
#' @return
#' @export apop
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
    tidytable::summarise(age_num = .N,
                         .by = c(year, species_code, sex, length, age)) %>%
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
    tidytable::summarise(n = .N,
                         .by = c(year, species_code, sex)) %>% 
    tidytable::filter(sex == 3) %>%
    tidytable::select(year, species_code, n) -> .sex_cnt_ag
  # if unsexed samples exist, compute unsexed age pop'n
  if(length(.sex_cnt_ag$n)>0){
    .lpop_long %>%
      tidytable::filter(sex == 3) -> .lpop_long_un
    agedat %>%
      tidytable::left_join(.sex_cnt_ag) %>%
      tidytable::filter(n > 0) %>%
      tidytable::summarise(age_num = .N,
                           .by = c(year, species_code, length, age)) %>%
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
                             tidytable::summarise(age_num = .N,
                                                  .by = c(year, species_code, length, age)) %>%
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
                             tidytable::summarise(age_num = .N,
                                                  .by = c(year, species_code, length, age)) %>% 
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

#' population at age following computations in gapindex package
#'
#' @param lpop length population data
#' @param agedat age dataframe
#' @param lngs all combinations of possible lengths and ages
#' @param by_strata are the length pop'n by strata or summed to region level (default = FALSE)
#' @param global fills in missing length bins with global alk (default = TRUE)
#'
#' @return
#' @export apop_gap
#'
#' @examples
apop_gap <- function(lpop, 
                     agedat,
                     lngs,
                     by_strata = FALSE,
                     global = TRUE){
  
  # Calculate distribution of age proportions for a given length, `p_yklm`. This is the non-global age-length key.
  # female/male/unsexed
  lngs %>% 
    tidytable::left_join(agedat %>%
                           tidytable::filter(sex != 0) %>%
                           tidytable::filter(!(sex %in% c(0, 3))) %>%
                           tidytable::bind_rows(agedat %>%
                                                  tidytable::filter(sex != 0) %>%
                                                  tidytable::mutate(sex = 3)) %>%
                           tidytable::summarise(age_num = .N,
                                                .by = c(year, species_code, sex, length, age)) %>%
                           tidytable::mutate(age_frac = age_num/sum(age_num), 
                                             .by = c(year, species_code, sex, length))) -> p_yklm
  # combined sex categories
  lngs %>% 
    tidytable::filter(sex == 1) %>% 
    tidytable::mutate(sex = 0) %>% 
    tidytable::left_join(agedat %>%
                           tidytable::filter(sex == 0) %>%
                           tidytable::summarise(age_num = .N,
                                                .by = c(year, species_code, sex, length, age)) %>%
                           tidytable::mutate(age_frac = age_num/sum(age_num), 
                                             .by = c(year, species_code, sex, length))) -> p_yklm_comb
  
  if(isTRUE(global)){
    # Append the globally-filled lengths with the the non-global `p_yklm` alk to get a now global alk. 
    # female/male/unsexed
    lngs %>% 
      tidytable::left_join(p_yklm %>% 
                             tidytable::filter(!is.na(age_frac)) %>% 
                             tidytable::bind_rows(p_yklm %>% 
                                                    # Determine missing lengths
                                                    tidytable::summarise(age_frac = sum(age_frac, na.rm = TRUE), .by = c(year, species_code, sex, length)) %>% 
                                                    tidytable::filter(age_frac == 0) %>% 
                                                    tidytable::select(-age_frac) %>% 
                                                    # for missing lengths, merge age probabilities from golbal ALK
                                                    tidytable::left_join(agedat %>%
                                                                           # Aggregate specimen information over years to calculate a global ALK,
                                                                           tidytable::filter(sex != 0) %>%
                                                                           tidytable::summarise(age_num = .N,
                                                                                                .by = c(species_code, sex, length, age)) %>%
                                                                           tidytable::mutate(age_frac = age_num/sum(age_num), 
                                                                                             .by = c(species_code, sex, length))) %>% 
                                                    tidytable::filter(!is.na(age_frac)))) %>% 
      tidytable::select(-age_num) %>% 
      tidytable::replace_na(list(age_frac = 0)) -> p_yklm
    # combined sex categories
    lngs %>% 
      tidytable::filter(sex == 1) %>% 
      tidytable::mutate(sex = 0) %>% 
      tidytable::left_join(p_yklm_comb %>% 
                             tidytable::filter(!is.na(age_frac)) %>% 
                             tidytable::bind_rows(p_yklm_comb %>% 
                                                    # Determine missing lengths
                                                    tidytable::summarise(age_frac = sum(age_frac, na.rm = TRUE), .by = c(year, species_code, sex, length)) %>% 
                                                    tidytable::filter(age_frac == 0) %>% 
                                                    tidytable::select(-age_frac) %>% 
                                                    # for missing lengths, merge age probabilities from golbal ALK
                                                    tidytable::left_join(agedat %>%
                                                                           # Aggregate specimen information over years to calculate a global ALK,
                                                                           tidytable::filter(sex == 0) %>%
                                                                           tidytable::summarise(age_num = .N,
                                                                                                .by = c(species_code, sex, length, age)) %>%
                                                                           tidytable::mutate(age_frac = age_num/sum(age_num), 
                                                                                             .by = c(species_code, sex, length))) %>% 
                                                    tidytable::filter(!is.na(age_frac)))) %>% 
      tidytable::select(-age_num) %>% 
      tidytable::replace_na(list(age_frac = 0)) -> p_yklm_comb
  }
  
  # Calculate numbers at age as the product of the age_frac and the numbers at length
  # at strata level
  if(isTRUE(by_strata)){
    # female/male/unsexed
    lpop %>% 
      tidytable::filter(sex != 0) %>% 
      tidytable::left_join(p_yklm) %>% 
      tidytable::replace_na(list(age = -9, age_frac = 1)) %>% 
      tidytable::mutate(agepop = abund * age_frac) %>% 
      tidytable::select(-age_frac, -abund) %>% 
      # summarize numbers at age across length, and compute mean length and sd by strata
      tidytable::mutate(mean_length = round(sum(length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2),
                        .by = c(year, species_code, stratum, sex, age)) %>% 
      tidytable::summarise(agepop = round(sum(agepop)),
                           mean_length = mean(mean_length),
                           sd_length = round(sqrt(sum(agepop / sum(agepop, na.rm = TRUE) * (length - mean_length) ^ 2)), digits = 2),
                           .by = c(year, species_code, stratum, sex, age)) %>% 
      tidytable::filter(agepop > 0 & age > 0) %>% 
      # combined sex categories
      tidytable::bind_rows(lpop %>% 
                             tidytable::filter(sex == 0) %>% 
                             tidytable::left_join(p_yklm_comb) %>% 
                             tidytable::replace_na(list(age = -9, age_frac = 1)) %>% 
                             tidytable::mutate(agepop = abund * age_frac) %>% 
                             tidytable::select(-age_frac, -abund) %>% 
                             # summarize numbers at age across length, and compute mean length and sd by strata
                             tidytable::mutate(mean_length = round(sum(length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2), 
                                               .by = c(year, species_code, stratum, sex, age)) %>% 
                             tidytable::summarise(agepop = round(sum(agepop)),
                                                  mean_length = mean(mean_length),
                                                  sd_length = round(sqrt(sum(agepop / sum(agepop, na.rm = TRUE) * (length - mean_length) ^ 2)), digits = 2),
                                                  .by = c(year, species_code, stratum, sex, age)) %>% 
                             tidytable::filter(agepop > 0 & age > 0)) -> agecomp_st
    # remove strata 82 and 90 from ebs data
    if(length(unique(agedat$survey)) == 1 & unique(agedat$survey) == 98){
      agecomp_st %>% 
        tidytable::filter(!(stratum %in% c(82, 90))) -> agecomp_st
    }
    # summarize numbers at age across strata, and compute mean length and sd (which is just a weighted mean and sd) at region level
    agecomp_st %>% 
      tidytable::summarise(agepop = sum(agepop),
                           mean_length = round(sum(mean_length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2),
                           sd_length = round(sum(sd_length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2),
                           .by = c(year, species_code, sex, age)) -> t_ebs
  } else{
    # region level
    # female/male/unsexed
    lpop %>% 
      tidytable::filter(sex != 0) %>% 
      tidytable::left_join(p_yklm) %>% 
      tidytable::replace_na(list(age = -9, age_frac = 1)) %>% 
      tidytable::mutate(agepop = abund * age_frac) %>% 
      tidytable::select(-age_frac, -abund) %>% 
      # summarize numbers at age across length, and compute mean length
      tidytable::mutate(mean_length = round(sum(length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2), 
                        .by = c(year, species_code, stratum, sex, age)) %>% 
      tidytable::summarise(agepop = round(sum(agepop)),
                           mean_length = mean(mean_length),
                           sd_length = round(sqrt(sum(agepop / sum(agepop, na.rm = TRUE) * (length - mean_length) ^ 2)), digits = 2),
                           .by = c(year, species_code, sex, age)) %>% 
      tidytable::filter(agepop > 0 & age > 0) %>% 
      # combined sex categories
      tidytable::bind_rows(lpop %>% 
                             tidytable::filter(sex == 0) %>% 
                             tidytable::left_join(p_yklm_comb) %>% 
                             tidytable::replace_na(list(age = -9)) %>% 
                             tidytable::replace_na(list(age_frac = 1)) %>% 
                             tidytable::mutate(agepop = abund * age_frac) %>% 
                             tidytable::select(-age_frac, -abund) %>% 
                             # summarize numbers at age across length, and compute mean length
                             tidytable::mutate(mean_length = round(sum(length * agepop, na.rm = TRUE) / sum(agepop, na.rm = TRUE), digits = 2), 
                                               .by = c(year, species_code, stratum, sex, age)) %>% 
                             tidytable::summarise(agepop = round(sum(agepop)),
                                                  mean_length = mean(mean_length),
                                                  sd_length = round(sqrt(sum(agepop / sum(agepop, na.rm = TRUE) * (length - mean_length) ^ 2)), digits = 2),
                                                  .by = c(year, species_code, sex, age)) %>% 
                             tidytable::filter(agepop > 0 & age > 0))
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
    tidytable::summarise(age_num = .N,
                         .by = c(year, species_code, sex, length, age)) %>%
    tidytable::mutate(caal = age_num/sum(age_num), 
                      .by = c(year, species_code, sex, length)) %>% 
    tidytable::select(-age_num) %>% 
    tidytable::bind_rows(agedat %>%
                           tidytable::drop_na() %>%
                           tidytable::summarise(age_num = .N,
                                                .by = c(year, species_code, sex, length, age)) %>%
                           tidytable::mutate(caal = age_num/sum(age_num), 
                                             .by = c(year, species_code, sex, length)) %>% 
                           tidytable::select(-age_num))
  
}
