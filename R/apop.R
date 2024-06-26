#' Compute population at age
#' 
#' @description
#' Function to expand age-length specimen data to population at age aligned with gapindex package.
#'
#' @param lpop population at length dataframe created from surveyISS::lpop_gap()
#' @param agedat age-length specimen input dataframe
#' @param lngs all combinations of possible lengths and ages
#' @param by_strata Boolean. Are the pop'n data structed by strata or summed to region level? (default = FALSE)
#' @param global Boolean. Fill in missing length bins with global age-length key? (default = FALSE)
#'
#' @return dataframe of population at age by year and species, also includes mean length-at-age and sd in length-at-age
#' 
#' @export
#'
apop <- function(lpop, 
                 agedat,
                 lngs,
                 by_strata = FALSE,
                 global = FALSE){
  
  # Calculate distribution of age proportions for a given length, `p_yklm`. This is the non-global age-length key.
  # female/male/unsexed
  agedat %>%
    tidytable::filter(sex != 0) %>%
    tidytable::filter(!(sex %in% c(0, 3))) %>%
    tidytable::bind_rows(agedat %>%
                           tidytable::filter(sex != 0) %>%
                           tidytable::mutate(sex = 3)) %>%
    tidytable::summarise(age_num = .N,
                         .by = c(year, species_code, sex, length, age)) %>%
    tidytable::mutate(age_frac = age_num/sum(age_num), 
                      .by = c(year, species_code, sex, length)) -> p_yklm
  
  # combined sex categories
  agedat %>%
    tidytable::filter(sex == 0) %>%
    tidytable::summarise(age_num = .N,
                         .by = c(year, species_code, sex, length, age)) %>%
    tidytable::mutate(age_frac = age_num/sum(age_num), 
                      .by = c(year, species_code, sex, length)) -> p_yklm_comb
  
  if(isTRUE(global)){
    # Append the globally-filled lengths with the the non-global `p_yklm` alk to get a now global alk. 
    # female/male/unsexed
    lngs %>% 
      tidytable::left_join(p_yklm %>% 
                             tidytable::filter(!is.na(age_frac)) %>% 
                             tidytable::bind_rows(lngs %>% 
                                                    tidytable::left_join(p_yklm) %>% 
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
                             tidytable::bind_rows(lngs %>% 
                                                    tidytable::filter(sex == 1) %>% 
                                                    tidytable::mutate(sex = 0) %>% 
                                                    tidytable::left_join(p_yklm_comb) %>% 
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
    if(length(unique(agedat$survey)) == 1 && unique(agedat$survey) %in% 98){
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
                        .by = c(year, species_code, sex, age)) %>% 
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
                                               .by = c(year, species_code, sex, age)) %>% 
                             tidytable::summarise(agepop = round(sum(agepop)),
                                                  mean_length = mean(mean_length),
                                                  sd_length = round(sqrt(sum(agepop / sum(agepop, na.rm = TRUE) * (length - mean_length) ^ 2)), digits = 2),
                                                  .by = c(year, species_code, sex, age)) %>% 
                             tidytable::filter(agepop > 0 & age > 0))
  }
}


#' Compute conditional age-at-length.
#' 
#'
#' @param agedat age-length specimen input dataframe
#' 
#' @return dataframe of proportions-at-age by length
#' 
#' @export
#'
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
                           tidytable::filter(sex == 0) %>%
                           tidytable::summarise(age_num = .N,
                                                .by = c(year, species_code, sex, length, age)) %>%
                           tidytable::mutate(caal = age_num/sum(age_num), 
                                             .by = c(year, species_code, sex, length)) %>% 
                           tidytable::select(-age_num))
  
}
