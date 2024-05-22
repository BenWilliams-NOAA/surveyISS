#' Compute surveyISS statistics
#' 
#' @description
#' Wrapper function to compute statistics of bootstrap resampling of age and length composition.
#'
#' @param r_age list of replicated abundance at age
#' @param oga original abundance at age (computed with data that has not been resampled)
#' @param r_length list of replicated abundance at length
#' @param ogl original abundance at length (computed with data that has not been resampled)
#' @param specimen_data age-length specimen input dataframe
#' @param lfreq_data length frequency input dataframe
#' @param survey_region overall region, i.e., goa, ai, ebs, nbs... If defined, will compute 
#' statistics for spatially-explicit results. (default = NULL)
#'
#' @return list of dataframes for realized sample size by replicate (.rss_age for age composition, 
#' .rss_length for length composition), input sample size (.iss_age for age composition, 
#' .iss_length for length composition), bias in resampled comp data compared to original values
#' (.bias_age for age composition, .bias_length for length composition), and mean length-at-age
#' (.mean_length, computed ala gapindex package)
#' 
#' @export
#'
comp_stats <- function(r_age, 
                       oga,
                       r_length,
                       ogl,
                       specimen_data,
                       lfreq_data,
                       survey_region = NULL){
  
  # compute realized sample size of bootstrapped age/length
  rss_age(r_age, oga) %>%
    tidytable::mutate(sex_desc = dplyr::case_when(sex == 0 ~ 'total_pre',
                                                  sex == 1 ~ 'male',
                                                  sex == 2 ~ 'female',
                                                  sex == 12 ~ 'female_male',
                                                  sex == 4 ~ 'total_post')) -> .rss_age
  
  rss_length(r_length, ogl) %>%
    tidytable::mutate(sex_desc = dplyr::case_when(sex == 0 ~ 'total_pre',
                                                  sex == 1 ~ 'male',
                                                  sex == 2 ~ 'female',
                                                  sex == 12 ~ 'female_male',
                                                  sex == 4 ~ 'total_post')) -> .rss_length
  
  # age comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_age <- iss_age(.rss_age, specimen_data, survey_region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_age <- bias_age(r_age, oga)
  
  # compute mean length-at-age and sd (if using gap fcns)
  if("mean_length" %in% names(r_age)){
    .mean_length <- grwth_stats(r_age, oga)
  } else{
    .mean_length = NULL
  }
  
  # length comps: 
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_length <- iss_length(.rss_length, lfreq_data, survey_region)
  
  # compute average relative bias in pop'n estimates (avg relative bias across length)
  .bias_length <- bias_length(r_length, ogl)
  
  # return
  list(rss_age = .rss_age, 
       rss_length = .rss_length, 
       iss_age = .iss_age, 
       bias_age = .bias_age,
       mean_length = .mean_length, 
       iss_length = .iss_length, 
       bias_length = .bias_length)
  
}

#' Compute surveyISS statistics
#' 
#' @description
#' Wrapper function to compute statistics of bootstrap resampling for conditional age-at-length
#'
#' @param r_caal list of replicated conditional age-at-length
#' @param ogcaal original conditional age-at-length (computed with data that has not been resampled)
#' @param specimen_data age-length specimen input dataframe
#'
#' @return list of dataframes for realized sample size by replicate (.rss_caal), input sample size (.iss_caal),
#' and bias in resampled comp data compared to original values (.bias_caal)
#' 
#' @export
#'
comp_stats_caal <- function(r_caal, 
                            ogcaal,
                            specimen_data){
  
  # compute realized sample size of bootstrapped age/length
  rss_caal(r_caal, ogcaal) %>%
    tidytable::mutate(sex_desc = dplyr::case_when(sex == 0 ~ 'total',
                                                  sex == 1 ~ 'male',
                                                  sex == 2 ~ 'female')) -> .rss_caal
  
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  .iss_caal <- iss_caal(.rss_caal, specimen_data)
  
  # compute average relative bias in pop'n estimates (avg relative bias across age)
  .bias_caal <- bias_caal(r_caal, ogcaal)
  
  # return
  list(rss_caal = .rss_caal, 
       iss_caal = .iss_caal,
       bias_caal = .bias_caal)
  
}

#' Calculate age composition realized sampled size (rss)
#' 
#' @description
#' Function to calculate rss for resampled age composition following McAllister and Ianelli 1997.
#' Will compute rss for regional or spatially-explicit cases automatically.
#'
#' @param sim_data list of replicated abundance by age
#' @param og_data original abundance by age
#'
#' @export
#'
rss_age <- function(sim_data, 
                    og_data){
  
  # for simulations at regional level
  if(!("region" %in% names(sim_data))){
    # compute post-expansion total age pop'n and add to og and sim data
    og_data %>% 
      tidytable::bind_rows(og_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::select(year, species_code, sex, age, og_agepop = agepop) -> .og_data
    sim_data %>% 
      tidytable::bind_rows(sim_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::select(sim, year, species_code, sex, age, agepop) -> .sim_data
    
    # compute realized sample size
    .sim_data %>% 
      tidytable::full_join(.og_data) %>% 
      tidytable::drop_na(sim) %>% 
      tidytable::replace_na(list(agepop = 0, og_agepop = 0)) %>% 
      tidytable::filter(sex != 3) %>%
      tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                        p_sim = agepop / sum(agepop),
                        .by = c(sim, year, species_code, sex)) %>% 
      tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og) ^ 2),
                           .by = c(sim, year, species_code, sex)) %>% 
      tidytable::drop_na() %>% 
      # compute realized sample size for female-male comps that sum to 1
      tidytable::bind_rows(.sim_data %>% 
                             tidytable::full_join(.og_data) %>% 
                             tidytable::replace_na(list(agepop = 0, og_agepop = 0)) %>% 
                             tidytable::filter(sex %in% c(1,2)) %>% 
                             tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                                               p_sim = agepop / sum(agepop),
                                               .by = c(sim, year, species_code)) %>%
                             tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                  .by = c(sim, year, species_code)) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::arrange(year, species_code)
  } else{ # compute rss at subregion scale
    # compute post-expansion total age pop'n and add to og and sim data
    og_data %>% 
      tidytable::bind_rows(og_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop), .by = c(year, region, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::select(year, region, species_code, sex, age, og_agepop = agepop)  -> .og_data
    sim_data %>% 
      tidytable::bind_rows(sim_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, region, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::select(sim, year, region, species_code, sex, age, agepop) -> .sim_data
    
    # compute realized sample size  
    .sim_data %>% 
      tidytable::full_join(.og_data) %>% 
      tidytable::drop_na(sim) %>% 
      tidytable::replace_na(list(agepop = 0, og_agepop = 0)) %>% 
      tidytable::filter(sex != 3) %>%
      tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                        p_sim = agepop / sum(agepop),
                        .by = c(sim, year, region, species_code, sex)) %>% 
      tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                           .by = c(sim, year, region, species_code, sex)) %>% 
      tidytable::drop_na() %>% 
      # compute realized sample size for female-male comps that sum to 1
      tidytable::bind_rows(.sim_data %>% 
                             tidytable::full_join(.og_data) %>% 
                             tidytable::replace_na(list(agepop = 0, og_agepop = 0)) %>% 
                             tidytable::filter(sex %in% c(1,2)) %>% 
                             tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                                               p_sim = agepop / sum(agepop),
                                               .by = c(sim, year, region, species_code)) %>%
                             tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                  .by = c(sim, year, region, species_code)) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::arrange(year, species_code)
  }
}

#' Calculate conditional age-at-length realized sampled size (rss)
#' 
#' @description
#' Function to calculate rss for conditional age-at-length following McAllister and Ianelli 1997.
#'
#' @param sim_data list of replicated conditional age-at-length
#' @param og_data original conditional age-at-length
#'
#' @export
#'
rss_caal <- function(sim_data, 
                     og_data){
  
  # compute realized sample size
  sim_data %>% 
    tidytable::full_join(og_data %>% 
                           tidytable::rename(og_caal = caal)) %>% 
    tidytable::drop_na(sim) %>% 
    tidytable::replace_na(list(caal = 0, og_caal = 0)) %>% 
    tidytable::summarise(rss = sum(caal * (1 - caal)) / sum((caal - og_caal)^2),
                         .by = c(sim, year, species_code, sex, length)) %>% 
    tidytable::drop_na()
  
}

#' Calculate length composition realized sampled size (rss)
#' 
#' @description
#' Function to calculate rss for length composition following McAllister and Ianelli 1997.
#' Will compute rss for regional or spatially-explicit cases automatically.
#'
#' @param sim_data list of replicated abundance at length
#' @param og_data original abundance at length
#'
#' @export rss_length
#'
rss_length <- function(sim_data,
                       og_data) {
  
  # for simulations at regional level
  if(!("region" %in% names(sim_data))){
    # compute post-expansion total length pop'n and add to og and sim data
    og_data %>% 
      tidytable::bind_rows(og_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::rename(og_abund = abund) -> .og_data
    sim_data %>% 
      tidytable::bind_rows(sim_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>%
                             tidytable::mutate(sex = 4)) -> .sim_data
    
    # compute realized sample size
    .sim_data %>% 
      tidytable::full_join(.og_data) %>% 
      tidytable::replace_na(list(abund = 0, og_abund = 0)) %>%
      tidytable::filter(sex != 3) %>%
      tidytable::mutate(p_og = og_abund / sum(og_abund),
                        p_sim = abund / sum(abund),
                        .by = c(sim, year, species_code, sex)) %>% 
      tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                           .by = c(sim, year, species_code, sex)) %>% 
      tidytable::drop_na() %>% 
      # compute realized sample size for female-male comps that sum to 1
      tidytable::bind_rows(.sim_data %>% 
                             tidytable::full_join(.og_data) %>% 
                             tidytable::replace_na(list(abund = 0, og_abund = 0)) %>% 
                             tidytable::filter(sex %in% c(1,2)) %>% 
                             tidytable::mutate(p_og = og_abund / sum(og_abund),
                                               p_sim = abund / sum(abund),
                                               .by = c(sim, year, species_code)) %>%
                             tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                  .by = c(sim, year, species_code)) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::arrange(year, species_code)
  } else{ # compute rss at subregion scale
    # compute post-expansion total length pop'n and add to og and sim data
    og_data %>% 
      tidytable::bind_rows(og_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(abund = sum(abund), .by = c(year, region, species_code, length)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::rename(og_abund = abund) -> .og_data
    sim_data %>% 
      tidytable::bind_rows(sim_data %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(abund = sum(abund), .by = c(sim, year, region, species_code, length)) %>%
                             tidytable::mutate(sex = 4)) -> .sim_data
    
    # compute realized sample size
    .sim_data %>% 
      tidytable::full_join(.og_data) %>% 
      tidytable::replace_na(list(abund = 0, og_abund = 0)) %>%
      tidytable::filter(sex != 3) %>%
      tidytable::mutate(p_og = og_abund / sum(og_abund),
                        p_sim = abund / sum(abund),
                        .by = c(sim, year, region, species_code, sex)) %>% 
      tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                           .by = c(sim, year, region, species_code, sex)) %>% 
      tidytable::drop_na() %>% 
      # compute realized sample size for female-male comps that sum to 1
      tidytable::bind_rows(.sim_data %>% 
                             tidytable::full_join(.og_data) %>% 
                             tidytable::replace_na(list(abund = 0, og_abund = 0)) %>% 
                             tidytable::filter(sex %in% c(1,2)) %>% 
                             tidytable::mutate(p_og = og_abund / sum(og_abund),
                                               p_sim = abund / sum(abund),
                                               .by = c(sim, year, region, species_code)) %>%
                             tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                  .by = c(sim, year, region, species_code)) %>% 
                             tidytable::drop_na() %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::arrange(year, species_code)
  }
}

#' Calculate age composition input sampled size (iss)
#' 
#' @description
#' Function to calculate age composition iss (harmonic mean of realized sample sizes) and add nominal sample
#' size (the number of ages actually aged) and the number of sampled hauls to output. Will compute for regional
#' or spatially-explicit cases (depending on whether survey_region is defined, see arguments below)
#'
#' @param rss_age iterated age composition realized sample size
#' @param specimen_data age-length specimen input dataframe
#' @param survey_region overall region, i.e., goa, ai, ebs, nbs... If defined, will compute 
#' statistics for spatially-explicit results. (default = NULL)
#'
#' @export
#'
iss_age <- function(rss_age,
                    specimen_data,
                    survey_region = NULL) {
  
  # compute for region scale
  if(is.null(survey_region)){
    # age comps: compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
    rss_age %>% 
      tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE, zero = FALSE),
                           .by = c(year, species_code, sex, sex_desc)) %>% 
      # add nominal sample size (nss) and number of hauls (nhls)
      tidytable::left_join(specimen_data %>% 
                             tidytable::drop_na(age) %>% 
                             tidytable::filter(sex != 3) %>% 
                             tidytable::summarize(nss = .N, .by = c(year, species_code, sex)) %>% 
                             tidytable::bind_rows(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::summarize(nss = .N, .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 0)) %>% 
                             tidytable::bind_rows(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::summarize(nss = .N, .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::bind_rows(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarize(nss = .N, .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::left_join(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code, sex)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 0)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 4)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 12)))) %>% 
      tidytable::filter(iss > 0)
  } else{ # compute for subregion scale
    # age comps: compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
    rss_age %>% 
      tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE, zero = FALSE),
                           .by = c(year, region, species_code, sex, sex_desc)) %>% 
      # add nominal sample size (nss) and number of hauls (nhls) by subregion and survey region
      tidytable::left_join(specimen_data %>% 
                             tidytable::drop_na(age) %>% 
                             tidytable::filter(sex != 3) %>% 
                             tidytable::summarize(nss = .N, .by = c(year, region, species_code, sex)) %>% 
                             tidytable::bind_rows(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::summarize(nss = .N, .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 0)) %>% 
                             tidytable::bind_rows(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::summarize(nss = .N, .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::bind_rows(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarize(nss = .N, .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::bind_rows(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarize(nss = .N, .by = c(year, species_code, sex)) %>% 
                                                    tidytable::mutate(region = survey_region) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::summarize(nss = .N, .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 0,
                                                                                             region = survey_region)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::summarize(nss = .N, .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 4,
                                                                                             region = survey_region)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarize(nss = .N, .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 12,
                                                                                             region = survey_region))) %>% 
                             tidytable::left_join(specimen_data %>% 
                                                    tidytable::drop_na(age) %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, region, species_code, sex)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, region, species_code)) %>% 
                                                                           tidytable::mutate(sex = 0)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, region, species_code)) %>% 
                                                                           tidytable::mutate(sex = 4)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, region, species_code)) %>% 
                                                                           tidytable::mutate(sex = 12)) %>% 
                                                    tidytable::bind_rows(specimen_data %>% 
                                                                           tidytable::drop_na(age) %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code, sex)) %>% 
                                                                           tidytable::mutate(region = survey_region) %>% 
                                                                           tidytable::bind_rows(specimen_data %>% 
                                                                                                  tidytable::drop_na(age) %>% 
                                                                                                  tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                                                  tidytable::mutate(sex = 0,
                                                                                                                    region = survey_region)) %>% 
                                                                           tidytable::bind_rows(specimen_data %>% 
                                                                                                  tidytable::drop_na(age) %>% 
                                                                                                  tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                                                  tidytable::mutate(sex = 4,
                                                                                                                    region = survey_region)) %>% 
                                                                           tidytable::bind_rows(specimen_data %>% 
                                                                                                  tidytable::drop_na(age) %>% 
                                                                                                  tidytable::filter(sex != 3) %>% 
                                                                                                  tidytable::summarize(nhl = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                                                  tidytable::mutate(sex = 12,
                                                                                                                    region = survey_region))))) %>% 
      tidytable::filter(iss > 0)
  }
}

#' Calculate conditional age-at-length input sampled size (iss)
#' 
#' @description
#' calculate conditional-age-at-length input sample size and add nominal sample size and number of sampled hauls to output
#'
#' @param rss_caal iterated conditional age-at-length realized sample size
#' @param specimen_data age-length specimen input dataframe
#'
#' @export
#'
iss_caal <- function(rss_caal,
                     specimen_data) {
  
  # compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
  rss_caal %>% 
    tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE, zero = FALSE),
                         .by = c(year, species_code, sex, sex_desc, length)) %>% 
    # add nominal sample size (nss)
    tidytable::left_join(specimen_data %>% 
                           tidytable::drop_na(age) %>% 
                           tidytable::filter(sex != 3) %>% 
                           tidytable::summarise(nss = .N, .by = c(year, species_code, sex, length)) %>% 
                           tidytable::bind_rows(specimen_data %>% 
                                                  tidytable::drop_na(age) %>% 
                                                  tidytable::summarise(nss = .N, .by = c(year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 0)) %>% 
                           tidytable::bind_rows(specimen_data %>% 
                                                  tidytable::drop_na(age) %>% 
                                                  tidytable::summarise(nss = .N, .by = c(year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::bind_rows(specimen_data %>% 
                                                  tidytable::drop_na(age) %>% 
                                                  tidytable::filter(sex != 3) %>% 
                                                  tidytable::summarise(nss = .N, .by = c(year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 12)))
  
}

#' Calculate length composition input sampled size (iss)
#' 
#' @description
#' Function to calculate length composition iss (harmonic mean of realized sample sizes) and add nominal sample
#' size (the number of lengths actually measured) and the number of sampled hauls to output. Will compute for regional
#' or spatially-explicit cases (depending on whether survey_region is defined, see arguments below)
#'
#' @param rss_length iterated length composition realized sample size
#' @param lfreq_data length frequency input dataframe
#' @param survey_region overall region, i.e., goa, ai, ebs, nbs... If defined, will compute 
#' statistics for spatially-explicit results. (default = NULL)
#'
#' @export
#'
iss_length <- function(rss_length,
                       lfreq_data,
                       survey_region = NULL) {
  
  # compute for region scale
  if(is.null(survey_region)){
    # length comps: compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
    rss_length %>% 
      tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE, zero = FALSE),
                           .by = c(year, species_code, sex, sex_desc)) %>% 
      # add nominal sample size (nss) and number of hauls (nhls)
      tidytable::left_join(lfreq_data %>% 
                             tidytable::filter(sex != 3) %>% 
                             tidytable::summarise(nss = sum(frequency), .by = c(year, species_code, sex)) %>% 
                             tidytable::bind_rows(lfreq_data %>% 
                                                    tidytable::summarise(nss = sum(frequency), .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 0)) %>% 
                             tidytable::bind_rows(lfreq_data %>% 
                                                    tidytable::summarise(nss = sum(frequency), .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::bind_rows(lfreq_data %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarise(nss = sum(frequency), .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::left_join(lfreq_data %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code, sex)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 0)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 4)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 12)))) %>% 
      tidytable::filter(iss > 0)
  } else{ # compute for subregion scale
    # length comps: compute harmonic mean of iterated realized sample size, which is the input sample size (iss)
    rss_length %>% 
      tidytable::summarise(iss = psych::harmonic.mean(rss, na.rm = TRUE, zero = FALSE),
                           .by = c(year, region, species_code, sex, sex_desc)) %>% 
      # add nominal sample size (nss) and number of hauls (nhls)
      tidytable::left_join(lfreq_data %>% 
                             tidytable::filter(sex != 3) %>% 
                             tidytable::summarise(nss = sum(frequency), .by = c(year, region, species_code, sex)) %>% 
                             tidytable::bind_rows(lfreq_data %>% 
                                                    tidytable::summarise(nss = sum(frequency), .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 0)) %>% 
                             tidytable::bind_rows(lfreq_data %>% 
                                                    tidytable::summarise(nss = sum(frequency), .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::bind_rows(lfreq_data %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarise(nss = sum(frequency), .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::left_join(lfreq_data %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, region, species_code, sex)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, region, species_code)) %>% 
                                                                           tidytable::mutate(sex = 0)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, region, species_code)) %>% 
                                                                           tidytable::mutate(sex = 4)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, region, species_code)) %>% 
                                                                           tidytable::mutate(sex = 12))) %>% 
                             tidytable::bind_rows(lfreq_data %>% 
                                                    tidytable::filter(sex != 3) %>% 
                                                    tidytable::summarise(nss = sum(frequency), .by = c(year, species_code, sex)) %>% 
                                                    tidytable::mutate(region = survey_region) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::summarise(nss = sum(frequency), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 0,
                                                                                             region = survey_region)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::summarise(nss = sum(frequency), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 4,
                                                                                             region = survey_region)) %>% 
                                                    tidytable::bind_rows(lfreq_data %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarise(nss = sum(frequency), .by = c(year, species_code)) %>% 
                                                                           tidytable::mutate(sex = 12,
                                                                                             region = survey_region)) %>% 
                                                    tidytable::left_join(lfreq_data %>% 
                                                                           tidytable::filter(sex != 3) %>% 
                                                                           tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code, sex)) %>% 
                                                                           tidytable::mutate(region = survey_region) %>%
                                                                           tidytable::bind_rows(lfreq_data %>% 
                                                                                                  tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                                                  tidytable::mutate(sex = 0,
                                                                                                                    region = survey_region)) %>% 
                                                                           tidytable::bind_rows(lfreq_data %>% 
                                                                                                  tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                                                  tidytable::mutate(sex = 4,
                                                                                                                    region = survey_region)) %>% 
                                                                           tidytable::bind_rows(lfreq_data %>% 
                                                                                                  tidytable::filter(sex != 3) %>% 
                                                                                                  tidytable::summarise(nhls = length(unique(hauljoin)), .by = c(year, species_code)) %>% 
                                                                                                  tidytable::mutate(sex = 12,
                                                                                                                    region = survey_region))))) %>% 
      tidytable::filter(iss > 0)
  }
}

#' calculate bias in bootstrapped age composition
#' 
#' @description
#' Computes the mean bias in bootstrapped population at age compared to original unsampled
#' population at age.
#' 
#' @param r_age iterated population at age
#' @param oga original population at age
#'
#' @export
#'
bias_age <- function(r_age,
                     oga) {
  
  # at region scale
  if(!("region" %in% names(r_age))){
    # compute average bias in pop'n estimates
    r_age %>%
      tidytable::bind_rows(r_age %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::summarise(agepop = mean(agepop), .by = c(year, species_code, sex, age)) %>% 
      tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
      tidytable::bind_rows(r_age %>% 
                             tidytable::filter(sex %in% c(1, 2)) %>% 
                             tidytable::summarise(agepop = mean(agepop), .by = c(year, species_code, sex, age)) %>% 
                             tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, species_code)) %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::drop_na() %>% 
      tidytable::select(-agepop) %>% 
      tidytable::left_join(oga %>% 
                             tidytable::bind_rows(oga %>% 
                                                    tidytable::filter(sex != 0) %>% 
                                                    tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
                             tidytable::bind_rows(oga %>% 
                                                    tidytable::filter(sex %in% c(1, 2)) %>% 
                                                    tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::select(-agepop)) %>% 
      tidytable::mutate(bias = (p_sim - p_og)) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))
  } else{ # at subregion scale
    # compute average bias in pop'n estimates
    r_age %>%
      tidytable::bind_rows(r_age %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop), .by = c(sim, region, year, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::summarise(agepop = mean(agepop), .by = c(year, region, species_code, sex, age)) %>% 
      tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, region, species_code, sex)) %>% 
      tidytable::bind_rows(r_age %>% 
                             tidytable::filter(sex %in% c(1, 2)) %>% 
                             tidytable::summarise(agepop = mean(agepop), .by = c(year, region, species_code, sex, age)) %>% 
                             tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, region, species_code)) %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::drop_na() %>% 
      tidytable::select(-agepop) %>% 
      tidytable::left_join(oga %>% 
                             tidytable::bind_rows(oga %>% 
                                                    tidytable::filter(sex != 0) %>% 
                                                    tidytable::summarise(agepop = sum(agepop), .by = c(year, region, species_code, age)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, region, species_code, sex)) %>% 
                             tidytable::bind_rows(oga %>% 
                                                    tidytable::filter(sex %in% c(1, 2)) %>% 
                                                    tidytable::mutate(p_og = agepop / sum(agepop), .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::select(-agepop)) %>% 
      tidytable::mutate(bias = (p_sim - p_og)) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(bias = mean(bias), .by = c(year, region, species_code, sex))
  }
}

#' calculate bias in bootstrapped conditional age-at-length
#' 
#' @description
#' Computes the mean bias in bootstrapped conditional age-at-length compared to original unsampled
#' conditional age-at-length.
#'
#' @param r_caal iterated conditional age-at-length
#' @param ogcaal original conditional age-at-length without any sampling
#'
#' @export
#'
bias_caal <- function(r_caal,
                      ogcaal) {
  
  # compute average relative bias in pop'n estimates (avg relative bias across age or length)
  r_caal %>%
    tidytable::left_join(ogcaal %>% 
                           tidytable::rename(og_caal = caal)) %>% 
    tidytable::mutate(bias = (caal - og_caal)) %>% 
    tidytable::drop_na() %>%
    tidytable::summarise(bias = mean(bias, na.rm = TRUE), .by = c(year, species_code, sex, length))
  
}

#' calculate bias in bootstrapped length composition
#' 
#' @description
#' Computes the mean bias in bootstrapped population at length compared to original unsampled
#' population at length
#'
#' @param r_length iterated population at length 
#' @param ogl original population at length without any sampling
#'
#' @export
#'
bias_length <- function(r_length,
                        ogl) {
  
  # at region scale
  if(!("region" %in% names(r_length))){
    # compute average relative bias in pop'n estimates (avg relative bias across age or length)
    r_length %>%
      tidytable::bind_rows(r_length %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::summarise(abund = mean(abund), .by = c(year, species_code, sex, length)) %>% 
      tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
      tidytable::bind_rows(r_length %>% 
                             tidytable::filter(sex %in% c(1, 2)) %>% 
                             tidytable::summarise(abund = mean(abund), .by = c(year, species_code, sex, length)) %>% 
                             tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, species_code)) %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::drop_na() %>% 
      tidytable::select(-abund) %>% 
      tidytable::left_join(ogl %>% 
                             tidytable::bind_rows(ogl %>% 
                                                    tidytable::filter(sex != 0) %>% 
                                                    tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::mutate(p_og = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                             tidytable::bind_rows(ogl %>% 
                                                    tidytable::filter(sex %in% c(1, 2)) %>% 
                                                    tidytable::mutate(p_og = abund / sum(abund), .by = c(year, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::select(-abund)) %>% 
      tidytable::mutate(bias = (p_sim - p_og)) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(bias = mean(bias), .by = c(year, species_code, sex))
  } else{ # at subregion scale
    # compute average relative bias in pop'n estimates (avg relative bias across age or length)
    r_length %>%
      tidytable::bind_rows(r_length %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(abund = sum(abund), .by = c(sim, year, region, species_code, length)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::summarise(abund = mean(abund), .by = c(year, region, species_code, sex, length)) %>% 
      tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, region, species_code, sex)) %>% 
      tidytable::bind_rows(r_length %>% 
                             tidytable::filter(sex %in% c(1, 2)) %>% 
                             tidytable::summarise(abund = mean(abund), .by = c(year, region, species_code, sex, length)) %>% 
                             tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, region, species_code)) %>% 
                             tidytable::mutate(sex = 12)) %>% 
      tidytable::drop_na() %>% 
      tidytable::select(-abund) %>% 
      tidytable::left_join(ogl %>% 
                             tidytable::bind_rows(ogl %>% 
                                                    tidytable::filter(sex != 0) %>% 
                                                    tidytable::summarise(abund = sum(abund), .by = c(year, region, species_code, length)) %>% 
                                                    tidytable::mutate(sex = 4)) %>% 
                             tidytable::mutate(p_og = abund / sum(abund), .by = c(year, region, species_code, sex)) %>% 
                             tidytable::bind_rows(ogl %>% 
                                                    tidytable::filter(sex %in% c(1, 2)) %>% 
                                                    tidytable::mutate(p_og = abund / sum(abund), .by = c(year, region, species_code)) %>% 
                                                    tidytable::mutate(sex = 12)) %>% 
                             tidytable::select(-abund)) %>% 
      tidytable::mutate(bias = (p_sim - p_og)) %>% 
      tidytable::drop_na() %>% 
      tidytable::summarise(bias = mean(bias), .by = c(year, region, species_code, sex))
  }
}

#' calculate mean and SD in length-at-age
#' 
#' @description
#' Calculate mean length-at-age and SD in length-at-age as aligned with gapindex package.
#'
#' @param r_age iterated population at age with mean length-at-age and sd in length-at-age output
#' @param oga original population at age without any sampling with mean length-at-age and sd in length-at-age output
#'
#' @export
#'
grwth_stats <- function(r_age,
                        oga) {
  
  # at region sacle
  if(!("region" %in% names(r_age))){
    # compute mean length-at-age and sd across bootstrap replicates and add orig values
    r_age %>%
      tidytable::bind_rows(r_age %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop),
                                                  mean_length = sum(agepop * mean_length) / sum(agepop),
                                                  sd_length = sum(agepop * sd_length) / sum(agepop),
                                                  .by = c(sim, year, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::summarise(mean_length_bs = mean(mean_length), 
                           sd_length_bs = sd(mean_length),
                           .by = c(year, species_code, sex, age)) %>% 
      tidytable::left_join(oga %>% 
                             tidytable::select(-agepop))
  } else{ # at subregion scale
    # compute mean length-at-age and sd across bootstrap replicates and add orig values
    r_age %>%
      tidytable::bind_rows(r_age %>% 
                             tidytable::filter(sex != 0) %>% 
                             tidytable::summarise(agepop = sum(agepop),
                                                  mean_length = sum(agepop * mean_length) / sum(agepop),
                                                  sd_length = sum(agepop * sd_length) / sum(agepop),
                                                  .by = c(sim, region, year, species_code, age)) %>% 
                             tidytable::mutate(sex = 4)) %>% 
      tidytable::summarise(mean_length_bs = mean(mean_length), 
                           sd_length_bs = sd(mean_length),
                           .by = c(year, region, species_code, sex, age)) %>% 
      tidytable::left_join(oga %>% 
                             tidytable::select(-agepop))
  }
}
