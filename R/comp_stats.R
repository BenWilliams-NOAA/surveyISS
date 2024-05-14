#' calculate realized sample size for age comps
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#'
#' @return
#' @export rss_age
#'
#' @examples
rss_age <- function(sim_data, 
                    og_data){
  
  # compute post-expansion total age pop'n and add to og and sim data
  og_data %>% 
    tidytable::bind_rows(og_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                           tidytable::mutate(sex = 4)) %>% 
    tidytable::select(year, species_code, sex, age, og_agepop = agepop) -> og
  sim_data %>% 
    tidytable::bind_rows(sim_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                           tidytable::mutate(sex = 4)) %>% 
    tidytable::select(year, species_code, sex, age, agepop)  -> sim
    
  # compute realized sample size
  sim %>% 
    tidytable::full_join(og) %>% 
    tidytable::replace_na(list(agepop = 0)) %>% 
    tidytable::replace_na(list(og_agepop = 0)) %>%
    tidytable::filter(sex != 3) %>%
    tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                      p_sim = agepop / sum(agepop),
                      .by = c(year, species_code, sex)) %>% 
    tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::drop_na() %>% 
  # compute realized sample size for female-male comps that sum to 1
    tidytable::bind_rows(sim %>% 
                           tidytable::full_join(og) %>% 
                           tidytable::replace_na(list(agepop = 0)) %>% 
                           tidytable::replace_na(list(og_agepop = 0)) %>%
                           tidytable::filter(sex %in% c(1,2)) %>% 
                           tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                                             p_sim = agepop / sum(agepop),
                                             .by = c(year, species_code)) %>%
                           tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                .by = c(year, species_code)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::mutate(sex = 12)) %>% 
    tidytable::arrange(year, species_code)
}

#' calculate realized sample size for age comps by region
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#'
#' @return
#' @export rss_age_reg
#'
#' @examples
rss_age_reg <- function(sim_data, 
                        og_data){
  
  # compute post-expansion total age pop'n and add to og and sim data
  og_data %>% 
    tidytable::bind_rows(og_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(agepop = sum(agepop), .by = c(year, region, species_code, age)) %>% 
                           tidytable::mutate(sex = 4)) %>% 
    tidytable::select(year, region, species_code, sex, age, og_agepop = agepop)  -> og
  sim_data %>% 
    tidytable::bind_rows(sim_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(agepop = sum(agepop), .by = c(year, region, species_code, age)) %>% 
                           tidytable::mutate(sex = 4)) %>% 
    tidytable::select(year, region, species_code, sex, age, agepop) -> sim
  
  # compute realized sample size  
  sim %>% 
    tidytable::full_join(og) %>% 
    tidytable::replace_na(list(agepop = 0)) %>% 
    tidytable::replace_na(list(og_agepop = 0)) %>%
    tidytable::filter(sex != 3) %>%
    tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                      p_sim = agepop / sum(agepop),
                      .by = c(year, region, species_code, sex)) %>% 
    tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                         .by = c(year, region, species_code, sex)) %>% 
    tidytable::drop_na() %>% 
    # compute realized sample size for female-male comps that sum to 1
    tidytable::bind_rows(sim %>% 
                           tidytable::full_join(og) %>% 
                           tidytable::replace_na(list(agepop = 0)) %>% 
                           tidytable::replace_na(list(og_agepop = 0)) %>%
                           tidytable::filter(sex %in% c(1,2)) %>% 
                           tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                                             p_sim = agepop / sum(agepop),
                                             .by = c(year, region, species_code)) %>%
                           tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                .by = c(year, region, species_code)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::mutate(sex = 12)) %>% 
    tidytable::arrange(year, species_code)
  
}

#' calculate realized sample size for conditional age-at-length
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#'
#' @return
#' @export rss_caal
#'
#' @examples
rss_caal <- function(sim_data, 
                     og_data){
  
  # compute realized sample size
  sim_data %>% 
    tidytable::full_join(og_data %>% 
                           tidytable::rename(og_caal = caal)) %>% 
    tidytable::replace_na(list(caal = 0)) %>% 
    tidytable::replace_na(list(og_caal = 0)) %>%
    tidytable::summarise(rss = sum(caal * (1 - caal)) / sum((caal - og_caal)^2),
                         .by = c(year, species_code, sex, length)) %>% 
    tidytable::drop_na()

}

#' calculate realized sample size for length comps
#'
#' @param sim_data list of abundance by length data
#' @param og_data original abundance by length data (single list)
#'
#' @return
#' @export rss_length
#'
#' @examples
rss_length <- function(sim_data,
                       og_data) {
  
  # compute post-expansion total length pop'n and add to og and sim data
  og_data %>% 
    tidytable::bind_rows(og_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>% 
                           tidytable::mutate(sex = 4)) %>% 
    tidytable::rename(og_abund = abund) -> og
  sim_data %>% 
    tidytable::bind_rows(sim_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(abund = sum(abund), .by = c(year, species_code, length)) %>%
                           tidytable::mutate(sex = 4)) -> sim
  
  # compute realized sample size
  sim %>% 
    tidytable::full_join(og) %>% 
    tidytable::replace_na(list(abund = 0)) %>%
    tidytable::filter(sex != 3) %>%
    tidytable::mutate(p_og = og_abund / sum(og_abund),
                      p_sim = abund / sum(abund),
                      .by = c(year, species_code, sex)) %>% 
    tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::drop_na() %>% 
    # compute realized sample size for female-male comps that sum to 1
    tidytable::bind_rows(sim %>% 
                           tidytable::full_join(og) %>% 
                           tidytable::replace_na(list(abund = 0)) %>% 
                           tidytable::replace_na(list(og_abund = 0)) %>%
                           tidytable::filter(sex %in% c(1,2)) %>% 
                           tidytable::mutate(p_og = og_abund / sum(og_abund),
                                             p_sim = abund / sum(abund),
                                             .by = c(year, species_code)) %>%
                           tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                .by = c(year, species_code)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::mutate(sex = 12)) %>% 
    tidytable::arrange(year, species_code)
  
}

#' calculate realized sample size for length comps by region
#'
#' @param sim_data list of abundance by length data
#' @param og_data original abundance by length data (single list)
#'
#' @return
#' @export rss_length_reg
#'
#' @examples
rss_length_reg <- function(sim_data,
                           og_data) {
  
  # compute post-expansion total length pop'n and add to og and sim data
  og_data %>% 
    tidytable::bind_rows(og_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(abund = sum(abund), .by = c(year, region, species_code, length)) %>% 
                           tidytable::mutate(sex = 4)) %>% 
    tidytable::rename(og_abund = abund) -> og
  sim_data %>% 
    tidytable::bind_rows(sim_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(abund = sum(abund), .by = c(year, region, species_code, length)) %>%
                           tidytable::mutate(sex = 4)) -> sim
  
  # compute realized sample size
  sim %>% 
    tidytable::full_join(og) %>% 
    tidytable::replace_na(list(abund = 0)) %>%
    tidytable::filter(sex != 3) %>%
    tidytable::mutate(p_og = og_abund / sum(og_abund),
                      p_sim = abund / sum(abund),
                      .by = c(year, region, species_code, sex)) %>% 
    tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                         .by = c(year, region, species_code, sex)) %>% 
    tidytable::drop_na() %>% 
    # compute realized sample size for female-male comps that sum to 1
    tidytable::bind_rows(sim %>% 
                           tidytable::full_join(og) %>% 
                           tidytable::replace_na(list(abund = 0)) %>% 
                           tidytable::replace_na(list(og_abund = 0)) %>%
                           tidytable::filter(sex %in% c(1,2)) %>% 
                           tidytable::mutate(p_og = og_abund / sum(og_abund),
                                             p_sim = abund / sum(abund),
                                             .by = c(year, region, species_code)) %>%
                           tidytable::summarise(rss = sum(p_sim * (1 - p_sim)) / sum((p_sim - p_og)^2),
                                                .by = c(year, region, species_code)) %>% 
                           tidytable::drop_na() %>% 
                           tidytable::mutate(sex = 12)) %>% 
    tidytable::arrange(year, species_code)
  
}


#' calculate age comp input sample size and add nominal sample size and number of sampled hauls to output
#'
#' @param rss_age iterated realized sample size
#' @param specimen_data input dataframe
#'
#' @return
#' @export iss_age
#'
#' @examples
iss_age <- function(rss_age,
                    specimen_data) {
  
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

}


#' calculate bias in bootstrapped age comp
#'
#' @param r_age iterated age pop'n
#' @param oga original age comps without any sampling
#'
#' @return
#' @export bias_age
#'
#' @examples
bias_age <- function(r_age,
                     oga) {

  # compute average relative bias in pop'n estimates (avg relative bias across age or length)
  r_age %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::bind_rows(r_age %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(agepop = sum(agepop), .by = c(sim, year, species_code, age)) %>% 
                           tidytable::mutate(sex = 4)) %>% 
    tidytable::summarise(agepop = mean(agepop), .by = c(year, species_code, sex, age)) %>% 
    tidytable::mutate(p_sim = agepop / sum(agepop), .by = c(year, species_code, sex)) %>% 
    tidytable::bind_rows(r_age %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
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
}

#' calculate mean length-at-age and sd
#'
#' @param r_age iterated age pop'n
#' @param oga original age comps without any sampling
#'
#' @return
#' @export bias_age
#'
#' @examples
grwth_stats <- function(r_age,
                        oga) {
  
  # compute mean length-at-age and sd across bootstrap replicates and add orig values
  r_age %>%
    tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
    tidytable::bind_rows(r_age %>% 
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
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
}


#' calculate length comp input sample size and add nominal sample size and number of sampled hauls to output
#'
#' @param rss_length iterated realized sample size
#' @param lfreq_data input dataframe
#'
#' @return
#' @export iss_length
#'
#' @examples
iss_length <- function(rss_length,
                       lfreq_data) {
  
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
  
}


#' calculate bias in bootstrapped length comp
#'
#' @param r_length iterated length pop'n
#' @param ogl original length comps without any sampling
#'
#' @return
#' @export bias_length
#'
#' @examples
bias_length <- function(r_length,
                        ogl) {
  
    # compute average relative bias in pop'n estimates (avg relative bias across age or length)
    r_length %>%
                           tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                           tidytable::bind_rows(r_length %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
                                                  tidytable::filter(sex != 0) %>% 
                                                  tidytable::summarise(abund = sum(abund), .by = c(sim, year, species_code, length)) %>% 
                                                  tidytable::mutate(sex = 4)) %>% 
                           tidytable::summarise(abund = mean(abund), .by = c(year, species_code, sex, length)) %>% 
                           tidytable::mutate(p_sim = abund / sum(abund), .by = c(year, species_code, sex)) %>% 
                           tidytable::bind_rows(r_length %>% 
                                                  tidytable::map_df(., ~as.data.frame(.x), .id = "sim") %>% 
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

}












