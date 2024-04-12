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
    tidytable::rename(og_agepop = agepop) -> og
  sim_data %>% 
    tidytable::bind_rows(sim_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, age)) %>% 
                           tidytable::mutate(sex = 4)) -> sim
    
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
    tidytable::rename(og_agepop = agepop) -> og
  sim_data %>% 
    tidytable::bind_rows(sim_data %>% 
                           tidytable::filter(sex != 0) %>% 
                           tidytable::summarise(agepop = sum(agepop), .by = c(year, region, species_code, age)) %>% 
                           tidytable::mutate(sex = 4)) -> sim
  
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
                           tidytable::replace_na(list(agepop = 0)) %>% 
                           tidytable::replace_na(list(og_agepop = 0)) %>%
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
                           tidytable::replace_na(list(agepop = 0)) %>% 
                           tidytable::replace_na(list(og_agepop = 0)) %>%
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

