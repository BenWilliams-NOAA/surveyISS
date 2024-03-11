#' calculate realized sample size for age comps
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#'
#' @return
#' @export
#'
#' @examples
rss_age <- function(sim_data, og_data){
  
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
    
  # compute realized sample size and relative bias  
  sim %>% 
    tidytable::full_join(og) %>% 
    tidytable::replace_na(list(agepop = 0)) %>%
    tidytable::filter(sex != 3) %>%
    tidytable::mutate(p_og = og_agepop / sum(og_agepop),
                      p_sim = agepop / sum(agepop),
                      rel_bias = (agepop - og_agepop) / og_agepop,
                      .by = c(year, species_code, sex)) %>% 
    tidytable::summarise(rss = sum(p_og * (1 - p_og)) / sum((p_sim - p_og)^2),
                         rel_bias = mean(rel_bias),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::drop_na()

}