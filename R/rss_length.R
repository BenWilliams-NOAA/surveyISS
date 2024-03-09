#' calculate realized sample size for length comps
#'
#' @param sim_data list of abundance by length data
#' @param og_data original abundance by length data (single list)
#'
#' @return
#' @export
#'
#' @examples
rss_length <- function(sim_data, og_data) {
  
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
    tidytable::summarise(rss = sum(p_og * (1 - p_og)) / sum((p_sim - p_og)^2),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::drop_na()

}
