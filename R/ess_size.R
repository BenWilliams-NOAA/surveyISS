#' calculate effective sample size for length comps
#'
#' @param sim_data list of abundance by length data
#' @param og_data original abundance by length data (single list)
#' @param sex_spec determine whether to do sex specific or total comps (default = TRUE)
#'
#' @return
#' @export
#'
#' @examples
ess_size <- function(sim_data, og_data, sex_spec) {
  
  if(isTRUE(sex_spec)){
    og_data %>% 
      tidytable::rename('og_unsexed' = unsexed,
                         'og_males' = males,
                         'og_females' = females) %>% 
      tidytable::mutate(og_total = og_unsexed + og_males + og_females) -> og
    
    sim_data %>% 
      tidytable::mutate(total = unsexed + males + females) %>% 
      tidytable::full_join(og) %>% 
      tidytable::drop_na() %>% 
      tidytable::replace_na(list(og_unsexed = 0, og_males = 0, og_females = 0, og_total = 0, 
                                 unsexed = 0, males = 0, females = 0, total = 0)) %>%
      tidytable::mutate(og_m = og_males / sum(og_males),
                         og_f = og_females / sum(og_females),
                         og_t = og_total/sum(og_total),
                         prop_m = males / sum(males),
                         prop_f = females / sum(females),
                         prop_t = total/sum(total),
                         .by = c(year, species_code, type)) %>% 
      tidytable::mutate(ess_f = sum(og_f * (1 - og_f)) / sum((prop_f - og_f)^2),
                         ess_m = sum(og_m * (1 - og_m)) / sum((prop_m - og_m)^2),
                         ess_t = sum(og_t * (1 - og_t)) / sum((prop_t - og_t)^2),
                         .by = c(year, species_code, type)) %>%
      tidytable::pivot_longer(cols = c(ess_f, ess_m, ess_t), names_to = "ess") %>%
      tidytable::distinct(year, species_code, type, ess, value)
  } else{
    og_data %>% 
      tidytable::rename('og_total' = total) -> og
    
    sim_data %>% 
      tidytable::full_join(og) %>% 
      tidytable::drop_na() %>% 
      tidytable::replace_na(list(og_total = 0, total = 0)) %>%
      tidytable::mutate(og_t = og_total/sum(og_total),
                         prop_t = total/sum(total),
                         .by = c(year, species_code, type)) %>% 
      tidytable::mutate(ess_t = sum(og_t * (1 - og_t)) / sum((prop_t - og_t)^2),
                         .by = c(year, species_code, type)) %>%
      tidytable::pivot_longer(cols = c(ess_t), names_to = "ess") %>%
      tidytable::distinct(year, species_code, type, ess, value)
  }
}
