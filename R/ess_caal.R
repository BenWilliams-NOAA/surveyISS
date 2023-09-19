#' calculate effective sample size for age comps
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#' @param sex_spec determine whether to do sex specific or total comps (default = TRUE)
#'
#' @return
#' @export
#'
#' @examples
ess_caal <- function(sim_data, og_data, sex_spec){
  
  if(isTRUE(sex_spec)){
    og_data %>% 
      tidytable::rename('og_total' = total,
                         'og_males' = males,
                         'og_females' = females) -> og
    sim_data %>% 
      tidytable::full_join(og) %>% 
      tidytable::replace_na(list(og_males = 0, og_females = 0, og_total = 0, 
                                 males = 0, females = 0, total = 0)) %>%

      tidytable::mutate(ess_f = sum(og_females * (1 - og_females)) / sum((females - og_females)^2),
                         ess_m = sum(og_males * (1 - og_males)) / sum((males - og_males)^2),
                         ess_t = sum(og_total * (1 - og_total)) / sum((total - og_total)^2),
                         .by = c(year, species_code, type, length)) %>%
      tidytable::drop_na() %>% 
      tidytable::filter(is.finite(ess_t) & is.finite(ess_f) & is.finite(ess_m)) %>% 
      tidytable::select(year, species_code, type, length, ess_f, ess_m, ess_t) %>% 
      tidytable::pivot_longer(cols = c(ess_f, ess_m, ess_t), names_to = "ess") %>%
      tidytable::distinct(year, species_code, type, length, ess, value) 
  } else{
    og_data %>% 
      tidytable::rename('og_total' = total) -> og
    
    sim_data %>% 
      tidytable::full_join(og) %>% 
      tidytable::drop_na() %>%
      tidytable::replace_na(list(og_total = 0, total = 0)) %>%
      tidytable::rename(og_t = 'og_total',
                        prop_t = 'total') %>% 
      tidytable::mutate(ess_t = sum(og_t * (1 - og_t)) / sum((prop_t - og_t)^2),
                         .by = c(year, species_code, length, type)) %>%
      tidytable::drop_na() %>% 
      tidytable::filter(is.finite(ess_t)) %>% 
      tidytable::select(year, species_code, type, length, ess_t) %>% 
      tidytable::pivot_longer(cols = c(ess_t), names_to = "ess") %>%
      tidytable::distinct(year, species_code, type, length, ess, value)
    
  }
  
}