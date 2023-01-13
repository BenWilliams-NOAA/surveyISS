#' calculate effective sample size for length comps
#'
#' @param sim_data list of abundance by length data
#' @param og_data original abundance by length data (single list)
#' @param strata switch to perform analysis at regional or strata level (default = NULL)
#'
#' @return
#' @export
#'
#' @examples
ess_size <- function(sim_data, og_data, strata) {
  
  if ("stratum" %in% names(og_data) & isFALSE(strata) |
      "stratum" %in% names(sim_data) & isFALSE(strata)) {
    stop("check your strata")
  }
  
  if (isTRUE(strata)) {
    og_data %>%
      tidytable::mutate.(og_m = males / sum(males),
              og_f = females / sum(females),
              og_t = (males + females + unsexed)/ (sum(males) + sum(females) + sum(unsexed)),
              .by = c(year, species_code, stratum)) %>%
      tidytable::select.(year, species_code, stratum, length, og_m, og_f, og_t) -> og_prop
    
    
    sim_data %>%
      tidytable::mutate.(prop_m = males / sum(males),
              prop_f = females / sum(females),
              prop_t = (males + females + unsexed)/(sum(males) + sum(females) + sum(unsexed)),
              .by = c(year, species_code, stratum)) %>%
      tidytable::left_join.(og_prop) %>%
      tidytable::summarise.(ess_f = sum(prop_f * (1 - prop_f)) / sum((prop_f - og_f)^2),
                 ess_m = sum(prop_m * (1 - prop_m)) / sum((prop_m - og_m)^2),
                 ess_t = sum(prop_t * (1 - prop_t)) / sum((prop_t - og_t)^2),
                 .by = c(year, species_code, stratum)) %>%
      tidytable::drop_na.() %>%
      tidytable::pivot_longer.(cols = c(ess_f, ess_m, ess_t), names_to = "ess") %>%
      tidytable::mutate.(in_out = ifelse(is.infinite(value), "out", "in")) %>%
      tidytable::select.(year, species_code, stratum, ess, value, in_out) %>% 
      unique(., by = c('year', 'species_code', 'stratum', 'ess', 'value', 'in_out'))
    
  } else {
    
    og_data %>%
      tidytable::mutate.(og_m = males / sum(males),
              og_f = females / sum(females),
              og_t = (males + females + unsexed)/ (sum(males) + sum(females) + sum(unsexed)),
              .by = c(year, species_code)) %>%
      tidytable::select.(year, species_code, length, og_m, og_f, og_t) -> og_prop
    
    sim_data %>%
      tidytable::mutate.(prop_m = males / sum(males),
              prop_f = females / sum(females),
              prop_t = (males + females + unsexed) / 
                (sum(males) + sum(females) + sum(unsexed)),
              .by = c(year, species_code)) %>%
      tidytable::left_join.(og_prop) %>%
      tidytable::mutate.(ess_f = sum(prop_f * (1 - prop_f)) / sum((prop_f - og_f)^2),
              ess_m = sum(prop_m * (1 - prop_m)) / sum((prop_m - og_m)^2),
              ess_t = sum(prop_t * (1 - prop_t)) / sum((prop_t - og_t)^2),
              .by = c(year, species_code)) %>%
      tidytable::drop_na.() %>%
      tidytable::pivot_longer.(cols = c(ess_f, ess_m, ess_t), names_to = "ess") %>%
      tidytable::mutate.(in_out = ifelse(is.infinite(value), "out", "in")) %>%
      tidytable::select.(year, species_code, ess, value, in_out) %>% 
      unique(., by = c('year', 'species_code', 'ess', 'value', 'in_out'))
    
  }
  
}
