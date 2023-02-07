#' calculate effective sample size for age comps
#'
#' @param sim_data list of abundance by age data
#' @param og_data original abundance by age data (single list)
#'
#' @return
#' @export
#'
#' @examples
ess_age <- function(sim_data, og_data){
  
  og_data %>% 
    tidytable::rename.('og_unsexed' = unsexed,
                       'og_males' = males,
                       'og_females' = females) %>% 
    tidytable::mutate.(og_total = og_unsexed + og_males + og_females) -> og
  
  sim_data %>% 
    tidytable::mutate.(total = unsexed + males + females) %>% 
    tidytable::full_join.(og) %>% 
    tidytable::mutate.(og_unsexed = case_when.(is.na(og_unsexed) ~ 0, !is.na(og_unsexed) ~ og_unsexed),
                       og_males = case_when.(is.na(og_males) ~ 0, !is.na(og_males) ~ og_males),
                       og_females = case_when.(is.na(og_females) ~ 0, !is.na(og_females) ~ og_females),
                       og_total = case_when.(is.na(og_total) ~ 0, !is.na(og_total) ~ og_total),
                       unsexed = case_when.(is.na(unsexed) ~ 0, !is.na(unsexed) ~ unsexed),
                       males = case_when.(is.na(males) ~ 0, !is.na(males) ~ males),
                       females = case_when.(is.na(females) ~ 0, !is.na(females) ~ females),
                       total = case_when.(is.na(total) ~ 0, !is.na(total) ~ total)) %>%
    tidytable::mutate.(og_m = og_males / sum(og_males),
                       og_f = og_females / sum(og_females),
                       og_t = og_total/sum(og_total),
                       prop_m = males / sum(males),
                       prop_f = females / sum(females),
                       prop_t = total/sum(total),
                       .by = c(year, species_code)) %>% 
    tidytable::mutate.(ess_f = sum(prop_f * (1 - prop_f)) / sum((prop_f - og_f)^2),
                       ess_m = sum(prop_m * (1 - prop_m)) / sum((prop_m - og_m)^2),
                       ess_t = sum(prop_t * (1 - prop_t)) / sum((prop_t - og_t)^2),
                       .by = c(year, species_code)) %>%
    tidytable::select.(year, species_code, age, ess_f, ess_m, ess_t) %>% 
    tidytable::pivot_longer.(cols = c(ess_f, ess_m, ess_t), names_to = "ess") %>%
    dplyr::group_by(year, species_code, ess, value) %>%
    dplyr::distinct(value)

}