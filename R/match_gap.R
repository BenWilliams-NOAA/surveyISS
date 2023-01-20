#' compare with gap estimates of pop'n at age and length
#'
#' @param oga surveyISS computed pop'n at age
#' @param ogl surveyISS computed pop'n at length
#' @param gap_apop gap computed pop'n at age
#' @param gap_lpop gap computed pop'n at length
#' @param thresh threshold for desired match between gap and surveyISS (set at 0.01 percent difference)
#' @param region region for which your comparing
#' 
#' @return
#' @export match_gap
#'
#' @examples

match_gap <- function(oga, ogl, gap_apop, gap_lpop, thresh = 0.01, region = NULL){

  # Compare between surveyISS and GAP estimates of pop'n at length
  
  ogl %>% 
    mutate.(total = males + females + unsexed) %>% 
    filter.(total > 0) -> .lpop
  
  gap_lpop %>% 
    rename.(gap_males = males, gap_females = females, gap_unsexed = unsexed, gap_total = total) %>% 
    select.(-summary_area) %>% 
    left_join.(.lpop) %>% 
    mutate.(match_males_num = abs(gap_males - males)/gap_males,
            match_females_num = abs(gap_females - females)/gap_females,
            match_unsexed_num = abs(gap_unsexed - unsexed)/gap_unsexed,
            match_total_num = abs(gap_total - total)/gap_total,
            .by = c(year, species_code, length)) %>% 
    summarize.(match_males = sum(match_males_num, na.rm = TRUE),
               match_females = sum(match_females_num, na.rm = TRUE),
               match_total = sum(match_total_num, na.rm = TRUE),
               .by = c(year, species_code)) %>% 
    mutate.(test_m = case_when(match_males < thresh ~ 'Y',
                               match_males > thresh ~ 'N'),
            test_f = case_when(match_females < thresh ~ 'Y',
                               match_females > thresh ~ 'N'),
            test_t = case_when(match_total < thresh ~ 'Y',
                               match_total > thresh ~ 'N')) -> gap_match_len_n
  
  # Compare between surveyISS and GAP estimates of proportions at length
  
  gap_lpop %>% 
    rename.(gap_males = males, gap_females = females, gap_unsexed = unsexed, gap_total = total) %>% 
    select.(-summary_area) %>% 
    left_join.(.lpop) %>% 
    select.(-gap_unsexed, -unsexed) %>% 
    mutate.(gap_males = gap_males / sum(gap_males, na.rm = TRUE),
            gap_females = gap_females / sum(gap_females, na.rm = TRUE),
            gap_total = gap_males / sum(gap_males, na.rm = TRUE),
            males = males / sum(males, na.rm = TRUE),
            females = females / sum(females, na.rm = TRUE),
            total = males / sum(males, na.rm = TRUE),
            .by = c(year, species_code)) %>% 
    mutate.(match_males_num = abs(gap_males - males),
            match_females_num = abs(gap_females - females),
            match_total_num = abs(gap_total - total),
            .by = c(year, species_code, length)) %>% 
    summarize.(match_males = sum(match_males_num, na.rm = TRUE),
               match_females = sum(match_females_num, na.rm = TRUE),
               match_total = sum(match_total_num, na.rm = TRUE),
               .by = c(year, species_code)) %>% 
    mutate.(test_m = case_when(match_males < thresh ~ 'Y',
                               match_males > thresh ~ 'N'),
            test_f = case_when(match_females < thresh ~ 'Y',
                               match_females > thresh ~ 'N'),
            test_t = case_when(match_total < thresh ~ 'Y',
                               match_total > thresh ~ 'N')) -> gap_match_len_p
  
  # Compare between surveyISS and GAP estimates of pop'n at age
  
  oga %>% 
    mutate.(total = males + females + unsexed) %>% 
    filter.(total > 0) -> .apop
  
  gap_apop %>%
    pivot_wider.(names_from = sex, values_from = agepop) %>% 
    rename.(gap_males = '1', gap_females = '2', gap_unsexed = '3', year = survey_year) %>% 
    select.(-mean_length, -standard_deviation, -survey) %>%  
    filter.(age >= 0) %>% 
    summarize.(gap_males = sum(gap_males, na.rm = TRUE),
               gap_females = sum(gap_females, na.rm = TRUE),
               gap_unsexed = sum(gap_unsexed, na.rm = TRUE),
               .by = c(year, species_code, age)) %>% 
    mutate.(gap_total = gap_males + gap_females + gap_unsexed) %>% 
    left_join.(.apop) %>% 
    mutate.(match_males_num = abs(gap_males - males)/gap_males,
            match_females_num = abs(gap_females - females)/gap_females,
            match_unsexed_num = abs(gap_unsexed - unsexed)/gap_unsexed,
            match_total_num = abs(gap_total - total)/gap_total,
            .by = c(year, species_code, age)) %>% 
    summarize.(match_males = sum(match_males_num, na.rm = TRUE),
               match_females = sum(match_females_num, na.rm = TRUE),
               match_total = sum(match_total_num, na.rm = TRUE),
               .by = c(year, species_code)) %>% 
    mutate.(test_m = case_when(match_males < thresh ~ 'Y',
                               match_males > thresh ~ 'N'),
            test_f = case_when(match_females < thresh ~ 'Y',
                               match_females > thresh ~ 'N'),
            test_t = case_when(match_total < thresh ~ 'Y',
                               match_total > thresh ~ 'N')) -> gap_match_age_n
  
  # Compare between surveyISS and GAP estimates of proportions at age
  
  gap_apop %>%
    pivot_wider.(names_from = sex, values_from = agepop) %>% 
    rename.(gap_males = '1', gap_females = '2', gap_unsexed = '3', year = survey_year) %>% 
    select.(-mean_length, -standard_deviation, -survey) %>%  
    filter.(age >= 0) %>% 
    summarize.(gap_males = sum(gap_males, na.rm = TRUE),
               gap_females = sum(gap_females, na.rm = TRUE),
               gap_unsexed = sum(gap_unsexed, na.rm = TRUE),
               .by = c(year, species_code, age)) %>% 
    mutate.(gap_total = gap_males + gap_females + gap_unsexed) %>% 
    left_join.(.apop) %>% 
    mutate.(gap_males = gap_males / sum(gap_males, na.rm = TRUE),
            gap_females = gap_females / sum(gap_females, na.rm = TRUE),
            gap_total = gap_males / sum(gap_males, na.rm = TRUE),
            males = males / sum(males, na.rm = TRUE),
            females = females / sum(females, na.rm = TRUE),
            total = males / sum(males, na.rm = TRUE),
            .by = c(year, species_code)) %>% 
    mutate.(match_males_num = abs(gap_males - males),
            match_females_num = abs(gap_females - females),
            match_total_num = abs(gap_total - total),
            .by = c(year, species_code, age)) %>% 
    summarize.(match_males = sum(match_males_num, na.rm = TRUE),
               match_females = sum(match_females_num, na.rm = TRUE),
               match_total = sum(match_total_num, na.rm = TRUE),
               .by = c(year, species_code)) %>% 
    mutate.(test_m = case_when(match_males < thresh ~ 'Y',
                               match_males > thresh ~ 'N'),
            test_f = case_when(match_females < thresh ~ 'Y',
                               match_females > thresh ~ 'N'),
            test_t = case_when(match_total < thresh ~ 'Y',
                               match_total > thresh ~ 'N')) -> gap_match_age_p
  
  
  # Write out results
  vroom::vroom_write(gap_match_len_n, file = here::here("output", region, "gap_match_len_n.csv"), delim = ",")
  vroom::vroom_write(gap_match_age_n, file = here::here("output", region, "gap_match_age_n.csv"), delim = ",")
  vroom::vroom_write(gap_match_len_p, file = here::here("output", region, "gap_match_len_p.csv"), delim = ",")
  vroom::vroom_write(gap_match_age_p, file = here::here("output", region, "gap_match_age_p.csv"), delim = ",")
  
}





