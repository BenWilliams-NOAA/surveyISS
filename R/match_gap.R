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
  
  gap_lpop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(ogl) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(match_num = abs(abund - abund_gap)/abund_gap) %>% 
    tidytable::summarize(match = mean(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test = tidytable::case_when(match < thresh ~ 'Y',
                                                  match > thresh ~ 'N')) -> gap_match_len_n
  
  # Compare between surveyISS and GAP estimates of proportions at length
  
  gap_lpop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(ogl) %>% 
    tidytable::filter(sex != 3) %>% 
    tidytable::mutate(p_gap = abund_gap / sum(abund_gap, na.rm = TRUE),
                      p = abund / sum(abund, na.rm = TRUE),
                      .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(match_num = abs(p_gap - p)) %>% 
    tidytable::summarize(match = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test = tidytable::case_when(match < thresh ~ 'Y',
                                                  match > thresh ~ 'N')) -> gap_match_len_p
  
  # Compare between surveyISS and GAP estimates of pop'n at age
  
  gap_apop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(oga) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(match_num = abs(agepop - abund_gap)/abund_gap) %>% 
    tidytable::summarize(match = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test = tidytable::case_when(match < thresh ~ 'Y',
                                                  match > thresh ~ 'N')) -> gap_match_age_n

  # Compare between surveyISS and GAP estimates of proportions at age
  
  gap_apop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(oga) %>% 
    tidytable::filter(sex != 3 & age > 0) %>% 
    tidytable::mutate(p_gap = abund_gap / sum(abund_gap, na.rm = TRUE),
                      p = agepop / sum(agepop, na.rm = TRUE),
                      .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(match_num = abs(p_gap - p)) %>% 
    tidytable::summarize(match = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test = tidytable::case_when(match < thresh ~ 'Y',
                                                  match > thresh ~ 'N')) -> gap_match_age_p
  
  
  # Write out results
  # vroom::vroom_write(gap_match_len_n, file = here::here("output", region, "gap_match_len_n.csv"), delim = ",")
  # vroom::vroom_write(gap_match_age_n, file = here::here("output", region, "gap_match_age_n.csv"), delim = ",")
  # vroom::vroom_write(gap_match_len_p, file = here::here("output", region, "gap_match_len_p.csv"), delim = ",")
  # vroom::vroom_write(gap_match_age_p, file = here::here("output", region, "gap_match_age_p.csv"), delim = ",")
  list(gap_match_len_n, gap_match_age_n, gap_match_len_p, gap_match_age_p)
}





