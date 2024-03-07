#' compare with gap estimates of pop'n at age and length
#'
#' @param oga surveyISS computed pop'n at age
#' @param ogl surveyISS computed pop'n at length
#' @param gap_apop gap computed pop'n at age
#' @param gap_lpop gap computed pop'n at length
#' @param thresh_mapd threshold for desired max abs percent difference between gap and surveyISS pop'n numbers (default is 1 percent difference)
#' @param thresh_sad threshold for desired sum of absolute difference between gap and surveyISS proportions (default is 0.1)
#' 
#' @return
#' @export match_gap
#'
#' @examples

match_gap <- function(oga, ogl, gap_apop, gap_lpop, thresh_mapd = 1, thresh_sad = 0.1){

  # Compare between surveyISS and GAP estimates of pop'n at length
  
  gap_lpop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(ogl) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(match_num = abs(abund - abund_gap)/abund_gap * 100) %>% 
    tidytable::summarize(match_mapd_l = max(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_mapd_l = tidytable::case_when(match_mapd_l < thresh_mapd ~ 'Y',
                                                         match_mapd_l > thresh_mapd ~ 'N')) -> gap_match_len_n
  
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
    tidytable::summarize(match_sad_l = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_sad_l = tidytable::case_when(match_sad_l < thresh_sad ~ 'Y',
                                                        match_sad_l > thresh_sad ~ 'N')) -> gap_match_len_p
  
  # Compare between surveyISS and GAP estimates of pop'n at age
  
  gap_apop %>% 
    tidytable::select(-stratum) %>% 
    tidytable::rename(abund_gap = population_count) %>% 
    tidytable::left_join(oga) %>% 
    tidytable::drop_na() %>% 
    tidytable::mutate(match_num = abs(agepop - abund_gap)/abund_gap) %>% 
    tidytable::summarize(match_mapd_a = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_mapd_a = tidytable::case_when(match_mapd_a < thresh_mapd ~ 'Y',
                                                         match_mapd_a > thresh_mapd ~ 'N')) -> gap_match_age_n

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
    tidytable::summarize(match_sad_a = sum(match_num, na.rm = TRUE),
                         .by = c(year, species_code, sex)) %>% 
    tidytable::mutate(test_sad_a = tidytable::case_when(match_sad_a < thresh_sad ~ 'Y',
                                                        match_sad_a > thresh_sad ~ 'N')) -> gap_match_age_p
  
  list(gap_match_len_n, gap_match_age_n, gap_match_len_p, gap_match_age_p)
}





