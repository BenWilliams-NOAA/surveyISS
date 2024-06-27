#' reduce total number of lengths or ages
#'
#' @param data lfreq or afreq dataframe
#' @param samples number of lengths to take
#' @param grp grouping variable(s)
#' @param type either age or length reduction (default = NULL)
#'
reduce_samples <- function(data, samples, grp = c('year', 'species_code', 'stratum', 'hauljoin'), type = NULL){
  
  data %>% 
    tidytable::mutate(id = .I) -> .inter
  
  if(type == 'length'){
    .inter %>% 
      .[,.SD[base::sample.int(.N, min(samples,.N), replace = FALSE)], by = grp] -> .new_samp
  } else if (type == 'age'){
    .inter %>% 
      tidytable::slice_sample(prop = samples, .by = c(year, species_code)) -> .new_samp
  }
  
  .inter %>%
    tidytable::anti_join(.new_samp, by = "id") %>% 
    dplyr::group_by(year, species_code, stratum, hauljoin, length) %>%
    dplyr::count(name = 'frequency') -> .redux_samp
  
  .out = list(data = .new_samp, nosamp = .redux_samp)
  
}