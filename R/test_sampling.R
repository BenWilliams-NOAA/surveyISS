#' Reduce length frequency samples
#' 
#' @description
#' Function to test reductions in length sampling for whatever grouping is desired (default is reduction in haul-level sampling)
#' 
#' @param data length sample dataframe
#' @param samples number of lengths to take (default = NULL)
#' @param grp grouping variable(s) (default is to reduce haul-level sampling)
#'
#' @export
#' 
red_len_samples <- function(data, 
                            samples = NULL, 
                            grp = c('year', 'species_code', 'stratum', 'hauljoin')){
  
  # reduce sex-specific sampling 
  data %>% 
    tidytable::filter(sex != 0) %>% 
    .[,.SD[base::sample.int(.N, min(samples,.N), replace = TRUE)], by = grp] -> .inter
  
  # add combined sex samples
  .inter %>% 
    tidytable::bind_rows(.inter %>% 
                           tidytable::mutate(sex = 0))
}
  
#' Test age sampling magnitude
#' 
#' @description
#' Function to test changes in age sampling for whatever grouping is desired (default is changes at survey-level sampling)
#' 
#' @param data age specimen sample dataframe
#' @param rate proportion of age samples (default = NULL)
#' @param grp grouping variable(s) (default is to change survey-level sampling)
#'
#' @export
#' 
adj_age_samples <- function(data, 
                            rate = NULL, 
                            grp = c('year', 'species_code')){
  
  # change sex-specific sampling 
  data %>% 
    tidytable::drop_na() %>% 
    tidytable::filter(sex != 0) %>%
    tidytable::slice_sample(prop = rate, .by = grp, replace = TRUE) -> .inter
  
  # add combined sex samples
  .inter %>% 
    tidytable::bind_rows(.inter %>% 
                           tidytable::mutate(sex = 0))

}