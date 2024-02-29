#' population at length
#'
#' @param lcomp length comp
#' @param cpue cpue data
#' @param lngs complete lengths by year
#'
#' @return
#' @export
#'
#' @examples
lpop <- function(lcomp, cpue, lngs) {
  lcomp %>%
    tidytable::summarise(comp = sum(comp) / mean(nhauls), 
                          .by = c(year, species_code, stratum, sex, length, type)) -> .unk
  
  # id hauls without lengths
  # see issue #35 for reasoning 
  cpue %>%
    tidytable::drop_na(catchjoin) %>% 
    tidytable::distinct(hauljoin, species_code)  %>% 
    tidytable::anti_join(lcomp %>%
                            tidytable::summarise(hauljoin = unique(hauljoin),
                                                  .by = c(species_code, stratum, type))) %>% 
    tidytable::left_join(cpue) -> .no_length
  
  # compute population est by year, species, strata
  cpue %>%
    tidytable::mutate(st_num = mean(numcpue) * area,
                       tot = sum(numcpue), 
                       .by = c(year, species_code, stratum)) %>%
    tidytable::summarise(abund = mean(numcpue) / tot * st_num,
                          .by = c(year, species_code, stratum, hauljoin)) -> .pop
  
  # if there are any samples w/o lengths rejoin them
  if(nrow(.no_length) == 0){
    lcomp %>%
      tidytable::left_join(.pop) %>%
      tidytable::mutate(sz_pop = round(comp * abund, 0)) %>% 
      tidytable::filter(!is.na(sex)) -> .temp
  } else {
    .no_length %>%
      tidytable::left_join(.unk) %>%
      tidytable::select(year, species_code, stratum, hauljoin, sex, length, comp, type) %>%
      tidytable::bind_rows(lcomp) %>%
      tidytable::left_join(.pop) %>%
      tidytable::mutate(sz_pop = round(comp * abund, 0)) %>% 
      tidytable::filter(!is.na(sex)) -> .temp
  }
  
  .temp %>%
    tidytable::filter(sex == 3) %>%
    tidytable::count(c(year, species_code), name = 'n') -> .sex_cnt_sz
  
  if("stratum" %in% names(lngs)){
    .temp %>%
      tidytable::summarise(abund = sum(sz_pop, na.rm = T), 
                            .by = c(year, species_code, stratum, length, sex)) %>%
      tidytable::pivot_wider(names_from = sex, values_from = abund) %>%
      tidytable::left_join(lngs, .) %>%
      tidytable::mutate(across(.cols = c(`1`, `2`, `3`), ~tidytable::replace_na(.x, 0))) %>%
      tidytable::drop_na() -> .temp2
    
    if(length(.sex_cnt_sz$n)>0){
      .temp2 %>% 
        tidytable::select(year, species_code, stratum, length, males = `1`, females = `2`, unsexed = `3`) 
    } else{
      .temp2 %>% 
        tidytable::select(year, species_code, stratum, length, males = `1`, females = `2`) %>%
        tidytable::mutate(unsexed = 0) 
    }
  } else {
    if(length(.sex_cnt_sz$n)>0){
      if(length(unique(.temp$sex)) > 1){
        .temp %>%
          tidytable::summarise(abund = sum(sz_pop, na.rm = T), 
                                .by = c(year, species_code, length, sex, type)) %>%
          tidytable::pivot_wider(names_from = sex, values_from = abund) %>%
          tidytable::left_join(lngs, .) %>%
          tidytable::mutate(across(.cols = c(`1`, `2`, `3`), ~tidytable::replace_na(.x, 0))) %>%
          tidytable::drop_na() %>% 
          tidytable::select(year, species_code, length, males = `1`, females = `2`, unsexed = `3`, type)
      } else{
        .temp %>%
          tidytable::summarise(abund = sum(sz_pop, na.rm = T), 
                                .by = c(year, species_code, length, sex, type)) %>%
          tidytable::pivot_wider(names_from = sex, values_from = abund) %>%
          tidytable::left_join(lngs, .) %>%
          tidytable::mutate(across(.cols = c(`1`), ~tidytable::replace_na(.x, 0))) %>%
          tidytable::drop_na() %>% 
          tidytable::select(year, species_code, length, total = `1`, type)
      }
    } else{
      if(length(unique(.temp$sex)) > 1){
        .temp %>%
          tidytable::summarise(abund = sum(sz_pop, na.rm = T), 
                                .by = c(year, species_code, length, sex, type)) %>%
          tidytable::pivot_wider(names_from = sex, values_from = abund) %>%
          tidytable::left_join(lngs, .) %>%
          tidytable::mutate(across(.cols = c(`1`, `2`), ~tidytable::replace_na(.x, 0))) %>%
          tidytable::drop_na() %>%  
          tidytable::select(year, species_code, length, males = `1`, females = `2`, type) %>%
          tidytable::mutate(unsexed = 0) 
      } else{
        .temp %>%
          tidytable::summarise(abund = sum(sz_pop, na.rm = T), 
                                .by = c(year, species_code, length, sex, type)) %>%
          tidytable::pivot_wider(names_from = sex, values_from = abund) %>%
          tidytable::left_join(lngs, .) %>%
          tidytable::mutate(across(.cols = c(`1`), ~tidytable::replace_na(.x, 0))) %>%
          tidytable::drop_na() %>%  
          tidytable::select(year, species_code, length, total = `1`, type) 
      }
    }
  }
}