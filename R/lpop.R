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
                          .by = c(year, species_code, stratum, sex, length)) -> .unk
  
  # id hauls without lengths
  # see issue #35 for reasoning 
  cpue %>%
    tidytable::filter(numcpue > 0) %>% 
    tidytable::distinct(hauljoin, species_code)  %>% 
    tidytable::anti_join(lcomp %>%
                            tidytable::summarise(hauljoin = unique(hauljoin),
                                                  .by = c(species_code, stratum))) %>% 
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
      tidytable::select(year, species_code, stratum, hauljoin, sex, length, comp) %>%
      tidytable::bind_rows(lcomp) %>%
      tidytable::left_join(.pop) %>%
      tidytable::mutate(sz_pop = round(comp * abund, 0)) %>% 
      tidytable::filter(!is.na(sex)) -> .temp
  }
  
  # get annual pop'n @ length
  .temp %>%
    tidytable::summarise(abund = sum(sz_pop, na.rm = T), 
                         .by = c(year, species_code, length, sex)) %>%
    tidytable::left_join(lngs, .) %>%
    tidytable::drop_na()

}