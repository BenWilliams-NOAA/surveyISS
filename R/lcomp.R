#' marginal length comp by haul
#'
#' @param lfreq_un expanded length frequency data
#'
#' @return
#' @export
#'
#' @examples
lcomp <- function(lfreq_un) {
  
  # compute haul-specific marginal length comp for sex-specific data
  lfreq_un %>%
    tidytable::filter(sex != 0) %>% 
    group_by(year, species_code, stratum, hauljoin, sex, length) %>% 
    tidytable::summarise(frequency = .N) %>% 
    ungroup %>% 
    tidytable::mutate(nhauls = data.table::uniqueN(hauljoin),
                      .by = c(year, species_code, stratum)) %>%
    tidytable::mutate(tot = sum(frequency),
                      .by = c(year, species_code, stratum, hauljoin)) %>%
    tidytable::summarise(comp = sum(frequency) / mean(tot),
                         nhauls = mean(nhauls),
                         .by = c(year, species_code, stratum, hauljoin, sex, length)) %>% 
    # bind with haul-specific marginal length comps for total (combined sex) data
    tidytable::bind_rows(lfreq_un %>%
                           tidytable::filter(sex == 0) %>% 
                           group_by(year, species_code, stratum, hauljoin, sex, length) %>% 
                           tidytable::summarise(frequency = .N) %>% 
                           ungroup %>% 
                           tidytable::mutate(nhauls = data.table::uniqueN(hauljoin),
                                             .by = c(year, species_code, stratum)) %>%
                           tidytable::mutate(tot = sum(frequency),
                                             .by = c(year, species_code, stratum, hauljoin)) %>%
                           tidytable::summarise(comp = sum(frequency) / mean(tot),
                                                nhauls = mean(nhauls),
                                                .by = c(year, species_code, stratum, hauljoin, sex, length)))

}