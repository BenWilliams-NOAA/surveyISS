#' marginal length comp by haul
#'
#' @param lfreq_un expanded length frequency data
#' @param sex_spec determine whether computing for sex-specific or total comps
#'
#' @return
#' @export
#'
#' @examples
lcomp <- function(lfreq_un, sex_spec = TRUE) {
  
  # compute haul-sepcific marginal length comp for either sex-specific or sex-combined (total) data
  if(isTRUE(sex_spec)){ # for sex-specific data
  .lfreq_un %>%
    tidytable::summarise(frequency = .N,
                          .by = c(year, species_code, stratum, hauljoin, sex, length, type)) %>% 
    tidytable::mutate(nhauls = data.table::uniqueN(hauljoin),
                       .by = c(year, species_code, stratum, type)) %>%
    tidytable::mutate(tot = sum(frequency),
                       .by = c(year, species_code, stratum, hauljoin, type)) %>%
    tidytable::summarise(comp = sum(frequency) / mean(tot),
                          nhauls = mean(nhauls),
                          .by = c(year, species_code, stratum, hauljoin, sex, length, type))
  } else{ # for combined-sex (total) data
    .lfreq_un %>%
      tidytable::mutate(sex = 4) %>% 
      tidytable::summarise(frequency = .N,
                           .by = c(year, species_code, stratum, hauljoin, sex, length, type)) %>% 
      tidytable::mutate(nhauls = data.table::uniqueN(hauljoin),
                        .by = c(year, species_code, stratum, type)) %>%
      tidytable::mutate(tot = sum(frequency),
                        .by = c(year, species_code, stratum, hauljoin, type)) %>%
      tidytable::summarise(comp = sum(frequency) / mean(tot),
                           nhauls = mean(nhauls),
                           .by = c(year, species_code, stratum, hauljoin, sex, length, type))
  }
}