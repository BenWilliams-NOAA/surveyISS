#' length comp
#'
#' @param lfreq_un expanded length frequency data
#'
#' @return
#' @export
#'
#' @examples
lcomp <- function(lfreq_un) {
  lfreq_un %>%
    tidytable::summarise.(frequency = .N,
                          .by = c(year, species_code, stratum, hauljoin, sex, length, type)) %>% 
    tidytable::mutate.(nhauls = data.table::uniqueN(hauljoin),
                       .by = c(year, species_code, stratum, type)) %>%
    tidytable::mutate.(tot = sum(frequency),
                       .by = c(year, species_code, stratum, hauljoin, type)) %>%
    tidytable::summarise.(comp = sum(frequency) / mean(tot),
                          nhauls = mean(nhauls),
                          .by = c(year, species_code, stratum, hauljoin, sex, length, type))
}