#'
core_samp <- function(data, samp, grp = c('year', 'species_code', 'stratum', 'hauljoin'), replace = FALSE) {
  data %>%
    .[,.SD[base::sample.int(.N, min(samp,.N), replace = replace)], by = grp]
}
