#' Query data to run surveyISS
#' 
#' @description
#' Function that connects to AKFIN database (using afscdata package) and pulls catch per-unit-effort (cpue), 
#' length frequency (lfreq), age-length specimen (specimen), survey strata (strata), and species information (species)
#' data from the GAP_PRODUCTS tables
#' 
#' @param survey survey number for gap_products survey_definition_id (ai = 52, goa = 47, ebs = 98, nbs = 143, ebs slope = 78)
#' @param region region description for output data file (i.e., 'goa')
#' @param species species_codes, i.e., c(10110, 21740)
#' @param yrs minimum survey year to consider (default = NULL)
#'
#' @return a list of necessary data sources, dataframes are also written to csv files within the 'data/region' folder.
#' 
#' @export
#'
query_data <- function(survey,
                       region, 
                       species, 
                       yrs = NULL) {

  # create folder
  if (!dir.exists(paste0("data/", region))) {dir.create(paste0("data/", region))}

  # year switch
  if (is.null(yrs)) yrs <- 0
  
  # connect to database ----
  # get connected to akfin
  db = 'akfin'
  conn = afscdata::connect(db)
  
  # length frequency data ----

  cat("pulling length frequency...\n")
  
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_length')),
                      by = c('HAULJOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  species_code,
                  stratum,
                  hauljoin,
                  latitude_dd_start,
                  latitude_dd_end,
                  longitude_dd_start,
                  longitude_dd_end,
                  sex,
                  length_mm,
                  frequency) %>% 
    dplyr::filter(survey_definition_id %in% survey,
                  species_code %in% species,
                  year >= yrs) %>% 
    dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                  long_mid = (longitude_dd_start + longitude_dd_end) / 2) %>% 
    dplyr::select(-latitude_dd_start, 
                  -latitude_dd_end,
                  -longitude_dd_start, 
                  -longitude_dd_end, 
                  survey = survey_definition_id,
                  length = length_mm) %>% 
    dplyr::collect() %>% 
    vroom::vroom_write(here::here('data', region, "lfreq.csv"), 
                       delim = ',') -> lfreq
  
  # specimen data ----
  
  cat("pulling specimen...\n")
  
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen')),
                      by = c('HAULJOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  species_code,
                  stratum,
                  hauljoin,
                  latitude_dd_start,
                  latitude_dd_end,
                  longitude_dd_start,
                  longitude_dd_end,
                  sex,
                  length_mm,
                  age) %>% 
    dplyr::filter(survey_definition_id %in% survey,
                  species_code %in% species,
                  year >= yrs) %>% 
    dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                  long_mid = (longitude_dd_start + longitude_dd_end) / 2) %>% 
    dplyr::select(-latitude_dd_start, 
                  -latitude_dd_end,
                  -longitude_dd_start, 
                  -longitude_dd_end, 
                  survey = survey_definition_id,
                  length = length_mm) %>% 
    dplyr::collect() %>% 
    vroom::vroom_write(here::here('data', region, "specimen.csv"), 
                       delim = ',') -> specimen

  # cpue data ----
  
  cat("pulling cpue...\n")
  
  # get gap_products cpue
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cpue')),
                      by = c('HAULJOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  species_code,
                  stratum,
                  hauljoin,
                  latitude_dd_start,
                  latitude_dd_end,
                  longitude_dd_start,
                  longitude_dd_end,
                  cpue_nokm2) %>% 
    dplyr::filter(survey_definition_id %in% survey,
                  species_code %in% species,
                  year >= yrs) %>% 
    dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                  long_mid = (longitude_dd_start + longitude_dd_end) / 2) %>% 
    dplyr::select(-latitude_dd_start, 
                  -latitude_dd_end,
                  -longitude_dd_start, 
                  -longitude_dd_end, 
                  survey = survey_definition_id,
                  numcpue = cpue_nokm2) %>% 
    dplyr::collect() -> cpue
  
  # get gap_products catch
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>% 
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_catch')),
                      by = c('HAULJOIN')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::select(year,
                  survey_definition_id,
                  species_code,
                  stratum,
                  hauljoin,
                  latitude_dd_start,
                  latitude_dd_end,
                  longitude_dd_start,
                  longitude_dd_end,
                  distance_fished_km,
                  net_width_m,
                  count) %>% 
    dplyr::filter(survey_definition_id %in% survey,
                  species_code %in% species,
                  year >= yrs) %>% 
    dplyr::mutate(lat_mid = (latitude_dd_start + latitude_dd_end) / 2,
                  long_mid = (longitude_dd_start + longitude_dd_end) / 2,
                  numcpue = count / (distance_fished_km * (0.001 * net_width_m))) %>% 
    dplyr::select(-latitude_dd_start, 
                  -latitude_dd_end,
                  -longitude_dd_start, 
                  -longitude_dd_end, 
                  -count, 
                  -distance_fished_km, 
                  -net_width_m,
                  survey = survey_definition_id) %>% 
    dplyr::collect() -> cpue_calc

  # filling in 0's like gapindex and write cpue data
  tidytable::expand_grid(hauljoin = unique(cpue$hauljoin), species_code = species) %>% 
    tidytable::left_join(cpue %>% 
                           tidytable::select(-species_code, -numcpue) %>% 
                           tidytable::slice_head(n = 1, .by = c(year, survey, stratum, lat_mid, long_mid)), .by = hauljoin) %>% 
    tidytable::left_join(cpue_calc %>% 
                           tidytable::replace_na(list(numcpue = -1))) %>% 
    tidytable::replace_na(list(numcpue = 0)) %>% 
    vroom::vroom_write(here::here('data', region, "cpue.csv"), 
                       delim = ',') -> cpue
  
  # strata data ----
  
  cat("pulling strata...\n")
  
  # strata with area sizes
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(survey_definition_id %in% survey,
                  area_type == 'STRATUM') %>% 
    dplyr::select(survey = survey_definition_id,
                  design_year,
                  stratum = area_id,
                  area = area_km2) %>% 
    dplyr::collect() -> st_area
  
  # subregion level with description (e.g., wgoa, etc)
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(survey_definition_id %in% survey,
                  area_type == 'REGULATORY AREA') %>% 
    dplyr::select(area_id,
                  subarea_name = description,
                  design_year) %>% 
    dplyr::collect() -> subreg
  
  # strata within subregions
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_stratum_groups')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(survey_definition_id %in% survey) %>% 
    dplyr::select(stratum, 
                  area_id) %>% 
    dplyr::collect() -> st_subreg
  
  # join all to get strata with area sizes and subregion ids
  st_area %>% 
    tidytable::left_join(st_subreg %>% 
                           tidytable::left_join(subreg) %>% 
                           tidytable::drop_na()) %>%
    tidytable::filter(design_year == max(design_year), .by = c(stratum)) %>% 
    vroom::vroom_write(here::here('data', region, "strata.csv"), 
                       delim = ',') -> strata
  
  # species names ----
  
  cat("pulling species info...\n")
  
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_taxonomic_classification')) %>% 
    dplyr::rename_all(tolower) %>% 
    dplyr::filter(species_code %in% species) %>% 
    dplyr::select(species_code,
                  species_name,
                  common_name) %>% 
    vroom::vroom_write(here::here('data', region, "species.csv"), 
                       delim = ',') -> species

  DBI::dbDisconnect(conn)
  cat("finished.\n")
  list(lfreq = lfreq, specimen = specimen, cpue = cpue, strata = strata)
}


