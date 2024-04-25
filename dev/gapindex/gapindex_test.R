library(gapindex)


# get goa data ----
year_set = seq(1990,2023)
survey_set = "GOA"
spp_codes = c(10110, 10130, 10180, 20510, 21720, 21740, 30060, 30420, 30050, 30051, 30052, 30150, 30152, 10261, 10262, 10200)

gapdata <- gapindex::get_data(year_set = year_set,
                              survey_set = survey_set,
                              spp_codes = spp_codes,
                              pull_lengths = TRUE)

saveRDS(gapdata, here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))


# check speciment data
gapdata$specimen %>% 
  dplyr::rename_all(tolower)  %>% 
  summarise(rows = length(age))

vroom::vroom(here::here('data', 'specimen_goa.csv')) %>% 
  tidytable::drop_na() %>% 
  summarise(rows = length(age))

# check length frequency data
gapdata$size %>% 
  dplyr::rename_all(tolower) %>% 
  summarise(rows = length(frequency))

vroom::vroom(here::here('data', 'lfreq_goa.csv')) %>% 
  tidytable::drop_na() %>% 
  summarise(rows = length(frequency))


# check cpue data
cpue_gap <- gapindex::calc_cpue(gapdata)
cpue_iss <- vroom::vroom(here::here('data', 'cpue_goa.csv'))

cpue_gap %>% 
  dplyr::rename_all(tolower) %>% 
  filter(cpue_nokm2 == 0) %>%
  summarise(rows = length(cpue_nokm2), .by = year) %>% 
  tidytable::left_join(cpue_iss %>% 
                         filter(numcpue == 0) %>%
                         summarise(rows_iss = length(numcpue), .by = year)) %>% 
  tidytable::mutate(diff = rows - rows_iss)




tidytable::expand_grid(hauljoin = unique(cpue_iss$hauljoin), species_code = spp_codes) %>% 
  tidytable::left_join(cpue_iss %>% 
                         tidytable::replace_na(list(numcpue = -1)) %>%
                         tidytable::select(-species_code, -numcpue) %>% 
                         tidytable::slice_head(n = 1, .by = c(year, survey, stratum, lat_mid, long_mid)), .by = hauljoin) %>% 
  tidytable::left_join(cpue_iss %>% 
                         tidytable::replace_na(list(numcpue = -1))) %>% 
  tidytable::replace_na(list(numcpue = 0)) %>% 
  tidytable::filter(numcpue >= 0) -> cpue_iss_0







# mimicking gapindex

cruisedat <- gapdata$cruise %>% 
  dplyr::rename_all(tolower)
haul <- gapdata$haul %>% 
  dplyr::rename_all(tolower)
catch <- gapdata$catch %>% 
  dplyr::rename_all(tolower)
species <- gapdata$species %>% 
  dplyr::rename_all(tolower)

haul %>% 
  tidytable::left_join(cruisedat %>% 
                         tidytable::select(cruisejoin, survey, survey_definition_id, design_year)) -> dat_iss

all_combos_iss <- tidytable::expand_grid(hauljoin = dat_iss$hauljoin,
                                         species_code = spp_codes)

all_combos_iss %>% 
  tidytable::left_join(dat_iss, .by = hauljoin)

nrow(cpue_iss_0)


cruisedat <- gapdata$cruise
haul <- gapdata$haul
catch <- gapdata$catch
species <- gapdata$species

dat <- merge(x = haul,
             y = cruisedat[, c("CRUISEJOIN", "SURVEY", 
                               "SURVEY_DEFINITION_ID", "DESIGN_YEAR")],
             by = "CRUISEJOIN")

all_combos <- expand.grid(HAULJOIN = dat$HAULJOIN, 
                          SPECIES_CODE = sort(x = unique(x = species$GROUP)),
                          stringsAsFactors = F)

dat <- merge(x = all_combos, 
             y = dat, 
             by = "HAULJOIN", 
             all.x = TRUE)

nrow(cpue_gap)


cpue_gap %>% 
  dplyr::rename_all(tolower) %>% 
  filter(cpue_nokm2 == 0) %>%
  summarise(rows = length(cpue_nokm2), .by = year) %>% 
  tidytable::left_join(cpue_iss_0 %>% 
                         filter(numcpue == 0) %>%
                         summarise(rows_iss = length(numcpue), .by = year)) %>% 
  tidytable::mutate(diff = rows - rows_iss)




lfreq <- vroom::vroom(here::here('data', 'lfreq_goa.csv'))
strata <- vroom::vroom(here::here('data', 'strata_goa.csv'))
