library(gapindex)
library(tidyverse)



# gapindex::calc_sizecomp ----

gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_nbs.RDS'))


cruise <- gapdata$cruise
haul <- gapdata$haul
cpue <- gapindex::calc_cpue(gapdata)
size <- gapdata$size
racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                        cpue)


cpue %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(species_code == 21740 & year == 2023 & stratum == 70) %>% 
  tidytable::select(year, hauljoin, stratum, cpue_nokm2) %>% 
  print(n = 150)
  


haul <- merge(x = haul, 
              y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
              by = "CRUISEJOIN")

size <- merge(x = size[, c("HAULJOIN",  "SPECIES_CODE", "LENGTH",
                           "FREQUENCY", "SEX")], 
              y = haul[, c("HAULJOIN", "SURVEY", "YEAR", "STRATUM")],
              by = "HAULJOIN")

s_ijk <- stats::aggregate(
  FREQUENCY ~ SURVEY + YEAR + HAULJOIN + SPECIES_CODE,
  data = size,
  FUN = sum)
names(x = s_ijk)[names(x = s_ijk) == "FREQUENCY"] <- "s_ijk"

size <- merge(x = size, 
              y = s_ijk[, c("SPECIES_CODE", "HAULJOIN", "s_ijk")],
              by = c("SPECIES_CODE", "HAULJOIN"))
 
size <- merge(x = size, 
              y = cpue[, c("HAULJOIN", "SPECIES_CODE", "CPUE_NOKM2")],
              by = c("HAULJOIN", "SPECIES_CODE"))

size$S_ijklm <- size$FREQUENCY / size$s_ijk * size$CPUE_NOKM2

size %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::filter(species_code == 21740 & year == 2021 & stratum == 71) %>% 
  tidytable::arrange(-length, sex) %>% 
  print(n = 150)




# using goa fill method

missing_hauljoins <- data.frame()

for (ispp in gapdata$species$SPECIES_CODE){
  temp_hauljoins <- unique(x = size$HAULJOIN[size$SPECIES_CODE == ispp])
  
  missing_hauljoins <- 
    rbind(missing_hauljoins,
          subset(x = cpue,
                 subset = CPUE_NOKM2 > 0 & 
                   SPECIES_CODE == ispp &
                   !HAULJOIN %in% temp_hauljoins))
}
 
size$p_ijklm <- with(size, FREQUENCY / s_ijk)

imputted_size <- 
  stats::aggregate(p_ijklm ~ SURVEY + YEAR + STRATUM + SPECIES_CODE + 
                     SEX + LENGTH,
                   data = size,
                   FUN = sum)

hauls_w_length_by_stratum <-
  stats::aggregate(HAULJOIN ~ SURVEY + YEAR + STRATUM + SPECIES_CODE,
                   data = size,
                   FUN = function(x) length(x = unique(x = x)))
names(hauls_w_length_by_stratum)[
  names(hauls_w_length_by_stratum) == "HAULJOIN"
] <- "HAULJOIN_TOTAL"

imputted_size <- merge(x = imputted_size,
                       y = hauls_w_length_by_stratum,
                       by = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE"))
imputted_size$p_ijklm <- with(imputted_size, p_ijklm / HAULJOIN_TOTAL)

imputted_size <- merge(x = missing_hauljoins,
                       y = imputted_size,
                       by = c("SURVEY", "YEAR", "STRATUM", "SPECIES_CODE"))

imputted_size$S_ijklm <- with(imputted_size, p_ijklm * CPUE_NOKM2)

size <- rbind(size[, c("HAULJOIN", "STRATUM", "SPECIES_CODE", "LENGTH", 
                       "SEX", "SURVEY", "YEAR", "CPUE_NOKM2", "S_ijklm")], 
              imputted_size[, c("HAULJOIN", "STRATUM", "SPECIES_CODE", 
                                "LENGTH", "SEX", "SURVEY", "YEAR", 
                                "CPUE_NOKM2", "S_ijklm")])
# finish goa fill method




S_ik <- stats::aggregate(S_ijklm ~ SURVEY + YEAR + STRATUM + SPECIES_CODE,
                         data = size,
                         FUN = sum)
names(S_ik)[names(S_ik) == "S_ijklm"] <- "S_ik"

S_iklm <- 
  stats::aggregate(S_ijklm ~ SURVEY + YEAR + STRATUM + SPECIES_CODE + 
                     LENGTH + SEX,
                   data = size,
                   FUN = sum)
names(S_iklm)[names(S_iklm) == "S_ijklm"] <- "S_iklm" 

S_iklm <- merge(x = S_iklm,
                y = S_ik,
                by = c("SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE"))


S_iklm <- merge(x = S_iklm,
                y = racebase_stratum_popn[, c("SURVEY", "YEAR", 'STRATUM', 
                                              "SPECIES_CODE", 
                                              "POPULATION_COUNT")],
                by = c("SURVEY", "YEAR", 'STRATUM', "SPECIES_CODE"),
                all = TRUE)



S_iklm %>% 
  tidytable::filter(YEAR == 2021 & STRATUM == 71 & SPECIES_CODE == 21740) %>% 
  arrange(-LENGTH, SEX) %>% 
  print(n = 150)


S_iklm$LENGTH[is.na(x = S_iklm$LENGTH)] <- -9
S_iklm$SEX[is.na(x = S_iklm$SEX)] <- 3
S_iklm$S_iklm[is.na(x = S_iklm$S_iklm)] <- 1
S_iklm$S_ik[is.na(x = S_iklm$S_ik)] <- 1

S_iklm$NUMBER <- 
  round(x = S_iklm$POPULATION_COUNT * S_iklm$S_iklm / S_iklm$S_ik)






S_iklm <- subset(x = S_iklm, 
                 subset = NUMBER > 0,
                 select = -c(S_iklm, S_ik, POPULATION_COUNT))

names(x = S_iklm)[names(x = S_iklm) == "NUMBER"] <- "POPULATION_COUNT"

names(x = S_iklm)[names(x = S_iklm) == "LENGTH"] <- "LENGTH_MM"



S_iklm <- merge(x = S_iklm, 
                y = gapdata$survey, 
                by = "SURVEY")

S_iklm <- subset(x = S_iklm,
                 select = c(SURVEY_DEFINITION_ID, SURVEY, YEAR, STRATUM,
                            SPECIES_CODE, LENGTH_MM, SEX, POPULATION_COUNT))





