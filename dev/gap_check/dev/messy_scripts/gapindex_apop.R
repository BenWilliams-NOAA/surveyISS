library(gapindex)
library(tidyverse)



# gapindex::calc_sizecomp ----

gapdata <- readRDS(here::here('dev', 'gap_check', 'data', 'gapdata_ebs.RDS'))

species_test = 21740

cruise <- gapdata$cruise
haul <- gapdata$haul
gapdata$catch %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$catch
gapdata$size %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$size
gapdata$specimen %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$specimen
gapdata$species %>% 
  tidytable::filter(SPECIES_CODE %in% species_test) -> gapdata$species

# calc cpue
cpue <- gapindex::calc_cpue(gapdata)

# calc stratum popn
racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                        cpue)

# calc popn at length
size_comp <- calc_sizecomp_stratum(gapdata,
                                   cpue,
                                   racebase_stratum_popn,
                                   fill_NA_method = "BS") 

# get alk
alk <- gapindex::calc_alk(gapdata, unsex = "all")

# get stratum-level age pop'n
gap_age_comp_st <- gapindex::calc_agecomp_stratum(gapdata,
                                                  alk,
                                                  size_comp)

# get region level age pop'n
gap_ac <- gapindex::calc_agecomp_region(gapdata,
                                        gap_age_comp_st)










# start of calc_agecomp_region fcn


## Extract objects within function arguments
survey_designs <- gapdata$survey
strata <- gapdata$strata
age_comps <- gap_age_comp_st$age_comp
count_length_age <- gap_age_comp_st$length_at_age

## Empty dataframe to append region-specific age composition
region_age_comp_df <- data.frame()


## Extract the different regions to calculate over. Currently, age comps
## and mean/sd lengths are only being aggregated over regions. This 
## excludes calculations across subareas (e.g., Shumagin or 1-100 m). 
subareas <- subset(x = gapdata$subarea,
                   subset = SURVEY_DEFINITION_ID == 
                     survey_designs$SURVEY_DEFINITION_ID[1] &
                     AREA_TYPE == "REGION" &
                     DESIGN_YEAR == survey_designs$DESIGN_YEAR[1])



strata_in_region1 <- 
  subset(x = gapdata$stratum_groups,
         subset = AREA_ID %in% subareas$AREA_ID[1])$STRATUM

strata_in_region2 <- 
  subset(x = gapdata$stratum_groups,
         subset = AREA_ID %in% subareas$AREA_ID[2])$STRATUM






