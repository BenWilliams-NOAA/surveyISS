library(gapindex)
library(tidyverse)



# gapindex::calc_sizecomp ----

gapdata <- readRDS(here::here('dev', 'gapindex', 'data', 'gapdata_goa.RDS'))


cruise <- gapdata$cruise
haul <- gapdata$haul
cpue <- gapindex::calc_cpue(gapdata)
size <- gapdata$size
racebase_stratum_popn <- gapindex::calc_biomass_stratum(gapdata,
                                                        cpue)
size_comp <- calc_sizecomp_stratum(gapdata,
                                   cpue,
                                   racebase_stratum_popn) 

specimen <- subset(x = gapdata$specimen,
                   subset = !is.na(x = AGE) )


specimen <- merge(x = specimen,
                  y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
                  by = "CRUISEJOIN")
size <- merge(x = size,
              y = cruise[, c("CRUISEJOIN", "SURVEY", "YEAR")],
              by = "CRUISEJOIN")

specimen <- merge(x = specimen, 
                  y = haul[, c("HAULJOIN", "STRATUM", "STATIONID")],
                  by = "HAULJOIN",
                  all.x = T)                  

specimen$YEAR <- ifelse(test = is.na(specimen$YEAR), 
                        no = specimen$YEAR,
                        yes = as.numeric(substr(x = specimen$CRUISE, 
                                                start = 1, stop = 4)))

specimen <- subset(x = specimen, 
                   subset = !is.na(STRATUM))

names(specimen)[names(specimen) == "LENGTH"] <- "LENGTH_MM"
specimen$FREQ <- 1

s_yklm <- rbind(
  ## Aggregate FEMALES and MALES
  stats::aggregate(
    FREQ ~ SURVEY + YEAR + SPECIES_CODE + SEX + LENGTH_MM + AGE,
    data = specimen,
    subset = SEX %in% 1:2,
    FUN = sum),
  
  ## For UNSEXED, we pool MALES and FEMALES AND UNSEXED TOGETHER
  cbind(
    stats::aggregate(FREQ ~ SURVEY + YEAR + SPECIES_CODE + LENGTH_MM + AGE,
                     data = specimen,
                     FUN = sum), 
    SEX = 3)[, c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", 
                 "LENGTH_MM", "AGE", "FREQ")] 
)

p_yklm <- 
  do.call(what = rbind, 
          args = lapply(X = split(x = s_yklm, 
                                  f = list(s_yklm$SURVEY,
                                           s_yklm$YEAR, 
                                           s_yklm$SPECIES_CODE, 
                                           s_yklm$SEX,
                                           s_yklm$LENGTH_MM)),
                        FUN = function(df) {
                          
                          df$AGE_FRAC <- df$FREQ / sum(df$FREQ)
                          
                          return(subset(x = df, select = -FREQ))
                        }))

every_combo_of_lengths <- 
  expand.grid(SURVEY = sort(x = unique(x = size$SURVEY)),
              YEAR = sort(x = unique(x = size$YEAR)),
              SPECIES_CODE = sort(x = unique(x = size$SPECIES_CODE)),
              SEX = sort(x = unique(x = size$SEX)),
              LENGTH_MM = seq(from = min(size$LENGTH, na.rm = TRUE),
                              to = max(size$LENGTH, na.rm = TRUE),
                              by = 10),
              AGE = seq(from = min(specimen$AGE, na.rm = TRUE),
                        to = max(specimen$AGE, na.rm = TRUE),
                        by = 1))

p_yklm <- merge(x = every_combo_of_lengths,
                y = p_yklm,
                all.x = TRUE,
                by = c("SURVEY", "YEAR", "SPECIES_CODE", 
                       "SEX", "LENGTH_MM", "AGE"))


missing_lengths <- 
  do.call(what = rbind, 
          args = lapply(X = split(x = p_yklm, 
                                  f = with(p_yklm, list(SURVEY,
                                                        YEAR, 
                                                        SPECIES_CODE, 
                                                        SEX,
                                                        LENGTH_MM))),
                        FUN = function(df) {
                          if (sum(df$AGE_FRAC, na.rm = TRUE) == 0)
                            return(data.frame(
                              SURVEY = unique(df$SURVEY),
                              YEAR =  unique(df$YEAR),
                              SPECIES_CODE = unique(df$SPECIES_CODE),
                              SEX = unique(df$SEX),
                              LENGTH_MM = unique(df$LENGTH_MM))) 
                        }
          )
  )


s_klm <- stats::aggregate(
  FREQ ~ SURVEY + SPECIES_CODE + SEX + LENGTH_MM + AGE,
  data = specimen,
  FUN = sum)

p_klm <- 
  do.call(what = rbind, 
          args = lapply(X = split(x = s_klm, 
                                  f = list(s_klm$SURVEY,
                                           s_klm$SPECIES_CODE, 
                                           s_klm$SEX,
                                           s_klm$LENGTH_MM)),
                        FUN = function(df) {
                          df$AGE_FRAC <- df$FREQ / sum(df$FREQ)
                          return(df)
                        }))

## For each record in `missing_lengths`, merge all the age probabilities
## from `p_klm` for that length bin. 
filled_in_lengths <- 
  merge(x = missing_lengths,
        y = p_klm,
        all.x = TRUE,
        by = c("SURVEY", "SPECIES_CODE", "SEX", "LENGTH_MM"))

## Append the globally-filled lengths with the the non-global `p_yklm` alk
## to get a now global alk. 
p_yklm <- rbind(subset(x = p_yklm, subset = !is.na(AGE_FRAC)),
                subset(x = filled_in_lengths, 
                       subset = !is.na(AGE_FRAC),
                       select = names(p_yklm)))


p_yklm <- merge(x = every_combo_of_lengths,
                y = p_yklm,
                all.x = TRUE,
                by = c("SURVEY", "YEAR", "SPECIES_CODE", 
                       "SEX", "LENGTH_MM", "AGE"))

p_yklm$AGE_FRAC[is.na(x = p_yklm$AGE_FRAC)] <- 0
row.names(x = p_yklm) <- NULL




age_comp <- 
  merge(x = subset(x = size_comp, 
                   subset = SPECIES_CODE %in% unique(x = p_yklm$SPECIES_CODE)),
        y = p_yklm,
        by = c("SURVEY", "YEAR", "SPECIES_CODE", "SEX", "LENGTH_MM"),
        all.x = TRUE)
age_comp$AGE[is.na(x = age_comp$AGE)] <- -9
age_comp$AGE_FRAC[is.na(x = age_comp$AGE_FRAC)] <- 1

age_comp$AGEPOP <- age_comp$AGE_FRAC * age_comp$POPULATION_COUNT

count_length_age <- age_comp
count_length_age$AGE[count_length_age$LENGTH_MM < 0] <- -99


mean_length_at_age <- data.frame()


for (ispp in sort(unique(p_yklm$SPECIES_CODE))){
  
  mean_length_at_age <- 
    rbind(mean_length_at_age,
          do.call(
            what = rbind,
            args = lapply(
              X = split(x = subset(x = age_comp, 
                                   subset = LENGTH_MM > 0 & 
                                     SPECIES_CODE == ispp),
                        f = with(subset(x = age_comp, 
                                        subset = LENGTH_MM > 0 & 
                                          SPECIES_CODE == ispp),
                                 list(SURVEY, YEAR, 
                                      STRATUM, SEX, AGE))),
              ## Within each sublist of split_df, calculate the
              ## weighted mean and std.dev of length
              FUN = function(df) {
                
                ## temporary output df
                output_df <- data.frame()
                
                if (nrow(x = df) > 0) {
                  ## record the combo of "SURVEY", "YEAR",
                  ## "SPECIES_CODE", "SEX", and "AGE" of the sublist.
                  output_df[1, c("SURVEY", "YEAR",
                                 "STRATUM", "SEX", "AGE")] <-
                    unique(x = subset(x = df,
                                      select = c(SURVEY, YEAR,
                                                 STRATUM, SEX, AGE)))
                  
                  ## weighted mean length
                  mean_length <- stats::weighted.mean(x = df$LENGTH_MM,
                                                      w = df$AGEPOP)
                  
                  ## weighted std.dev length
                  sd_length <- sqrt(x = sum(df$AGEPOP/sum(df$AGEPOP) *
                                              (df$LENGTH_MM - mean_length)^2))
                  
                  ## append `mean_length` and `sd_length` to `output_df`
                  output_df[, c("LENGTH_MM_MEAN", "LENGTH_MM_SD")] <-
                    round(x = c(mean_length, sd_length),
                          digits = 2)
                  
                  output_df$SPECIES_CODE = ispp
                }
                
                return(output_df)
                
              }))
    )
  
  
  
}

rownames(x = mean_length_at_age) <- NULL




age_comp <- stats::aggregate(AGEPOP ~ SURVEY + YEAR + STRATUM + 
                               SPECIES_CODE + SEX + AGE,
                             data = age_comp,
                             FUN = function(x) round(x = sum(x)))

age_comp <- merge(x = age_comp,
                  y = subset(x = mean_length_at_age,
                             select = c(SURVEY, YEAR, STRATUM, 
                                        SPECIES_CODE, SEX, AGE, 
                                        LENGTH_MM_MEAN, LENGTH_MM_SD)),
                  by = c("SURVEY", "YEAR", "STRATUM", 
                         "SPECIES_CODE", "SEX", "AGE"),
                  all.x = TRUE)


age_comp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::summarise(agepop = sum(agepop),
                       mean_length = mean(length_mm_mean, na.rm = TRUE), .by = c(year, species_code, sex, age)) %>% 
  filter(agepop > 0 & age > 0)
































age_comp %>% 
  dplyr::rename_all(tolower) %>% 
  tidytable::summarise(agepop = sum(agepop), .by = c(year, species_code, sex, age)) %>% 
  tidytable::arrange(year, species_code, sex, age)

unique(age_comp$STRATUM)




%>% 
  tidytable::filter(sex!=3) %>% 
  print(n = 50)





