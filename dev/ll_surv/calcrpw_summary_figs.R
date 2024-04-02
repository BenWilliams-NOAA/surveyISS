# Area summary figs for calcrpw
# jane.sullivan@noaa.gov
# July 2021

# set up ----
libs <- c("tidyverse")
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
lapply(libs, library, character.only = TRUE)

OUTPUTPATH <- "C:/Data/Longline/Longline/R/CalcRPW"
# OUTPUTPATH <- "C:/Users/Jane.Sullivan/Work/Longline/R/CalcRPW"

# Data ----
load(file = paste0(OUTPUTPATH, "/results_calcrpw_summaries.RData"))

# Definitions for all objects loaded, sourced from calcrpw_summaries.R:

# arearpns = geographic area rpns from calcrpw.R

# areafixed = from AreaEffortsFixed table in database

# arearpns_fixed = arearpns replaced with fixed values from areafixed

# councilarea = rpns by council sablefish management area summarized from arearpns (no fixed data)

# councilarea_fixed = rpns by council sablefish management area summarized from
# fixed values in arearpns_fixed, BS and AI values assumed in the stock
# assessment for 1990-1997, fixed CV = 0.05 (from CouncilSablefishAreaFixed
# table in database), and interpolated values in BS and AI values 1996-present
# using GOA ratios and fixed CV = 0.05, and finally with US 1988 and 1989 data removed
# for sablefish and US 1998-1991 for all other species removed (deemed unquality data)

# fmpsubarea_fixed = rpns summarized from councilarea_fixed to management area (fmp subarea level)

# fmp_fixed = rpns summarized from councilarea_fixed to fmp (GOA, BSAI)

# akwide_fixed = rpns summarized from councilarea_fixed to all Alaska (aka coastwide)

# akwide = rpns summarized from councilarea (no fixed data) to all Alaska
     
# akwide_final = rpns summarized from councilarea_fixed to all Alaska (aka
# coastwide), but with historical sablefish JPN data overridden with rpn/rpws
# from the sablefish assessment data file used in 2020 (they conflict with the
# summed values from areafixed, could find no documentation on why)

# akfixed = the JPN historical sablefish values from the sablefish assessment
# data file used in 2020, stored in AKwideFixed table in db

# sablefish rpn/rpw comparisons ----

# Sablefish fixed values from the AreaEffortsFixed table don't match the values
# in the stock assessment when summed for 1979-1982 (1983 has different RPW but
# same RPN). This is probably due to inconsistent sampling (several areas aren't
# sampled in 1979-1981) and possibly Sigler trying to reconcile those
# differences.

areafixed %>% 
  filter(common_name == "Sablefish" & year <= 1983) %>% 
  group_by(year) %>% 
  summarize(n_areas = length(unique(geoarea)))


dbfixed <- areafixed %>% 
  filter(common_name == "Sablefish") %>% 
  group_by(year, country, species, common_name) %>% 
  summarise_at(vars(rpn, rpw), sum, na.rm=TRUE) %>% 
  mutate(source = "AreaEffortsFixed (Exploitable=1)") %>% 
  filter(country == "Japan")

dbfixed <- akfixed %>% 
  # rename(country = survey, rpn = RPN, rpw = RPW) %>% 
  filter(country == "Japan") %>% 
  mutate(source = "Sablefish assessment") %>% 
  bind_rows(dbfixed)

dbfixed <- akwide %>%
  filter(country == "Japan" & common_name == "Sablefish" & strata == "3to7" & dep == "depred") %>%
  mutate(source = "Calculated from data in database (strata 3-7)") %>%
  bind_rows(dbfixed)

dbfixed <- akwide %>%
  filter(country == "Japan" & common_name == "Sablefish" & strata == "AllStrata" & dep == "depred") %>%
  mutate(source = "Calculated from data in database (All strata)") %>%
  bind_rows(dbfixed)

dbfixed_l <- dbfixed %>% 
  mutate(RPN = rpn / 1e3,
         RPW = rpw / 1e3) %>% 
  select(source, year, RPN, RPW) %>% 
  pivot_longer(cols = c(RPN, RPW))

theme_set(theme_minimal(base_size = 13))

ggplot(dbfixed_l, aes(x = year, y = value, col = source, shape = source)) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = NULL) +
  facet_wrap(~name, scales = "free_y", ncol = 1)

ggsave(paste0(OUTPUTPATH, "/fixed_sabie_rpns.png"), units = "in",
       height = 8, width = 10)

# New RPNs to operational assessments -----

assess <- read_csv("operational_assessment_rpns_2020.csv")
assess %>% distinct(common_name, species, region, description)
unique(assess$region)
unique(assess$description)

# assess <- assess %>% 
#   mutate(rpn_se = ifelse(is.na(rpn_se), NA, rpn_se * rpn),
#          rpw_se = ifelse(is.na(rpw_se), NA, rpw_se * rpw)) #%>% 
# select(-c(rpn_se, rpw_se))

assess %>% 
  group_by(description, country) %>% 
  summarize(syr = min(year),
            lyr = max(year))

# (1) sablefish

tmp_sable <- akwide_final %>% 
  filter(species == 20510 & strata == "3to7" & dep == "depred") %>% 
  mutate(description = "AKwideFixed_db") %>% 
  # bind_rows(akwide %>% 
  #             filter(species == 20510 & strata == "3to7" & dep == "depred") %>% 
  #             mutate(description = "AKwide_db")) %>% 
  mutate(rpn = rpn / 1e3,
         rpn_se = sqrt(rpn_var) / 1e3,
         rpw = rpw / 1e3,
         rpw_se = sqrt(rpw_var) / 1e3) %>% 
  select(-c(strata, dep, fixed)) %>% 
  bind_rows(assess %>% 
              filter(description == "sable_assess_t3"))

# rpns  
tmp_sable %>% ggplot(aes(x = year, y = rpn, col = description, 
                         fill = description,
                         shape = description, lty = description)) +
  geom_ribbon(aes(ymin = rpn - 1.96 * rpn_se,
                  ymax = rpn + 1.96 * rpn_se),
              alpha = 0.25) + # col = NA
  geom_point() +
  geom_line() +
  labs(x = 'Year', y = 'RPN', col = NULL, 
       shape = NULL, lty = NULL, fill = NULL) +
  scale_fill_manual(values = c("#30D2EB", "#FFBB21")) +
  scale_colour_manual(values = c("#30D2EB", "#FFBB21")) +
  facet_wrap(~country, ncol = 1, scales = "free_y")

ggsave(paste0(OUTPUTPATH, "/new_sable_akwide_rpns.png"), units = "in",
       height = 4.5, width = 8)

# rpws
tmp_sable %>% ggplot(aes(x = year, y = rpw, col = description, 
                         fill = description,
                         shape = description, lty = description)) +
  # geom_ribbon(aes(ymin = rpw - 1.96 * rpw_se,
  #                 ymax = rpw + 1.96 * rpw_se),
  #             alpha = 0.25) + # col = NA
  geom_point() +
  geom_line() +
  labs(x = 'Year', y = 'RPW', col = NULL, 
       shape = NULL, lty = NULL, fill = NULL) +
  scale_fill_manual(values = c("#30D2EB", "#FFBB21")) +
  scale_colour_manual(values = c("#30D2EB", "#FFBB21")) +
  facet_wrap(~country, ncol = 1, scales = "free_y")

ggsave(paste0(OUTPUTPATH, "/new_sable_akwide_rpws.png"), units = "in",
       height = 4.5, width = 8)

# rockishes...

tmp_rf <- fmpsubarea_fixed %>% 
  filter(species %in% c(30020, 30050, 30576) &
           strata == "AllStrata" &
           dep == "nodep" &
           fmp == "GOA" &
           country == "United States") %>% 
  mutate(rpn = rpn,
         rpn_se = sqrt(rpn_var),
         rpw = rpw,
         rpw_se = sqrt(rpw_var),
         description = "FMPsubareaFixed_db") %>% 
  bind_rows(assess %>% 
              filter(species %in% c(30020, 30050, 30576) & 
                       region != "Gulf of Alaska") %>% 
              rename(fmp_subarea = region) )

# sst 
tmp_rf %>% 
  filter(species == 30020) %>%
  ggplot(aes(x = year, y = rpw / 1e3, fill = description, 
             col = description, shape = description, 
             lty = description)) +
  geom_ribbon(aes(ymin = rpw / 1e3 - 1.96 * (rpw_se / 1e3),
                  ymax = rpw / 1e3 + 1.96 * (rpw_se / 1e3)),
              alpha = 0.25) +
  scale_fill_manual(values = c("#30D2EB", "#FFBB21")) +
  scale_colour_manual(values = c("#30D2EB", "#FFBB21")) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "RPW", col = NULL, 
       shape = NULL, lty = NULL, fill = NULL) +
  facet_wrap(~fmp_subarea, scales = "free_y", ncol = 1)

ggsave(paste0(OUTPUTPATH, "/new_sst_fmpsubarea_rpws.png"), units = "in",
       height = 4.5, width = 8)

# Shortraker
tmp_rf %>% 
  filter(species == 30576) %>% 
  ggplot(aes(x = year, y = rpw / 1e3, fill = description, 
             col = description, shape = description, 
             lty = description)) +
  geom_ribbon(aes(ymin = rpw / 1e3 - 1.96 * (rpw_se / 1e3),
                  ymax = rpw / 1e3 + 1.96 * (rpw_se / 1e3)),
              alpha = 0.25) +
  scale_fill_manual(values = c("#30D2EB", "#FFBB21")) +
  scale_colour_manual(values = c("#30D2EB", "#FFBB21")) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "RPW", col = NULL, 
       shape = NULL, lty = NULL, fill = NULL) +
  facet_wrap(~fmp_subarea, scales = "free_y", ncol = 1)

ggsave(paste0(OUTPUTPATH, "/new_shortraker_fmpsubarea_rpws.png"), units = "in",
       height = 4.5, width = 8)


# RE apportionment
tmp_rf %>% 
  filter(species == 30050) %>% 
  ggplot(aes(x = year, y = rpw / 1e3, fill = description, 
             col = description, shape = description, 
             lty = description)) +
  geom_ribbon(aes(ymin = rpw / 1e3 - 1.96 * (rpw_se / 1e3),
                  ymax = rpw / 1e3 + 1.96 * (rpw_se / 1e3)),
              alpha = 0.25) +
  scale_fill_manual(values = c("#30D2EB", "#FFBB21")) +
  scale_colour_manual(values = c("#30D2EB", "#FFBB21")) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "RPW", col = NULL, 
       shape = NULL, lty = NULL, fill = NULL) +
  facet_wrap(~fmp_subarea, scales = "free_y", ncol = 1)

ggsave(paste0(OUTPUTPATH, "/new_rougheye_fmpsubarea_rpws.png"), units = "in",
       height = 4.5, width = 8)

# rougheye assessment (GOA wide)

tmp_rf2 <- fmp_fixed %>% 
  filter(species %in% c(30050) &
           strata == "AllStrata" &
           dep == "nodep" &
           fmp == "GOA" &
           country == "United States") %>% 
  mutate(rpn = rpn,
         rpn_se = sqrt(rpn_var),
         rpw = rpw,
         rpw_se = sqrt(rpw_var),
         description = "FMPFixed_db") %>% 
  bind_rows(assess %>% 
              filter(species %in% c(30050) & 
                       region == "Gulf of Alaska") %>% 
              rename(fmp = region) %>% 
              mutate(fmp = 'GOA'))
tmp_rf2 %>% 
  filter(species == 30050) %>% 
  ggplot(aes(x = year, y = rpn / 1e3, fill = description, 
             col = description, shape = description, 
             lty = description)) +
  geom_ribbon(aes(ymin = rpn / 1e3 - 1.96 * (rpn_se / 1e3),
                  ymax = rpn / 1e3 + 1.96 * (rpn_se / 1e3)),
              alpha = 0.25) +
  scale_fill_manual(values = c("#30D2EB", "#FFBB21")) +
  scale_colour_manual(values = c("#30D2EB", "#FFBB21")) +
  geom_point() +
  geom_line() +
  labs(x = "Year", y = "RPN", col = NULL, 
       shape = NULL, lty = NULL, fill = NULL) +
  facet_wrap(~fmp, scales = "free_y", ncol = 1)

ggsave(paste0(OUTPUTPATH, "/new_rougheye_fmp_rpns.png"), units = "in",
       height = 4.5, width = 8)

