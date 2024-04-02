# Area-wide summaries for calcrpw
# jane.sullivan@noaa.gov
# original: July 2021

# Aug 2022: (1) arranging summaries by year, (2) fixing non-sablefish bs/ai cv interpolations
# Integrated with database and added SQL | CSV options - RB 7/23/21

# set up ----
libs <- c("tidyverse", "fs", "readxl", "purrr", "RODBC") #, "zoo"
if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)], repos = "http://cran.us.r-project.org")}
lapply(libs, library, character.only = TRUE)

args <- commandArgs(TRUE)
DSN <- "LonglineLocal64" 
#conn <- odbcDriverConnect(connection="Driver={SQL Server};server=161.55.120.71,1919;database=LONGLINE;trusted_connection=yes;")
INPUTMODE <- "CSV"		# "SQL" | "CSV"
INPUTPATH  <- "C:/Data/Longline/Outputs"
OUTPUTMODE <- "CSV"  		# "SQL" | "CSV"
OUTPUTPATH <- "C:/Data/Longline/Outputs"
EXPORT_WEB_VIEWS <- TRUE 	# set to TRUE to export the web views to csv or sql tables

if (!is.na(args[1])) {
  DSN <- args[1]
} 
if (!is.na(args[2])) {
  INPUTMODE <- args[2]
} 
if (!is.na(args[2])) {
  INPUTPATH <- args[3]
} 
if (!is.na(args[3])) {
  OUTPUTMODE <- args[4]
} 
if (!is.na(args[4])) {
  OUTPUTPATH <- args[5]
} 
if (!is.na(args[6])) {
  EXPORT_WEB_VIEWS <- as.logical(args[6])
} 


fs::dir_ls(path = OUTPUTPATH)

# queries ----

# some areas don't get used for RPN calculations (only use Exploitable == 1)

sqlGeoArea <- "select Area_ID, area_code, Geographic_area_name, Habitat_type, INPFC_area, Council_management_area,"
sqlGeoArea <- paste(sqlGeoArea, "Council_sablefish_management_area, Region, FMP_management_area, Exploitable,")
sqlGeoArea <- paste(sqlGeoArea, "Council_sablefish_management_area_ID, Council_Management_Area_Id")
sqlGeoArea <- paste(sqlGeoArea, "from Areas")

# fixed values for geographic area levels
sqlAreaFixed <- "select year, survey, council_sablefish_management_area_ID, council_sablefish_management_area,"
sqlAreaFixed <- paste(sqlAreaFixed, "geographic_area_name, area_code, species_code, species, area_size_km,")
sqlAreaFixed <- paste(sqlAreaFixed, "CPUE, RPN, RPW, mean_length, mean_weight, LastModifiedDate")
sqlAreaFixed <- paste(sqlAreaFixed, "from AreaEffortsFixed")

# fixed values for council sablefish management area level
sqlCouncilFixed <- "select year, survey, council_sablefish_management_area_ID, council_sablefish_management_area,"
sqlCouncilFixed <- paste(sqlCouncilFixed, "species_code, species, CPUE, CPUE_var, RPN, RPN_var, RPW, RPW_var, LastModifiedDate")
sqlCouncilFixed <- paste(sqlCouncilFixed, "from CouncilSablefishAreaFixed")

# fixed values for AK wide sablefish 
sqlAKFixed <- "select year, survey, species_code, species, CPUE, CPUE_var, RPN, RPN_var, RPW, RPW_var, LastModifiedDate"
sqlAKFixed <- paste(sqlAKFixed, "from AKwideFixed")

if (INPUTMODE == "SQL") {
  conn <- odbcConnect(DSN)
  geoareas <- sqlQuery(conn, sqlGeoArea)
  areafixed <- sqlQuery(conn, sqlAreaFixed)
  councilfixed <- sqlQuery(conn, sqlCouncilFixed)
  akfixed <- sqlQuery(conn, sqlAKFixed)
  odbcClose(conn)
} else {
  geoareas <- read_csv("geoareas.csv")
  
  # 1979 - 1996 fixed geographic area data (all areas) for several species (sablefish, atf, rockfish, pcod)
  areafixed <- read_csv("AreaEffortsFixed.csv")
  
  # 1990 - 1997 sablefish RPN and RPWs + variances only in the BS and AI. RPN CV
  # fixed at 0.05 for both RPN and RPW
  councilfixed <- read_csv("CouncilSablefishAreaFixed.csv")
  
  # 1979 - 1994 Japanese sablefish RPN and RPWs Alaska-wide. No variances estimates.
  akfixed <- read_csv("AKwideFixed.csv")
}

# reformat fixed tables ----

geoareas <- geoareas %>% rename(geoarea = Geographic_area_name, 
                                exploitable = Exploitable)

areafixed <- areafixed %>% 
  select(year, country = survey, geoarea = geographic_area_name, area_code, 
         council_area = council_sablefish_management_area, area_size = area_size_km,
         species = species_code, common_name = species, cpue = CPUE,
         rpn = RPN, rpw = RPW) %>% 
  left_join(geoareas %>% select(geoarea, exploitable)) %>% 
  filter(exploitable == 1) %>% 
  select(-exploitable)

councilfixed <- councilfixed %>% 
  select(year, country = survey, council_area = council_sablefish_management_area,
         species = species_code, common_name = species, cpue = CPUE, cpue_var = CPUE_var,
         rpn = RPN, rpn_var = RPN_var, rpw = RPW, rpw_var = RPW_var)

akfixed <- akfixed %>% 
  select(year, country = survey, species = species_code, common_name = species, 
         cpue = CPUE, cpue_var = CPUE_var, rpn = RPN, rpn_var = RPN_var, 
         rpw = RPW, rpw_var = RPW_var)

print(paste("INPUTMODE: ", INPUTMODE))

###########################################################################################################
# Read in # geoarea data -- CSV
###########################################################################################################
if (INPUTMODE == "CSV")
{
  # read all geo area rpn data in. use pattern matching to pull files for each
  # year, labeling them correctly
  full_arearpns <- dir_ls(path = INPUTPATH, regexp = "[0-9]{4}_AreaRPN_AllStrata.csv") %>% 
    map(read_csv) %>% 
    bind_rows() %>% 
    mutate(strata = "AllStrata",
           dep = "nodep") %>%
    bind_rows(dir_ls(path = INPUTPATH, regexp = "[0-9]{4}_AreaRPN_AllStrata_depred.csv") %>% 
                map(read_csv) %>% 
                bind_rows() %>% 
                mutate(strata = "AllStrata",
                       dep = "depred")) %>% 
    bind_rows(dir_ls(path = INPUTPATH, regexp = "[0-9]{4}_AreaRPN_3to7.csv") %>% 
                map(read_csv) %>% 
                bind_rows() %>% 
                mutate(strata = "3to7",
                       dep = "nodep")) %>% 
    bind_rows(dir_ls(path = INPUTPATH, regexp = "[0-9]{4}_AreaRPN_3to7_depred.csv") %>% 
                map(read_csv) %>% 
                bind_rows() %>% 
                mutate(strata = "3to7",
                       dep = "depred")) 
  
}

###########################################################################################################
# Read in # geoarea data -- SQL
###########################################################################################################
if (INPUTMODE == "SQL")
{
  print("selecting data")
  
  sqlAreaRPNtable = "from AreaRPN_AllStrata"
  sqlAreaRPN <- "select Year, Survey, FMP_management_area, Council_management_area_id, Council_management_area,"
  sqlAreaRPN <- paste(sqlAreaRPN, "Council_sablefish_management_area_ID, Council_sablefish_management_area,")
  sqlAreaRPN <- paste(sqlAreaRPN, "Geographic_area_name, area_code, Exploitable, Species_Code, species, area_size_km,")
  sqlAreaRPN <- paste(sqlAreaRPN, "CPUE, CPUE_var, RPN, RPN_var, RPW, RPW_var, mean_length, mean_weight, LastModifiedDate")
  sql <- paste(sqlAreaRPN, sqlAreaRPNtable)
  
  conn <- odbcConnect(DSN)
  full_arearpns <- as.data.frame(sqlQuery(conn, sql)) %>% mutate(strata = "AllStrata", dep = "nodep")
  
  sqlAreaRPNtable = "from AreaRPN_AllStrata_depred"
  sql <- paste(sqlAreaRPN, sqlAreaRPNtable)
  full_arearpns <- bind_rows(full_arearpns, (as.data.frame(sqlQuery(conn, sql)) %>% mutate(strata = "AllStrata", dep = "depred")) )
  
  sqlAreaRPNtable = "from AreaRPN_3to7"
  sql <- paste(sqlAreaRPN, sqlAreaRPNtable)
  full_arearpns <- bind_rows(full_arearpns, (as.data.frame(sqlQuery(conn, sql)) %>% mutate(strata = "3to7", dep = "nodep")) )
  
  sqlAreaRPNtable = "from AreaRPN_3to7_depred"
  sql <- paste(sqlAreaRPN, sqlAreaRPNtable)
  full_arearpns <- bind_rows(full_arearpns, (as.data.frame(sqlQuery(conn, sql)) %>% mutate(strata = "3to7", dep = "depred")) )
  
  odbcClose(conn)
  
  print("got data")
  
  full_arearpns = as_tibble(full_arearpns)
}

arearpns <- full_arearpns %>% 
  select(strata, dep, year = Year, country = Survey, 
         geoarea = Geographic_area_name, 
         area_code, # FLAG - diagnostics on arearpns %>% filter(rpn==0 & rpn_var>0)
         council_area = Council_sablefish_management_area,
         fmp_subarea = Council_management_area, fmp = FMP_management_area, 
         species = Species_Code, exploitable = Exploitable, 
         common_name = species, cpue = CPUE, cpue_var = CPUE_var,
         rpn = RPN, rpn_var = RPN_var, rpw = RPW, rpw_var = RPW_var)

# QAQC ----

# make sure there are no instances of variances/covariances spilling from 2b-7
# to 3-7 (should be none)
arearpns %>% 
  filter(rpn==0 & rpn_var>0) %>% 
  print(n=Inf)

# make sure there are no duplicates (should be TRUE)
arearpns %>% distinct() %>% nrow() == nrow(arearpns)

# only use exploitable = 1 (these are predefined geographic areas used to
# calculate RPN summaries)
arearpns <- arearpns %>% 
  filter(exploitable == 1) %>% 
  select(-exploitable)


print(paste0(INPUTPATH, "/variance_species.csv"))

if (INPUTMODE == "SQL")
{
  # rpn_var with 0s should be based on true zeros, i.e. for species for which
  # variances are being calculated
  arearpns %>% filter(rpn==0 & rpn_var==0) %>% distinct(species) %>% arrange(species) ==
    read_csv(paste0(INPUTPATH, "/variance_species.csv")) %>% distinct(species) %>% arrange(species)  
}

# fixed geoarea ----

arearpns_fixed <- arearpns %>% 
  left_join(areafixed %>% 
              mutate(fixed = "fixed"), 
            by = c("year", "country", "geoarea", "area_code", 
                   "council_area", "species", "common_name"))

# test the join (should be TRUE)
arearpns_fixed %>% filter(fixed == "fixed") %>% distinct(year, country, geoarea, species) %>% nrow() == nrow(areafixed)

arearpns_fixed <- arearpns_fixed %>% 
  mutate(cpue = ifelse(is.na(fixed), cpue.x, cpue.y),
         rpn = ifelse(is.na(fixed), rpn.x, rpn.y),
         rpw = ifelse(is.na(fixed), rpw.x, rpw.y)) %>% 
  # the fixed values are highlighted using a "fixed" field, and the past area
  # size is included for posterity
  select(names(arearpns), fixed, area_size) 

# council sablefish area ----

councilarea <- arearpns %>% 
  group_by(strata, dep, country, year, fmp, fmp_subarea, council_area,
           species, common_name)

councilarea <- councilarea %>% 
  dplyr::summarise_at(vars(rpn, rpw, rpn_var, rpw_var),
                      sum, na.rm = TRUE) %>% 
  left_join(councilarea %>% dplyr::summarise(cpue = mean(cpue))) %>% 
  ungroup() %>% 
  mutate(rpn_cv = sqrt(rpn_var) / rpn,
         cpue_var = ifelse(is.na(cpue), NA, ifelse(is.nan(rpn_cv), 0, (cpue * rpn_cv)^2))) %>% 
  select(-rpn_cv) 

councilarea %>% filter(is.na(rpn)) # should be 0

# fixed council sablefish area ----

# step 1: fixed values from the AreaEffortsFixed table
# step 2: BS and AI values assumed in the stock assessment for 1990-1997, CV =
# fixed to 0.05
# step 3: interpolate BS and AI values 1996-present using GOA ratios,  CV =
# fixed to long-term average CV of modern data (post-1996) (in 2021 CV = 0.05
# was incorrectly assumed for all species, fixed in 2022)
# step 4: get rid of US 1988 and 1989 data for sablefish and US 1988-1991 for

# everything else

councilarea_fixed <- arearpns_fixed %>% 
  group_by(strata, dep, country, year, fmp, fmp_subarea, council_area,
           species, common_name)

councilarea_fixed <- councilarea_fixed %>% 
  dplyr::summarise_at(vars(rpn, rpw, rpn_var, rpw_var),
                      sum, na.rm = TRUE) %>% 
  left_join(councilarea_fixed %>% dplyr::summarise(cpue = mean(cpue))) %>% 
  ungroup() %>% 
  mutate(rpn_cv = sqrt(rpn_var) / rpn,
         cpue_var = ifelse(is.na(cpue), NA, ifelse(is.nan(rpn_cv), 0, (cpue * rpn_cv)^2))) %>% 
  select(-rpn_cv) 

councilarea_fixed %>% filter(is.na(rpn)) # should be 0
councilarea_fixed %>% filter(is.na(strata)) # should be 0

# councilarea_fixed %>% nrow()

# step 2, replace with BS and AI 1990-1997 values as needed - use a full join
# because all these values aren't from years with a survey in that area (with
# the exception of 1996 AI, where area 15 raw data was lost and is now fixed)

# Create expanded data set for the 1990-present data set. Sablefish fixed values
# in BS and AI for 1990-1997 for sablefish only, all species for 1997-present.
nonsable <- councilarea %>% 
  filter(species != 20510) %>% 
  distinct(species) %>% 
  pull()

councilarea_fixed <- expand.grid(strata =  unique(councilarea$strata),
                                 dep = unique(councilarea$dep),
                                 country = "United States",
                                 species = 20510,
                                 council_area = c("Bering Sea", "Aleutians"),
                                 # 1990 is the year we "start" the BS and AI time series for sablefish.
                                 year = 1990:max(councilarea$year)) %>% 
  bind_rows(expand.grid(strata =  unique(councilarea$strata),
                        dep = "nodep",
                        country = "United States",
                        species = nonsable,
                        council_area = c("Bering Sea", "Aleutians"),
                        # 1996 is the year we start interpolating in the BS and
                        # AI time series for all species
                        year = 1996:max(councilarea$year))) %>% 
  left_join(councilarea %>%
              distinct(fmp, fmp_subarea, council_area, species, common_name)) %>% 
  # get rid of duplicate rows
  anti_join(councilarea_fixed) %>% 
  bind_rows(councilarea_fixed) %>%
  arrange(strata, dep, year)

# councilarea_fixed %>% nrow()

# 1990-1997 values have fixed CV=0.05 (previously (pre-2021), Dana fixed it to
# the mean CV in BS/AI for all modern data... the problem is that made the
# variance change every year. Fixing to 0.05 made minimal change, very similar
# to modern data CV)
councilarea_fixed <- councilarea_fixed %>%
  left_join(councilfixed %>% 
              mutate(fixed = "fixed"), 
            by = c("year", "country", "council_area", "species", "common_name"))

councilarea_fixed %>% filter(is.na(strata)) # should be 0

# test the join (should be TRUE)
councilarea_fixed %>% filter(fixed == "fixed") %>% distinct(year, country, council_area, species) %>% nrow() == nrow(councilfixed)

councilarea_fixed <- councilarea_fixed %>% 
  mutate(cpue = ifelse(is.na(fixed), cpue.x, cpue.y),
         cpue_var = ifelse(is.na(fixed), cpue_var.x, cpue_var.y),
         rpn = ifelse(is.na(fixed), rpn.x, rpn.y),
         rpn_var = ifelse(is.na(fixed), rpn_var.x, rpn_var.y),
         rpw = ifelse(is.na(fixed), rpw.x, rpw.y),
         rpw_var = ifelse(is.na(fixed), rpw_var.x, rpw_var.y)) 

# Note that the 1996 AI values when summed from AreaEffortsFixed does not match
# what has been used in the stock assessment. I'm using the assessment values
# for this table. (Note: .x = summed from AreaEffortsFixed; .y = assessment values)
councilarea_fixed %>% filter(year == 1996 & council_area == "Aleutians" & common_name == "Sablefish") #%>% View()

councilarea_fixed <- councilarea_fixed %>% 
  # the fixed values are highlighted using a "fixed" field
  select(names(councilarea), fixed)

# step 3: summarize GOA wide then get ratio
# AI_{j} = (GOA_{j} / GOA_{j-1}) * AI[j-1]
# BS_{j} = (GOA_{j} / GOA_{j-1}) * BS[j-1]

dim(councilarea_fixed)

councilarea_fixed %>% filter(is.na(strata)) # should be 0

# checking structure of missing vs. fixed values 
# councilarea_fixed %>% filter(year == 1996 & council_area %in% c("Bering Sea", "Aleutians") & fixed == "fixed")

councilarea_fixed %>% filter(strata == "3to7" & dep == "depred" & year >= 1996 & council_area %in% c("Bering Sea", "Aleutians")) #%>% View()

councilarea_fixed <- councilarea_fixed %>% 
  left_join(councilarea_fixed %>% 
              filter(!(council_area %in% c("Aleutians", "Bering Sea"))) %>% 
              group_by(strata, dep, country, species, year) %>% 
              # GOA wide CPUE/RPN/RPWs
              summarize(goa_cpue = mean(cpue, na.rm = TRUE),
                        goa_rpn = sum(rpn, na.rm = TRUE),
                        goa_rpw = sum(rpw, na.rm = TRUE)) %>% 
              group_by(strata, dep, country, species) %>%
              # Lag GOA CPUE/RPN/RPWs, i.e. GOA_{j} and GOA_{j-1}
              mutate(goa_cpue_lyr = lag(goa_cpue, default = goa_cpue[1]),
                     goa_rpn_lyr = lag(goa_rpn, default = goa_rpn[1]),
                     goa_rpw_lyr = lag(goa_rpw, default = goa_rpw[1])) %>%
              select(strata, dep, country, year, species, goa_cpue, goa_cpue_lyr, goa_rpn, goa_rpn_lyr, 
                     goa_rpw, goa_rpw_lyr)) %>% 
  # Lag CPUE/RPN/RPW values by Council area (only AI and BS end up getting
  # used); also get modern mean CV for interpolation
  group_by(strata, dep, country, council_area, species) %>% 
  mutate(cpue_lyr = lag(cpue, default = cpue[1]),
         rpn_lyr = lag(rpn, default = rpn[1]),
         rpw_lyr = lag(rpw, default = rpw[1]),
         mean_cv = ifelse(year >= 1996, mean(sqrt(rpn_var) / rpn, na.rm = TRUE), 0.05),
         mean_cv = ifelse(is.nan(mean_cv), 0, mean_cv)) %>% 
  ungroup() %>% 
  # Extrapolate missing BSAI years using GOA_{j} / GOA_{j-1}
  mutate(cpue = ifelse(council_area %in% c("Aleutians", "Bering Sea") & is.na(cpue),
                       cpue_lyr * (goa_cpue / goa_cpue_lyr), cpue),
         rpn = ifelse(council_area %in% c("Aleutians", "Bering Sea") & is.na(rpn),
                      rpn_lyr * (goa_rpn / goa_rpn_lyr), rpn),
         rpw = ifelse(council_area %in% c("Aleutians", "Bering Sea") & is.na(rpw),
                      rpw_lyr * (goa_rpw / goa_rpw_lyr), rpw)) %>% 
  # Assumed mean CV for these interpolations
  mutate(cpue_var = ifelse(council_area %in% c("Aleutians", "Bering Sea") & 
                             between(year, 1997, max(councilarea$year)) & 
                             !is.na(cpue) & is.na(cpue_var), 
                           (mean_cv * cpue)^2, cpue_var),
         rpn_var = ifelse(council_area %in% c("Aleutians", "Bering Sea") & 
                            between(year, 1997, max(councilarea$year)) & 
                            !is.na(rpn) & is.na(rpn_var), 
                          (mean_cv * rpn)^2, rpn_var),
         rpw_var = ifelse(council_area %in% c("Aleutians", "Bering Sea") & 
                            between(year, 1997, max(councilarea$year)) & 
                            !is.na(rpw) & is.na(rpw_var), 
                          (mean_cv * rpw)^2, rpw_var)) %>% 
  
  # There are going to be some species (e.g. greenland turbot, mud skate,
  # kamchatka flounder, etc.) where there is no data in the GOA to inform BS or
  # AI off years. In these case Drop unused cols. Per convo with Cara Rodgveller
  # 2021-06-07, simply replace these instances with NAs. I think we could
  # probably try another method in the future (e.g. interpolation), though that
  # may present problems for the terminal year
  mutate(cpue = ifelse(council_area %in% c("Aleutians", "Bering Sea") & 
                         between(year, 1997, max(councilarea$year)) & 
                         is.infinite(cpue)|is.nan(rpn), NA, cpue),
         rpn = ifelse(council_area %in% c("Aleutians", "Bering Sea") & 
                        between(year, 1997, max(councilarea$year)) & 
                        is.infinite(rpn)|is.nan(rpn), NA, rpn),
         rpw = ifelse(council_area %in% c("Aleutians", "Bering Sea") & 
                        between(year, 1997, max(councilarea$year)) & 
                        is.infinite(rpw)|is.nan(rpw), NA, rpw)) %>% 
  select(-c(contains(c("goa", "lyr")), mean_cv)) %>% 
  arrange(year)

dim(councilarea_fixed)

councilarea_fixed %>% filter(is.nan(rpw_var)|is.nan(cpue_var)|is.nan(rpn_var))
councilarea_fixed %>% filter(is.na(strata)|is.na(dep))

# there are some undefined species
councilarea_fixed %>% filter(is.na(common_name)) %>% count(country, species)
councilarea_fixed %>% filter(is.na(common_name)) %>% distinct(species) %>% pull(species) -> undefined_spp
arearpns %>% filter(species %in% undefined_spp) %>% count(year) %>% print(n=Inf)
# in database 21730=NA, 21930=Hexagrammos sp., 22201=Liparis sp., 24220=Bothrocara sp., 39000=NA, 99997=NA

# step 4: get rid of US 1988 and 1989 data for sablefish and US 1998-1991 for
# everything else
councilarea_fixed <- councilarea_fixed %>%
  filter(!c(country == "United States" & species == 20510 & year < 1990)) %>%
  filter(!c(country == "United States" & species != 20510 & year < 1992))

# FMP sub area fixed ----

fmpsubarea_fixed <- councilarea_fixed %>% 
  dplyr::group_by(strata, dep, country, year, fmp, fmp_subarea, species, common_name) 

fmpsubarea_fixed <- fmpsubarea_fixed %>% 
  summarize_at(vars(rpn, rpn_var, rpw, rpw_var), sum, na.rm = TRUE) %>% 
  left_join(fmpsubarea_fixed %>% dplyr::summarise(cpue = mean(cpue))) %>% 
  ungroup() %>% 
  mutate(rpn_cv = sqrt(rpn_var) / rpn,
         cpue_var = ifelse(is.na(cpue), NA, ifelse(is.nan(rpn_cv), 0, (cpue * rpn_cv)^2))) %>% 
  select(-rpn_cv) %>% 
  arrange(strata, dep, year, country, fmp_subarea)

# FMP fixed ----

fmp_fixed <- councilarea_fixed %>% 
  dplyr::group_by(strata, dep, country, year, fmp, species, common_name) 

fmp_fixed <- fmp_fixed %>% 
  summarize_at(vars(rpn, rpn_var, rpw, rpw_var), sum, na.rm = TRUE) %>% 
  left_join(fmp_fixed %>% dplyr::summarise(cpue = mean(cpue))) %>% 
  ungroup() %>% 
  mutate(rpn_cv = sqrt(rpn_var) / rpn,
         cpue_var = ifelse(is.na(cpue), NA, ifelse(is.nan(rpn_cv), 0, (cpue * rpn_cv)^2))) %>% 
  select(-rpn_cv) %>% 
  arrange(strata, dep, year, country, fmp)


# AK wide ----

# using only values in the db (no fixed data)

akwide <- councilarea %>% 
  group_by(strata, dep, country, species, common_name, year) 

akwide <- akwide %>% 
  summarize_at(vars(rpn, rpn_var, rpw, rpw_var), sum, na.rm = TRUE) %>% 
  left_join(akwide %>% dplyr::summarise(cpue = mean(cpue))) %>% 
  ungroup() %>% 
  mutate(rpn_cv = sqrt(rpn_var) / rpn,
         cpue_var = ifelse(is.na(cpue), NA, ifelse(is.nan(rpn_cv), 0, (cpue * rpn_cv)^2))) %>% 
  select(-rpn_cv) %>% 
  arrange(strata, dep, year, country)

# AK wide fixed ----

akwide_fixed <- councilarea_fixed %>% 
  group_by(strata, dep, country, year, species, common_name) 

akwide_fixed <- akwide_fixed %>% 
  summarize_at(vars(rpn, rpn_var, rpw, rpw_var), sum, na.rm = TRUE) %>% 
  left_join(akwide_fixed %>% dplyr::summarise(cpue = mean(cpue))) %>% 
  ungroup() %>% 
  mutate(rpn_cv = sqrt(rpn_var) / rpn,
         cpue_var = ifelse(is.na(cpue), NA, ifelse(is.nan(rpn_cv), 0, (cpue * rpn_cv)^2))) %>% 
  select(-rpn_cv) %>% 
  arrange(strata, dep, year, country)

# then replace sablefish values for Japanese years with values from assessment
akwide_final <- akwide_fixed %>% #dim()
  left_join(akfixed %>% 
              mutate(fixed = "fixed"), 
            by = c("year", "country", "species", "common_name")) #%>% dim()

akwide_final <- akwide_final %>% 
  mutate(cpue = ifelse(is.na(fixed), cpue.x, cpue.y),
         cpue_var = ifelse(is.na(fixed), cpue_var.x, cpue_var.y),
         rpn = ifelse(is.na(fixed), rpn.x, rpn.y),
         rpn_var = ifelse(is.na(fixed), rpn_var.x, rpn_var.y),
         rpw = ifelse(is.na(fixed), rpw.x, rpw.y),
         rpw_var = ifelse(is.na(fixed), rpw_var.x, rpw_var.y)) %>% 
  select(names(akwide_fixed), fixed)

# decision Cara Rodveller, Jane Sullivan, Rick Bush Jul 2021 to fix variances
# for the fixed historical sablefish RPN/RPWs (i.e. akfixed) using the
# CV from estimates variances for those years using data from the database.
akwide_final <- akwide_final %>% 
  left_join(akwide %>% 
              filter(year %in% unique(akfixed$year) &
                       species %in% unique(akfixed$species) &
                       country %in% unique(akfixed$country)) %>%
              select(-c(cpue, rpn, rpw)) %>% 
              mutate(fix_histjpn_var = "fix_histjpn_var"),
            by = c("strata", "dep", "country", "year", "species", "common_name")) %>% 
  mutate(cpue_var = ifelse(is.na(fix_histjpn_var) | is.na(cpue), cpue_var.x, cpue_var.y),
         rpn_var = ifelse(is.na(fix_histjpn_var) | is.na(rpn), rpn_var.x, rpn_var.y),
         rpw_var = ifelse(is.na(fix_histjpn_var) | is.na(rpw), rpw_var.x, rpw_var.y)) %>% 
  select(names(akwide_fixed), fixed)

akwide_final %>% filter(dep == 'depred' & strata == '3to7') %>% print(n=Inf)

# save output ----

last_mod <- max(as.Date(unique(full_arearpns$LastModifiedDate)[1], na.rm=TRUE))
print(paste("last_mod: ", last_mod))

###########################################################################################################
# beginning of CSV output
###########################################################################################################
if (OUTPUTMODE == "CSV") {
  
  # council sablefish management area
  
  n_councilarea <- councilarea_fixed %>% 
    mutate(fn = ifelse(dep == "nodep", strata, paste(strata, dep, sep = "_"))) %>% 
    left_join(geoareas %>% 
                select(council_area = Council_sablefish_management_area,
                       Council_sablefish_management_area_ID)) %>% 
    group_by(fn) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, 
                                    ~dplyr::select(.,
                                                   year, survey = country, 
                                                   council_sablefish_management_area_id = Council_sablefish_management_area_ID,
                                                   council_sablefish_management_area = council_area,
                                                   species_code = species, species = common_name,
                                                   CPUE = cpue, CPUE_var = cpue_var,
                                                   RPN = rpn, RPN_var = rpn_var,
                                                   RPW = rpw, RPW_var = rpw_var)),
                  data = purrr::map(data, ~dplyr::mutate(., LastModifiedDate = last_mod)),
                  data = purrr::map(data, ~dplyr::arrange(., species, council_sablefish_management_area, year, survey))) 
  
  purrr::walk2(.x = n_councilarea$data, .y = n_councilarea$fn, 
               ~ write_csv(x = .x, file = paste0(OUTPUTPATH, "/CouncilSablefishArea_", .y, ".csv")))
  
  # council area / fmp subarea
  
  n_fmpsubarea <- fmpsubarea_fixed %>% 
    mutate(fn = ifelse(dep == "nodep", strata, paste(strata, dep, sep = "_"))) %>% 
    left_join(geoareas %>% 
                select(fmp_subarea = Council_management_area,
                       Council_Management_Area_Id)) %>% 
    group_by(fn) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, 
                                    ~dplyr::select(.,
                                                   year, survey = country, 
                                                   council_management_area_id = Council_Management_Area_Id,
                                                   council_management_area = fmp_subarea,
                                                   species_code = species, species = common_name,
                                                   CPUE = cpue, CPUE_var = cpue_var,
                                                   RPN = rpn, RPN_var = rpn_var,
                                                   RPW = rpw, RPW_var = rpw_var)),
                  data = purrr::map(data, ~dplyr::mutate(., LastModifiedDate = last_mod)),
                  data = purrr::map(data, ~dplyr::arrange(., species, council_management_area, year, survey))) 
  
  n_fmpsubarea %>% unnest(cols = c(data))
  purrr::map2(.x = n_fmpsubarea$data, .y = n_fmpsubarea$fn, 
              ~write_csv(x = .x, file = paste0(OUTPUTPATH, "/FMPSubarea_", .y, ".csv")))
  
  # FMP
  
  n_fmp <- fmp_fixed %>% 
    mutate(fn = ifelse(dep == "nodep", strata, paste(strata, dep, sep = "_"))) %>% 
    group_by(fn) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, 
                                    ~dplyr::select(.,
                                                   year, survey = country, 
                                                   FMP_management_area = fmp,
                                                   species_code = species, species = common_name,
                                                   CPUE = cpue, CPUE_var = cpue_var,
                                                   RPN = rpn, RPN_var = rpn_var,
                                                   RPW = rpw, RPW_var = rpw_var)),
                  data = purrr::map(data, ~dplyr::mutate(., LastModifiedDate = last_mod)),
                  data = purrr::map(data, ~dplyr::arrange(., species, FMP_management_area, year, survey))) 
  
  n_fmp %>% unnest(cols = c(data))
  purrr::map2(.x = n_fmp$data, .y = n_fmp$fn, 
              ~write_csv(x = .x, file = paste0(OUTPUTPATH, "/FMP_", .y, ".csv")))
  
  # AK Wide
  n_akwide <- akwide_final %>% 
    mutate(fn = ifelse(dep == "nodep", strata, paste(strata, dep, sep = "_"))) %>% 
    group_by(fn) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(data = purrr::map(data, 
                                    ~dplyr::select(.,
                                                   year, survey = country, 
                                                   species_code = species, species = common_name,
                                                   CPUE = cpue, CPUE_var = cpue_var,
                                                   RPN = rpn, RPN_var = rpn_var,
                                                   RPW = rpw, RPW_var = rpw_var)),
                  data = purrr::map(data, ~dplyr::mutate(., LastModifiedDate = last_mod)),
                  data = purrr::map(data, ~dplyr::arrange(., species, year, survey))) 
  
  n_akwide %>% unnest(cols = c(data))
  purrr::map2(.x = n_akwide$data, .y = n_akwide$fn, 
              ~write_csv(x = .x, file = paste0(OUTPUTPATH, "/AK_wide_", .y, ".csv")))
  
  # save(arearpns, arearpns_fixed, areafixed, councilarea, councilarea_fixed, fmpsubarea_fixed, fmp_fixed, akwide, 
  #    akwide_fixed, akwide_final, akfixed, file = paste0(OUTPUTPATH, "/results_calcrpw_summaries.RData"))
} 
###########################################################################################################
# end of CSV output
###########################################################################################################

###########################################################################################################
# beginning of SQL output
###########################################################################################################

if (OUTPUTMODE=="SQL") {
  
  conn <- odbcConnect(DSN)
  
  ###########################################################################################################
  #AKwide
  ###########################################################################################################
  print("Exporting AKwide_*")
  
  #AllStrata, nodep
  data.out <- as.data.frame(akwide_final[akwide_final$strata=="AllStrata"&akwide_final$dep=="nodep",c("year","country","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  qry <- paste("delete from AK_wide_AllStrata")
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename="AK_wide_AllStrata", append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #AllStrata, depred
  data.out <- as.data.frame(akwide_final[akwide_final$strata=="AllStrata"&akwide_final$dep=="depred",c("year","country","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  qry <- paste("delete from AK_wide_AllStrata_depred")
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename="AK_wide_AllStrata_depred", append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, nodep
  data.out <- as.data.frame(akwide_final[akwide_final$strata=="3to7"&akwide_final$dep=="nodep",c("year","country","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  qry <- paste("delete from AK_wide_3to7")
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename="AK_wide_3to7", append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, depred
  data.out <- as.data.frame(akwide_final[akwide_final$strata=="3to7"&akwide_final$dep=="depred",c("year","country","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  qry <- paste("delete from AK_wide_3to7_depred")
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename="AK_wide_3to7_depred", append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  ###########################################################################################################
  #FMP
  ###########################################################################################################
  print("Exporting FMP_*")
  
  #AllStrata, nodep
  data.out <- as.data.frame(fmp_fixed[fmp_fixed$strata=="AllStrata"&fmp_fixed$dep=="nodep",c("year","country","fmp","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","FMP_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMP_AllStrata"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #AllStrata, depred
  data.out <- as.data.frame(fmp_fixed[fmp_fixed$strata=="AllStrata"&fmp_fixed$dep=="depred",c("year","country","fmp","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","FMP_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMP_AllStrata_depred"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, nodep
  data.out <- as.data.frame(fmp_fixed[fmp_fixed$strata=="3to7"&fmp_fixed$dep=="nodep",c("year","country","fmp","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","FMP_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMP_3to7"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, depred
  data.out <- as.data.frame(fmp_fixed[fmp_fixed$strata=="3to7"&fmp_fixed$dep=="depred",c("year","country","fmp","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  names(data.out)=c("year","survey","FMP_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMP_3to7_depred"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  ###########################################################################################################
  #FMPSubarea
  ###########################################################################################################
  print("Exporting FMPSubarea_*")
  
  #AllStrata, nodep
  data.out <- as.data.frame(fmpsubarea_fixed[fmpsubarea_fixed$strata=="AllStrata"&fmpsubarea_fixed$dep=="nodep",c("year","country","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$fmp_subarea, geoareas$Council_management_area)
  data.out$fmp_subarea_id = geoareas[temp.match,"Council_Management_Area_Id"]
  data.out = data.out[,c("year","country","fmp_subarea_id","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_management_area_id","council_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMPSubarea_AllStrata"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #AllStrata, depred
  data.out <- as.data.frame(fmpsubarea_fixed[fmpsubarea_fixed$strata=="AllStrata"&fmpsubarea_fixed$dep=="depred",c("year","country","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$fmp_subarea, geoareas$Council_management_area)
  data.out$fmp_subarea_id = geoareas[temp.match,"Council_Management_Area_Id"]
  data.out = data.out[,c("year","country","fmp_subarea_id","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_management_area_id","council_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMPSubarea_AllStrata_depred"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, nodep
  data.out <- as.data.frame(fmpsubarea_fixed[fmpsubarea_fixed$strata=="3to7"&fmpsubarea_fixed$dep=="nodep",c("year","country","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$fmp_subarea, geoareas$Council_management_area)
  data.out$fmp_subarea_id = geoareas[temp.match,"Council_Management_Area_Id"]
  data.out = data.out[,c("year","country","fmp_subarea_id","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_management_area_id","council_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMPSubarea_3to7"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, depred
  data.out <- as.data.frame(fmpsubarea_fixed[fmpsubarea_fixed$strata=="3to7"&fmpsubarea_fixed$dep=="depred",c("year","country","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$fmp_subarea, geoareas$Council_management_area)
  data.out$fmp_subarea_id = geoareas[temp.match,"Council_Management_Area_Id"]
  data.out = data.out[,c("year","country","fmp_subarea_id","fmp_subarea","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_management_area_id","council_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "FMPSubarea_3to7_depred"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  ###########################################################################################################
  #CouncilSablefishArea
  ###########################################################################################################
  print("Exporting CouncilSablefishArea_*")
  
  #AllStrata, nodep
  data.out <- as.data.frame(councilarea_fixed[councilarea_fixed$strata=="AllStrata"&councilarea_fixed$dep=="nodep",c("year","country","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$council_area, geoareas$Council_sablefish_management_area)
  data.out$council_area_id = geoareas[temp.match,"Council_sablefish_management_area_ID"]
  data.out = data.out[,c("year","country","council_area_id","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_sablefish_management_area_id","council_sablefish_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "CouncilSablefishArea_AllStrata"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #AllStrata, depred
  data.out <- as.data.frame(councilarea_fixed[councilarea_fixed$strata=="AllStrata"&councilarea_fixed$dep=="depred",c("year","country","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$council_area, geoareas$Council_sablefish_management_area)
  data.out$council_area_id = geoareas[temp.match,"Council_sablefish_management_area_ID"]
  data.out = data.out[,c("year","country","council_area_id","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_sablefish_management_area_id","council_sablefish_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "CouncilSablefishArea_AllStrata_depred"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, nodep
  data.out <- as.data.frame(councilarea_fixed[councilarea_fixed$strata=="3to7"&councilarea_fixed$dep=="nodep",c("year","country","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$council_area, geoareas$Council_sablefish_management_area)
  data.out$council_area_id = geoareas[temp.match,"Council_sablefish_management_area_ID"]
  data.out = data.out[,c("year","country","council_area_id","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_sablefish_management_area_id","council_sablefish_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "CouncilSablefishArea_3to7"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  #3to7, depred
  data.out <- as.data.frame(councilarea_fixed[councilarea_fixed$strata=="3to7"&councilarea_fixed$dep=="depred",c("year","country","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")])
  
  #get area id
  temp.match = match(data.out$council_area, geoareas$Council_sablefish_management_area)
  data.out$council_area_id = geoareas[temp.match,"Council_sablefish_management_area_ID"]
  data.out = data.out[,c("year","country","council_area_id","council_area","species","common_name","cpue","cpue_var","rpn","rpn_var","rpw","rpw_var")]
  data.out$last_mod = rep(Sys.time(), dim(data.out)[1])
  
  names(data.out)=c("year","survey","council_sablefish_management_area_id","council_sablefish_management_area","species_code","species","CPUE","CPUE_var","RPN","RPN_var","RPW","RPW_var","LastModifiedDate")
  
  #fix inf values
  is.na(data.out) <- sapply(data.out, is.infinite)
  
  table_name <- "CouncilSablefishArea_3to7_depred"
  qry <- paste("delete from", table_name)
  sqlQuery(conn, qry)
  sqlSave(conn, data.out, tablename=table_name, append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  
  odbcClose(conn)
}

## save all datasets to an RData file for additional reporting using R
save(arearpns, arearpns_fixed, areafixed, councilarea, councilarea_fixed, fmpsubarea_fixed, fmp_fixed, akwide, 
     akwide_fixed, akwide_final, akfixed, file = paste0(OUTPUTPATH, "/results_calcrpw_summaries.RData"))


if (EXPORT_WEB_VIEWS==TRUE)
{
  ##############################################################################################
  # Output Web Stations
  # Output: web_stations.out
  #
  # This is just a convenience to make the annual process self-contained.
  # The following just selects from the WebStationView (which does all the work) for the current
  # year and puts it back into the WebStations table which supports the web application
  ##############################################################################################
  #
  #WebStations
  sql <- "select Year, Survey, Station, Area_ID, Area, Species_Code, Species, TotalCatch,"
  sql <- paste(sql, "MeanLength, MeanWeight, Latitude, Longitude")
  sql <- paste(sql, "from WebStationView")
  
  #take it out
  conn <- odbcConnect(DSN)
  web_stations.out <- sqlQuery(conn, sql)
  
  #Name columns to match database table column names exactly
  names(web_stations.out)=c("Year","Survey",
                            "Station", "Area_ID", "Area", "species_code", "Species", "TotalCatch",
                            "MeanLength", "MeanWeight", "Latitude", "Longitude")
  
  print(paste("Writing WebStations to ", OUTPUTMODE))
  if (OUTPUTMODE=="SQL") {
    #put it back
    qry <- "delete from WebStations"
    sqlQuery(conn, qry)
    sqlSave(conn, web_stations.out, tablename="WebStations", append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  }
  if (OUTPUTMODE=="CSV") {
    #Output to file
    print(paste("writing file: ", OUTPUTPATH, '\\', "WebStations.csv", sep=''))
    write.table(web_stations.out, file=paste(OUTPUTPATH, '\\', "WebStations.csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
  }
  
  ##############################################################################################
  # Output Web Areas
  # Output: web_areas.out
  #
  # This is just a convenience to make the annual process self-contained.
  # The following just selects from the WebAreaView (which does all the work) for the current
  # year and puts it back into the WebArea table which supports the web application
  ##############################################################################################
  #
  #WebAreas
  sql <- "select year, survey, Area_ID, Area, species_code, species,"
  sql <- paste(sql, "CPUE, RPN, RPW")
  sql <- paste(sql, "from WebAreaView")
  
  web_areas.out <- sqlQuery(conn, sql)
  
  print(paste("Writing WebAreas to ", OUTPUTMODE))
  if (OUTPUTMODE=="SQL") {
    #put it back
    qry <- "delete from WebAreas"
    sqlQuery(conn, qry)
    sqlSave(conn, web_areas.out, tablename="WebAreas", append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  }
  if (OUTPUTMODE=="CSV") {
    #Output to file
    print(paste("writing file: ", OUTPUTPATH, '\\', "WebAreas.csv", sep=''))
    write.table(web_areas.out, file=paste(OUTPUTPATH, '\\', "WebAreas.csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
  }
  
  odbcClose(conn)  
}