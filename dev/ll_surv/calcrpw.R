# Calculate RPN and RPW for Sablefish Longline Survey database
# Created: Rick Busch 6/2/2011
# Updated for SQL/CSV output options: Mike P. 9/19/2012
# Fixed problem updating AreaEffortsFixed: Rick B. 11/28/2012
# Added test clauses to import from .csv for vessels and strata: Rick B. 5/8/2014 
# Added export to LengthFrequenciesStrata3to7: Rick B. 11/6/2014
# Added export to AreaEffortsAll2b: Rick B. 11/13/2014
# Null out RPN and RPW for records with no area size (strata 1, 2a, 8): Rick B. 1/13/2015
# Add LastModifiedDate to all output tables, fix bug in LengthFrequenciesStrata3to7 that was causing dups: Rick B 2/3/2015
# Added sections to calculate splits for Kamchatka and Arrowtooth: Rick 7/9/2019
# Added variance, sperm whale depredation, and found diffs between this script
# and the rockfish/PT and sablefish scripts: Jane 6/9/2021
# Added variance by country: Jane 6/25/2021

# Usage: Rscript calcrpw.R <DSN> <Year>

# set up ----
# libs <- c("tidyverse", "RODBC")
# if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)], repos = "http://cran.us.r-project.org")}
# lapply(libs, library, character.only = TRUE)

library(RODBC)
library(tidyverse)

options(error = quote({
  dump.frames();
  print(attr(last.dump,"error.message"));
print(traceback())
  sink();
  q()}))

sink("rpw_output.log", append=FALSE, split=TRUE)

print("starting RPW")

# Get the ODBC data source name and Year from the args
# If Year is omitted, calculates for all years
args <- commandArgs(TRUE)
DSN <- "Longline64" 
#(connection="Driver={SQL Server};server=161.55.120.71,1919;database=LONGLINE;trusted_connection=yes;")
YEAR <- "2020"
DEP <- FALSE  			# account for sperm whale depredation on sablefish
OUTPUTMODE <- "SQL"  		# "SQL" | "CSV"
OUTPUTPATH <- "I:/Longline survey/R/CalcRPW"
# OUTPUTPATH <- "C:/Users/Jane.Sullivan/Work/Longline/R/CalcRPW"

if (!is.na(args[1])) {
	DSN <- args[1]
} 
if (!is.na(args[2])) {
	YEAR <- args[2]
} 
if (!is.na(args[3])) {
	OUTPUTMODE <- args[3]
} 
if (!is.na(args[4])) {
	OUTPUTPATH <- args[4]
} 
if (!is.na(args[5])) {
	DEP <- as.logical(args[5])
} 

##############################################################################################
# Constants
##############################################################################################

##############################################################################################
# SQL Statements
##############################################################################################
#
#Catch data (includes Depth/Hachi data, even where no catch for the hachi)
sqlCatches <- "select Cruise_Station_ID, Year, Country, Vessel_Number,"
sqlCatches <- paste(sqlCatches, "Cruise_Number, Station_Number, area_code, size_km,")
sqlCatches <- paste(sqlCatches, "hachi, IsNull(ineffective, 0), stratum, species_code, catch_freq, stratum2, size_km2,")
sqlCatches <- paste(sqlCatches, "mammal_sighting, Depredation_flag")
sqlCatches <- paste(sqlCatches, "from Catches_View")
sqlCatches <- paste(sqlCatches, "where rpn_filter is null")
sqlCatches <- paste(sqlCatches, "and IsNull(ineffective, 0) between 0 and 5")
sqlCatches <- paste(sqlCatches, "and Station_Type_ID = 1")
if (nchar(YEAR)>0) {
	sqlCatches <- paste(sqlCatches , "and Year =", YEAR)
}
sqlCatches <- paste(sqlCatches, "order by Cruise_Station_ID, Year, Country, Vessel_Number,")
sqlCatches <- paste(sqlCatches, "Cruise_Number, Station_Number,")
sqlCatches <- paste(sqlCatches, "hachi, stratum, stratum2, species_code")

#Length data
sqlLengths <- "select Cruise_Station_ID, Year, Country, Vessel_Number,"  
sqlLengths <- paste(sqlLengths, "Cruise_Number, Station_Number, area_code,")
sqlLengths <- paste(sqlLengths, "depth_stratum_id, species_code, sex, length, frequency")
sqlLengths <- paste(sqlLengths, "from Lengths_View")
sqlLengths <- paste(sqlLengths, "where Station_Type_ID = 1")
sqlLengths <- paste(sqlLengths, "and frequency > 0")
sqlLengths <- paste(sqlLengths, "and depth_stratum_id > 0")
if (nchar(YEAR)>0) {
	sqlLengths <- paste(sqlLengths, "and Year =", YEAR)
}
sqlLengths <- paste(sqlLengths, "order by Cruise_Station_ID, Year, Country, Vessel_Number,")
sqlLengths <- paste(sqlLengths, "Cruise_Number, Station_Number,")
sqlLengths <- paste(sqlLengths, "depth_stratum_id, species_code, sex, length")

#Species
sqlSpecies <- "select distinct scc.Species_Code, scc.Common_Name,"
sqlSpecies <- paste(sqlSpecies, "scc.a_vonBert, scc.b_vonBert, scc.web_reporting_flag,")
sqlSpecies <- paste(sqlSpecies, "scc.NW_AI_ratio, scc.SW_AI_ratio")
sqlSpecies <- paste(sqlSpecies, "from Specie_Catch_Codes scc")
sqlSpecies <- paste(sqlSpecies, "join TotalCatchByStratum c on scc.species_code = c.species_code")
sqlSpecies <- paste(sqlSpecies, "or scc.species_code in (10110, 10111, 10112, 30040, 30050, 30051, 30576)")
sqlSpecies <- paste(sqlSpecies, "where scc.species_code > 0")
sqlSpecies <- paste(sqlSpecies, "order by scc.Species_Code")

#Area Stratum Sizes
sqlAreaStrata <- "select a.area_code, ast.depth_stratum_id, ast.size_km"
sqlAreaStrata <- paste(sqlAreaStrata, "from area_stratum ast")
sqlAreaStrata <- paste(sqlAreaStrata, "join areas a on ast.area_id = a.area_id")

#Areas
sqlArea <- "select area_id, area_code, Council_Sablefish_Management_Area,"
sqlArea <- paste(sqlArea, "Council_sablefish_management_area_ID, Geographic_area_name, FMP_management_area,")
sqlArea <- paste(sqlArea, "Council_Management_Area_Id, Council_management_area, Exploitable")
sqlArea <- paste(sqlArea, "from Areas")
sqlArea <- paste(sqlArea, "order by area_code")

#Vessels
sqlVessels <- "select Vessel_Number, Vessel_name, Country"
sqlVessels <- paste(sqlVessels , "from Vessels")

#Depths
sqlDepths <- "select stratum, description from Depth_Stratum"

#Variance species
sqlVarSp <- "select Species_Code as species, Common_Name as common_name from Specie_Catch_Codes"
sqlUSVarSp <- paste(sqlVarSp, "where Species_Code in (310,320,440,455,475,480,10110,10112,10115,10120,20510,21220,21230,21720,30020,30050,30470,30576,99995)")

#Japanese Variance species
sqlJapanVarSp <- paste(sqlVarSp , "where Species_Code in (310,10110,10115,10120,20510,21220,21230,21720,30020)")
##############################################################################################

##############################################################################################
# Initialization
# Use either direct data access (uncomment first block of lines) 
# or exported text file (uncomment second block of lines)
##############################################################################################
#
# Initialize data tables
print(DSN)
conn <- odbcConnect(DSN)
species <- sqlQuery(conn, sqlSpecies)
catches <- sqlQuery(conn, sqlCatches)
lengths <- sqlQuery(conn, sqlLengths)
area_strata <- sqlQuery(conn, sqlAreaStrata)
areas <- sqlQuery(conn, sqlArea)
vessels <- sqlQuery(conn, sqlVessels)
strata <- sqlQuery(conn, sqlDepths)

var_us_species <- as.data.frame(sqlQuery(conn, sqlUSVarSp)) %>% mutate(country = "United States")
var_japan_species <- as.data.frame(sqlQuery(conn, sqlJapanVarSp)) %>% mutate(country = "Japan")
var_species <- bind_rows(var_us_species, var_japan_species)
var_species <- as_tibble(var_species[,c("country", "species", "common_name")])

odbcClose(conn)

#For testing in disconnected scenario: Initialize data tables from text files
#species <- read.table(file="rpn_species.csv", sep=",")
#catches <- read.table(file="catches.csv", sep=",")
#lengths <- read.table(file="lengths.csv", sep=",")
#area_strata <- read.table(file="area_strata.csv", sep=",")
#areas <- read.table(file="areas.csv", sep=",")
#vessels <- read.table(file="vessels.csv", sep=",")
#strata <- read.table(file="strata.csv", sep=",")

# The way the variances are calculated currently (in 4D arrays), R will run out
# of memory if we do all species. Jane asked Cara to identify a discrete list
# for variance estimates, these were what was provided. Japanese variances are
# only reported for species that are sampled every year. Previously variance was
# only calculated for web_reporting_flag=1, except Greenland turbot which was
# added manually.
#var_species<-read.csv(paste0(OUTPUTPATH, "/variance_species.csv"))

names(species)=c("code","common_name","vonbert_a","vonbert_b","web_flag","nw_ratio","sw_ratio")
names(catches)=c("csid","year","country","vessel","cruise","station","area","area_size",
			"hachi","ineffective","stratum","species","freq","stratum2","size_km2","sperm","spdep")
names(lengths)=c("csid","year","country","vessel","cruise","station","area",
			"stratum","species","sex","length","freq")
names(area_strata)=c("area","stratum","area_size")
names(areas)=c("area_id","area","council_area","council_area_id","geo_area","fmp_area", "council_mgmt_area_id", "council_mgmt_area", "exploitable")
names(strata)=c("stratum","description")
print("All data loaded")
##############################################################################################

##############################################################################################
#NEW: Filters, area sizes, whale depredation - some of the dep code was already
#commented out and is legacy from the Fisheries Research paper.

# Sperm whale depredation
if(length(which(is.na(catches$sperm)))) catches[which(is.na(catches$sperm)),]$sperm<-0
if(length(which(is.na(catches$spdep)))) catches[which(is.na(catches$spdep)),]$spdep<-0 # in sablefish but not rockfish/PT script
if(length(which(catches$sperm!=0))) catches[catches$sperm=="S",]$sperm<-1

# Add some random depredation events (legacy)
#yy<-cbind(unique(catches$station),rbinom(length(unique(catches$station)),1,0.2))

# yy now 'station_sperm_pres'=sperm whales present (*this is what is exists in
# rockfish/PT and what is used to increase uncertainty estimates, even though
# wcfd is the var name in the sablefish script)
# yyy now 'station_sperm_dep'=evidence of sperm whale depredation (it does not appear that this is used...)
station_sperm_pres<-data.frame(names(tapply(catches$sperm,catches$station,max)),tapply(catches$sperm,catches$station,max))
station_sperm_dep<-data.frame(names(tapply(catches$spdep,catches$station,max)),tapply(catches$spdep,catches$station,max))
if(length(station_sperm_pres[,])) colnames(station_sperm_pres)<-c("station","pres")
if(length(station_sperm_dep[,])) colnames(station_sperm_dep)<-c("station","dep") # in sablefish but not rockfish/PT script

# Depredation flag 
if(DEP==TRUE) {
  # Model 2 depredation and presence
  wcfd<-1.176471  # 1/exp(-0.162) from Table 2 (model 2)
  ## calculated from log-scale and delta method,
  ## sqrt((1/exp(-0.162))*(0.031)*(1/exp(-0.162))) from Table 2
  wcfd.se<-0.2070316 
} else {
  wcfd<-1.00
  wcfd.se<-0.00
}
# These are legacy parameters that may be useful in future sensitivity analyses.
# when the wcf=1, it doesn't change the estimate.
wcfp<-1.00
wcfp.se<-0

catches<-catches[,-c(16,17)] # makes catches identical to the version that exists prior to sperm whale dep

##############################################################################################
# Mean Weights
# Join lengths to species to get vonbert factors and calculate mean lengths
# and weights, rounded to 2 decimal places (NO they are not rounded to 2 decimal places?! RB 5/13/21)
# Output: mean_weights
##############################################################################################
#
#tack on vonberts and calculate weights
ls.match = match(lengths$species, species$code)
lengths$vonbert_a = rep(NA, dim(lengths)[1])
lengths$vonbert_a = species[ls.match, 3]
lengths$vonbert_b = rep(NA, dim(lengths)[1])
lengths$vonbert_b = species[ls.match, 4]
lengths$unit_wt = rep(NA, dim(lengths)[1])
lengths$unit_wt = (lengths$vonbert_a * (lengths$length ^ lengths$vonbert_b))
rm(ls.match)

#summarize total length frequency by cruise, station, species, and stratum
lengths$cs.str.spc = paste(lengths$csid, lengths$stratum, lengths$species)
total_freq = tapply(lengths$freq, lengths$cs.str.spc, sum)
temp.match = match(names(total_freq), lengths$cs.str.spc)
mean_weights = lengths[temp.match, 1:9]
mean_weights$total_freq = rep(NA, dim(mean_weights)[1])
mean_weights$total_freq = as.numeric(total_freq)
rm(total_freq)

#tack on mean length
lenxfreq = tapply(lengths$length * lengths$freq, lengths$cs.str.spc, sum)
mean_weights$lenxfreq = rep(NA, dim(mean_weights)[1])
mean_weights$lenxfreq = as.numeric(lenxfreq)
mean_weights$mean_len = rep(NA, dim(mean_weights)[1])
mean_weights$mean_len = mean_weights$lenxfreq / mean_weights$total_freq
rm(lenxfreq)

#tack on mean weight
wtxfreq = tapply(lengths$unit_wt * lengths$freq, lengths$cs.str.spc, sum)
mean_weights$wtxfreq = rep(NA, dim(mean_weights)[1])
mean_weights$wtxfreq = as.numeric(wtxfreq)
mean_weights$mean_wt = rep(NA, dim(mean_weights)[1])
mean_weights$mean_wt = mean_weights$wtxfreq / mean_weights$total_freq
rm(wtxfreq)
print("Calculated Mean Weights")
##############################################################################################

##############################################################################################
# Expand Total Catch
# Extract depth records from catches (1 record for each skate)
# Cross join depth on species to get unique rows for each skate and species
# Expand catches table to include rows with 0 catch for all skates and species where
# no catch occurred
# Output: catches
##############################################################################################
#
#filter down to the unique hachis (which correspond 1-1 to depth records)
catches$cs.h = paste(catches$csid, catches$hachi)
temp = tapply(catches$freq, catches$cs.h, sum)
temp.match = match(names(temp), catches$cs.h)
depths = catches[temp.match,]

#cross join depths on species to get unique skate (depth) record for each skate and species
temp=depths[,c(1:11,14:15)]
depths = merge(temp, species[,1])
names(depths) = c("csid","year","country","vessel","cruise","station","area","area_size",
			"hachi","ineffective","stratum","stratum2","area_size2","species")
depths$freq=rep(0, dim(depths)[1])
depths$cs.h.spc = paste(depths$csid, depths$hachi, depths$species)
rm(temp)

#expand catches to include rows for all species with 0 catch at all skates
catches$cs.h.spc = paste(catches$csid, catches$hachi, catches$species)
temp.match = match(depths$cs.h.spc, catches$cs.h.spc)
depths$freq = catches[temp.match,13]
depths$freq = ifelse(is.na(depths$freq),0,depths$freq)
catches = depths
rm(depths)
rm(temp.match)
print("Expanded Total Catch")
##############################################################################################

##############################################################################################
# SRRE
# Split catch of SRRE (code 30051) to individual codes for 
# Shortraker (30576) and Rougheye (30050) based on rates from Length data
# NOTE: catches have been pre-filtered to exclude catch with killer whale 
#  predation (rpn_filter = 'k') - lengths have not, so there may be cases with non-zero
#  shortraker and rougheye rates calculated from lengths but no corresponding catch frequencies
# More Notes: The US survey used code 30051 from 1998 on for SRRE, but used 30040 before 1998 
#  so this code splits US data accordingly based on year.
#  The Japaneese survey collected lengths for SR and RE in all years (1979-1994), but all length data
#  has stratum = 0, so none of it can be used to calculate rates for SR and RE from the group code
#  they used (30040)
#  Catch data for the Japaneese survey appears to have been split to 30050 and 30576 in 1993 and 1994 
#  Just 4 rougheyes (30050) recorded in 1992
#  Anyway, we don't attempt to split the Japaneese catch data, just take it as it is  
# Output: catches
##############################################################################################
#
#copy out just the SRRE catch and key it by station and stratum
if (YEAR>=1998)
	catches.srre = catches[catches$species==30051,]
if (YEAR<1998)
	catches.srre = catches[catches$species==30040&catches$country=='United States',]

catches.srre$cntry.sta.str=paste(catches.srre$country, catches.srre$station, catches.srre$stratum)

#link catch summary of SRREs to length summary for shortrakers
lengths.srkr=mean_weights[mean_weights$species==30576,]
lengths.srkr$cntry.sta.str=paste(lengths.srkr$country, lengths.srkr$station, lengths.srkr$stratum)
temp.match=match(catches.srre$cntry.sta.str,lengths.srkr$cntry.sta.str)
catches.srre$srkr_freq=rep(NA,dim(catches.srre)[1])
catches.srre$srkr_freq=lengths.srkr[temp.match,10]
rm(lengths.srkr)

#link catch summary of SRREs to length summary for rougheyes
lengths.reye=mean_weights[mean_weights$species==30050,]
lengths.reye$cntry.sta.str=paste(lengths.reye$country, lengths.reye$station, lengths.reye$stratum)
temp.match=match(catches.srre$cntry.sta.str,lengths.reye$cntry.sta.str)
catches.srre$reye_freq=rep(NA,dim(catches.srre)[1])
catches.srre$reye_freq=lengths.reye[temp.match,10]
rm(lengths.reye)
rm(temp.match)

#replace NAs with 0s in length fields
catches.srre$reye_freq=ifelse(is.na(catches.srre$reye_freq),0,catches.srre$reye_freq)
catches.srre$srkr_freq=ifelse(is.na(catches.srre$srkr_freq),0,catches.srre$srkr_freq)

#calculate rates and total catch of rougheye vs shortraker
catches.srre$srkr_rate=rep(NA,dim(catches.srre)[1])
catches.srre$srkr_rate=catches.srre$srkr_freq / (catches.srre$srkr_freq + catches.srre$reye_freq)
catches.srre$srkr_rate=ifelse(is.nan(catches.srre$srkr_rate),0,catches.srre$srkr_rate) # not in the sablefish or rockfish/PT script					
catches.srre$reye_rate=rep(NA,dim(catches.srre)[1])
catches.srre$reye_rate=catches.srre$reye_freq / (catches.srre$srkr_freq + catches.srre$reye_freq)
catches.srre$reye_rate=ifelse(is.nan(catches.srre$reye_rate),0,catches.srre$reye_rate) # not in the sablefish or rockfish/PT script					
catches.srre$srkr_catch=rep(NA,dim(catches.srre)[1])
catches.srre$srkr_catch=catches.srre$freq * catches.srre$srkr_rate
catches.srre$reye_catch=rep(NA,dim(catches.srre)[1])
catches.srre$reye_catch=catches.srre$freq * catches.srre$reye_rate

#duplicate rows into new table for shortrakers, using the split catch freq in place of freq 
catches.sr = catches.srre[catches.srre$srkr_catch>=0, c(1:13,22)]
names(catches.sr) = c("csid","year","country","vessel","cruise","station","area","area_size","hachi","ineffective","stratum","stratum2","area_size2","freq")
catches.sr$species = rep(30576, dim(catches.sr)[1])
catches.sr$cs.h.spc = paste(catches.sr$csid, catches.sr$hachi, catches.sr$species)

#duplicate rows into new table for rougheyes, using the split catch freq in place of freq 
catches.re = catches.srre[catches.srre$reye_catch>=0, c(1:13,23)]
names(catches.re) = c("csid","year","country","vessel","cruise","station","area","area_size","hachi","ineffective","stratum","stratum2","area_size2","freq")
catches.re$species = rep(30050, dim(catches.re)[1])
catches.re$cs.h.spc = paste(catches.re$csid, catches.re$hachi, catches.re$species)

#strip out rows with srre group code
#  Apparently some catch was recorded with species code 30576 in 2017
#  and also one with species code 30050 in 2003
#  the database views filter these codes out of CPUE calculations assuming they will be split
#  from catch recorded with 30051, so that's what we do here now too

if (YEAR>=1998)
	catches = catches[catches$species!=30051&catches$species!=30576&catches$species!=30050,]
if (YEAR<1998)
	catches = catches[catches$species!=30040|catches$country=='Japan',]

#append sr and re tables from above
catches = rbind(catches, catches.sr)
catches = rbind(catches, catches.re)
rm(catches.sr)
rm(catches.re)
rm(catches.srre)
print("Split SRRE")
##############################################################################################

##############################################################################################
#KMAR Split catch of Kamchatka/Arrowtooth (code 10111) to individual codes for
#Arrowtooth (10110) and Kamchatka (10112) based on rates from Length data for
#10111 NOTE: catches have been pre-filtered to exclude catch with killer whale
#predation (rpn_filter = 'k') - lengths have not, so there may be cases with
#non-zero kamchatka and arrowtooth rates calculated from lengths but no
#corresponding catch frequencies More Notes: KMAR splits begin in 2019. Before
#2019 all catch and lengths were reported as just Arrowtooth (10110) 
# Output: catches
##############################################################################################
#
# ```
# This entire 'KMAR' code section does not appear in the sablefish or rockfish/PT
# scripts... In the rockfish/PT script there is the following chunk (it doesn't
# appear in the sablefish script):
###
# Change Atheresthes sp. (10111) back to ATF code (10110) in Catches since only one year being separated
# Change Kam (10112) code in lengths to ATF code (10110) for comparing previous years
###
# catches$species = ifelse(catches$species==10111, 10110, catches$species)
# lengths$species = ifelse(lengths$species==10112, 10110, lengths$species)
# ```

if (YEAR>=2019)
{
	#copy out just the KMAR catch and key it by station and stratum
	catches.kmar = catches[catches$species==10111,]
	catches.kmar$cntry.sta.str=paste(catches.kmar$country, catches.kmar$station, catches.kmar$stratum)

	#link catch summary of KMARs to length summary for Arrowtooth
	lengths.ar=mean_weights[mean_weights$species==10110,]
	lengths.ar$cntry.sta.str=paste(lengths.ar$country, lengths.ar$station, lengths.ar$stratum)
	temp.match=match(catches.kmar$cntry.sta.str,lengths.ar$cntry.sta.str)
	catches.kmar$ar_freq=rep(NA,dim(catches.kmar)[1])
	catches.kmar$ar_freq=lengths.ar[temp.match,"total_freq"]
	rm(lengths.ar)

	#link catch summary of KMARs to length summary for kamchatka
	lengths.km=mean_weights[mean_weights$species==10112,]
	lengths.km$cntry.sta.str=paste(lengths.km$country, lengths.km$station, lengths.km$stratum)
	temp.match=match(catches.kmar$cntry.sta.str,lengths.km$cntry.sta.str)
	catches.kmar$km_freq=rep(NA,dim(catches.kmar)[1])
	catches.kmar$km_freq=lengths.km[temp.match,"total_freq"]
	rm(lengths.km)
	rm(temp.match)

	#replace NAs with 0s in length fields
	catches.kmar$km_freq=ifelse(is.na(catches.kmar$km_freq),0,catches.kmar$km_freq)
	catches.kmar$ar_freq=ifelse(is.na(catches.kmar$ar_freq),0,catches.kmar$ar_freq)

	#calculate rates and total catch of kamchatka vs arrowtooth
	catches.kmar$ar_rate=rep(NA,dim(catches.kmar)[1])
	catches.kmar$ar_rate=catches.kmar$ar_freq / (catches.kmar$ar_freq + catches.kmar$km_freq)
	catches.kmar$ar_rate=ifelse(is.nan(catches.kmar$ar_rate),0,catches.kmar$ar_rate)					
	catches.kmar$km_rate=rep(NA,dim(catches.kmar)[1])
	catches.kmar$km_rate=catches.kmar$km_freq / (catches.kmar$ar_freq + catches.kmar$km_freq)
	catches.kmar$km_rate=ifelse(is.nan(catches.kmar$km_rate),0,catches.kmar$km_rate)					
	catches.kmar$ar_catch=rep(NA,dim(catches.kmar)[1])
	catches.kmar$ar_catch=catches.kmar$freq * catches.kmar$ar_rate
	catches.kmar$km_catch=rep(NA,dim(catches.kmar)[1])
	catches.kmar$km_catch=catches.kmar$freq * catches.kmar$km_rate

	#duplicate rows into new table for arrowtooth, using the split catch freq in place of freq 
	catches.ar = catches.kmar[catches.kmar$ar_catch>=0, c(1:13,22)]
	names(catches.ar) = c("csid","year","country","vessel","cruise","station","area","area_size","hachi","ineffective","stratum","stratum2","area_size2","freq")
	catches.ar$species = rep(10110, dim(catches.ar)[1])
	catches.ar$cs.h.spc = paste(catches.ar$csid, catches.ar$hachi, catches.ar$species)

	#duplicate rows into new table for kamchatka, using the split catch freq in place of freq 
	catches.km = catches.kmar[catches.kmar$km_catch>=0, c(1:13,23)]
	names(catches.km) = c("csid","year","country","vessel","cruise","station","area","area_size","hachi","ineffective","stratum","stratum2","area_size2","freq")
	catches.km$species = rep(10112, dim(catches.km)[1])
	catches.km$cs.h.spc = paste(catches.km$csid, catches.km$hachi, catches.km$species)

	#strip out rows with kmar group code
	catches = catches[catches$species!=10111,]

	#append ar and km tables from above
	catches = rbind(catches, catches.ar)
	catches = rbind(catches, catches.km)
	rm(catches.ar)
	rm(catches.km)
	rm(catches.kmar)
	print("Split KMAR")
}
##############################################################################################

print("Reticulating Splines")

##############################################################################################
# Total Catch
# Calculate total catch by stratum, adjusting for ineffective hooks to 45 hooks
# per skate
# Output: total_catch
##############################################################################################
#
#summarize total catch frequency by cruise, station, hachi, and species, adjusted to 45 hooks
adj_freq = tapply((catches$freq / (45 - catches$ineffective)) * 45, catches$cs.h.spc, sum)
temp.match = match(names(adj_freq), catches$cs.h.spc)
hachi_catch = catches[temp.match, 1:14]
hachi_catch$adj_catch_freq = rep(NA, dim(hachi_catch)[1])
hachi_catch$adj_catch_freq = as.numeric(adj_freq)
rm(adj_freq)

#summarize adjusted catch to stratum2
hachi_catch$cs.str.spc = paste(hachi_catch$csid, hachi_catch$stratum2, hachi_catch$species)

# save an uncorrected version for variance calculations later, i.e. the version
# of hachi_catch used to get variances doesn't include the whale-corrected
# CPUEs, though ultimately the variance is inflated for stations with
# depredation present. The latter is a separate correction. hachi_catch_nodep replace whalehachi
hachi_catch_nodep<-hachi_catch
hachinames<-names(hachi_catch)
hachi_catch<-merge(hachi_catch,station_sperm_pres,all.x=TRUE)
hachi_catch<-merge(hachi_catch,station_sperm_dep,all.x=TRUE) # only in sablefish script, not in rockfish/PT scripts
hachi_catch$pres<-as.numeric(hachi_catch$pres)
hachi_catch[hachi_catch$dep==1&hachi_catch$species==20510,]$adj_catch_freq=wcfd*hachi_catch[hachi_catch$dep==1&hachi_catch$species==20510,]$adj_catch_freq
hachi_catch[hachi_catch$dep==0&hachi_catch$species==20510&hachi_catch$pres==1,]$adj_catch_freq=wcfp*hachi_catch[hachi_catch$dep==0&hachi_catch$species==20510&hachi_catch$pres==1,]$adj_catch_freq # only in sablefish script, not in rockfish/PT scripts
hachi_catch<-hachi_catch[,hachinames]

catch_sum = tapply(hachi_catch$adj_catch_freq, hachi_catch$cs.str.spc, sum)
temp.match = match(names(catch_sum), hachi_catch$cs.str.spc)

#keep both stratum and stratum2 because we need stratum to link to mean lengths/weights below
total_catch = hachi_catch[temp.match,c(1:8,13,11,12,14)]
total_catch$tot_catch_freq = rep(NA, dim(total_catch)[1])
total_catch$tot_catch_freq = as.numeric(catch_sum)
#rm(hachi_catch)
#rm(catch_sum)
#rm(temp.match)
print("Calculated Total Catch")
##############################################################################################

##############################################################################################
# Total Effort
# Calculate total skates by stratum
# Output: total_skates
##############################################################################################
#
#filter down to the unique hachis (which correspond 1-1 to depth records)
catches$cs.h = paste(catches$csid, catches$hachi)
temp = tapply(catches$freq, catches$cs.h, sum)
temp.match = match(names(temp), catches$cs.h)
skates = catches[temp.match,]

#count the unique hachis at each stratum
skates$cs.str = paste(skates$csid, skates$stratum2)
num_skates = tapply(skates$hachi, skates$cs.str, length)
temp.match = match(names(num_skates), skates$cs.str)
#keep both stratum and stratum2 because we need stratum to link to mean lengths/weights below
total_skates = skates[temp.match, c(1:8,13,11,12,14)]
total_skates$num_hachis = rep(NA, dim(total_skates)[1])
total_skates$num_hachis = as.numeric(num_skates)
rm(skates)
rm(temp)
print("Calculated Total Effort")
##############################################################################################

##############################################################################################
# CPUE
# Now given the summaries of Catch, Effort (skates) and Length/Weight information from above:
# Calculate the Catch Per Unit of Effort by station, species, and stratum 
# Also calculates the Relative Population Numbers (RPN) and Relative Population Weights (RPWs)
# Returns a table of catch summary information by station-stratum-species
# Output: total_catch
##############################################################################################
#tie skates to catch on station and stratum
total_catch$cs.str2 = paste(total_catch$csid, total_catch$stratum2)
total_skates$cs.str2 = paste(total_skates$csid, total_skates$stratum2)
temp.match = match(total_catch$cs.str2, total_skates$cs.str2)
total_catch$total_skates = rep(NA, dim(total_catch)[1])
total_catch$total_skates = total_skates[temp.match, 13]
total_catch$cpue = rep(NA, dim(total_catch)[1])
total_catch$cpue = total_catch$tot_catch_freq / total_catch$total_skates
rm(total_skates)

#tack on mean lengths and weights
#note we use stratum (not stratum2) to link to weights because there are no stratum2 values in weights/lengths
total_catch$cs.str.spc = paste(total_catch$csid, total_catch$stratum, total_catch$species)
mean_weights$cs.str.spc = paste(mean_weights$csid, mean_weights$stratum, mean_weights$species)
temp.match = match(total_catch$cs.str.spc, mean_weights$cs.str.spc)
total_catch$total_len_freq = rep(NA, dim(total_catch)[1])
total_catch$total_len_freq = mean_weights[temp.match, 10]
total_catch$mean_len = rep(NA, dim(total_catch)[1])
total_catch$mean_len = mean_weights[temp.match, 12]
total_catch$mean_wt = rep(NA, dim(total_catch)[1])
total_catch$mean_wt = mean_weights[temp.match, 14]

#calculate RPN & RPW
total_catch$RPN = rep(NA, dim(total_catch)[1])
total_catch$RPN = total_catch$cpue * total_catch$area_size2
total_catch$RPW = rep(NA, dim(total_catch)[1])
total_catch$RPW = total_catch$cpue * total_catch$area_size2 * total_catch$mean_wt
print("Calculated CPUE, RPN, RPW")
##############################################################################################

##############################################################################################
# Extrapolate Gullies
# Extrapolate CPUE, RPN, and RPW for species in stations 142, 143 at depth stratum 5 
# to depth stratum 6 and in stations 144, 145 at depth stratum 4 to depth stratum 5
# Creates copies all rows from total_catch for the stratums where there is data and assigns
# the deeper stratum to the copies. Then recalculates the RPN and RPW using the area size
# of the new stratum
# Output: total_catch
##############################################################################################
#
#extrapolate values for depth stratum 5 on stations 142, 143 to depth stratum 6
total_catch.extrap = total_catch[(total_catch$station==142|total_catch$station==143)
						&total_catch$stratum==5,]
total_catch.extrap$stratum = rep(6, dim(total_catch.extrap)[1])
total_catch.extrap$stratum2 = rep(6, dim(total_catch.extrap)[1])
total_catch.extrap$cs.str2 = paste(total_catch.extrap$csid, total_catch.extrap$stratum2)
temp.match = match(paste(total_catch.extrap$area, total_catch.extrap$stratum2), 
				paste(area_strata$area, area_strata$stratum))
total_catch.extrap$area_size = area_strata[temp.match,3]
total_catch.extrap$area_size2 = area_strata[temp.match,3]
total_catch.extrap$RPN = total_catch.extrap$cpue * total_catch.extrap$area_size2
total_catch.extrap$RPW = total_catch.extrap$cpue * total_catch.extrap$area_size2 * total_catch.extrap$mean_wt
total_catch.extrap$cs.str.spc = paste(total_catch.extrap$csid, total_catch.extrap$stratum, total_catch.extrap$species)
total_catch = rbind(total_catch, total_catch.extrap)

#extrapolate values for depth stratum 4 on stations 144, 145 to depth stratum 5
total_catch.extrap = total_catch[(total_catch$station==144|total_catch$station==145)
						&total_catch$stratum==4,]
total_catch.extrap$stratum = rep(5, dim(total_catch.extrap)[1])
total_catch.extrap$stratum2 = rep(5, dim(total_catch.extrap)[1])
total_catch.extrap$cs.str2 = paste(total_catch.extrap$csid, total_catch.extrap$stratum2)
temp.match = match(paste(total_catch.extrap$area, total_catch.extrap$stratum2), 
				paste(area_strata$area, area_strata$stratum))
total_catch.extrap$area_size = area_strata[temp.match,3]
total_catch.extrap$area_size2 = area_strata[temp.match,3]
total_catch.extrap$RPN = total_catch.extrap$cpue * total_catch.extrap$area_size2
total_catch.extrap$RPW = total_catch.extrap$cpue * total_catch.extrap$area_size2 * total_catch.extrap$mean_wt
total_catch.extrap$cs.str.spc = paste(total_catch.extrap$csid, total_catch.extrap$stratum, total_catch.extrap$species)
total_catch = rbind(total_catch, total_catch.extrap)
rm(total_catch.extrap)
rm(temp.match)
print("Extrapolated Gullies")
##############################################################################################

##############################################################################################
# Calculate Mean Lengths and Weights by Area and Stratum
# Aggregate total_catch to area (summing over station) to come up with mean lengths and weights
# by area
# Output: mean_lengths, mean_wts, num_rpw_stations
##############################################################################################
#
#count the number of station records in each area-stratum where catch data exists for each species
total_catch$yr.ves.area.str2.spc = paste(total_catch$year,
							total_catch$vessel,
							total_catch$area,
							total_catch$stratum2,
							total_catch$species)
num_stations = tapply(total_catch$station, total_catch$yr.ves.area.str2.spc, length)
temp.match = match(total_catch$yr.ves.area.str2.spc, names(num_stations))
total_catch$num_stations = rep(NA, dim(total_catch)[1])
total_catch$num_stations = num_stations[temp.match]
rm(num_stations)

#count the number of station records in each area-stratum excluding any
# where catch data exists for each species, area sizes are known, but mean weight can not be calculated
total_catch_rpw = total_catch[!(total_catch$tot_catch_freq>0
					&total_catch$total_skates>0
					&!is.na(total_catch$area_size2)
					&total_catch$area_size2>0
					&is.na(total_catch$mean_wt)),]
num_rpw_stations = tapply(total_catch_rpw$station, total_catch_rpw$yr.ves.area.str2.spc, length)
temp.match = match(total_catch$yr.ves.area.str2.spc, names(num_rpw_stations))
total_catch$rpw_stations = num_rpw_stations[temp.match]

#calculate mean lengths by area-stratum 
#only use rows where mean length has been calculated (i.e. lengths exist for the area-species-stratum)
total_catch_w_len=total_catch[!is.na(total_catch$mean_len),]
cpuexlen = tapply(total_catch_w_len$cpue * total_catch_w_len$mean_len, total_catch_w_len$yr.ves.area.str2.spc, sum)
temp.match = match(names(cpuexlen), total_catch_w_len$yr.ves.area.str2.spc)
mean_lengths = total_catch_w_len[temp.match,c(2:5,7:12,23:25)]
mean_lengths$cpuexlen = rep(NA, dim(mean_lengths)[1])
mean_lengths$cpuexlen = cpuexlen
rm(cpuexlen)
cpue = tapply(total_catch_w_len$cpue, total_catch_w_len$yr.ves.area.str2.spc, sum)
mean_lengths$mean_len = rep(NA, dim(mean_lengths)[1])
mean_lengths$mean_len = mean_lengths$cpuexlen / cpue
rm(total_catch_w_len)

#now join species so we can recalculate mean weight
ls.match = match(mean_lengths$species, species$code)
mean_lengths$vonbert_a = rep(NA, dim(mean_lengths)[1])
mean_lengths$vonbert_a = species[ls.match, 3]
mean_lengths$vonbert_b = rep(NA, dim(mean_lengths)[1])
mean_lengths$vonbert_b = species[ls.match, 4]
mean_lengths$mean_wt = rep(NA, dim(mean_lengths)[1])
mean_lengths$mean_wt = (mean_lengths$vonbert_a * (mean_lengths$mean_len ^ mean_lengths$vonbert_b))
rm(ls.match)
print("Calculated Mean Weights and Lengths")
##############################################################################################

# in both sablefish and rockfish/PT scripts, the 'Length Frequencies' section is
# next, followed by variance calcs which originally was nested within the
# 'Calculate Ave CPUE, RPN, and RPW' section (it's been put into it's own
# section and slightly refactored). Because length freq methods were updated in
# 2021 in this script, it's doubtful they reconcile with the other scripts

##############################################################################################
# Variance-covariance calculations with whale correction if specified. Variances
# reported by geographic area, accounting for covariance among strata within a
# geographic area. Variances reported for US and Japanese data separately. User
# beware: that the Japanese survey design has not been reviewed thoroughly to
# ensure the same variance calculations are valid for this data set.
# Output: allvar_2b_7 and allvar_3_7
##############################################################################################

# Variance species defined by Cara
hachi_catch_nodep<-hachi_catch_nodep[hachi_catch_nodep$species%in%unique(var_species$species),]

countries <- unique(hachi_catch_nodep$country)

for(c in countries) {
  
  # Calculate variances for US an Japanese data separately. The survey design for
  # Japanese data were not reviewed to ensure these variance methods are
  # appropriate - user beware.
  hachi_catch_country<-hachi_catch_nodep[hachi_catch_nodep$country==c,]
  
  # Dimension for output array
  n.species<-length(unique(var_species$species)) # length(species[species$web_flag==1&!is.na(species$web_flag),1])
  n.area<-length(levels(factor(hachi_catch_country$area)))
  n.stratum<-length(levels(factor(hachi_catch_country$stratum2)))
  # area.vcov.cf <- array(NA, c(n.area,n.area,n.species))
  
  # weights = area sizes
  weights<-tapply(hachi_catch_country$area_size2,list(hachi_catch_country$stratum2,hachi_catch_country$area),min)
  weights[is.na(weights)]<-0
  
  # station_obs (formerly station.obs) = mean catch per hachi (cpue) by station x
  # stratum2 x geographic area and species. Each of these means equals one
  # observation; observations of mean cpue are not weighted by the number of
  # hachis in the station x stratum2 x area combination.
  station_obs<-tapply(hachi_catch_country$adj_catch_freq,list(hachi_catch_country$station,hachi_catch_country$stratum2,hachi_catch_country$species,hachi_catch_country$area), mean)
  
  dim(station_obs)==c(length(unique(hachi_catch_country$station)),
                      n.stratum,
                      n.species,
                      n.area)
  
  # sperm_pres (formerly 'G') is an array with same dimensions as station_obs
  # populated with 1 or 0 for presence/absence of sperm whales at that station
  # observation
  sperm_pres<-station_obs
  sperm_pres[,,,]<-NA
  # index pointing to sablefish slice in the array (only sablefish are corrected
  # for whale depredation)
  species_index <- sort(unique(hachi_catch_country$species))
  sable_index<-which(species_index %in% 20510)
  not_sable_index<-which(!c(species_index %in% 20510))
  # populate array
  sperm_pres[,,sable_index,]<-as.numeric(station_sperm_pres[station_sperm_pres$station %in% unique(hachi_catch_country$station),"pres"])
  sperm_pres[,,not_sable_index,]<-0
  
  # sperm_pres_wcf (formerly 'D') is an array with same dimensions as station_obs
  # and sperm_pres. replace 1s with the whale correction factors and 0s with 1s.
  # Stations with values not equal to 1 will be corrected.
  sperm_pres_wcf<-sperm_pres
  sperm_pres_wcf[sperm_pres==1]=wcfd
  sperm_pres_wcf[sperm_pres==0]=1
  station=station_obs*sperm_pres_wcf
  
  all.equal(which(is.na(station)), which(is.na(station_obs))) # should be TRUE
  
  # Area covariances
  
  ##(B) Variances + Covariances due to stations observations only (without wcfd.se)
  
  stratum <- apply(station, c(2,3,4), mean, na.rm=TRUE)	# stratum means (by area, each station equal weight)
  for (i in 1:n.area) {stratum[,,i] <- stratum[,,i]*weights[,i]}	# multiply stratum means by area-stratum weights
  # area <- apply(stratum, c(2,3), sum, na.rm=T) # area sum (across weighted strata)
  # index <- rowSums(area) # total (across areas)
  
  stratum.var <- apply(station, c(2,3,4), var, na.rm=TRUE)/apply(!is.na(station), c(2,3,4), sum)	# variances of strata means
  for (i in 1:n.area) {stratum.var[,,i] <- stratum.var[,,i]*weights[,i]^2} # multiply stratum var by weights^2
  stratum.cv<-sqrt(stratum.var)/stratum # stratum CV
  
  # Identify cases where the number of stations in a species/geographic
  # area/stratum is 1, then fix the area/strata combinations with only one station
  # to the average CV e.g. variance = (mean(cv) * mean)^2 for that species across
  # all areas.
  onestation<-apply(!is.na(station), c(2,3,4), sum)
  onestation[onestation>1]<-0
  # stratum.var2 <- stratum.var
  for(s in 1:n.species) {
    stratum.var[,s,][onestation[,s,]==1]=(mean(stratum.cv[,s,],na.rm=TRUE)*stratum[,s,][onestation[,s,]==1])^2
  }
  # length(which(stratum.var == stratum.var2)) + length(which(is.na(stratum.var2))) == prod(dim(stratum.var))
  
  # area.var <- apply(stratum.var, c(2,3), sum, na.rm=T) # geographic area variances (ignoring covariances for now)
  # index.var <- rowSums(area.var) # index variances by year (ignoring covariances)
  # index.var.cv <- sqrt(index.var)/rowSums(area)	# CVs without covariances
  
  # objects for covariance results
  # area.cov <- area.var; area.cov[,] <- NA
  stratum.vcov <- array(NA, c(n.stratum, n.stratum, n.species, n.area))
  dimnames(stratum.vcov) <- list(levels(factor(hachi_catch_country$stratum2)), 
                                 levels(factor(hachi_catch_country$stratum2)),
                                 levels(factor(hachi_catch_country$species)),
                                 levels(factor(hachi_catch_country$area)))
  
  # covariance without whale depredation, using same notation as in the
  # documentation
  for (i in 1:n.area) {		 # loop over areas				
    for (s in 1:n.species) {	# loop over species
      for (j in 1:n.stratum) {	# loop over strata j
        for (m in 1:n.stratum) { # loop over strata m
          w.j <- weights[j,i]; w.m <- weights[m,i]				# stratum weights (j and m)
          k <- station[,j,s,i]; z <- station[,m,s,i]			# station means {k} for j and {z} for m
          q <- cov(k, z, use="pairwise.complete.obs")			# station variance (when j = m) or covariance (j != m)
          n.k <- sum(!is.na(k)); n.z <- sum(!is.na(z))		# numbers of stations with data (j or m)
          n.kz <- sum(!is.na(k*z))						            # numbers of station pairs (j and m) both with data 
          stratum.vcov[j,m,s,i] <- w.j*w.m*n.kz/(n.k*n.z)*q	
          diag(stratum.vcov[,,s,i])=stratum.var[,s,i]
        }
      }
      # area.cov[s,i] <- sum(stratum.vcov[,,s,i], na.rm=T)	# sum of weighted var-covariance by area
    }
  }
  
  # Final geographic areas should be separated for strata 2b-7 and 3-7 so
  # covariances sum properly
  strat_2b_7 <- c("2b","3","4","5","6","7")
  strat_3_7 <- c("3","4","5","6","7")
  
  # Sum stratum variance-covariances to the geographic area level. On the rare
  # occurence when the stratum var-cov matrix sums to a negative number, replace
  # it with the sum of the stratum-specific variances (i.e. ignore the
  # covariances). Repeat for strata 2b-7 and 3-7 combinations.
  var_2b_7 <- apply(stratum.vcov[strat_2b_7, strat_2b_7, , ], c(3,4), sum, na.rm=TRUE)
  var_2b_7[var_2b_7 < 0] <- (apply(stratum.var[strat_2b_7, , ], c(2,3), sum, na.rm=TRUE))[var_2b_7 < 0]
  
  var_3_7 <- apply(stratum.vcov[strat_3_7, strat_3_7, , ], c(3,4), sum, na.rm=TRUE)
  var_3_7[var_3_7 < 0] <- (apply(stratum.var[strat_3_7, , ], c(2,3), sum, na.rm=TRUE))[var_3_7 < 0]
  
  # Sablefish variances and covariances due to sperm whale PRESENCE. Note
  # that only the variances have been used for sablefish RPNs.
  sperm_vcov_2b_7 <- sperm_vcov_3_7 <- array(NA, dim = c(n.area, n.area), 
                                             dimnames = list(levels(factor(hachi_catch_country$area)), 
                                                             levels(factor(hachi_catch_country$area))))
  
  # index pointing to strata slices in the array (only sablefish are corrected
  # for whale depredation)
  strata_index <- sort(unique(hachi_catch_country$stratum2))
  index_2b_7 <- which(strata_index %in% strat_2b_7)
  index_3_7 <- which(strata_index %in% strat_3_7)
  
  # Variance is inflated at stations where sperm whales are present, not just on
  # stations where there is evidence of depredation. As such, the observed station
  # CPUE is used rather than the whale-corrected station CPUE. Note that only the
  # variances by geographic area have been used for the whale-corrected sablefish
  # RPNs, and we have preserved that convention. However, the covariance among
  # areas IS calculated, and could theoretically be used if someone wanted that.
  # station_obs = array indexed as c(station, stratum2, species, geographic area)
  
  # Depth strata 2b-7
  for (i in 1:n.area) {
    for (r in 1:n.area) {
      temp.cov <- 0
      for (j in index_2b_7) {
        for (m in index_2b_7) {
          w.j <- weights[j,i]; w.m <- weights[m,r]				                        # stratum area sizes / weights (j,i) and (r,m)
          k <- station_obs[,j,sable_index,i]; z <- station_obs[,m,sable_index,r]	# observed station means {k} for j and {z} for m
          n.k <- sum(!is.na(k)); n.z <- sum(!is.na(z))			                      # numbers of stations with data (j or m)
          k.sum <- sum(sperm_pres[,j,sable_index,i]*k, na.rm=TRUE)				        # sum of sperm_present (0 or 1) * observed station mean CPUEs {k} in stratum j
          z.sum <- sum(sperm_pres[,m,sable_index,r]*z, na.rm=TRUE)			        	# sum of sperm_present (0 or 1) * observed station mean CPUEs {z} in stratum m
          q <- (w.j*w.m)/(n.k*n.z)*(wcfd.se^2*k.sum*z.sum)			                  # variance/covariance due to correction factor
          if (is.na(q)) q <- 0
          temp.cov <- temp.cov + q	                                              # Sum of stratum combos
        }
      }
      sperm_vcov_2b_7[i,r] <- temp.cov
    }
  }
  
  # Repeat for depth strata 3-7
  for (i in 1:n.area) {
    for (r in 1:n.area) {
      temp.cov <- 0
      for (j in index_3_7) {
        for (m in index_3_7) {
          w.j <- weights[j,i]; w.m <- weights[m,r]				                        # stratum area sizes / weights (j,i) and (r,m)
          k <- station_obs[,j,sable_index,i]; z <- station_obs[,m,sable_index,r]	# observed station means {k} for j and {z} for m
          n.k <- sum(!is.na(k)); n.z <- sum(!is.na(z))			                      # numbers of stations with data (j or m)
          k.sum <- sum(sperm_pres[,j,sable_index,i]*k, na.rm=TRUE)				        # sum of sperm_present (0 or 1) * observed station mean CPUEs {k} in stratum j
          z.sum <- sum(sperm_pres[,m,sable_index,r]*z, na.rm=TRUE)			        	# sum of sperm_present (0 or 1) * observed station mean CPUEs {z} in stratum m
          q <- (w.j*w.m)/(n.k*n.z)*(wcfd.se^2*k.sum*z.sum)			                  # variance/covariance due to correction factor
          if (is.na(q)) q <- 0
          temp.cov <- temp.cov + q	                                              # Sum of stratum combos
        }
      }
      sperm_vcov_3_7[i,r] <- temp.cov
    }
  }
  
  # Following methods in the  sablefish and rockfish/PT code, only add variances
  # (not covariances) for the sperm whale presence correction factor to get final
  # variances for sablefish.
  var_2b_7[sable_index,] <- var_2b_7[sable_index,] + diag(sperm_vcov_2b_7) 
  var_3_7[sable_index,] <- var_3_7[sable_index,] + diag(sperm_vcov_3_7)
  
  # reformat 2b-7
  var_2b_7 <- data.frame(var_2b_7) %>% 
    rownames_to_column(var = "species") 
  
  colnames(var_2b_7) <- c("species", sort(unique(hachi_catch_country$area)))
  
  var_2b_7 <- var_2b_7 %>% 
    mutate(country = c) %>% 
    pivot_longer(-c(species, country), names_to = "area", values_to = "RPN_var") %>%
    mutate(species = as.numeric(species),
           area = as.numeric(area))
  
  # reformat var 3_7
  var_3_7 <- data.frame(var_3_7) %>% 
    rownames_to_column(var = "species") 
  
  colnames(var_3_7) <- c("species", sort(unique(hachi_catch_country$area)))
  
  var_3_7 <- var_3_7 %>% 
    mutate(country = c) %>% 
    pivot_longer(-c(species, country), names_to = "area", values_to = "RPN_var") %>%
    mutate(species = as.numeric(species),
           area = as.numeric(area))
  
  # bind variance output
  if(c == countries[1]) {
    allvar_2b_7 <- var_2b_7
    allvar_3_7 <- var_3_7
  } else {
    allvar_2b_7 <- bind_rows(allvar_2b_7, var_2b_7)
    allvar_3_7 <- bind_rows(allvar_3_7, var_3_7)
  }
  
  # Remove variances for Japanese data, which uses a smaller set of species
  jpn_var_spp <- var_species %>% 
    filter(country == "Japan") %>% 
    pull(species)
  
  # anti_join(allvar_2b_7,  
  #           allvar_2b_7 %>% 
  #             filter(!c(country == "Japan" & 
  #                         !c(species %in% jpn_var_spp)))) %>%
  #   left_join(var_species, by = "species")
  
  allvar_2b_7 <- allvar_2b_7 %>% filter(!c(country == "Japan" & !c(species %in% jpn_var_spp)))
  allvar_3_7 <- allvar_3_7 %>% filter(!c(country == "Japan" & !c(species %in% jpn_var_spp)))
  
  rm(jpn_var_spp)
  rm(hachi_catch_country)
  rm(var_2b_7,var_3_7)
  rm(sperm_vcov_2b_7,sperm_vcov_3_7)
  rm(n.area, n.species, n.stratum)
  rm(weights, station_obs, sperm_pres, 
     species_index, not_sable_index, 
     sable_index, sperm_pres_wcf, station, 
     onestation, stratum, stratum.cv, 
     stratum.vcov, index_2b_7, index_3_7,
     strata_index)
  rm(i, s, j, m, w.j, w.m, r, z, k, q, n.k, n.z, n.kz, 
     k.sum, z.sum, temp.cov)
}

##############################################################################################

##############################################################################################
# Calculate Ave CPUE, RPN, and RPW
# Aggregate total_catch to area, stratum, and species
# and calculate average CPU, RPN, and RPW
# Output: area_stratum_effort
##############################################################################################
#
#summarize cpue by area-stratum-species
cpue = tapply(total_catch$cpue, total_catch$yr.ves.area.str2.spc, sum)
temp.match = match(names(cpue), total_catch$yr.ves.area.str2.spc)
#area_stratum_effort = total_catch[temp.match,c(2:5,7:12,23:25)]
area_stratum_effort = total_catch[temp.match,c("year","country","vessel","cruise","area","area_size","area_size2","stratum","stratum2","species","yr.ves.area.str2.spc","num_stations","rpw_stations")]

#calculate avg cpue
area_stratum_effort$cpue = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$cpue = cpue / area_stratum_effort$num_stations 
rm(cpue)

#calculate avg rpn
totrpn = tapply(total_catch$RPN, total_catch$yr.ves.area.str2.spc, sum, na.rm=TRUE)
area_stratum_effort$RPN = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$RPN = totrpn / area_stratum_effort$num_stations 
# blank out RPN for rows with no area_size2 - NOTE: the following line of code is not in the sablefish or rockfish/PT script
area_stratum_effort$RPN = ifelse(is.na(area_stratum_effort$area_size2),NA,area_stratum_effort$RPN)
rm(totrpn)

#calculate avg rpw
#first update the rpw_station number count from up above - NOTE: the following
#two lines of code are not commented out in the sablefish and rockfish/PT
#scripts
#temp.match = match(area_stratum_effort$yr.ves.area.str2.spc, names(num_rpw_stations))
#area_stratum_effort$rpw_stations = num_rpw_stations[temp.match]
area_stratum_effort$rpw_stations = ifelse(is.na(area_stratum_effort$rpw_stations),
						area_stratum_effort$num_stations,
						area_stratum_effort$rpw_stations)
#rm(num_rpw_stations)
#then calculate the ave RPW
totrpw = tapply(total_catch$RPW, total_catch$yr.ves.area.str2.spc, sum, na.rm=TRUE)
area_stratum_effort$RPW = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$RPW = totrpw / area_stratum_effort$rpw_stations 
# NOTE: the following line of code is not in the sablefish or rockfish/PT script
area_stratum_effort$RPW = ifelse(is.na(area_stratum_effort$area_size2),NA,area_stratum_effort$RPW)
rm(totrpw)

#left outer join mean lengths and mean weights onto area_stratum_effort
temp.match = match(area_stratum_effort$yr.ves.area.str2.spc, mean_lengths$yr.ves.area.str2.spc)
area_stratum_effort$mean_len = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$mean_len = mean_lengths[temp.match,15]

area_stratum_effort$mean_wt = rep(NA, dim(area_stratum_effort)[1])
area_stratum_effort$mean_wt = mean_lengths[temp.match,18]
rm(temp.match)
print("Summarized to Area Stratum")

##############################################################################################

##############################################################################################
# Length Frequencies
# UPDATED 2021
# Summarize Length frequencies by area, species, sex, and length (across stations and strata)
# Also extrapolates for gully stations from strata 5 to 6 at stations 142, 143
#  and strata 4 to 5 at stations 144, 145
# Output: len_freq
##############################################################################################
#
# sum up frequencies by csid, stratum, species, sex, length to match LengthFrequenciesGulliesView
lengths$csid.stratum.species.sex.length = paste(lengths$csid,lengths$stratum,lengths$species,lengths$sex,lengths$length)
temp = tapply(lengths$freq, lengths$csid.stratum.species.sex.length, sum)
temp.match = match(names(temp), lengths$csid.stratum.species.sex.length)
lengths_gullies = lengths[temp.match,c(1:11,13:14)]
lengths_gullies$freq = rep(NA, dim(lengths_gullies)[1])
lengths_gullies$freq = temp

#lookup and tack on fmp_area
temp.match = match(lengths_gullies$area, areas$area)
lengths_gullies$fmp_area = areas[temp.match,"fmp_area"]

#extraploate for gullies
lengths142_3 = lengths_gullies[(lengths_gullies$station==142|lengths_gullies$station==143)&lengths_gullies$stratum==5,]
lengths142_3$stratum = rep(6, dim(lengths142_3)[1])
lengths144_5 = lengths_gullies[(lengths_gullies$station==144|lengths_gullies$station==145)&lengths_gullies$stratum==4,]
lengths144_5$stratum = rep(5, dim(lengths144_5)[1])

lengths_gullies = rbind(lengths_gullies, lengths142_3)
lengths_gullies = rbind(lengths_gullies, lengths144_5)

#calculate fmp area wide averages
#get cross join of distinct species from lengths by areas by year (US only) by depths (2-7)
lengthed_species = unique(lengths[lengths$species!=10120,c("vessel","species")], incomparables=FALSE) 
species_by_area = merge(unique(lengthed_species[,"species"]), areas[,c("area","fmp_area")])
names(species_by_area) = c("species","area","fmp_area")
by_year_country = merge(unique(lengths[lengths$country=="United States",c("year","country")]),species_by_area)
templ = merge(by_year_country,strata[strata$stratum==2|strata$stratum==3|strata$stratum==4|strata$stratum==5|strata$stratum==6|strata$stratum==7,])
templ = unique(templ[,c("year","country","fmp_area","species","stratum")])

rm(species_by_area)
rm(by_year_country)

#unique species-area-strata where catch occurred this year
se = area_stratum_effort[!is.na(area_stratum_effort$RPN)&area_stratum_effort$RPN!=0,c("year","country","area","stratum","species")]
temp.match = match(se$area, areas$area)
se$fmp_area = areas[temp.match,"fmp_area"]
se = unique(se[,c("year","country","fmp_area","species","stratum")])

#join unique species-area_strata with catch on template to get new template
se$year.country.fmp_area.stratum.species = paste(se$year, se$country, se$fmp_area, se$stratum, se$species)
templ$year.country.fmp_area.stratum.species = paste(templ$year, templ$country, templ$fmp_area, templ$stratum, templ$species)
temp.match = match(se$year.country.fmp_area.stratum.species, templ$year.country.fmp_area.stratum.species)
templ1 = templ[temp.match,c(1:6)]
templ1 = templ1[!is.na(templ1$year),]

#calculate the fmp-wide averages of length frequencies at each station
lengths_gullies$year.country.fmp_area.stratum.species.sex.length = paste(lengths_gullies$year, lengths_gullies$country, lengths_gullies$fmp_area, lengths_gullies$stratum, lengths_gullies$species, lengths_gullies$sex, lengths_gullies$length)
temp = tapply(lengths_gullies$freq, lengths_gullies$year.country.fmp_area.stratum.species.sex.length, mean)
temp.match = match(names(temp), lengths_gullies$year.country.fmp_area.stratum.species.sex.length)
avg_length_freqs = lengths_gullies[temp.match,c("year","country","fmp_area","stratum","species","sex","length")]
avg_length_freqs$avg_freq = temp;
#join avgs to templ1 to filter to only length freqs in the strata we care about and where we have catch
avg_length_freqs$year.country.fmp_area.stratum.species = paste(avg_length_freqs$year, avg_length_freqs$country, avg_length_freqs$fmp_area, avg_length_freqs$stratum, avg_length_freqs$species)
temp.match = match(avg_length_freqs$year.country.fmp_area.stratum.species,templ1$year.country.fmp_area.stratum.species)
avg_length_freqs = avg_length_freqs[!is.na(temp.match),]

#get the area-stratum RPNs for this year to distribute across length frequencies
ase = area_stratum_effort[!is.na(area_stratum_effort$RPN)&area_stratum_effort$RPN!=0,c("year","country","vessel","cruise","area","stratum","stratum2","species","RPN","RPW")]
#filter test cruises from 95-97 and any species that are never lengthed
ase = ase[ase$cruise!=199502&ase$cruise!=199602&ase$cruise!=199603&ase$cruise!=199702&ase$cruise!=199703,]
ase$vessel.species = paste(ase$vessel, ase$species)
temp.match = match(ase$vessel.species,paste(lengthed_species[,"vessel"],lengthed_species[,"species"]))
ase$species_match = lengthed_species[temp.match,"species"]
ase = ase[!is.na(ase$species_match),c("year","country","vessel","cruise","area","stratum","stratum2","species","RPN","RPW")]
ase$year.country.cruise.area.stratum.species = paste(ase$year,ase$country,ase$cruise,ase$area,ase$stratum,ase$species)

#sum frequencies up to the area level
lengths_gullies$year.country.cruise.area.stratum.species.sex.length = paste(lengths_gullies$year, lengths_gullies$country, lengths_gullies$cruise, lengths_gullies$area, lengths_gullies$stratum, lengths_gullies$species, lengths_gullies$sex, lengths_gullies$length)
temp = tapply(lengths_gullies$freq, lengths_gullies$year.country.cruise.area.stratum.species.sex.length, sum)
temp.match = match(names(temp), lengths_gullies$year.country.cruise.area.stratum.species.sex.length)
area_stratum_lengths = lengths_gullies[temp.match,c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area")]
area_stratum_lengths$freq = temp;
#join area_stratum_lengths to ase (area_stratum_efforts)
area_stratum_lengths$year.country.cruise.area.stratum.species = paste(area_stratum_lengths$year, area_stratum_lengths$country, area_stratum_lengths$cruise, area_stratum_lengths$area, area_stratum_lengths$stratum, area_stratum_lengths$species)
#find area-stratum-efforts where no lengths exists
temp.match = match(ase$year.country.cruise.area.stratum.species, area_stratum_lengths$year.country.cruise.area.stratum.species)
unmatched_efforts = ase[is.na(temp.match),c("year","country","vessel","cruise","area","stratum","species","RPN","RPW")]
unmatched_efforts$sex = rep(3, dim(unmatched_efforts)[1])
unmatched_efforts$length = rep(999, dim(unmatched_efforts)[1])
unmatched_efforts$freq = rep(NA, dim(unmatched_efforts)[1])
unmatched_efforts$total_lengths = rep(NA, dim(unmatched_efforts)[1])
unmatched_efforts$lf_RPN = unmatched_efforts$RPN
unmatched_efforts$lf_RPW = unmatched_efforts$RPW
temp.match = match(unmatched_efforts$area, areas$area)
unmatched_efforts$fmp_area = areas[temp.match,"fmp_area"]
#now join again the other way to get RPNs and RPWs from ase
temp.match = match(area_stratum_lengths$year.country.cruise.area.stratum.species, ase$year.country.cruise.area.stratum.species)
area_stratum_lengths$RPN = rep(NA, dim(area_stratum_lengths)[1])
area_stratum_lengths$RPN = ase[temp.match,"RPN"]
area_stratum_lengths$RPW = rep(NA, dim(area_stratum_lengths)[1])
area_stratum_lengths$RPW = ase[temp.match,"RPW"]

#sum lengths by area-stratum-species
temp = tapply(area_stratum_lengths$freq, area_stratum_lengths$year.country.cruise.area.stratum.species, sum)
temp.match = match(area_stratum_lengths$year.country.cruise.area.stratum.species, names(temp))
area_stratum_lengths$total_lengths = temp[temp.match]

#sum freq x RPN / total_lengths by area-stratum-species-sex-length
area_stratum_lengths$lf_RPN = (area_stratum_lengths$freq * area_stratum_lengths$RPN) / area_stratum_lengths$total_lengths
area_stratum_lengths$lf_RPW = (area_stratum_lengths$freq * area_stratum_lengths$RPW) / area_stratum_lengths$total_lengths

#remove records where RPN is null
area_stratum_lengths = area_stratum_lengths[!is.na(area_stratum_lengths$RPN),]

#find the averages that match any of the unmatched efforts from above and join them together
unmatched_efforts$year.country.fmp_area.stratum.species = paste(unmatched_efforts$year, unmatched_efforts$country, unmatched_efforts$fmp_area, unmatched_efforts$stratum, unmatched_efforts$species)
temp.match = match(avg_length_freqs$year.country.fmp_area.stratum.species, unmatched_efforts$year.country.fmp_area.stratum.species)
avg_length_freqs$matched = temp.match
avg_area_stratum_lengths = avg_length_freqs[!is.na(avg_length_freqs$matched),]
avg_area_stratum_lengths = merge(x = avg_length_freqs[!is.na(avg_length_freqs$matched),], y = unmatched_efforts[,c("year.country.fmp_area.stratum.species","area","cruise","vessel","RPN","RPW")], by = "year.country.fmp_area.stratum.species", all.y = TRUE)
avg_area_stratum_lengths = avg_area_stratum_lengths[!is.na(avg_area_stratum_lengths$year),]

#remove the records from unmatched_efforts that have matches from the averages
temp.match = match(unmatched_efforts$year.country.fmp_area.stratum.species, avg_area_stratum_lengths$year.country.fmp_area.stratum.species)
unmatched_efforts$matched = temp.match
unmatched_efforts = unmatched_efforts[is.na(unmatched_efforts$match),]

#sum avg length freqs by area-stratum-species
temp = tapply(avg_length_freqs$avg_freq, avg_length_freqs$year.country.fmp_area.stratum.species, sum)
temp.match = match(avg_area_stratum_lengths$year.country.fmp_area.stratum.species, names(temp))
avg_area_stratum_lengths$total_avg_lengths = temp[temp.match]

#sum freq x RPN / total_avg_lengths by area-stratum-species-sex-length
avg_area_stratum_lengths$lf_RPN = (avg_area_stratum_lengths$avg_freq * avg_area_stratum_lengths$RPN) / avg_area_stratum_lengths$total_avg_lengths
avg_area_stratum_lengths$lf_RPW = (avg_area_stratum_lengths$avg_freq * avg_area_stratum_lengths$RPW) / avg_area_stratum_lengths$total_avg_lengths

#remove records where RPN is null (shouldn't be any?)
avg_area_stratum_lengths = avg_area_stratum_lengths[!is.na(avg_area_stratum_lengths$RPN),]

all_area_stratum_lengths = area_stratum_lengths[,c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area","RPN","RPW","total_lengths","lf_RPN","lf_RPW")]
avg_area_stratum_lengths = avg_area_stratum_lengths[,c("year","country","vessel","cruise","area","stratum","species","sex","length","avg_freq","fmp_area","RPN","RPW","total_avg_lengths","lf_RPN","lf_RPW")]
names(avg_area_stratum_lengths) = c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area","RPN","RPW","total_lengths","lf_RPN","lf_RPW")
unmatched_efforts = unmatched_efforts[,c("year","country","vessel","cruise","area","stratum","species","sex","length","freq","fmp_area","RPN","RPW","total_lengths","lf_RPN","lf_RPW")]

all_area_stratum_lengths = rbind(all_area_stratum_lengths, avg_area_stratum_lengths)
all_area_stratum_lengths = rbind(all_area_stratum_lengths, unmatched_efforts)

#create copy with strata 3-7 only
all_area_stratum_lengths3to7 = all_area_stratum_lengths[all_area_stratum_lengths$stratum>2,]

#now sum both strata 2b-7 and 3-7 sets to the Area level
all_area_stratum_lengths$year.country.cruise.area.species.sex.length = paste(all_area_stratum_lengths$year, all_area_stratum_lengths$country, all_area_stratum_lengths$cruise, all_area_stratum_lengths$area, all_area_stratum_lengths$species, all_area_stratum_lengths$sex, all_area_stratum_lengths$length)
area_lfs_RPN = tapply(all_area_stratum_lengths$lf_RPN, all_area_stratum_lengths$year.country.cruise.area.species.sex.length, sum)
area_lfs_RPW = tapply(all_area_stratum_lengths$lf_RPW, all_area_stratum_lengths$year.country.cruise.area.species.sex.length, sum)
temp.match = match(names(area_lfs_RPN), all_area_stratum_lengths$year.country.cruise.area.species.sex.length)
len_freq = all_area_stratum_lengths[temp.match,c("year","country","vessel","cruise","area","species","sex","length")]
len_freq$RPN = area_lfs_RPN
len_freq$RPW = area_lfs_RPW

all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length = paste(all_area_stratum_lengths3to7$year, all_area_stratum_lengths3to7$country, all_area_stratum_lengths3to7$cruise, all_area_stratum_lengths3to7$area, all_area_stratum_lengths3to7$species, all_area_stratum_lengths3to7$sex, all_area_stratum_lengths3to7$length)
area_lfs_RPN = tapply(all_area_stratum_lengths3to7$lf_RPN, all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length, sum)
area_lfs_RPW = tapply(all_area_stratum_lengths3to7$lf_RPW, all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length, sum)
temp.match = match(names(area_lfs_RPN), all_area_stratum_lengths3to7$year.country.cruise.area.species.sex.length)
len_freq3to7 = all_area_stratum_lengths3to7[temp.match,c("year","country","vessel","cruise","area","species","sex","length")]
len_freq3to7$RPN = area_lfs_RPN
len_freq3to7$RPW = area_lfs_RPW

rm(lengthed_species)
rm(templ)
rm(templ1)
rm(all_area_stratum_lengths)
rm(all_area_stratum_lengths3to7)
rm(area_lfs_RPN)
rm(area_lfs_RPW)
rm(unmatched_efforts)
rm(temp)
rm(temp.match)
#rm(avg_area_stratum_lengths) -- maybe these should be saved to a table and sent to AKFIN too?

print("Calculated Length Frequencies")
##############################################################################################


##############################################################################################
#Summarize CPUE, RPN, and RPW to Area Aggregate area_stratum_effort over stratum
#to area and species, only including strata 3-7 RPN and RPW are simply summed
#across strata mean_len and cpue are scaled by area_size mean_wt is recalculated
#using vonbert growth factors on newly calculated mean_len Output: area_effort
##############################################################################################
#
#get rid of anything not in strata 3-7. NOTE: in sablefish and rockfish/PT
#scripts good strata is defined using stratum2 not stratum. Tested this and it
#appears these are equivalent.
# good_strata2 = area_stratum_effort[area_stratum_effort$stratum2 %in% c(3:7),]
# length(which(good_strata != good_strata2))
good_strata = area_stratum_effort[area_stratum_effort$stratum %in% c(3:7),]
#summarize good_strata by area-species
#calc sum of area size
good_strata$cntry.area.spc = paste(good_strata$country, good_strata$area, good_strata$species)
area_size = tapply(good_strata$area_size, good_strata$cntry.area.spc, sum, na.rm=TRUE)
temp.match = match(names(area_size), good_strata$cntry.area.spc)
area_effort = good_strata[temp.match,c(1:6,10)]
area_effort$area_size = area_size

#calc sum of cpue, scaled by area size
#replace na with 0
good_strata$area_size = ifelse(is.na(good_strata$area_size),
						0,
						good_strata$area_size)
good_strata$cpue = ifelse(is.na(good_strata$cpue),
					0,
					good_strata$cpue)
cpue = tapply(good_strata$cpue * good_strata$area_size, good_strata$cntry.area.spc, sum)
area_effort$cpue = ifelse(area_effort$area_size==0,
				NA,
				cpue / area_effort$area_size)
rm(cpue)
#calc sum of RPN - NOTE: in sablefish and rockfish/PT scripts, the tapply sums
#for arearpn and arearpw do not include na.rm = TRUE
arearpn = tapply(good_strata$RPN, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort$RPN = arearpn
rm(arearpn)
#calc sum of RPW
arearpw = tapply(good_strata$RPW, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort$RPW = arearpw
rm(arearpw)

# NOTE: This is where area variances are added in the sablefish and rockfish/PT
# code. To avoid conflicts with indexing/etc., I've waited to add variances
# until the very end.

#calc sum of mean_len, scaled by area size
#first remove all cases where mean_len is NA
good_mean_len = good_strata[!is.na(good_strata$mean_len),]
#then calculate mean length scaling by area size
sizexlen = tapply(good_mean_len$mean_len * good_mean_len$area_size, good_mean_len$cntry.area.spc, sum)
areasize = tapply(good_mean_len$area_size, good_mean_len$cntry.area.spc, sum)
meanlen = sizexlen / areasize 
#then join to area_effort
area_effort$cntry.area.spc = paste(area_effort$country, area_effort$area, area_effort$species)
temp.match = match(area_effort$cntry.area.spc, names(meanlen))
area_effort$mean_len = rep(0, dim(area_effort)[1])
area_effort$mean_len = meanlen[temp.match]
#replace NaNs caused by divide by 0 (no area sizes)
area_effort$mean_len = ifelse(is.nan(area_effort$mean_len), 
						NA,
						area_effort$mean_len)
#get rid of cntry.area.spc key field no longer needed
area_effort = area_effort[c(1:10,12)]
rm(sizexlen)
rm(areasize)
rm(meanlen)
rm(temp.match)

#now join species so we can recalculate mean weight
ls.match = match(area_effort$species, species$code)
area_effort$vonbert_a = rep(NA, dim(area_effort)[1])
area_effort$vonbert_a = species[ls.match, 3]
area_effort$vonbert_b = rep(NA, dim(area_effort)[1])
area_effort$vonbert_b = species[ls.match, 4]
area_effort$mean_wt = rep(NA, dim(area_effort)[1])
area_effort$mean_wt = (area_effort$vonbert_a * (area_effort$mean_len ^ area_effort$vonbert_b))
rm(ls.match)
print("Summarized to Area")
##############################################################################################

##############################################################################################
# Extrapolate Area Effort in areas 16 and 17
# extrapolate area 16 from area 15 multiplying all of CPUE, RPN, RPW, mean_len, and mean_wt
# by the historical ratio of RPNs from area 16 (NW) over those from area 15 (NE) and
# area 17 (SW) over area 18 (SE) from the Japanese surveys from 1985 to 1994
# These ratios were calculated from the survey data and stored in the 
# NW_AI_ratio and SW_AI_ratio colums, respectively, in the Specie_Catch_Codes table
# for all species EXCEPT for Sablefish.
# Sablefish ratios are set to 0.182026918519442 (NW/NE) and 0.658068926357902 (SW/SE)
# (also in the Specie_Catch_Code table) which are the ratios which have been used 
# historically before the ratios for all other species were calculated using RPNs
# Output: area_effort
##############################################################################################
#

species_ratios = species[!is.na(species$nw_ratio),]
area_16 = area_effort[area_effort$area==15&area_effort$country=='United States',]
temp.match = match(area_16$species, species_ratios$code)
area_16$nw_ratio = species_ratios[temp.match,6]
area_16 = area_16[!is.na(area_16$nw_ratio),]
area_16$cpue = area_16$cpue * area_16$nw_ratio
area_16$RPN = area_16$RPN * area_16$nw_ratio
area_16$RPW = area_16$RPW * area_16$nw_ratio
area_size = sum(area_strata[area_strata$area==16&area_strata$stratum %in% c(3:7),3])
area_16$area_size = rep(area_size, dim(area_16)[1])
if (length(area_16[,1]) > 0) area_16$area = 16
area_effort = rbind(area_effort, area_16[c(1:14)])
rm(area_16)

species_ratios = species[!is.na(species$sw_ratio),]
area_17 = area_effort[area_effort$area==18&area_effort$country=='United States',]
temp.match = match(area_17$species, species_ratios$code)
area_17$sw_ratio = species_ratios[temp.match,7]
area_17 = area_17[!is.na(area_17$sw_ratio),]
area_17$cpue = area_17$cpue * area_17$sw_ratio
area_17$RPN = area_17$RPN * area_17$sw_ratio
area_17$RPW = area_17$RPW * area_17$sw_ratio
area_size = sum(area_strata[area_strata$area==17&area_strata$stratum %in% c(3:7),3])
area_17$area_size = rep(area_size, dim(area_17)[1])
if (length(area_17[,1]) > 0) area_17$area = 17
area_effort = rbind(area_effort, area_17[c(1:14)])
rm(area_17)
rm(species_ratios)
rm(temp.match)
print("Extrapolated Areas 16 and 17")
##############################################################################################

##############################################################################################
# Summarize CPUE, RPN, and RPW to Area INCLUDING new stratum 2b
# This repeats the above three steps (summarization over strata, then extrapolation for areas 16 and 17)
# but including efforts in area_stratum_effort for all strata (i.e. adding in 2b)
# Output: area_effort2b
##############################################################################################
#
good_strata = area_stratum_effort
#summarize good_strata by area-species
#calc sum of area size 2 (includes sizes for stratum 2b)
good_strata$cntry.area.spc = paste(good_strata$country, good_strata$area, good_strata$species)
area_size2 = tapply(good_strata$area_size2, good_strata$cntry.area.spc, sum, na.rm=TRUE)
temp.match = match(names(area_size2), good_strata$cntry.area.spc)
area_effort2b = good_strata[temp.match,c(1:6,10)]
area_effort2b$area_size = area_size2

#calc sum of cpue, scaled by area size
#replace na with 0
good_strata$area_size2 = ifelse(is.na(good_strata$area_size2),
						0,
						good_strata$area_size2)
good_strata$cpue = ifelse(is.na(good_strata$cpue),
					0,
					good_strata$cpue)
cpue = tapply(good_strata$cpue * good_strata$area_size2, good_strata$cntry.area.spc, sum)
area_effort2b$cpue = ifelse(area_effort2b$area_size==0,
				NA,
				cpue / area_effort2b$area_size)
rm(cpue)
#calc sum of RPN
arearpn = tapply(good_strata$RPN, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort2b$RPN = arearpn
rm(arearpn)
#calc sum of RPW
arearpw = tapply(good_strata$RPW, good_strata$cntry.area.spc, sum, na.rm=TRUE)
area_effort2b$RPW = arearpw
rm(arearpw)
#calc sum of mean_len, scaled by area size
#first remove all cases where mean_len is NA
good_mean_len = good_strata[!is.na(good_strata$mean_len),]
#then calculate mean length scaling by area size
sizexlen = tapply(good_mean_len$mean_len * good_mean_len$area_size2, good_mean_len$cntry.area.spc, sum)
areasize = tapply(good_mean_len$area_size2, good_mean_len$cntry.area.spc, sum)
meanlen = sizexlen / areasize 
#then join to area_effort
area_effort2b$cntry.area.spc = paste(area_effort2b$country, area_effort2b$area, area_effort2b$species)
temp.match = match(area_effort2b$cntry.area.spc, names(meanlen))
area_effort2b$mean_len = rep(0, dim(area_effort2b)[1])
area_effort2b$mean_len = meanlen[temp.match]
#replace NaNs caused by divide by 0 (no area sizes)
area_effort2b$mean_len = ifelse(is.nan(area_effort2b$mean_len), 
						NA,
						area_effort2b$mean_len)
#get rid of cntry.area.spc key field no longer needed
area_effort2b = area_effort2b[c(1:10,12)]
rm(sizexlen)
rm(areasize)
rm(meanlen)
rm(temp.match)

#now join species so we can recalculate mean weight
ls.match = match(area_effort2b$species, species$code)
area_effort2b$vonbert_a = rep(NA, dim(area_effort2b)[1])
area_effort2b$vonbert_a = species[ls.match, 3]
area_effort2b$vonbert_b = rep(NA, dim(area_effort2b)[1])
area_effort2b$vonbert_b = species[ls.match, 4]
area_effort2b$mean_wt = rep(NA, dim(area_effort2b)[1])
area_effort2b$mean_wt = (area_effort2b$vonbert_a * (area_effort2b$mean_len ^ area_effort2b$vonbert_b))
rm(ls.match)
print("Summarized to Area including Stratum 2b")
##############################################################################################

##############################################################################################
# Extrapolate Area Effort in areas 16 and 17 - same as above, but not excluding stratum 2b
# Output: area_effort2b
##############################################################################################
#

species_ratios = species[!is.na(species$nw_ratio),]
area_16 = area_effort2b[area_effort2b$area==15&area_effort2b$country=='United States',]
temp.match = match(area_16$species, species_ratios$code)
area_16$nw_ratio = species_ratios[temp.match,6]
area_16 = area_16[!is.na(area_16$nw_ratio),]
area_16$cpue = area_16$cpue * area_16$nw_ratio
area_16$RPN = area_16$RPN * area_16$nw_ratio
area_16$RPW = area_16$RPW * area_16$nw_ratio
area_size = sum(area_strata[area_strata$area==16,3])
area_16$area_size = rep(area_size, dim(area_16)[1])
if (length(area_16[,1]) > 0) area_16$area = 16
area_effort2b = rbind(area_effort2b, area_16[c(1:14)])
rm(area_16)

species_ratios = species[!is.na(species$sw_ratio),]
area_17 = area_effort2b[area_effort2b$area==18&area_effort2b$country=='United States',]
temp.match = match(area_17$species, species_ratios$code)
area_17$sw_ratio = species_ratios[temp.match,7]
area_17 = area_17[!is.na(area_17$sw_ratio),]
area_17$cpue = area_17$cpue * area_17$sw_ratio
area_17$RPN = area_17$RPN * area_17$sw_ratio
area_17$RPW = area_17$RPW * area_17$sw_ratio
area_size = sum(area_strata[area_strata$area==17,3])
area_17$area_size = rep(area_size, dim(area_17)[1])
if (length(area_17[,1]) > 0) area_17$area = 17
area_effort2b = rbind(area_effort2b, area_17[c(1:14)])
rm(area_17)
rm(species_ratios)
rm(temp.match)
print("Extrapolated areas 16 and 17 including Stratum 2b")
##############################################################################################

# depredation label:
if(DEP == TRUE) {
  dep_label <- "_depred"
} else {
  dep_label <- ""
}

##############################################################################################
# Output Station Effort 
# Output: station_effort.out
##############################################################################################
#
#get area information
station_effort.out = total_catch[,c(2,1,5,4,3,6)]
station_effort.out$rpw_station_number = rep(NA,dim(station_effort.out)[1])
station_effort.out$rpw_station_number = ifelse((total_catch$tot_catch_freq>0
								&total_catch$total_skates>0
								&!is.na(total_catch$area_size2)
								&total_catch$area_size2>0
								&is.na(total_catch$mean_wt)),
							NA,
							total_catch$station)

#lookup area information
station_effort.out$area_id = rep(NA,dim(station_effort.out)[1])
station_effort.out$area = total_catch$area
temp.match = match(station_effort.out$area, areas$area)
station_effort.out$area_id = areas[temp.match,1]
station_effort.out$geo_area = areas[temp.match,5]
station_effort.out$council_area_id = areas[temp.match,4]
station_effort.out$council_area = areas[temp.match,3]
station_effort.out$area_size2 = total_catch$area_size2
station_effort.out$depth_stratum_id2 = total_catch$stratum2
temp = replace(as.character(total_catch$stratum2), which(total_catch$stratum2 %in% '2a'), c(11))
temp = replace(as.character(temp), which(total_catch$stratum2 %in% '2b'), c(12))
station_effort.out$depth_stratum_id2 = as.numeric(temp)
station_effort.out$stratum2 = total_catch$stratum2
rm(temp)

#get species name
station_effort.out$species = total_catch$species
temp.match = match(station_effort.out$species, species$code)
station_effort.out$common_name = species[temp.match,2]
station_effort.out$web_flag = species[temp.match,5]

#get the value columns
station_effort.out$total_skates = rep(NA, dim(station_effort.out)[1])
station_effort.out$total_catch = rep(NA, dim(station_effort.out)[1])
station_effort.out[,19:20] = total_catch[,c(15,13)]
station_effort.out$total_len_freq = rep(NA, dim(station_effort.out)[1])
station_effort.out$cpue = rep(NA, dim(station_effort.out)[1])
station_effort.out$mean_len = rep(NA, dim(station_effort.out)[1])
station_effort.out$mean_wt = rep(NA, dim(station_effort.out)[1])
station_effort.out$rpn = rep(NA, dim(station_effort.out)[1])
station_effort.out$rpw = rep(NA, dim(station_effort.out)[1])
station_effort.out[,21:26] = total_catch[,c(18,16,19,20,21,22)]

#These are only needed to be backwards-compatible with what the materialized views created
station_effort.out$a_vonBert = rep(NA, dim(station_effort.out)[1])
station_effort.out$b_vonBert = rep(NA, dim(station_effort.out)[1])

#Stamp last modified date
station_effort.out$last_mod = rep(Sys.time(), dim(station_effort.out)[1])


#format for output
#Name columns to match database table column names exactly
names(station_effort.out)=c("Year","Cruise_Station_ID","Cruise_Number","Vessel_Number","Country","Station_Number",
	"RPW_Station_Number","area_id","area_code","Geographic_area_name","Council_sablefish_management_area_ID",
	"Council_sablefish_management_area","size_km","depth_stratum_id2","stratum2","Species_Code","Common_Name","web_reporting_flag",
	"total_skates","total_catch","total_length_freq","CPUE","mean_length","mean_weight","RPN","RPW","a_vonBert","b_vonBert", "LastModifiedDate")

# Only report sablefish for whale-corrected tables
if(DEP == TRUE) {
  station_effort.out <- station_effort.out[station_effort.out$Species_Code == 20510,]
}

print(paste("Writing StationEfforts", dep_label, " to ", OUTPUTMODE, sep=""))
if (OUTPUTMODE=="SQL") {
  #Save to database
  conn <- odbcConnect(DSN)
  qry <- paste("delete from StationEfforts", dep_label, " where Year=", YEAR, sep="")
  print(paste("Deleting StationEfforts", dep_label, sep=""))
  sqlQuery(conn, qry)
  odbcClose(conn)
  conn <- odbcConnect(DSN)
  print(paste("Saving StationEfforts", dep_label, sep=""))
  sqlSave(conn, station_effort.out, tablename=paste("StationEfforts", dep_label, sep=""), append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  odbcClose(conn)
}
if (OUTPUTMODE=="CSV") {
  #Output to file
  print(paste("writing file: ", OUTPUTPATH, '\\', YEAR, "_StationEfforts", dep_label, ".csv", sep=''))
  write.table(station_effort.out, file=paste(OUTPUTPATH, '\\', YEAR, "_StationEfforts", dep_label, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
}

##############################################################################################

##############################################################################################
# Output Length Frequencies By Stratum
# Output: area_stratum_lengths.out
##############################################################################################
#
#lookup species info
temp.match = match(area_stratum_lengths$species, species$code)
area_stratum_lengths$Common_Name = species[temp.match,2]

#lookup vessel name
temp.match = match(area_stratum_lengths$vessel, vessels$Vessel_Number)
area_stratum_lengths$Vessel_name = vessels[temp.match,2]
rm(temp.match)

#lookup area information
temp.match = match(area_stratum_lengths$area, areas$area)
area_stratum_lengths$geo_area = areas[temp.match,"geo_area"]
area_stratum_lengths$council_area = areas[temp.match,"council_area"]

area_stratum_lengths.out = area_stratum_lengths[,c("year","country","vessel","Vessel_name","cruise","area","geo_area","council_area",
                                                   "fmp_area","species","Common_Name","stratum","sex","length","lf_RPN","lf_RPW")]
names(area_stratum_lengths.out)=c("Year","Country","Vessel_Number","Vessel_name","Cruise_Number","area_code",
                                  "Geographic_area_name","Council_sablefish_management_area","FMP_management_area",
                                  "species_code","Common_Name","depth_stratum_id","sex","length","RPN","RPW")

# Only report sablefish for whale-corrected tables
if(DEP == TRUE) {
  area_stratum_lengths.out <- area_stratum_lengths.out[area_stratum_lengths.out$species_code == 20510,]
}

print(paste("Writing LengthRPN_byStratum", dep_label, " to ", OUTPUTMODE, sep=""))
if (OUTPUTMODE=="SQL") {
  conn <- odbcConnect(DSN)
  qry <- paste("delete from LengthRPN_byStratum", dep_label, " where Year=", YEAR, sep="")
  sqlQuery(conn, qry)
  sqlSave(conn, area_stratum_lengths.out, tablename=paste("LengthRPN_byStratum", dep_label, sep=""), append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  odbcClose(conn)
  rm(area_stratum_lengths.out)
  
}
if (OUTPUTMODE=="CSV") {
  #Output to file
  print(paste("writing file: ", OUTPUTPATH, '\\', YEAR, "_LengthRPN_byStratum", dep_label, ".csv", sep=''))
  write.table(area_stratum_lengths.out, file=paste(OUTPUTPATH, '\\', YEAR, "_LengthRPN_byStratum", dep_label, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
}
##############################################################################################


##############################################################################################
# Output Length Frequencies by RPN (all strata)
# Output: len_freq.out
##############################################################################################

#lookup area information
temp.match = match(len_freq$area, areas$area)
len_freq$geo_area = areas[temp.match,5]
len_freq$council_area = areas[temp.match,3]

#lookup species info
temp.match = match(len_freq$species, species$code)
len_freq$Common_Name = species[temp.match,2]

#lookup vessel name
temp.match = match(len_freq$vessel, vessels$Vessel_Number)
len_freq$Vessel_name = vessels[temp.match,2]
rm(temp.match)

len_freq.out = len_freq[,c("year","country","vessel","Vessel_name","cruise","area","geo_area","council_area","species","Common_Name","sex","length","RPN","RPW")]

#Stamp last modified date
len_freq.out$last_mod = rep(Sys.time(), dim(len_freq.out)[1])

#Name columns to match database table column names exactly
names(len_freq.out)=c("Year","Country","Vessel_Number","Vessel_name","Cruise_Number","area_code",
	"Geographic_area_name","Council_sablefish_management_area","species_code","Common_Name",
	"sex","length","RPN","RPW","LastModifiedDate")

# Only report sablefish for whale-corrected tables
if(DEP == TRUE) {
  len_freq.out <- len_freq.out[len_freq.out$species_code == 20510,]
}

print(paste("Writing LengthRPN_byArea_AllStrata", dep_label, " to ", OUTPUTMODE, sep=""))
if (OUTPUTMODE=="SQL") {
  #Save to database
  conn <- odbcConnect(DSN)
  qry <- paste("delete from LengthRPN_byArea_AllStrata", dep_label, " where Year=", YEAR, sep="")
  sqlQuery(conn, qry)
  sqlSave(conn, len_freq.out, tablename=paste("LengthRPN_byArea_AllStrata", dep_label, sep=""), append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
  odbcClose(conn)
}
if (OUTPUTMODE=="CSV") {
  #Output to file
  print(paste("writing file: ", OUTPUTPATH, '\\', YEAR, "_LengthRPN_byArea_AllStrata", dep_label, ".csv", sep=''))
  write.table(len_freq.out, file=paste(OUTPUTPATH, '\\', YEAR, "_LengthRPN_byArea_AllStrata", dep_label, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
}

##############################################################################################

##############################################################################################
# Output Length Frequencies by RPN Strata 3 to 7
# Output: len_freq.out
##############################################################################################
#

#lookup area information
temp.match = match(len_freq3to7$area, areas$area)
len_freq3to7$geo_area = areas[temp.match,5]
len_freq3to7$council_area = areas[temp.match,3]

#lookup species info
temp.match = match(len_freq3to7$species, species$code)
len_freq3to7$Common_Name = species[temp.match,2]

#lookup vessel name
temp.match = match(len_freq3to7$vessel, vessels$Vessel_Number)
len_freq3to7$Vessel_name = vessels[temp.match,2]
rm(temp.match)

len_freq.out = len_freq3to7[,c("year","country","vessel","Vessel_name","cruise","area","geo_area","council_area","species","Common_Name","sex","length","RPN","RPW")]

#Stamp last modified date
len_freq.out$last_mod = rep(Sys.time(), dim(len_freq.out)[1])

#Name columns to match database table column names exactly
names(len_freq.out)=c("Year","Country","Vessel_Number","Vessel_name","Cruise_Number","area_code",
	"Geographic_area_name","Council_sablefish_management_area","species_code","Common_Name",
	"sex","length","RPN","RPW","LastModifiedDate")

# Only report sablefish for whale-corrected tables
if(DEP == TRUE) {
  len_freq.out <- len_freq.out[len_freq.out$species_code == 20510,]
}

print(paste("Writing LengthRPN_byArea_3to7", dep_label, " to ", OUTPUTMODE, sep=""))
if (OUTPUTMODE=="SQL") {
	#Save to database
	conn <- odbcConnect(DSN)
	qry <- paste("delete from LengthRPN_byArea_3to7", dep_label," where Year=", YEAR, sep="")
	sqlQuery(conn, qry)
	sqlSave(conn, len_freq.out, tablename=paste("LengthRPN_byArea_3to7", dep_label, sep=""), append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
	odbcClose(conn)
}
if (OUTPUTMODE=="CSV") {
	#Output to file
	print(paste("writing file: ", OUTPUTPATH, '\\', YEAR, "_LengthRPN_byArea_3to7", dep_label, ".csv", sep=''))
	write.table(len_freq.out, file=paste(OUTPUTPATH, '\\', YEAR, "_LengthRPN_byArea_3to7", dep_label, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
}

##############################################################################################

##############################################################################################
# Output Area Stratum Effort (CPUE, RPN, RPW)
# Output: area_stratum_effort.out
##############################################################################################
#
#get area information
area_stratum_effort.out = area_stratum_effort[,c(1:2)]
temp.match = match(area_stratum_effort$area, areas$area)
area_stratum_effort.out$geo_area = areas[temp.match,5]
area_stratum_effort.out$area = area_stratum_effort$area
area_stratum_effort.out$council_area_id = areas[temp.match,4]
area_stratum_effort.out$council_area = areas[temp.match,3]
area_stratum_effort.out$exploitable = areas[temp.match,"exploitable"]

#get species name
temp.match = match(area_stratum_effort$species, species$code)
area_stratum_effort.out$species = area_stratum_effort$species
area_stratum_effort.out$common_name = species[temp.match,2]
area_stratum_effort.out$web_flag = species[temp.match,5]

#get the value columns
area_stratum_effort.out$depth_stratum_id2 = area_stratum_effort$stratum2
temp = replace(as.character(area_stratum_effort$stratum2), which(area_stratum_effort$stratum2 %in% '2a'), c(11))
temp = replace(as.character(temp), which(area_stratum_effort$stratum2 %in% '2b'), c(12))
area_stratum_effort.out$depth_stratum_id2 = as.numeric(temp)
area_stratum_effort.out$stratum2 = area_stratum_effort$stratum2
rm(temp)
stratum.match = match(area_stratum_effort.out$stratum2, strata$stratum)
#area_stratum_effort.out$StratumDescription = rep(NA, dim(area_stratum_effort.out)[1])
area_stratum_effort.out$StratumDescription = strata[stratum.match,2]
area_stratum_effort.out$area_size = area_stratum_effort$area_size2
area_stratum_effort.out[,c(15:21)] = area_stratum_effort[,c(12,13,17,18,14,15,16)]

#These are only needed to be backwards-compatible with what the materialized views created
area_stratum_effort.out$a_vonBert = rep(NA, dim(area_stratum_effort.out)[1])
area_stratum_effort.out$b_vonBert = rep(NA, dim(area_stratum_effort.out)[1])

#Stamp last modified date
area_stratum_effort.out$last_mod = rep(Sys.time(), dim(area_stratum_effort.out)[1])

#dim(area_stratum_effort.out); dim(area_stratum_effort)
#Name columns to match database table column names exactly
names(area_stratum_effort.out)=c("Year","Survey","Geographic_area_name","area_code",
	"Council_sablefish_management_area_ID","Council_sablefish_management_area",
	"Exploitable","Species_Code","species",
	"web_reporting_flag","depth_stratum_id2","stratum2","StratumDescription","size_km","num_stations",
	"num_rpw_stations","mean_length","mean_weight","CPUE","RPN","RPW","a_vonBert","b_vonBert","LastModifiedDate")

# Only report sablefish for whale-corrected tables
if(DEP == TRUE) {
  area_stratum_effort.out <- area_stratum_effort.out[area_stratum_effort.out$Species_Code == 20510,]
}

print(paste("Writing AreaStratumRPN", dep_label, " to ", OUTPUTMODE, sep="")) # AreaStratumRPN
if (OUTPUTMODE=="SQL") {
	#Save to database
	conn <- odbcConnect(DSN)
	qry <- paste("delete from AreaStratumRPN", dep_label, " where Year=", YEAR, sep="")
	sqlQuery(conn, qry)
	sqlSave(conn, area_stratum_effort.out, tablename=paste("AreaStratumRPN", dep_label, sep=""), append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
	odbcClose(conn)
}
if (OUTPUTMODE=="CSV") {
	#Output to file
	print(paste("writing file: ", OUTPUTPATH, '\\', YEAR, "_AreaStratumRPN", dep_label, ".csv", sep=''))
	write.table(area_stratum_effort.out, file=paste(OUTPUTPATH, '\\', YEAR, "_AreaStratumRPN", dep_label, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
}
##############################################################################################

##############################################################################################
# Output Area Effort Strata 3to7
# Output: area_effort.out
##############################################################################################
#
#get area information
area_effort.out = area_effort[,c(1:2)]
temp.match = match(area_effort$area, areas$area)
area_effort.out$fmp_area = areas[temp.match, 6]
area_effort.out$council_mgmt_area_id = areas[temp.match,7]
area_effort.out$council_mgmt_area = areas[temp.match,8]
area_effort.out$council_area_id = areas[temp.match,4]
area_effort.out$council_area = areas[temp.match,3]
area_effort.out$geo_area = areas[temp.match,5]
area_effort.out$area = area_effort$area

#get species name
temp.match = match(area_effort$species, species$code)
area_effort.out$species = area_effort$species
area_effort.out$common_name = species[temp.match,2]

#get the value columns
area_effort.out$area_size = rep(NA, dim(area_effort.out)[1])
area_effort.out$cpue = rep(NA, dim(area_effort.out)[1])
area_effort.out$RPN = rep(NA, dim(area_effort.out)[1])
area_effort.out$RPW = rep(NA, dim(area_effort.out)[1])
area_effort.out$mean_len = rep(NA, dim(area_effort.out)[1])
area_effort.out$mean_wt = rep(NA, dim(area_effort.out)[1])
area_effort.out[,12:17] = area_effort[,c(6,8:11,14)]

# Add variances

# species with ratios for extrapolating into areas 16 and 17 from 15 and 18
nw_species <- species[!is.na(species$nw_ratio),]$code
sw_species <- species[!is.na(species$sw_ratio),]$code

area_effort.out <- area_effort.out %>%
  left_join(allvar_3_7) %>% 
  # area_size: should be NA and not 0 for areas that don't have area sizes
  # cpue: always appropriate to use 0 instead of NA
  # RPN: appropriate to use 0 unless area_size is NA
  # RPW: should be NA not 0 when mean_len/mean_wt is NA
  # mean_len/mean_wt: should be NA when missing
  # RPN_var: NA when RPN is NA, also NA for species not identified in Cara's
  # variance species list. 0 is okay only if the RPN is a true 0 and the other
  # conditions are met.
  # cpue_var: NA when RPN_var is NA (remember variance is area-weighted, i.e.
  # calculated for RPNs not cpues).  0 is okay only if the cpue is a true 0 and
  # the other conditions are met.
  # RPW_var: NA when RPN_var is NA, also NA when mean_len/mean_wt is NA
mutate(area_size = ifelse(area_size == 0, NA, area_size),
       cpue = ifelse(is.na(cpue), 0, cpue),
       RPN = ifelse(is.na(area_size), NA, RPN),
       RPW = ifelse(is.na(mean_len) | is.na(mean_wt) | is.na(RPN), NA, RPW),
       RPN_var = ifelse(is.na(RPN), NA, RPN_var)) %>% 
  mutate(RPN_cv = ifelse(is.na(RPN), NA, sqrt(RPN_var) / RPN)) %>% 
  # Assume the CV are the same for area 16 as 15 and 17 as 18 then derive the
  # RPN_var for 16 and 17 from the CV. Note that this reflects a change from the
  # sablefish/rockfish PT scripts, where it was assumed that the variances were
  # the same instead of deriving them from the CV
  mutate(RPN_cv = replace(RPN_cv, which(area == 16 & species %in% nw_species), RPN_cv[area == 15 & species %in% nw_species]),
         RPN_cv = replace(RPN_cv, which(area == 17 & species %in% sw_species), RPN_cv[area == 18 & species %in% sw_species]),
         RPN_var = ifelse(area %in% c(16, 17) & !is.na(RPN) & !is.nan(RPN_cv), (RPN * RPN_cv)^2, RPN_var)) %>% 
  # derive RPW and cpue variances from RPN cv
  mutate(RPW_var = ifelse(is.na(RPW), NA, ifelse(is.nan(RPN_cv), 0, (RPW * RPN_cv)^2)),
         cpue_var = ifelse(is.nan(RPN_cv), 0, (cpue * RPN_cv)^2)) %>% 
  # In some years the extrapolation from area 16/18 to 15/17 results in
  # situations where CPUE=0 and CPUE_var=0 but RPN_var=NA. Fix this.
  mutate(RPN_var = ifelse(cpue == 0 & cpue_var == 0 & is.na(RPN_var), 0, RPN_var)) %>% 
  select(-RPN_cv) %>% 
  left_join(areas %>% select(geo_area, exploitable)) %>% 
  select(Year = year, Survey = country, FMP_management_area = fmp_area, Council_management_area_ID = council_mgmt_area_id,
	   Council_management_area = council_mgmt_area,  Council_sablefish_management_area_ID = council_area_id,
         Council_sablefish_management_area = council_area, Geographic_area_name = geo_area,
         area_code = area, Exploitable = exploitable, Species_Code = species, species = common_name, area_size_km = area_size,
         CPUE = cpue, CPUE_var = cpue_var, RPN, RPN_var, RPW, RPW_var, 
         mean_length = mean_len, mean_weight = mean_wt)

names(area_effort.out)

# area_effort.out %>% filter(CPUE==0 & CPUE_var == 0 & is.na(RPN_var)) %>% distinct(Year,Survey,area_code,species) %>% arrange(Year)# %>% print(n=Inf) # area 16/17

# should be 0
# area_effort.out %>% filter(Survey == "Japan" & !c(Species_Code %in% var_species[var_species$country=="Japan",]$species)) %>% filter(!is.na(RPN_var))

# tests
# filter(is.na(area_size) & RPN == 0)
# filter(is.na(mean_len) & !is.na(mean_wt))

#Stamp last modified date
area_effort.out$LastModifiedDate = rep(Sys.time(), dim(area_effort.out)[1])

# Only report sablefish for whale-corrected tables
if(DEP == TRUE) {
  area_effort.out <- area_effort.out[area_effort.out$Species_Code == 20510,]
}

print(paste("Writing AreaRPN_3to7", dep_label, " to ", OUTPUTMODE, sep=""))
if (OUTPUTMODE=="SQL") {
	#Save to database
	conn <- odbcConnect(DSN)
	qry <- paste("delete from AreaRPN_3to7", dep_label, " where Year=", YEAR, sep="")
	sqlQuery(conn, qry)
	sqlSave(conn, area_effort.out, tablename=paste("AreaRPN_3to7", dep_label, sep=""), append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
	odbcClose(conn)
}
if (OUTPUTMODE=="CSV") {
	#Output to file
	print(paste("writing file: ", OUTPUTPATH, '\\', YEAR, "_AreaRPN_3to7", dep_label, ".csv", sep=''))
	write.table(area_effort.out, file=paste(OUTPUTPATH, '\\', YEAR, "_AreaRPN_3to7", dep_label, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
}
##############################################################################################

##############################################################################################
# Output Area Effort (All strata, i.e. 2b-7)
# Output: area_effort2b.out
##############################################################################################
#
#get area information
area_effort2b.out = area_effort2b[,c(1:2)]
temp.match = match(area_effort2b$area, areas$area)
area_effort2b.out$fmp_area = areas[temp.match, 6]
area_effort2b.out$council_mgmt_area_id = areas[temp.match,7]
area_effort2b.out$council_mgmt_area = areas[temp.match,8]
area_effort2b.out$council_area_id = areas[temp.match,4]
area_effort2b.out$council_area = areas[temp.match,3]
area_effort2b.out$geo_area = areas[temp.match,5]
area_effort2b.out$area = area_effort$area

#get species name
temp.match = match(area_effort2b$species, species$code)
area_effort2b.out$species = area_effort2b$species
area_effort2b.out$common_name = species[temp.match,2]

#get the value columns
area_effort2b.out$area_size = rep(NA, dim(area_effort2b.out)[1])
area_effort2b.out$cpue = rep(NA, dim(area_effort2b.out)[1])
area_effort2b.out$RPN = rep(NA, dim(area_effort2b.out)[1])
area_effort2b.out$RPW = rep(NA, dim(area_effort2b.out)[1])
area_effort2b.out$mean_len = rep(NA, dim(area_effort2b.out)[1])
area_effort2b.out$mean_wt = rep(NA, dim(area_effort2b.out)[1])
area_effort2b.out[,12:17] = area_effort2b[,c(6,8:11,14)]

# Add variances

area_effort2b.out <- area_effort2b.out %>% 
  left_join(allvar_2b_7) %>% 
  # - area_size: should be NA and not 0 for areas that don't have area sizes
  # - cpue: always appropriate to use 0 instead of NA
  # - RPN: appropriate to use 0 unless area_size is NA
  # - RPW: should be NA not 0 when mean_len/mean_wt is NA
  # - mean_len/mean_wt: should be NA when missing
  # - RPN_var: NA when RPN is NA, also NA for species not identified in Cara's
  # variance species list. 0 is okay only if the RPN is a true 0 and the other
  # conditions are met.
  # - cpue_var: NA when RPN_var is NA (remember variance is area-weighted, i.e.
  # calculated for RPNs not cpues).  0 is okay only if the cpue is a true 0 and
  # the other conditions are met.
# - RPW_var: NA when RPN_var is NA, also NA when mean_len/mean_wt is NA
mutate(area_size = ifelse(area_size == 0, NA, area_size),
       cpue = ifelse(is.na(cpue), 0, cpue),
       RPN = ifelse(is.na(area_size), NA, RPN),
       RPW = ifelse(is.na(mean_len) | is.na(mean_wt) | is.na(RPN), NA, RPW),
       RPN_var = ifelse(is.na(RPN), NA, RPN_var)) %>% 
  mutate(RPN_cv = ifelse(is.na(RPN), NA, sqrt(RPN_var) / RPN)) %>% 
  # Assume the CV are the same for area 16 as 15 and 17 as 18 then derive the
  # RPN_var for 16 and 17 from the CV. Note that this reflects a change from the
  # sablefish/rockfish PT scripts, where it was assumed that the variances were
  # the same instead of deriving them from the CV
  mutate(RPN_cv = replace(RPN_cv, which(area == 16 & species %in% nw_species), RPN_cv[area == 15 & species %in% nw_species]),
         RPN_cv = replace(RPN_cv, which(area == 17 & species %in% sw_species), RPN_cv[area == 18 & species %in% sw_species]),
         RPN_var = ifelse(area %in% c(16, 17) & !is.na(RPN) & !is.nan(RPN_cv), (RPN * RPN_cv)^2, RPN_var)) %>% 
  # derive RPW and cpue variances from RPN cv
  mutate(RPW_var = ifelse(is.na(RPW), NA, ifelse(is.nan(RPN_cv), 0, (RPW * RPN_cv)^2)),
         cpue_var = ifelse(is.nan(RPN_cv), 0, (cpue * RPN_cv)^2)) %>% 
  # In some years the extrapolation from area 16/18 to 15/17 results in
  # situations where CPUE=0 and CPUE_var=0 but RPN_var=NA. Fix this.
  mutate(RPN_var = ifelse(cpue == 0 & cpue_var == 0 & is.na(RPN_var), 0, RPN_var)) %>% 
  select(-RPN_cv) %>% 
  left_join(areas %>% select(geo_area, exploitable)) %>% 
  select(Year = year, Survey = country, FMP_management_area = fmp_area, Council_management_area_ID = council_mgmt_area_id,
	   Council_management_area = council_mgmt_area,  Council_sablefish_management_area_ID = council_area_id,
         Council_sablefish_management_area = council_area, Geographic_area_name = geo_area,
         area_code = area, Exploitable = exploitable, Species_Code = species, species = common_name, area_size_km = area_size,
         CPUE = cpue, CPUE_var = cpue_var, RPN, RPN_var, RPW, RPW_var, 
         mean_length = mean_len, mean_weight = mean_wt)

# should be 0
# area_effort2b.out %>% filter(Survey == "Japan" & !c(Species_Code %in% var_species[var_species$country=="Japan",]$species)) %>% filter(!is.na(RPN_var))

# future tests
# filter(is.na(area_size) & RPN == 0)
# filter(is.na(mean_len) & !is.na(mean_wt))

#Stamp last modified date
area_effort2b.out$LastModifiedDate = rep(Sys.time(), dim(area_effort2b.out)[1])

# Only report sablefish for whale-corrected tables
if(DEP == TRUE) {
  area_effort2b.out <- area_effort2b.out[area_effort2b.out$Species_Code == 20510,]
}

print(paste("Writing AreaRPN_AllStrata", dep_label, " to ", OUTPUTMODE, sep=""))
if (OUTPUTMODE=="SQL") {
	#Save to database
	conn <- odbcConnect(DSN)
	qry <- paste("delete from AreaRPN_AllStrata", dep_label, " where Year=", YEAR, sep="")
	sqlQuery(conn, qry)
	sqlSave(conn, area_effort2b.out, tablename=paste("AreaRPN_AllStrata", dep_label, sep=""), append=TRUE, rownames=FALSE, safer=TRUE, fast=TRUE) 
	odbcClose(conn)
}
if (OUTPUTMODE=="CSV") {
	#Output to file
	print(paste("writing file: ", OUTPUTPATH, '\\', YEAR, "_AreaRPN_AllStrata", dep_label, ".csv", sep=''))
	write.table(area_effort2b.out, file=paste(OUTPUTPATH, '\\', YEAR, "_AreaRPN_AllStrata", dep_label, ".csv", sep=''), append=FALSE, sep=",", row.names=FALSE, col.names=TRUE, quote=TRUE)
}

print("finished RPW")

quit()