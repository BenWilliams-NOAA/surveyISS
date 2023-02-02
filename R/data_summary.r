#Remove oracle_pw ans user name
rm(oracle_pw)
rm(oracle_user)

# packages
library(sumfish)
library(tidyverse)
library(tidytable)
library(vroom)
library(here)
library(purrr)
library(rsample)
library(data.table)
library(scico)
library(extrafont)
remotes::install_version("Rttf2pt1", version = "1.3.8")
extrafont::font_import()

loadfonts(device="win")

# add fonts to all text (last line)
ggplot2::theme_set(
  ggplot2::theme_light() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      # axis.ticks.length = grid::unit(base_ / 2.2, "pt"),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      strip.text.x = element_text(colour = "black"),
      strip.text.y = element_text(colour = "black"),
      panel.border = element_rect(fill = NA),
      legend.key.size = grid::unit(0.9, "lines"),
      legend.key = ggplot2::element_rect(colour = NA, fill = NA),
      legend.background = ggplot2::element_rect(colour = NA, fill = NA),
      text = element_text(family = "Times New Roman")
    )
)
spec<-vroom::vroom(here::here('data', 'species_code_name.csv')) #species_code and common names

#Species codes and yrs by area
ebs_sp_code=c(21740, 21741, 21742, 21720, 21721, 21722, 10210, 10209, 10261, 10263, 10130, 10110, 10112, 10115, 10116, 10285)
ebs_yr=c(2017,2018,2019)

ai_sp_code=c(30060,30420,21740, 21741, 21742, 21720, 21721, 21722, 10261, 10263, 10110, 10112, 21921, 10130, 10112)
ai_yr=c(2014,2016,2018)

goa_sp_code=c(30060, 21740, 21741, 21742, 21720, 21721, 21722, 10110, 10130, 30020, 10262, 10261, 20510, 10200, 20510, 30420)
goa_yr=c(2017,2019,2021)

#query Racebase - returns list object
#also keep the species of interest for each area
ebs_data=sumfish::getRacebase(year=ebs_yr,surv="EBS_SHELF")
ai_data=sumfish::getRacebase(year=ai_yr,surv="AI")
goa_data=sumfish::getRacebase(year=goa_yr,surv="GOA")

#Combine objects and filter for area specific species codes
#raw lengths
raw_len=bind_rows(data.frame(ebs_data$raw_length %>% rename_all(tolower) %>% filter(species_code %in% ebs_sp_code),surv="EBS_SHELF"),
                  data.frame(ai_data$raw_length %>% rename_all(tolower) %>% filter(species_code %in% ai_sp_code),surv="AI"),
                  data.frame(goa_data$raw_length %>% rename_all(tolower) %>% filter(species_code %in% goa_sp_code),surv="GOA"))
#lengths from length table
lfreq=bind_rows(data.frame(ebs_data$length %>% rename_all(tolower) %>% filter(species_code %in% ebs_sp_code),surv="EBS_SHELF"),
                data.frame(ai_data$length %>% rename_all(tolower) %>% filter(species_code %in% ai_sp_code),surv="AI"),
                data.frame(goa_data$length %>% rename_all(tolower) %>% filter(species_code %in% goa_sp_code),surv="GOA"))
#survey catch
catch=bind_rows(data.frame(ebs_data$catch %>% rename_all(tolower) %>% filter(species_code %in% ebs_sp_code),surv="EBS_SHELF"),
                data.frame(ai_data$catch %>% rename_all(tolower) %>% filter(species_code %in% ai_sp_code),surv="AI"),
                data.frame(goa_data$catch %>% rename_all(tolower) %>% filter(species_code %in% goa_sp_code),surv="GOA"))
#hauls
haul=bind_rows(data.frame(ebs_data$haul %>% rename_all(tolower),surv="EBS_SHELF"),
                data.frame(ai_data$haul %>% rename_all(tolower),surv="AI"),
                data.frame(goa_data$haul %>% rename_all(tolower),surv="GOA"))

#add year column
raw_len2 <- raw_len %>%
  mutate(year = as.numeric(substr(cruise,1,4)) ) %>%
  rename_all(tolower)

catch2<-catch %>%
  mutate(year = as.numeric(substr(cruise,1,4)) ) %>%
  rename_all(tolower)

lfreq2 <- lfreq %>%
  mutate(year = as.numeric(substr(cruise,1,4)) ) %>%
  inner_join(haul, by=c('hauljoin','surv')) %>%
  rename_all(tolower) %>%
  dplyr::select(year, species_code, hauljoin, sample_type,sex, surv,length, frequency)

#Changing juvenile codes so we can plot raw lengths per haul
raw_len2$species_code[raw_len2$species_code==21741]=21740
raw_len2$species_code[raw_len2$species_code==21742]=21740
raw_len2$species_code[raw_len2$species_code==21721]=21720
raw_len2$species_code[raw_len2$species_code==21722]=21720
raw_len2$species_code[raw_len2$species_code==10209]=10210
raw_len2$species_code[raw_len2$species_code==10116]=10115
raw_len2$species_code[raw_len2$species_code==10263]=10261

raw_len3=full_join(raw_len2,spec,by="species_code")

#number caught per haul, species, and survey
num_ct=catch2 %>% group_by(year,species_code,surv,hauljoin) %>% summarise(num=sum(number_fish))
#number lengths per haul, species and survey
len_hl=raw_len3 %>% group_by(year,species_code,species_name,surv,hauljoin) %>% summarise(freq=sum(frequency))
#number lengths per haul, species, sex, and survey
len_s=raw_len3 %>% group_by(year,species_code,species_name,surv,sex,hauljoin) %>% summarise(freq=sum(frequency))

#Annual number of lengths
ann_len=raw_len3 %>% group_by(year,species_name,surv) %>% summarise(freq=sum(frequency))
  ann_len=spread(ann_len,surv,freq)
  write.csv(ann_len,file="annLen_byAreaSpecies_rep.csv",row.names=FALSE)
#3-yr average table
all_3yrAvgTotLen=ann_len %>% group_by(species_name,surv) %>% summarise(avg_tot_num=round(mean(freq),digits=0))
  all_3yrAvgTotLen_v2=spread(all_3yrAvgTotLen,surv,avg_tot_num)
  write.csv(all_3yrAvgTotLen_v2,file="avg3yr_byAreaSP_rep.csv",row.names=FALSE)

#plot the number of sampled lengths per haul v number caught per haul by species and survey
lfreq_num_plot=function(dat,surv,sex=FALSE,pname="foo")
{

  if(sex==TRUE)
  {
    ls_plot=ggplot(dat,aes(x=num,y=freq,color=as.factor(sex)))+geom_point(pch=21)+
            facet_wrap(~species_name,scales="free")+
            geom_hline(yintercept = 200, colour = "red",linetype="dashed")+
            xlab("Catch (number) per haul")+ylab("Number of sampled lengths per haul")+
            scale_color_discrete(name="Sex")

    png(paste0(surv,"_samples_per_haul_bySex.png"),units="in",width=12,height=10,res=300)
    print(ls_plot)
    dev.off()
  }
  if(sex==FALSE)
  {
    ls_plot=ggplot(dat,aes(x=num,y=freq))+geom_point(pch=21)+
            facet_wrap(~species_name,scales="free")+
            geom_hline(yintercept = 200, colour = "red",linetype="dashed")+
            xlab("Catch (number) per haul")+ylab("Number of sampled lengths per haul")

    png(paste0(surv,"_samples_per_haul_",pname,".png"),units="in",width=12,height=10,res=300)
    print(ls_plot)
    dev.off()
  }
}

#Plot number of lengths per haul vs. catch numbers per haul
obs_hl=inner_join(num_ct,len_hl,by=c("year","species_code","surv","hauljoin"))
obs_hl$prop=obs_hl$freq/obs_hl$num

ebs=obs_hl[obs_hl$surv=="EBS_SHELF" & obs_hl$species_code %in% ebs_sp_code,]
lfreq_num_plot(ebs,tolower(unique(ebs$surv)),sex=FALSE,pname="total")

ai=obs_hl[obs_hl$surv=="AI" & obs_hl$species_code %in% ai_sp_code,]
lfreq_num_plot(ai,tolower(unique(ai$surv)),sex=FALSE,pname="total")

goa=obs_hl[obs_hl$surv=="GOA" & obs_hl$species_code %in% goa_sp_code,]
lfreq_num_plot(goa,tolower(unique(goa$surv)),sex=FALSE,pname="total")

#Plot number of female+male lengths per haul by the total number caught
len_hl_mf<-len_s %>% filter(sex!=3) %>%
  group_by(year,species_code,species_name,surv,hauljoin) %>%
  summarise(freq=sum(freq))

obs_hl_mf=inner_join(num_ct,len_hl_mf,by=c("year","species_code","surv","hauljoin"))

ebs_mf=obs_hl_mf[obs_hl_mf$surv=="EBS_SHELF",]
lfreq_num_plot(ebs_mf,tolower(unique(ebs_mf$surv)),sex=FALSE,pname="MF")

ai_mf=obs_hl_mf[obs_hl_mf$surv=="AI",]
lfreq_num_plot(ai_mf,tolower(unique(ai_mf$surv)),sex=FALSE,pname="MF")

goa_mf=obs_hl_mf[obs_hl_mf$surv=="GOA",]
lfreq_num_plot(goa_mf,tolower(unique(goa_mf$surv)),sex=FALSE,pname="MF")

#Remove oracle_pw ans user name before saving workspace
rm(oracle_pw)
rm(oracle_user)

