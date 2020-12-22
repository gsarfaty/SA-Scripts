library(tidyverse)
library(readxl)

memory.limit(size=500000)


#read in MER and reformat#
MER<-read_tsv("Processed_Files\\msd_genie_fy17to20_20200512_attributes.txt", col_types = c(.default = "c")) %>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus"),
         fiscal_year %in% c("2020"),
         fundingagency=="USAID",
         DSP=="YES")


MER_sub_long<-MER %>% 
  select(orgunituid,sitename,mech_code,psnu,indicator,sex,trendscoarse,fiscal_year,targets,qtr1,qtr2,qtr3,qtr4,cumulative) %>% 
  gather(period,val,targets:cumulative) %>% 
  mutate(source="MER") %>% 
  rename(agecoarse=trendscoarse) %>% 
  mutate(val=as.numeric(val))


#read in HFR
HFRpd3<-read_excel("HFR\\raw\\South Africa HFR 2020.03.xlsx", sheet="HFR Nov 25 - Dec 16", skip=1)
HFRpd4<-read_excel("HFR\\raw\\South Africa HFR 2020.04.xlsx", sheet="HFR Dec 23 - Jan 13", skip=1)
HFRpd5<-read_excel("HFR\\raw\\HFR_FY20_SouthAfrica_pd5.xlsx", sheet="HFR", skip=1)
HFRpd6<-read_excel("HFR\\raw\\HFR_FY20_SouthAfrica_pd6.xlsx", sheet="HFR", skip=1)
HFRpd7<-read_excel("HFR\\raw\\HFR_FY20_SouthAfrica_pd7.xlsx", sheet="HFR", skip=1) 
HFRpd8<-read_excel("HFR\\raw\\SA_hfr_pd8.xlsx", sheet="HFR", skip=1)

HFR<-bind_rows(HFRpd3,HFRpd4,HFRpd5,HFRpd6,HFRpd7,HFRpd8)
rm(HFRpd3,HFRpd4,HFRpd5,HFRpd6,HFRpd7,HFRpd8)

HFR<-HFR %>% 
  select(orgunituid,orgunit,mech_code,partner,psnu,indicator,sex,agecoarse,otherdisaggregate,date,val) %>% 
  mutate(source="HFR") %>% 
  rename(sitename=orgunit)


#siyenza
siyenza<-read_excel("Siyenza\\Raw\\Panagora Siyenza Data 20200527.xlsx", sheet=)
