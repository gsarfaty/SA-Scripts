#load packages
library(tidyverse)
library(ICPIutilities)
library(glamr)
library(readxl)
library(here)
library(magrittr)


memory.limit(size=500000)


#read in MER and reformat#
MER<-read_rds("Processed_Files\\msd_genie_fy17to20_20200512_attributes.rds") %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year %in% c("2020"))

MER2 <- MER %>% 
  group_by(orgunituid,sitename,snu1,psnu,psnuuid,community,communityuid,fundingagency,mech_code,mech_name,
           primepartner,DSP,SiyenzaFacility,
           UserFriendly_PartnerName_Modified,indicator,standardizeddisaggregate,fiscal_year) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  reshape_msd(clean = FALSE) %>%
  spread(period,val) %>% 
  rename(siyenza=SiyenzaFacility,
         facility=sitename,
         mechanismid=mech_code,
         implementingmechanismname=mech_name,
         dsp=DSP) 

#read in HRID
path<-here("HRID", "Interagency_HRID_dash_FY20Q1_20200520.xlsx")
excel_sheets(path)
HRID<-read_excel(path, sheet="data")

colnames(HRID) %<>% tolower

HRID<-HRID %>% 
  mutate(mechanismid=as.character(mechanismid))

#combine
MER_HRID<-bind_rows(HRID,MER2)


write_tsv(MER_HRID,"Processed_Files\\HRID_MER_FY20Q2.txt", na="")
