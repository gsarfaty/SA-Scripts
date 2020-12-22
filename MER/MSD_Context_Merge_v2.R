library(tidyverse)
library(readxl)
library(ICPIutilities)
library(here)
library(glamr)

memory.limit(size=500000)

#GLOBALS -----------------------------------------------------------------------------
msds<-here("MSD")
genies<-here("Genie")
user<-#insert username
folder_id<-#insert folder
folder_save<-(genies)


# MER DATA IN ------------------------------------------------------------------------

# genie pull from drive
access_files(user,folder_id,folder_save)

genie_files<-list.files(genies,pattern="Genie")

genie<-here("Genie",genie_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind) %>% 
  filter(fiscal_year %in% c("2020","2021"))


# MSD
msd_files<-list.files(msds,pattern="FY18")

msd<-here("MSD",msd_files) %>% 
  map(read_msd, save_rds=FALSE, remove_txt = FALSE) %>% 
  reduce(rbind)


#subset & merge
msd<-msd %>%
  filter(fiscal_year %in% c("2018","2019"))

final<-rbind(genie,msd)

rm(genie,msd)


# CONTEXT FILES IN -------------------------------------------------------------------
dsp<-read_excel(here("ContextFiles","UserFriendly_PartnerName_DSPcolumn.xlsx")) %>% 
  rename(mech_code=MechanismID,
         DSP_IM=DSP_18_19)

dsp_lookback<-read_tsv(here("ContextFiles","DSP_attributes_2020-09-29.txt")) %>% 
  rename(agency_lookback=`Agency lookback`) %>% 
  select(-MechanismID)

ethk<-read_excel(here("ContextFiles", "eThekwiniSiteShifts.xlsx")) %>% 
  filter(Transitionstat=="USAIDtoCDC") %>% 
  rename(fy19q1_sitetransition=Transitionstat,
         orgunituid=Facilityuid) %>% 
  select(orgunituid,fy19q1_sitetransition)

siyenza<-read_tsv(here("ContextFiles","siyenza_att_uid_20200626.txt")) %>%
  select(-c(facility))


factype<-read_excel(here("ContextFiles","Facility_Type.xlsx")) %>% 
  rename(facility=Facility)


# MSD/Genie/Context Merge & Munge ------------------------------------------------------
final_test<-final %>% 
  left_join(dsp, by="mech_code") %>% 
  left_join(ethk,by="orgunituid") %>% 
  left_join(factype, by="facility") %>%
  left_join(siyenza, by="orgunituid") %>% 
  mutate(short_name=psnu,
         short_name=str_replace_all(short_name, "District Municipality","DM"),
         short_name=str_replace_all(short_name, "Metropolitan Municipality", "MM")) %>% 
  unite(DSPID,mech_code,short_name,sep="",remove=FALSE) %>% 
  mutate(HIGHBURDEN=case_when(
    psnu=="gp City of Johannesburg Metropolitan Municipality" ~ "YES",
    psnu=="gp City of Tshwane Metropolitan Municipality" ~ "YES",
    psnu=="gp Ekurhuleni Metropolitan Municipality" ~ "YES",
    psnu=="kz eThekwini Metropolitan Municipality" ~ "YES",
    psnu=="wc City of Cape Town Metropolitan Municipality" ~ "YES",
    TRUE ~ "NO"
  ),
  priority_district=case_when(
    snuprioritization %in% c("2 - Scale-Up: Aggressive",
                             "1 - Scale-Up: Saturation") ~ "YES",
    TRUE ~ "NO"
  ))
  
  

final_test<-final_test %>% 
  left_join(dsp_lookback,by="DSPID") %>% 
  mutate(mechID_lookback=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ 18481,
    TRUE ~ mechID_lookback),
    Partner_lookback=case_when(
      fy19q1_sitetransition=="USAIDtoCDC" ~ "HST",
      TRUE ~ Partner_lookback),
    agency_lookback=case_when(
      fy19q1_sitetransition=="USAIDtoCDC" ~ "HHS/CDC",
      TRUE ~ agency_lookback))


rm(dsp,dsp_lookback,ethk,factype,siyenza)


# Sanity Check on File ------------------------------------------------------------------
data_check<-genie %>% 
  filter(standardizeddisaggregate=="Total Numerator") %>% 
  group_by(fiscal_year,indicator) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup()

prinf(data_check)
  
# Write file -----------------------------------------------------------------------------
filename<-paste("msd_fy17to20", Sys.Date(), "attributes.txt",sep="_")

write_tsv(final_test, file.path(here("Processed_Files/MSD_genie"),filename,na=""))


