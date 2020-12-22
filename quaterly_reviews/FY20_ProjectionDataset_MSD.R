library(tidyverse)
library(readxl)
library(ICPIutilities)


df<-read_msd("MSD\\fy20q2_i\\MER_Structured_Datasets_Site_IM_FY18-20_20200605_v1_1_South Africa.txt")

dsp<-read_excel("ContextFiles\\UserFriendly_PartnerName_DSPcolumn.xlsx") %>% 
  rename(mech_code=MechanismID,
         DSP=DSP_18_19)

### HTS -----------------------------------------------------------------------------
proj_HTS<-df %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS")) %>% 
  mutate(modality_group=case_when(
    modality %in% c ("Community Home-Based","Community Mobile","Community VCT",
                    "Emergency","Inpatient","Malnutrition","Other Community",
                    "Pediatric <5 Clinic","Post ANC1","STI Clinic","TB Clinic",
                    "VCT","VMMC") ~ "Other",
    modality %in% c("PMTCT (ANC)") ~ "PMTCT (ANC)",
    modality %in% c("Index (Community)",
                    "Index (Facility)") ~ "Index",
    TRUE ~ modality
  )) 

  # group_by(psnu, mech_code,indicator,fiscal_year,modality_group) %>% 
  # summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  # ungroup() %>% 
  # reshape_msd(clean=TRUE) %>% 
  # filter(period_type=="cumulative") %>% 
  # unite(ind_unite,indicator,modality_group,period,sep="_") %>% 
  # spread(ind_unite,val) %>% 
 
  


proj_TotalN<-df %>% 
  filter(fiscal_year %in% c("2018","2019","2020"),
         indicator %in% c("TX_NEW","TX_CURR","TX_NET_NEW"),
         standardizeddisaggregate=="Total Numerator") %>% 
  group_by(fundingagency,snuprioritization,psnu,mech_code,indicator,fiscal_year) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  unite(ind_unite,period,indicator,period_type,sep="_") %>% 
  spread(ind_unite,val) %>% 
  left_join(dsp,by="mech_code")


write_tsv(proj_TotalN,"Processed_Files\\MSD_Projections_20200618.txt", na="")
write_tsv(proj_TotalN,"Processed_Files\\MDS_Proj_long_20200618.txt", na="")