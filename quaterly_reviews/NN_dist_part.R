library(tidyverse)
library(readxl)
library(ICPIutilities)

memory.limit(size=500000)


df<-read_msd("MSD\\fy20q2_i\\MER_Structured_Datasets_Site_IM_FY18-20_20200605_v1_1_South Africa.txt")
dsp<-read_excel("ContextFiles\\UserFriendly_PartnerName_DSPcolumn.xlsx") %>% 
  rename(mech_code=MechanismID,
         DSP=DSP_18_19)


df_partner<-df %>% 
  left_join(dsp, by="mech_code") 


#district - not partner disagged
distNN_NEW<-df_partner %>% 
  filter(indicator %in% c("TX_NEW", "TX_NET_NEW","TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year=="2020",
         snuprioritization %in% c("1 - Scale-Up: Saturation","2 - Scale-Up: Aggressive"),
         DSP=="YES") %>% 
  group_by(fiscal_year,psnu,indicator) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="results",
         period=="FY20Q2") %>% 
  spread(indicator,val) %>% 
  filter(!psnu=="kz eThekwini Metropolitan Municipality")


distNN_NEW_etk<-df_partner %>% 
  filter(indicator %in% c("TX_NEW", "TX_NET_NEW","TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year=="2020",
         psnu=="kz eThekwini Metropolitan Municipality",
         DSP=="YES") %>% 
  group_by(fiscal_year,psnu,indicator,mech_code) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="results",
         period=="FY20Q2") %>% 
  spread(indicator,val) %>% 
  filter(mech_code %in% c("18481", "70289")) %>% 
  mutate(psnu=case_when(
    mech_code=="70289" ~ "eThekwini (MatCH)",
    mech_code=="18481" ~ "eThekwini (HST)",
    TRUE ~ psnu
  )) %>% 
  select(-c(mech_code))
  

final<-bind_rows(distNN_NEW,distNN_NEW_etk)


write_tsv(final,"Processed_Files\\Q2NN_NEW_etk_separate.txt", na="")