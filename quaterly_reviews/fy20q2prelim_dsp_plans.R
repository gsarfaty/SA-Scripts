library(tidyverse)
library(readxl)

memory.limit(size=500000)


#read in MER and reformat#
MER<-read_tsv("Processed_Files\\msd_genie_fy17to20_20200318_attributes.txt", col_types = c(.default = "c")) %>% 
  filter(indicator %in% c("HTS_TST","HTS_TST_POS","TX_NEW","TX_CURR"),
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result","Age/Sex/HIVStatus"),
         fiscal_year %in% c("2019","2020"),
         fundingagency=="USAID",
         DSP=="YES")


MER_sub_long<-MER %>% 
  select(orgunituid,operatingunit,sitename,mech_code,psnu,indicator,numeratordenom,sex,trendscoarse,fiscal_year,targets,qtr1,qtr2,qtr3,qtr4,cumulative) %>% 
  gather(period,val,targets:cumulative) %>% 
  mutate(source="MER") %>% 
  rename(agecoarse=trendscoarse) %>% 
  mutate(val=as.numeric(val))



#read in prelim q2 and reformat to match MER/DATIM#
q2<-read_excel("FY20\\FY20Q1_Outputs\\Q4DSPplans\\USAID FY20Q2 preliminary results_GS.xlsx")


q2_long<-q2 %>% 
  select(-c(TX_CURR_T,Q1_TX_Curr28,Q1_NET_NEW,Q2_Net_New)) %>% 
  rename(TX_CURR=Q2_TX_Curr28,
         TX_NEW=Q2_TX_New,
         HTS_TST_POS=HTS_POS,
         TB_PREV_N=`TB_Prev(N)`,
         TB_PREV_D=`TB_Prev(D)`) %>% 
  gather(indicator,val,TX_CURR:TB_PREV_D) %>% 
  rename(psnu=District) %>% 
  mutate(fiscal_year="2020",
         period="qtr2",
         source="prelim") %>% 
  mutate(numeratordenom=case_when(
    indicator=="TB_PREV_D"  ~ "D",
    TRUE ~ "N"
  )) %>% 
  mutate(indicator=case_when(
    indicator=="TB_PREV_D" ~ "TB_PREV",
    indicator=="TB_PREV_N" ~ "TB_PREV",
    TRUE ~ indicator
  )) %>% 
  mutate(operatingunit="South Africa")


#read in recent HFR #
HFRpd7<-read_excel("HFR\\HFR_FY20_SouthAfrica 04_29_2020.xlsx", sheet="HFR", skip=1)

HFR<-HFRpd7 %>% 
  select(orgunituid,orgunit,operatingunit,mech_code,partner,psnu,indicator,sex,agecoarse,otherdisaggregate,date,val) %>% 
  mutate(source="HFR") %>% 
  rename(sitename=orgunit)


final<-MER_sub_long %>% 
  bind_rows(q2_long) %>% 
  bind_rows(HFR)

############# if adding  q2 NN ############
NN<-final %>% 
  filter(indicator=="TX_CURR",
         source %in% c("MER","prelim"),
         fiscal_year=="2020",
         period %in% c("qtr1", "qtr2")) %>% 
  group_by(psnu,fiscal_year,period) %>% 
  summarise_at(vars(val), sum, na.rm=TRUE) %>% 
  mutate(lag = dplyr::lag(val, n = 1, default = NA)) %>% 
  filter(period=="qtr2") %>% 
  mutate(NN=val-lag) %>% 
  select(-c("val","lag")) %>% 
  mutate(indicator="TX_NET_NEW",
         operatingunit="South Africa")%>% 
  rename(val=NN) %>% 
  mutate(source="prelim")


finalNN<-final %>% 
  bind_rows(NN)



write_tsv(finalNN,"DSPplans_q2prelimNN_HFR.txt",na="")
       
  
  
# #read in HFR Q2
# HFRpd4<-read_excel("HFR\\HFR_FY20_PD4_SouthAfrica_20200206.xlsx", sheet="HFR", skip=1) 
# HFRpd5<-read_excel("HFR\\HFR_FY20_SouthAfrica_03_05_2020.xlsx", sheet="HFR", skip=1)
# 
# HFR<-bind_rows(HFRpd4,HFRpd5)
# 
# 
# HFR<-HFR %>% 
#   select(orgunituid,orgunit,mech_code,partner,psnu,indicator,sex,agecoarse,otherdisaggregate,date,val) %>% 
#   mutate(source="HFR") %>% 
#   rename(sitename=orgunit)
# 
# 
# MER_HFR<-bind_rows(MER_sub_long,HFR)
# 
# write_tsv(MER_HFR,"Processed_Files\\MER_fy20q1_HFR_fy20q2.txt", na="")
# 
# MER_HFR_CURRonly<-MER_HFR %>% 
#   filter(indicator=="TX_CURR")