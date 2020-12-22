library(tidyverse)


#HFR RETENTION ------------------------------------------------------------

tx_currr<-read_tsv("Processed_Files\\HFR\\HFR_pd3to8_20200524.txt") %>% 
  filter(indicator %in% c("TX_CURR")) %>% 
  group_by(date,psnu,indicator) %>% 
  summarise_at(vars(val), sum, na.rm=TRUE) %>% 
  filter(date >= as.Date("2020-04-04")) %>% 
  spread(date,val) %>% 
  rename(t1="2020-04-06",
         t2="2020-05-04") %>% 
  mutate(NN= t2-t1) %>% 
  select(-c("indicator","t1","t2"))


TX_NEW<-read_tsv("Processed_Files\\HFR\\HFR_pd3to8_20200524.txt") %>% 
  filter(indicator=="TX_NEW",
         date > as.Date("2020-04-06")) %>% 
  group_by(psnu,indicator) %>% 
  summarise_at(vars(val), sum, na.rm=TRUE) %>% 
  select(-c("indicator")) %>% 
  rename(TX_NEW=val) 


ret<-tx_currr %>% 
  left_join(TX_NEW,by="psnu") %>% 
  mutate(NN_NEW=((NN/TX_NEW)*100))


write_tsv(ret,"Processed_Files\\HFR\\HFR_May_Retention_20200604.txt",na="")

#CORE INTERVENTIONS --------------------------------------------------------

library(readxl)
library(stringr)

ret_int<-read_excel("Core_Interventions\\USAID DSP_Retention Interventions_05292020.xlsx", 
                    sheet="Appointments and Tracing", range="A2:O52",na="")

ret_int<-ret_int %>% 
  filter(!is.na(`Number of PEPFAR-supported (Siyenza) facilities`)) %>% 
  mutate(psnu=case_when(
    str_detect(District,"Alfred") ~ "ec Alfred Nzo District Municipality",
               str_detect(District,"Buffalo") ~ "ec Buffalo City Metropolitan Municipality",
               str_detect(District,"Lej") ~ "fs Lejweleputswa District Municipality",
               str_detect(District,"Thabo") ~ "fs Thabo Mofutsanyane District Municipality",
               str_detect(District, "Johannesburg") ~ "gp City of Johannesburg Metropolitan Municipality",
               str_detect(District, "Sedibeng") ~ "gp Sedibeng District Municipality",
               str_detect(District, "eThekwini") ~ "kz eThekwini Metropolitan Municipality",
               str_detect(District, "Harry") ~ "kz Harry Gwala District Municipality",
               str_detect(District, "King") ~ "kz King Cetshwayo District Municipality",
               str_detect(District, "Ugu") ~ "kz Ugu District Municipality",
               str_detect(District, "Capricorn") ~ "lp Capricorn District Municipality",
               str_detect(District, "Mopani") ~ "lp Mopani District Municipality",
               str_detect(District, "Ehlanzeni") ~ "mp Ehlanzeni District Municipality",
               str_detect(District, "Gert") ~ "mp Gert Sibande District Municipality",
               str_detect(District, "Nkangala") ~ "mp Nkangala District Municipality",
               str_detect(District, "Cape Town") ~ "wc City of Cape Town Metropolitan Municipality",
                          TRUE ~ District
               ))
# 
# drug_int<-read_excel("Core_Interventions\\USAID DSP_Retention Interventions_05292020.xlsx", 
# sheet="Access to meds", range="A4:S54",na="") %>% 
#   filter(!is.na(`Nu`)
         
         
# MERGE ------------------------------------------------------------------------------------------

merged<-ret %>%
  left_join(ret_int,by="psnu")

write_tsv(merged,"Core_Interventions\\testmerge_retentioninterventions.txt", na="")