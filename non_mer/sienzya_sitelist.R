library(tidyverse)
library(readxl)


siyenzadash<-read_excel("Siyenza\\PEPFAR-Phuthuma_New_Data20200619_Build2020625.xlsx", sheet="site_list", skip=2) %>% 
  rename(facility=Facility)

org<-read_csv("ContextFiles\\orgunits_20200625.csv") %>% 
  filter(orgunit_level=="7") %>% 
  select(orgunit_internal_id:moh_id) %>% 
  rename(orgunituid=orgunit_internal_id,
         facility=orgunit_name)


siyenza_att<-siyenzadash %>% 
  left_join(org,by="facility") %>% 
  mutate(orgunituid=(case_when(
    facility=="gp First Avenue Clinic" ~ "SPb6QS5q7KX",
    facility=="gp Itireleng (Region B) Clinic" ~ "ZiZmfp6A9Kx",
    facility=="kz Illovu Clinic" ~ "ZdB76RODl6b",
    TRUE ~ orgunituid
  )))


missing<-siyenza_att %>% 
  filter(is.na(orgunituid))


write_tsv(siyenza_att,"ContextFiles\\siyenza_att_uid_20200626.txt",na="")
                        