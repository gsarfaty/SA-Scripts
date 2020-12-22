library(tidyverse)
library(readxl)

memory.limit(size=500000)

##read in file w attributes as base##
df<-read_tsv("FY20Q1\\merged_files\\msd_genie_fy17to20_20200214_attributes_v18Feb.txt",
             col_types = c(.default = "c"))
             
##NN CALC for all partner shifts##
NN_calc<- df %>% 
  filter(indicator =="TX_NET_NEW",
         standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% c("2019","2020")) %>% 
  mutate(indicator="TX_NET_NEW_CALC") %>% 
  gather(period,value,targets:cumulative) %>% 
  mutate(mech_code=case_when(
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17020"  ~ "70310",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17037" ~ "70310",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17021" ~ "70310",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="gp Sedibeng District Municipality" & mech_code=="17023" ~ "70310",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="lp Capricorn District Municipality" & mech_code=="17036" ~ "70310",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="mp Nkangala District Municipality" & mech_code=="17036" ~ "70287",
    fiscal_year=="2019" & period %in% c("qtr1","qtr2","cumulative") 
    & psnu=="wc City of Cape Town Metropolitan Municipality" & mech_code=="17046" ~ "70310",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="fs Lejweleputswa District Municipality" & mech_code=="18481" ~ "70301",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="nw Dr Kenneth Kaunda District Municipality" & mech_code=="17037" ~ "18484",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & psnu=="kz eThekwini Metropolitan Municipality" &
      fy19q1_sitetransition=="USAIDtoCDC" ~ "18481",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="kz uMgungundlovu District Municipality" & mech_code=="17046" ~ "70289",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name=case_when(
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17020" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17037" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17021" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="gp Sedibeng District Municipality" & mech_code=="17023" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="lp Capricorn District Municipality" & mech_code=="17036" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="mp Nkangala District Municipality" & mech_code=="17036" ~ "BRHC: Accelerating Program Achievements to Control the epidemic- KZN: King Cetshwayo and Ugu Mpumalanga: Gert Sibande and Nkangala",
    fiscal_year=="2019" & period %in% c("qtr1","qtr2","cumulative") 
    & psnu=="wc City of Cape Town Metropolitan Municipality" & mech_code=="17046" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="fs Lejweleputswa District Municipality" & mech_code=="18481" ~ "WRHI: Accelerating Program Achievements to Control the epidemic- Gauteng (Tshwane) and Free State (Lejwekleputswa)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="nw Dr Kenneth Kaunda District Municipality" & mech_code=="17037" ~ "Aurum Comprehensive (CDC GH001981)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & psnu=="kz eThekwini Metropolitan Municipality" &
      fy19q1_sitetransition=="USAIDtoCDC" ~ "Health Systems Trust Comprehensive (CDC GH001980)",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="kz uMgungundlovu District Municipality" & mech_code=="17046" ~ "MatCH: Accelerating Program Achievements to Control the epidemic Eastern Cape (Alfred Nzo), KZN (Harry Gwala, eThekwini, uMgungundlovu)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner=case_when(
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17020" ~ "ANOVA HEALTH INSTITUTE",
    fiscal_year=="2019" & period %in% ("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17037" ~ "ANOVA HEALTH INSTITUTE",
    fiscal_year=="2019" & period %in% ("qtr1","cumulative") &
      psnu=="gp City of Johannesburg Metropolitan Municipality" & mech_code=="17021" ~ "ANOVA HEALTH INSTITUTE",
    fiscal_year=="2019" & period %in% ("qtr1","cumulative") & 
      psnu=="gp Sedibeng District Municipality" & mech_code=="17023" ~ "ANOVA HEALTH INSTITUTE",
    fiscal_year=="2019" & period %in% ("qtr1","cumulative") & 
      psnu=="lp Capricorn District Municipality" & mech_code=="17036" ~ "ANOVA HEALTH INSTITUTE",
    fiscal_year=="2019" & period %in% ("qtr1","cumulative") & 
      psnu=="mp Nkangala District Municipality" & mech_code=="17036" ~ "Broadreach Healthcare, LLC",
    fiscal_year=="2019" & period %in% ("qtr1","qtr2","cumulative") 
    & psnu=="wc City of Cape Town Metropolitan Municipality" & mech_code=="17046" ~ "ANOVA HEALTH INSTITUTE",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="fs Lejweleputswa District Municipality" & mech_code=="18481" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="nw Dr Kenneth Kaunda District Municipality" & mech_code=="17037" ~ "THE AURUM INSTITUTE",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="kz uMgungundlovu District Municipality" & mech_code=="17046" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ primepartner
  )) %>% 
  mutate(fundingagency=case_when(
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="fs Lejweleputswa District Municipality" & mech_code=="18481" ~ "USAID",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & 
      psnu=="nw Dr Kenneth Kaunda District Municipality" & mech_code=="17037" ~ "HHS/CDC",
    fiscal_year=="2019" & period %in% c("qtr1","cumulative") & psnu=="kz eThekwini Metropolitan Municipality" &
      fy19q1_sitetransition=="USAIDtoCDC" ~ "HHS/CDC",
    TRUE ~ fundingagency
  )) %>% 
  mutate(fy19q1_sitetransition=case_when(
    fiscal_year=="2019" & sitename=="kz Cato Manor CHC" & fundingagency=="HHS/CDC" ~ "CDC Exited in fy19q1",
    TRUE ~ fy19q1_sitetransition
  ))


##############################################################




#tshwane
tshwane<-wide_sub %>%
  filter(fiscal_year %in% c("2018","2019") & indicator=="TX_CURR" & psnu=="gp City of Tshwane Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "18484",
    mech_code=="17021" ~ "18484",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "Aurum Comprehensive (CDC GH001981)",
    mech_code=="17021" ~ "Aurum Comprehensive (CDC GH001981)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17036" ~ "THE AURUM INSTITUTE",
    mech_code=="17021" ~ "THE AURUM INSTITUTE",
    TRUE ~ primepartner
  )) %>% 
  mutate(fundingagency_modified=case_when(
    mech_code=="17036" ~ "HHS/CDC",
    mech_code=="17021" ~ "HHS/CDC",
    TRUE ~ fundingagency
  ))

#alfred Nzo
alfrednzo<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="ec Alfred Nzo District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17023" ~ "70289",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17023" ~ "MatCH: Accelerating Program Achievements to Control the epidemic Eastern Cape (Alfred Nzo), KZN (Harry Gwala, eThekwini, uMgungundlovu)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17023" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ primepartner
  ))

#buffalo city
buffalo<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="ec Buffalo City Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "70288",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "KI: Accelerating Program Achievements to Control the epidemicEastern Cape: Buffalo City Western Cape: Cape Town",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17036" ~ "KHETHIMPILO AIDS FREE LIVING",
    TRUE ~ primepartner
  ))

#oliver tambo
otambo<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="ec Oliver Tambo District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="18481" ~ "18482",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="18481" ~ "TB/HIV Care Comprehensive (CDC GH001933)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="18481" ~ "TB HIV CARE ASSOCIATION",
    TRUE ~ primepartner
  ))


#Chris Hani
chrishani<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="ec Chris Hani District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="18481" ~ "18482",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="18481" ~ "TB/HIV Care Comprehensive (CDC GH001933)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="18481" ~ "TB HIV CARE ASSOCIATION",
    TRUE ~ primepartner
  ))

#harry Gwala
HG<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="kz Harry Gwala District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17023" ~ "70289",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17023" ~ "MatCH: Accelerating Program Achievements to Control the epidemic Eastern Cape (Alfred Nzo), KZN (Harry Gwala, eThekwini, uMgungundlovu)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17023" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ primepartner
  ))
