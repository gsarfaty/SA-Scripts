library(tidyverse)
library(readxl)

memory.limit(50000)


# #read in dsp district level shift file
# dsp_shifts<-read_excel("ContextFiles\\FY18_19_DistrictShifts.xlsx",sheet="Agency")
# 
# #read in genie
# genie<-read_tsv("MER_Structured_Datasets_Site_IM_FY17-20_20191220_v2_1_South Africa.txt",
#                 col_types = c(.default = "c")) %>%
#   mutate_at(vars(targets,qtr1,qtr2,qtr3,qtr4,cumulative), as.double) %>% 
#   filter(indicator %in% c("TX_CURR","TX_NEW") & standardizeddisaggregate=="Total Numerator",
#          fiscal_year %in% c("2018","2019"))
# 
# 
# #merge
# genie_dspshifts<-merge(genie,dsp_shifts,by.x="psnu",by.y="PSNU",all.x = TRUE)
# 
# 
# #read in ethekwini fy19q1 41 site shifts
# ethk<-read_excel("ContextFiles\\eThekwiniSiteShifts.xlsx") %>% 
#   filter(Transitionstat=="USAIDtoCDC") %>% 
#   rename(fy19q1_sitetransition=Transitionstat)
# 
# #merge in etk attributes
# genie_dspandsiteshifts<-merge(genie_dspshifts,ethk,by.x="orgunituid",by.y="Facilityuid",all.x=TRUE)

#read in MSD + genie + attribute merge
df<-read_tsv("FY20Q1\\merged_files\\msd_genie_fy17to20_20200214_attributes.txt",
             col_types = c(.default = "c")) %>%
  mutate_at(vars(targets,qtr1,qtr2,qtr3,qtr4,cumulative), as.double) %>% 
  filter(indicator %in% c("HTS_SELF", "HTS_TST", "HTS_TST_POS","PMTCT_ART","PMTCT_STAT",
                          "TB_ART","TB_STAT","TX_CURR","TX_NEW","TX_NET_NEW", "TX_PLVS", "VMMC_CIRC") 
         & standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% c("2018","2019","2020"))


#make wide
wide<-df_sub %>%
  gather(period,value,targets:cumulative) %>% 
  filter(!value=="na") %>%
  unite(indicator_unite,fiscal_year,indicator,period,sep="_",remove=FALSE) %>%
  spread(indicator_unite,value)

            

#filter out TX_CURR FY18Q4 for districts & mechs with shifts
wide_sub<-wide %>%
  select(-c(`2018_TX_CURR_qtr1`,`2018_TX_CURR_qtr2`,`2018_TX_CURR_qtr3`,`2018_TX_CURR_qtr4`,
            `2018_TX_NEW_qtr1`,`2018_TX_NEW_qtr2`,`2018_TX_NEW_qtr3`,`2018_TX_NEW_qtr4`,`2018_TX_NEW_cumulative`,
            prime_partner_duns,pre_rgnlztn_hq_mech_code,award_number)) %>%
  mutate(fundingagency_modified=fundingagency,
         mech_code_modified=mech_code,
         mech_name_modified=mech_name,
         primepartner_modified=primepartner)
  
#base we will bind back to
wide_sub_foundation<-wide_sub %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="gp City of Johannesburg Metropolitan Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="gp Sedibeng District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="lp Capricorn District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="mp Nkangala District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="wc City of Cape Town Metropolitan Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="fs Lejweleputswa District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="nw Dr Kenneth Kaunda District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="kz eThekwini Metropolitan Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="kz uMgungundlovu District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="gp City of Tshwane Metropolitan Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="ec Alfred Nzo District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="ec Buffalo City Metropolitan Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="ec Chris Hani District Munici pality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="ec Oliver Tambo District Municipality") %>% 
  filter(!fiscal_year=="2018" | !indicator=="TX_CURR" | !psnu=="kz Harry Gwala District Municipality")
  


#COJ FY18Q4 CURR
coj<-wide_sub %>%
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="gp City of Johannesburg Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17020" ~ "70310",
    mech_code=="17037" ~ "70310",
    mech_code=="17021" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17020" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    mech_code=="17037" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    mech_code=="17021" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17020" ~ "ANOVA HEALTH INSTITUTE",
    mech_code=="17037" ~ "ANOVA HEALTH INSTITUTE",
    mech_code=="17021" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ primepartner
  ))


#sedibeng
sedibeng<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="gp Sedibeng District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17023" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17023" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17023" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ primepartner
  ))
  
#capricorn
capricorn<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="lp Capricorn District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17036" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ primepartner
  ))

#Nkangala
Nkangala<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="mp Nkangala District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17036" ~ "70287",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17036" ~ "BRHC: Accelerating Program Achievements to Control the epidemic- KZN: King Cetshwayo and Ugu Mpumalanga: Gert Sibande and Nkangala",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17036" ~ "Broadreach Healthcare, LLC",
    TRUE ~ primepartner
  ))

#CT
CT<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="wc City of Cape Town Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17046" ~ "70310",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17046" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17046" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ primepartner
  ))

#Lej
Lej<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="fs Lejweleputswa District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="18481" ~ "70301",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="18481" ~ "WRHI: Accelerating Program Achievements to Control the epidemic- Gauteng (Tshwane) and Free State (Lejwekleputswa)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="18481" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ primepartner
  )) %>% 
  mutate(fundingagency_modified=case_when(
    mech_code=="18481" ~ "USAID",
    TRUE ~ fundingagency
  ))


#dkk
dkk<-wide_sub %>%
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="nw Dr Kenneth Kaunda District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17037" ~ "18484",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17037" ~ "Aurum Comprehensive (CDC GH001981)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17037" ~ "THE AURUM INSTITUTE",
    TRUE ~ primepartner
  )) %>% 
  mutate(fundingagency_modified=case_when(
    mech_code=="17037" ~ "HHS/CDC",
    TRUE ~ fundingagency
  ))

#ethekwini
ethekwini<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="kz eThekwini Metropolitan Municipality") %>% 
  mutate(mech_code_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "18481",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "Health Systems Trust Comprehensive (CDC GH001980)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "TRUST FOR HEALTH SYSTEM PLANNING & DEVELOPMENT IT1098/92",
    TRUE ~ primepartner
  )) %>% 
  mutate(fundingagency_modified=case_when(
    fy19q1_sitetransition=="USAIDtoCDC" ~ "HHS/CDC",
    TRUE ~ fundingagency
  )) %>% 
  mutate(fy19q1_sitetransition=case_when(
    fiscal_year=="2018" & indicator=="TX_CURR" & sitename=="kz Cato Manor CHC" & fundingagency=="HHS/CDC" ~ "CDC Exited in fy19q1",
    TRUE ~ fy19q1_sitetransition
  ))
  
#umgung
umgung<-wide_sub %>% 
  filter(fiscal_year=="2018" & indicator=="TX_CURR" & psnu=="kz uMgungundlovu District Municipality") %>% 
  mutate(mech_code_modified=case_when(
    mech_code=="17046" ~ "70289",
    TRUE ~ mech_code
  )) %>% 
  mutate(mech_name_modified=case_when(
    mech_code=="17046" ~ "MatCH: Accelerating Program Achievements to Control the epidemic Eastern Cape (Alfred Nzo), KZN (Harry Gwala, eThekwini, uMgungundlovu)",
    TRUE ~ mech_name
  )) %>% 
  mutate(primepartner_modified=case_when(
    mech_code=="17046" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    TRUE ~ primepartner
  ))

#tshwane
tshwane<-wide_sub %>%
  filter(fiscal_yea %in% c("2018","2019") & indicator=="TX_CURR" & psnu=="gp City of Tshwane Metropolitan Municipality") %>% 
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

  
#re-bound
df<-bind_rows(wide_sub_foundation,coj,sedibeng,capricorn,Nkangala,CT,Lej,dkk,ethekwini,umgung,tshwane,alfrednzo,buffalo,otambo,chrishani,HG)


df_final<-df %>% 
  mutate(mech_code_modified=case_when(
    fiscal_year=="2019" & indicator=="TX_NEW" & psnu=="wc City of Cape Town Metropolitan Municipality" & mech_code=="70288" ~ "70310",
    TRUE ~ mech_code_modified
  )) %>% 
  mutate(mech_name_modified=case_when(
  fiscal_year=="2019" & indicator=="TX_NEW" & psnu=="wc City of Cape Town Metropolitan Municipality" & mech_code=="70288" ~ "Anova: Accelerating Program Achievements to Control the Epidemic - Gauteng (Joburg and Sedibeng), Limpopo (Capricorn and Mopani)",
  TRUE ~ mech_name_modified
  )) %>% 
  mutate(primepartner_modified=case_when(
    fiscal_year=="2019" & indicator=="TX_NEW" & psnu=="wc City of Cape Town Metropolitan Municipality" & mech_code=="70288" ~ "ANOVA HEALTH INSTITUTE",
    TRUE ~ primepartner_modified
  )) %>% 
  mutate(mech_code_modified=case_when(
    fiscal_year=="2019" & indicator %in% c("TX_NEW","TX_CURR") & psnu=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "18484",
    TRUE ~ mech_code_modified
  )) %>% 
  mutate(mech_name_modified=case_when(
    fiscal_year=="2019" & indicator %in% c("TX_NEW","TX_CURR") & psnu=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "Aurum Comprehensive (CDC GH001981)",
    TRUE ~ mech_name_modified
  )) %>% 
  mutate(primepartner_modified=case_when(
    fiscal_year=="2019" & indicator %in% c("TX_NEW","TX_CURR") & psnu=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "THE AURUM INSTITUTE",
    TRUE ~ primepartner_modified
  )) %>% 
  mutate(fundingagency_modified=case_when(
    fiscal_year=="2019" & indicator=="TX_NEW" & psnu=="gp City of Tshwane Metropolitan Municipality" & mech_code=="70301" ~ "HHS/CDC",
    TRUE ~ fundingagency_modified
  ))


#user friendly partner list
partner<-read_excel("ContextFiles/UserFriendly_PartnerName_DSPcolumn.xlsx",sheet="UserFriendly_PartnerName")

#merge in partner list to MODIFIED mech id
df_final_partner<-merge(df_final,partner,by.x="mech_code_modified",by.y ="MechanismID",all.x=TRUE)

#write file
write_tsv(df_final_partner,"SA_Retention_PartnerShiftAdjusted_20191220_partner_v20200211.txt",na="")
