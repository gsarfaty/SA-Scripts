library(tidyverse)
library(readxl)
library(ICPIutilities)
library(here)

memory.limit(size=500000)


#read in MER and reformat#
MER<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-09-29_attributes.txt"))
 
MER_sub<-MER %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year=="2020",
         fundingagency=="USAID",
         DSP=="Yes") %>% 
  select(psnu,sitename,indicator,qtr1,qtr2,qtr3,cumulative,targets) %>% 
  group_by(psnu,sitename,indicator) %>% 
  summarize_at(vars(qtr1:targets),sum,na.rm=TRUE) %>% 
  ungroup()


dates <- lubridate::as_date("2020-09-04") %>% seq(by = 7, length.out = 5)


MER_sub_mod <- purrr::map_dfr(.x = dates,
                               .f = ~dplyr::mutate(MER_sub, date = .x)) %>% 
  unite(key,sitename,indicator,date,remove=FALSE) %>% 
  select(key,indicator,psnu,date,qtr1,qtr2,qtr3,cumulative,targets)
  

# MER_sub_long<-MER %>% 
#   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
#          standardizeddisaggregate=="Total Numerator",
#          fiscal_year=="2020",
#          fundingagency=="USAID",
#          DSP=="Yes") %>% 
#   reshape_msd(clean=TRUE) %>% 
#   mutate(source="MER",
#          Start_Date=NA,
#          End_Date=NA) %>% 
#   filter(period_type %in% c("results","targets"),
#          period %in% c("FY20", "FY20Q3")) %>% 
#   select(psnu,sitename,Partner,period,Start_Date,End_Date,indicator,standardizeddisaggregate,val,source)


#read in HFR Q2
HFR<-read_excel(here("HFR","1. USAID SA HFR Dashboard 20201014_GS.xlsx"), sheet="Pivots", skip=52) %>% 
  rename(val=`Sum of Value`,
         sitename=orgunit,
         standardizeddisaggregate=otherdisaggregate)%>% 
  mutate(indicator=case_when(
           indicator=="TX_CURR_28" ~ "TX_CURR",
           TRUE ~ indicator
         ),
         date=case_when(
           Start_Date==as.Date("2020-08-31") ~ as.Date("2020-09-04"),
           Start_Date==as.Date("2020-09-11") ~ as.Date("2020-09-11"),
           Start_Date==as.Date("2020-09-12") ~ as.Date("2020-09-11"),
           Start_Date==as.Date("2020-09-18") ~ as.Date("2020-09-18"),
           Start_Date==as.Date("2020-09-19") ~ as.Date("2020-09-18"),
           Start_Date==as.Date("2020-09-25") ~ as.Date("2020-09-25"),
           Start_Date==as.Date("2020-09-26") ~ as.Date("2020-09-25"),
           Start_Date==as.Date("2020-10-02") ~ as.Date("2020-10-02"),
           Start_Date==as.Date("2020-10-03") ~ as.Date("2020-10-02")
         )) %>% 
  unite(key,sitename,indicator,date,remove=FALSE)




MER_HFR<-merge(HFR,MER_sub_mod,by="key",all=TRUE) %>% 
  mutate(date=case_when(
    is.na(date.x) ~ as.Date(date.y),
    TRUE ~ as.Date(date.x)),
    indicator=case_when(
      is.na(indicator.x) ~ indicator.y,
      TRUE ~ indicator.x
    ),
    psnu=case_when(
      is.na(psnu.x) ~ psnu.y,
      TRUE ~ psnu.x
    )) %>% 
  select(-c(date.x,date.y,indicator.x,indicator.y,psnu.x,psnu.y))


MER_HFR <- MER_HFR %>% 
  mutate(partner=case_when(
    psnu=="ec Alfred Nzo District Municipality" ~ "Maternal, Adolscent and Child Health (MatCH)",
    psnu=="ec Buffalo City Metropolitan Municipality" ~ "Maternal, Adolscent and Child Health (MatCH)",
    psnu=="fs Lejweleputswa District Municipality" ~ "WITS HEALTH CONSORTIUM (PTY) LTD",
    psnu=="fs Thabo Mofutsanyane District Municipality" ~ "RIGHT TO CARE",
    psnu=="gp City of Johannesburg Metropolitan Municipality" ~ "Anova Health Institute",
    psnu=="gp Sedibeng District Municipality" ~ "Anova Health Institute",
    psnu=="kz eThekwini Metropolitan Municipality" ~ " Maternal, Adolscent and Child Health (MatCH)",
    psnu=="kz Harry Gwala District Municipality" ~ "Maternal, Adolscent and Child Health (MatCH)",
    psnu=="kz King Cetshwayo District Municipality" ~ "BROADREACH HEALTHCARE (PTY) LTD",
    psnu=="kz Ugu District Municipality"~ "BROADREACH HEALTHCARE (PTY) LTD",
    psnu=="lp Capricorn District Municipality" ~ "Anova Health Institute",
    psnu=="lp Mopani District Municipality" ~ "Anova Health Institute",
    psnu=="mp Ehlanzeni District Municipality" ~ "RIGHT TO CARE",
    psnu=="mp Gert Sibande District Municipality" ~ "BROADREACH HEALTHCARE (PTY) LTD",
    psnu=="mp Nkangala District Municipality" ~ "BROADREACH HEALTHCARE (PTY) LTD",
    psnu=="wc City of Cape Town Metropolitan Municipality" ~ "Anova Health Institute",
    TRUE ~ partner
  ))


write_tsv(MER_HFR,here("Processed_Files/MER_HFR", "MER_HFR_20201018_v3.txt"), na="")
























