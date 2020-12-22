library(tidyverse)
library(readxl)
library(ICPIutilities)
library(here)
library(glitr)

memory.limit(size=500000)


#read in MER and reformat#
MER<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-11-17_attributes.txt"))


 
MER_sub<-MER %>% 
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year=="2020",
         fundingagency=="USAID",
         DSP=="Yes") %>% 
  select(orgunituid,psnu,sitename,indicator,qtr1,qtr2,qtr3,cumulative,targets) %>% 
  group_by(orgunituid,psnu,sitename,indicator) %>% 
  summarize_at(vars(qtr1:targets),sum,na.rm=TRUE) %>% 
  ungroup()


dates <- lubridate::as_date("2020-09-04") %>% seq(by = 7, length.out = 5)


MER_sub_mod <- purrr::map_dfr(.x = dates,
                               .f = ~dplyr::mutate(MER_sub, date = .x)) %>% 
  unite(key,sitename,indicator,date,remove=FALSE) %>% 
  select(key,indicator,orgunituid,psnu,date,qtr1,qtr2,qtr3,cumulative,targets)
  


#read in HFR
HFR<-read_excel(here("HFR","1. USAID SA HFR RawData_2020.10.16.xlsx"), sheet="Sheet1", skip=2) %>% 
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
    ),
    orgunituid=case_when(
      is.na(orgunituid.x) ~ orgunituid.y,
      TRUE ~ orgunituid.x
    )) %>% 
  select(-c(date.x,date.y,indicator.x,indicator.y,psnu.x,psnu.y,orgunituid.x,orgunituid.y))


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


PLHIV<-read_excel(here("Consolidated Dash","FY20Q3_PEPFAR Performance Dash_16Sept2020.xlsx"), sheet="Data") %>% 
  filter(indicator=="PLHIV",
         FundingAgency=="USAID",
         DSP=="Yes") %>% 
  select(PSNU,indicator,FY2020_cumulative) %>% 
  group_by(PSNU,indicator) %>% 
  summarize_at(vars(FY2020_cumulative),sum,na.rm=TRUE)


write_tsv(MER_HFR,here("Processed_Files/MER_HFR", "MER_HFR_20201018_v4.txt"), na="")





~~~~~~~~~~~~~~~~~~~~~~~~
viz_trend_mer<-MER_sub_mod %>% 
  filter(date==as.Date("2020-09-25"),
         indicator=="TX_CURR") %>% 
  group_by(psnu,date) %>% 
  summarize_at(vars(qtr1:qtr3),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  gather(period,val,qtr1:qtr3) %>% 
  mutate(date=case_when(
    period=="qtr1" ~ as.Date("2019-12-31"),
    period=="qtr2" ~ as.Date("2020-03-31"),
    period=="qtr3" ~ as.Date("2020-06-30"),
  )) %>% 
  mutate(source="MER") %>% 
  select(psnu,date,val,source)


viz_trend_hfr<-HFR %>% 
  filter(indicator=="TX_CURR") %>% 
  group_by(psnu,date) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE) %>% 
  mutate(source="HFR") %>% 
  select(psnu,date,val,source) 



viz_trend<-(rbind(viz_trend_mer,viz_trend_hfr))


viz_trend %>% 
  ggplot(aes(x=date,
             y=val))+
  geom_line()+
  si_style_xline()+
  facet_wrap(~psnu)

view(curr_trend)
  


ggsave("plot.png")





write_tsv(viz_trend,here("Processed_Files/MER_HFR", "MER_HFR_curr_trend.txt"), na="")















