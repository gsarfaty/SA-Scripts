library(extrafont)
library(tidyverse)
library(ICPIutilities)
library(here)
library(glitr)
library(scales)
library(patchwork)

# MER

df<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-11-17_attributes.txt"))


# Data - proxy linkage by metro ---------------------------------------------------------------------

df_testing<-df %>% 
  filter(indicator %in% c("TX_NEW","HTS_TST","HTS_TST_POS","TX_NET_NEW"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Modality/Age/Sex/Result"),
         fiscal_year=="2020",
         priority_district=="YES",
         DSP_lookback=="YES")%>% 
  group_by(fiscal_year,short_name,facility,facilityuid,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  mutate(period_group=case_when(
    period %in% c("FY20Q1","FY20Q2") ~ "Q1_Q2",
    period %in% c("FY20Q3","Fy20Q4") ~ "Q3_Q4"
  ))


OU_test<-df_testing %>% 
  group_by(period_group,short_name,indicator) %>% 
  summarise_at(vars(val),sum,na.rm=TRUE)

OU_test_mod<-OU_test %>% 
  filter(!is.na(period_group)) %>% 
  spread(period_group,val) %>% 
  mutate(prct_change=((Q3_Q4-Q1_Q2)/Q1_Q2)*100)

NNtargets<-df %>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus"),
         fiscal_year %in% c("2019","2020"),
         priority_district=="YES",
         DSP_lookback=="Yes")%>% 
  group_by(fiscal_year,short_name,facility,facilityuid,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  select(fiscal_year,short_name,facility,facilityuid,indicator,targets,cumulative) %>% 
  unite(key,fiscal_year,indicator) %>% 
  gather(indicator,val,targets:cumulative) %>% 
  unite(key2,key,indicator) %>% 
  spread(key2,val) %>% 
  mutate(fy20_NN_target=`2020_TX_CURR_targets`-`2019_TX_CURR_cumulative`) %>% 
  select(short_name,facility,facilityuid,fy20_NN_target) %>%
  mutate(fiscal_year=as.integer(2020),
         indicator="TX_NET_NEW") %>% 
  rename(targets=fy20_NN_target)


OU_table_AF<-df %>% 
  filter(indicator %in% c("TX_NEW","HTS_TST","HTS_TST_POS","TX_NET_NEW","TX_CURR"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus","Modality/Age/Sex/Result"),
         fiscal_year=="2020",
         priority_district=="YES",
         DSP_lookback=="Yes") 
  
OU_table_AF_nnT<-OU_table_AF %>% 
  bind_rows(NNtargets) %>% 
  group_by(fiscal_year,short_name,facility,facilityuid,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  mutate(achievement=case_when(
    targets>0 ~ (cumulative/targets)*100),
    HTS_ach=case_when(
      indicator=="HTS_TST" & targets >0 & cumulative <=0 ~ "targets, no HTS_TST results",
      indicator=="HTS_TST" & achievement <50 ~ "Less than 50%",
      indicator=="HTS_TST" & achievement >= 50 & achievement <70 ~ "50-70%",
      indicator=="HTS_TST" & achievement >= 70 & achievement <90 ~ "70-90%",
      indicator=="HTS_TST" & achievement >= 90 & achievement ~ "90-100+%",
      indicator=="HTS_TST" & is.na(achievement) ~ "no HTS_TST targets"
    )) %>% 
  arrange(facility,indicator) %>% 
  fill(HTS_ach,.direction="down")
  
write_tsv(OU_table_AF_nnT,here("Quarterly Reviews","FY20_FacilityAnalysis.txt"))

# Viz - NN & New trend -----------------------------------------------------------
viz<-OU_test_mod %>% 
  mutate(indicator=factor(indicator,levels=c("HTS_TST","HTS_TST_POS",
                                             "TX_NEW","TX_NET_NEW"))) 
  ggplot(aes(y =prct_change , 
             x = short_name))+
  geom_col() +
  coord_flip()+
  facet_wrap(~indicator)+
  si_style_nolines()+
  labs(y="% change from Q1+Q2 to Q3+Q4")+
  theme(axis.text.y = element_text(size=14))


print(viz)


viz2<-OU_test_mod %>% 
  mutate(indicator=factor(indicator,levels=c("TX_NET_NEW","TX_NEW","HTS_TST_POS",
                                             "HTS_TST"))) %>% 
  ggplot(aes(y =prct_change , 
             x = indicator))+
  geom_col() +
  coord_flip()+
  facet_wrap(~short_name)+
  si_style_xgrid()+
  labs(title="% change in results from Q1+Q2 to Q3+Q4")+
  theme(axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size=8),
        axis.text.y=element_text(size=7),
        strip.text.x = element_text(size=7)
      )

print(viz2)

ggsave(here("Quarterly Reviews","FY20Testing.png"))


q3<-coj %>% 
  filter(period=="FY20Q3") %>% 
  ggplot(aes(y = val, 
             x = reorder(facility,val),
             fill=NN_cat), color="white") +
  geom_col(show.legend = F) +
  scale_y_continuous(limits=c(-2000,800))+
  scale_fill_manual(values=c(USAID_mgrey,USAID_blue))+
  ggtitle(label="FY20Q3")+
  si_style_void()

q4<-coj %>% 
  filter(period=="FY20Q4") %>% 
  ggplot(aes(y = val, 
             x = reorder(facility,val),
             fill=NN_cat)) +
  geom_col(show.legend = F) +
  scale_y_continuous(limits=c(-2000,800))+
  scale_fill_manual(values=c(USAID_mgrey,USAID_blue))+
  theme(legend.position = none)+
  ggtitle(label="FY20Q4")+
  si_style_void()
  

(q3 + q4)



ggsave(here("Quarterly Reviews","coj_NN_site_v2.png"))