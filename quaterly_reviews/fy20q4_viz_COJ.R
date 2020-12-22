library(extrafont)
library(tidyverse)
library(ICPIutilities)
library(here)
library(glitr)
library(scales)
library(patchwork)
library(glamr)

# MER

df<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-11-17_attributes.txt"))


# Data - proxy linkage by metro ---------------------------------------------------------------------

coj<-df %>% 
  filter(indicator %in% c("TX_NET_NEW"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus"),
         fiscal_year=="2020",
         priority_district=="YES",
         fundingagency=="USAID",
         psnu=="gp City of Johannesburg Metropolitan Municipality")%>% 
  group_by(fiscal_year,short_name,facility,facilityuid,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  mutate(NN_cat=case_when(
    val <0 ~ "Negative",
    val >=0 ~ "Positive"
  ))

  


# Viz - NN & New trend -----------------------------------------------------------
viz<-coj %>% 
  filter(period %in% c("FY20Q3","FY20Q4")) %>% 
  mutate(period=factor(period,levels=c("FY20Q4","FY20Q3"))) %>%
  ggplot(aes(y=period,
             x=val,
             fill=NN_cat))+
  geom_jitter(shape=21,color="white",size=3)+
  scale_fill_manual(values=c(grey60k,usaid_blue))+
  geom_vline(xintercept = 0,linetype="dotted")+
  labs(x="",
       y="")+
  si_style_nolines()+
  theme(legend.position = "none")
  

print(viz)

ggsave(here("Quarterly Reviews","coj_NN_site_v1.1.png"))




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