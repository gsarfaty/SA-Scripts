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

Guateng<-df %>% 
  filter(indicator %in% c("TX_NET_NEW","TX_CURR","HTS_TST_POS","HTS_TST",
                          "TX_PVLS"),
         standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"),
         fiscal_year=="2020",
         DSP=="Yes",
         snu1=="gp Gauteng Province")%>% 
  mutate(indicator=paste0(indicator,"_",numeratordenom)) %>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,psnu,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>%
  reshape_msd(clean=TRUE)

Guateng_Index<-df %>% 
  filter(indicator %in% c("HTS_TST_POS","HTS_TST"),
         standardizeddisaggregate %in% c("Modality/Age/Sex/Result"),
         fiscal_year=="2020",
         DSP=="Yes",
         snu1=="gp Gauteng Province")%>% 
  mutate(indicator=paste0(indicator,"_",numeratordenom)) %>% 
  mutate(modality=case_when(
           modality=="Index" ~ "Index",
           modality=="IndexMod" ~ "Index",
           TRUE ~ "Other"
         )) %>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator,modality) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="results") %>% 
  unite(indmod,indicator,modality) %>% 
  spread(indmod,val) %>% 
  mutate(pos_total=HTS_TST_POS_N_Index+HTS_TST_POS_N_Other,
         pos_index=HTS_TST_POS_N_Index/pos_total,
         short_name=str_remove_all(short_name,"gp "),
         short_name=str_remove_all(short_name, "MM"),
         short_name=str_remove_all(short_name, "DM"))

Gauteng_Linkage<-df %>% 
  filter(indicator %in% c("HTS_TST_POS","TX_NEW"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year=="2020",
         DSP=="Yes",
         snu1=="gp Gauteng Province")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="cumulative") %>% 
  spread(indicator,val) %>% 
  mutate(linkage=TX_NEW/HTS_TST_POS,
         short_name=str_remove_all(short_name,"gp "),
         short_name=str_remove_all(short_name, "MM"),
         short_name=str_remove_all(short_name, "DM"))


Guateng_Growth<-df %>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year %in% c("2019", "2020"),
         DSP=="Yes",
         snu1=="gp Gauteng Province")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="cumulative") %>% 
  spread(period,val) %>% 
  mutate(fy20growth=round((FY20-FY19)/(FY20)*100,0),
         short_name=str_remove_all(short_name,"gp "),
         short_name=str_remove_all(short_name, "MM"),
         short_name=str_remove_all(short_name, "DM"),
         fy20growth=ifelse(short_name=="City of Johannesburg ",3,fy20growth))


# Viz - NN & New trend -----------------------------------------------------------

#index viz
viz<-Guateng_Index %>% 
  filter(period=="FY20Q4") %>% 
  ggplot(aes(y = pos_index, 
             x = reorder(short_name,-pos_index),
             fill=agency_lookback))+
  geom_bar(stat="identity", width=0.3)+
  scale_y_continuous(labels=percent,
                     limits=c(0,.18))+
  scale_fill_manual(values=c(usaid_lightblue,usaid_blue))+
  geom_text(aes(label=scales::percent(pos_index, accuracy=1)),
            hjust="center", vjust=-.5, size=5, color=grey80k, 
            fontface="bold",family="Source Sans Pro")+
  labs(caption="% of pos from inxex | FY20Q4")+
  si_style_void()+
  theme(axis.title.y = element_blank(),
          axis.title.x = element_blank(),
        axis.text.y = element_blank(),
          axis.text.x = element_text(size=12),
        legend.position = "none")
  
print(viz)

ggsave(here("Quarterly Reviews/Self_assessment","FY20Q4_GP_Index.png"),
       width=6, height=5, dpi=300, units="in")


# linkage viz
linkage_viz<-Guateng_Linkage %>% 
  ggplot(aes(x =reorder(short_name,-linkage), 
             y = linkage,
             fill=agency_lookback))+
  geom_bar(stat="identity", width=.3)+
  scale_y_continuous(labels=scales::percent_format(accuracy=1),
                     limit=c(0,1))+
  scale_fill_manual(values=c(usaid_lightblue,usaid_blue))+
  # geom_text(aes(x=short_name,label=agency_lookback,
  #           color=ifelse(linkage >.86, grey10k, usaid_black)),
  #           size = 6, family = "Source Sans Pro")+
  geom_text(aes(label=scales::percent(linkage, accuracy=1)),
            hjust="center", vjust=-.5, size=5, color=grey80k, fontface="bold",family="Source Sans Pro")+
  si_style_void()+
  labs(caption="Proxy Linkage | APR20")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=12),
        legend.position = "none")

print(linkage_viz)

ggsave(here("Quarterly Reviews/Self_assessment","FY20_GP_Linkage.png"),
       width=7,height=5, dpi=300, units="in")

#Growth Viz
growth_viz<-Guateng_Growth %>% 
  ggplot(aes(x =reorder(short_name,-fy20growth), 
             y = fy20growth,
             fill=agency_lookback))+
  geom_bar(stat="identity", width=.3)+
  scale_y_continuous(limit=c(0,4))+
  scale_fill_manual(values=c(usaid_lightblue,usaid_blue))+
  # geom_text(aes(x=short_name,label=agency_lookback,
  #           color=ifelse(linkage >.86, grey10k, usaid_black)),
  #           size = 6, family = "Source Sans Pro")+
  geom_text(aes(label=paste0(fy20growth,"%")),
            hjust="center", vjust=-.5, size=5, color=grey80k, fontface="bold",family="Source Sans Pro")+
  si_style_void()+
  labs(caption="Annual TX_CURR Growth | APR20")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size=12),
        legend.position="none")

print(growth_viz)

ggsave(here("Quarterly Reviews/Self_assessment","FY20_GP_Growth.png"),
       width=6, height=5, dpi=300, units="in")

