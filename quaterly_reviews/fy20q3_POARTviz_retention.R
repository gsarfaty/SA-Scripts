library(extrafont)
library(tidyverse)
library(ICPIutilities)
library(here)
library(glitr)
library(scales)
library(patchwork)

# MER

df<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-09-13_attributes.txt"))


# Data - proxy linkage by metro ---------------------------------------------------------------------

retention <-df %>% 
  filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% c("2019", "2020"),
         priority_district=="YES",
         DSP.y=="Yes")%>% 
  group_by(fiscal_year,psnu,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) 


district<-df %>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% c("2020"),
         priority_district=="YES")%>% 
  group_by(fiscal_year,psnu,shortname,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE)


# Viz - NN & New trend -----------------------------------------------------------
New_NN_bar<-retention %>% 
  filter(indicator %in% c("TX_NEW","TX_NET_NEW"),
         period_type=="results") %>% 
  mutate(val2=case_when(
    period=="FY19Q1" & indicator=="TX_NET_NEW" ~ 0,
    TRUE ~ val
  )) %>% 
  ggplot(aes(y=val2, x=period, fill=indicator)) + 
  geom_bar(position=position_dodge(.8),width = 0.75, stat="identity") +
  scale_y_continuous(label = scales::label_comma()) +
  scale_fill_manual(values=c(USAID_blue,USAID_ltblue))+
  labs(x="", 
       y="",
       title="Trends in <span style='color:#002F6C;'>TX_NET_NEW</span>, and 
    <span style='color:#A7C6ED;'>TX_NEW</span>",
       caption = "Data source: Genie 2020-09-11; 27 Priority Districts")+
  si_style_ygrid()+
  theme(legend.position = "none",
        axis.text = ggplot2::element_text(
          family = "Gill Sans MT",
          size = 12),
        plot.title = element_markdown(size=12))

print(New_NN_bar)

ggsave("Quarterly Reviews//FY20Q3_NNandNewTrend.png",
      dpi = 600, width = 6.2, height = 4.3, units = "in")



# Viz - TX_CURR
curr<-retention %>% 
  filter(indicator %in% c("TX_CURR"),
         period_type %in% c("results","targets"),
         !period=="FY19")%>%
  add_row(period="FY20Q4",indicator="TX_CURR",period_type="targets",val=0)%>% 
  mutate(period=case_when(
    period=="FY20" ~ "FY20 Target",
    TRUE ~ period
  )) %>% 
  mutate(period = factor(period,levels = c("FY19Q1", "FY19Q2","FY19Q3","FY19Q4", 
                                             "FY20Q1",
                                             "FY20Q2","FY20Q3",
                                             "FY20Q4", "FY20 Target"))) %>% 
  ggplot(aes(x=(period), y=val,fill=period_type)) +
  geom_bar(stat="identity")+
  geom_text(aes(label=comma(val)), family="Gill Sans MT", vjust=-1, nudge_y = 1, size=3)+
  scale_y_continuous(label = scales::label_comma())+ 
  scale_fill_manual(values=c(USAID_lgrey,USAID_dgrey))+
  annotate("rect", xmin = 7.5, xmax = 8.5, ymin = 3758974, ymax = 4879597,fill = USAID_red)+
  annotate(geom = "text", x = 8, y = 4320000, label = "gap to target: \n 1,120,803",hjust="center",
           size=3, color="white", family="Gill Sans MT")+
  labs(x="", 
       y="",
       title = "Trends in TX_CURR & Gap to Target",
       caption = "Data source: Genie 2020-09-11; 27 Priority Districts")+
  si_style_nolines()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "none",
        axis.text = ggplot2::element_text(
          family = "Gill Sans MT",
          size = 10,),
        plot.title = ggplot2::element_text(
          family = "Gill Sans MT",
          size = 14,))


print(curr)

ggsave("Quarterly Reviews//FY20Q3_TX_CURR_Trend.png",
       dpi = 600, width = 7, height =4.5, units = "in")


# PEPFAR CURR
pepfar_curr<-retention %>% 
  filter(indicator %in% c("TX_CURR"),
         period_type %in% c("results"),
         period %in% c("FY20Q1","FY20Q2","FY20Q3")) %>% 
  ggplot(aes(x=(period), y=val,fill=period_type)) +
  geom_bar(stat="identity", width = 0.35)+
  geom_text(aes(label=comma(val)), family="Gill Sans MT", vjust=-1, nudge_y = 1, size=3)+
  scale_y_continuous(label = scales::label_comma())+ 
  scale_fill_manual(values=c(USAID_lgrey))+
  labs(x="", 
       y="",
       title = "",
       caption = "Data source: Genie 2020-09-11; 27 Priority Districts")+
  si_style_yline()+
  theme(legend.position = "none",
        axis.text = ggplot2::element_text(
          family = "Gill Sans MT",
          size = 10,),
        plot.title = ggplot2::element_text(
          family = "Gill Sans MT",
          size = 14,),
        plot.caption = ggplot2::element_text(
          size=7
        ))


print(pepfar_curr)

ggsave("Quarterly Reviews//FY20Q3_PEPFAR_SA_CURR.png",
       dpi = 600, width = 3.5, height =4.5, units = "in")


# PEPFAR CURR Trend & Achievement | District
district_curr<-district %>% 
  filter(period_type=="results") %>% 
  ggplot(aes(y=val, x=period, fill=indicator)) + 
  geom_bar(position=position_dodge(.8),width = 0.75, stat="identity") +
  scale_y_continuous(label = scales::label_comma()) +
  scale_fill_manual(values=c(USAID_blue))+
  facet_wrap(~shortname,nrow = 1)+
  labs(x="", 
       y="",
       title="",
       caption = "Data source: Genie 2020-09-11; 27 Priority Districts")+
  si_style_nolines()+
  theme(legend.position = "none")


print(district_curr)

ggsave("Quarterly Reviews//District_CURR.png")