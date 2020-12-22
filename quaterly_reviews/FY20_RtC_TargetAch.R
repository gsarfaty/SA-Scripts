library(extrafont)
library(tidyverse)
library(ICPIutilities)
library(here)
library(glitr)
library(scales)
library(patchwork)
library(formattable)
library(gt)

# MER

df<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-11-17_attributes.txt"))


# Data - proxy linkage by metro ---------------------------------------------------------------------
partner_ach <-df%>% 
  filter(indicator %in% c("HTS_TST_POS","TX_NEW","TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year %in% c("2020"),
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type %in% c("cumulative","targets")) %>% 
  spread(period_type,val) %>% 
  mutate(ach=(cumulative)/(targets),
         flag=case_when(
           Partner_lookback=="Right to care" ~ "YES",
           TRUE ~ "NO"
         ))


partner_vlc <- df %>%
  filter(fiscal_year == "2020",
         indicator %in% c("TX_PVLS","TX_CURR"),
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus",
                                         "Age/Sex/Indication/HIVStatus"),
         DSP=="Yes") %>%
  mutate(indicator = ifelse(numeratordenom == "D",
                            paste0(indicator, "_D"), indicator)) %>%
  group_by(fiscal_year,Partner_lookback,indicator) %>%
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  reshape_msd(clean = TRUE) %>%
  select(-period_type) %>%
  spread(indicator, val)



partner_vlc <- partner_vlc %>%
  group_by(Partner_lookback) %>%
  mutate(
    VLC = TX_PVLS_D / lag(TX_CURR, 2, order_by = period))%>%
  ungroup() %>%
  mutate(VLS = (TX_PVLS/TX_PVLS_D) * VLC) %>%
  mutate(Not_Cov = case_when(VLC >1 ~ 0, TRUE ~ 1-VLC)) %>%
  filter(period == "FY20Q4") %>% 
  mutate(flag=case_when(
    Partner_lookback=="Right to care" ~ "YES",
    TRUE ~ "NO"
  ))


# VIZ -----------------------------------------------------------------------------------------------

ach_viz_pos<-partner_ach %>%
  filter(indicator=="HTS_TST_POS") %>% 
  ggplot(aes(x =reorder(Partner_lookback,-ach), 
             y = ach,
             fill=flag))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits=c(0,1))+
  geom_col(width=.6)+
  # coord_flip()+
  scale_fill_manual(values=c(grey40k,usaid_blue))+
  labs(title="HTS_TST_POS")+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=10, angle = 45,hjust=1,vjust=1),
        legend.position = "none")

ach_viz_new<-partner_ach %>%
  filter(indicator=="TX_NEW") %>% 
  ggplot(aes(x =reorder(Partner_lookback,-ach), 
             y = ach,
             fill=flag))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits=c(0,1))+
  geom_col(width=.6)+
  # coord_flip()+
  scale_fill_manual(values=c(grey40k,usaid_blue))+
  labs(title="TX_NEW")+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=10, angle = 45,hjust=1,vjust=1),
        legend.position = "none")


ach_viz_curr<-partner_ach %>%
  filter(indicator=="TX_CURR") %>% 
  ggplot(aes(x =reorder(Partner_lookback,-ach), 
             y = ach,
             fill=flag))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits=c(0,1))+
  geom_col(width=.6)+
  # coord_flip()+
  scale_fill_manual(values=c(grey40k,usaid_blue))+
  labs(title="TX_CURR")+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=10, angle = 45,hjust=1,vjust=1),
        legend.position = "none")


ach_viz_pos + ach_viz_new + ach_viz_curr



ggsave(here("Quarterly Reviews/Self_assessment","FY20_ach_RtC_plus_VLC.png"),
       width=10, height=4, dpi=300, units="in")


# VIZ VLC

vlc_viz_curr<-partner_vlc %>%
  ggplot(aes(x =reorder(Partner_lookback,-VLC), 
             y = VLC,
             fill=flag))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits=c(0,1))+
  geom_col(width=.6)+
  # coord_flip()+
  scale_fill_manual(values=c(grey40k,usaid_blue))+
  labs(title="Viral Load Coverage")+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=10, angle = 45,hjust=1,vjust=1),
        legend.position = "none")


ggsave(here("Quarterly Reviews/Self_assessment","FY20_partner_VLC.png"),
       width=4, height=4, dpi=300, units="in")
