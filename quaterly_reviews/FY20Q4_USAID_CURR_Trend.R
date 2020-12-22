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
CURR_trend <-df%>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year %in% c("2020"),
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="results")



## Trend Viz
curr_trend_viz<-CURR_trend %>%
  ggplot(aes(y =val, 
             x = period,
             fill=indicator))+
  geom_col(width = .6)+
  scale_fill_manual(values=c(grey40k))+
  scale_y_continuous(labels=label_comma())+
  si_style_yline()+
  labs(caption="TX_CURR | PEPFAR DSPs")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=14),
        legend.position = "none")

print(curr_trend_viz)

ggsave(here("Quarterly Reviews/Self_assessment","FY20_ALL_CURR.png"),
       width=4, height=4, dpi=300, units="in")