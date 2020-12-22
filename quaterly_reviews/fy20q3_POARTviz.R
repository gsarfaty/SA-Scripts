library(extrafont)
library(tidyverse)
library(ICPIutilities)
library(here)
library(glitr)
library(scales)
library(patchwork)

# MER

df<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-09-11_attributes.txt"))


# proxy linkage by metro ---------------------------------------------------------------------

linkage_metro <-df %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% c("2019", "2020"),
         HIGHBURDEN=="YES") %>% 
  group_by(fiscal_year,psnu,shortname,indicator) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="results") %>% 
  spread(indicator,val) %>% 
  mutate(linkage=TX_NEW/HTS_TST_POS) %>% 
  group_by(psnu) %>% 
  mutate(fill_col=case_when(
         linkage > lag(linkage, 1, order_by = period) ~ "increase",
         linkage < lag(linkage, 1, order_by = period) ~ "decrease",
         TRUE ~ "same"
         ),
         fill_col=case_when(
           period=="FY20Q3" ~ fill_col,
           TRUE ~ "other"
         )) %>% 
  ungroup()

linkage_metro_viz<-linkage_metro %>% 
  ggplot(aes(
    x=period,
    y=linkage,
    fill=fill_col))+
  geom_col()+
  scale_fill_manual(values=c(USAID_dkred,USAID_blue,grey20k))+
  scale_y_continuous(labels=percent)+
  geom_text(aes(
    label = sprintf("%1.0f%%", linkage*100)),
    position = position_dodge(0.9),
    vjust = 0,
  )+
  facet_wrap(~shortname, nrow=1)+
  annotate(
    geom = "curve", x = 12, y = 1, xend = 7, yend = .85,
    curvature = .3, arrow = arrow(length = unit(4, "mm")),
    color=grey50k
  ) +
  annotate(geom = "text", x = 13, y = 1, label = "decrease",hjust="left",
           size=4, color=grey50k, family="Gill Sans MT")+
  labs(x="", y="")+
  si_style_nolines()

print(linkage_metro_viz)

ggsave("Quarterly Reviews//FY20Q3_Linkage_Metros.png",
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")

  