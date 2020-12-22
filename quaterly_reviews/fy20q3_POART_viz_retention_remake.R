library(extrafont)
library(tidyverse)
library(ICPIutilities)
library(here)
library(glitr)
library(scales)
library(patchwork)
library(readxl)



df<-read_excel(here("Quarterly Reviews", "PEPFAR_SA_dashnumbers.xlsx"))

pepfar_curr<-df %>% 
  ggplot(aes(x=(period), y=val)) +
  geom_bar(stat="identity", width = 0.35)+
  geom_text(aes(label=comma(val)), family="Gill Sans MT", vjust=-1, nudge_y = 1, size=3)+
  scale_y_continuous(label = scales::label_comma())+ 
  scale_fill_manual(values=c(USAID_lgrey))+
  labs(x="", 
       y="",
       title = "",
       caption = "Data source: Genie 2020-09-11; 27 Priority Districts; DSP Only")+
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



DSPfilter<-df %>% 
  