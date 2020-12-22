# MER_HFR Concordance

library(tidyverse)
library(extrafont)
library(glitr)
library(patchwork)

scatter_CURR<-MER_HFR %>% 
  filter(date==as.Date("2020-10-02"),
         indicator=="TX_CURR",
         val <15000) %>%
  mutate(ratio=cumulative/val,
         diff=cumulative-val,
         partner=case_when(
           mech_code=="70289" ~ "MatCH",
           TRUE ~ partner
         )) %>% 
  ggplot(aes(
    x=val,
    y=cumulative))+
      geom_point(aes(fill=mech_code),
                 color="white",shape=21,size=4,alpha=0.8,
                 show.legend = F)+
      geom_abline(intercept=0,slope=1,linetype="dashed")+
      labs(x = "HFR date 2020-10-02", y="MER Q4")+
      scale_x_continuous(limits=c(0,12000), label = scales::label_comma())+
      scale_y_continuous(limits=c(0,12000), label = scales::label_comma())+
      ggtitle("Concordance of HFR & MER")+
      labs(caption = "Sites w volume >15,000 were removed")+
      theme(plot.title = element_text(size = 14, family = "Source Sans Pro", face=1))+
      si_style_nolines()+
  facet_wrap(~partner, nrow=2)


print(scatter_CURR)



ggsave(here("HFR", "MER_HFR_Concordance_Fy20q4.png"),
       scale = 1.2, dpi = 310, width = 10, height = 7, units = "in")
