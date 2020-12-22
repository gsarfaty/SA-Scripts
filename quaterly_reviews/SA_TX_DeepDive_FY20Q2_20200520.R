library(tidyverse)
library(ICPIutilities)
library(ggplot2)


memory.limit(size=500000)


df<-read_tsv("Processed_Files\\msd_genie_fy17to20_20200512_attributes.txt")

TX<-df %>% 
  filter(indicator %in% c("TX_NEW", "TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year=="2020")


write_tsv(TX, "Processed_Files\\SA_FY20q1q2_site_20200512.txt", na="")


### CURR ach - AGENCY ### -------------------------------

curr_ach_agency<-df %>% 
  filter(snuprioritization %in% c("1 - Scale-Up: Saturation", "2 - Scale-Up: Aggressive"),
         indicator %in% c("TX_CURR"),
         DSP=="YES",
         fiscal_year=="2020",
         standardizeddisaggregate=="Age/Sex/HIVStatus") %>% 
  group_by(fundingagency,period) %>% 
  summarize_at(vars(targets:cumulative), sum, na.rm=TRUE) %>% 
  spread(period,val)


ggplot(curr_ach, aes(period,val))+
         geom_point(mapping=aes(x=period,y=val))+
  facet_wrap(~fundingagency)

