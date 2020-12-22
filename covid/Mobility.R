## PROJECT: COVID MOBILITY REPORTS
## AUTHOR:  G.Sarfaty, adapted from A. Chafetz| USAID
## PURPOSE: compare mobility drops in SA
## LICENSE: MIT
## DATE:    2020-08-07

library(tidyverse)
library(extrafont)
library(glitr)
library(scales)
library(here)

#data source: https://www.google.com/covid19/mobility/

mobility <- read_csv(here("COVID","Global_Mobility_Report.csv"))

mobility <- mobility %>% 
  gather(ind, value, retail_and_recreation_percent_change_from_baseline:residential_percent_change_from_baseline) %>% 
  mutate(value = value /100)


mobility <- mobility %>% 
  mutate(ind = str_remove(ind, "_percent_change_from_baseline") %>% 
           str_replace("_", " ") %>% 
           str_to_sentence())

mobility %>% 
  filter(country_region == "South Africa",
         is.na(sub_region_1)) %>% 
  ggplot(aes(date, value)) +
  geom_vline(xintercept = as.Date(c("2020-04-01", "2020-07-01")), color = "gray20") +
  geom_area(fill = si_lblue,
            color = si_blue,
            alpha = .7) +
  facet_wrap(~ind) +
  scale_y_continuous(label = scales::percent) +
  labs(x = NULL, y = NULL,
       title = "SOUTH AFRICA MOBILITY CHANGES",
       subtitle = "change from baseline",
       caption = "Source: Google Mobility Report") +
  si_style() 


ggsave(here("COVID","SA Mobility Report.png"))


#subnat
mobility %>% 
  filter(country_region == "South Africa",
         ind=="Workplaces",
         !is.na(sub_region_1))%>% 
  ggplot(aes(date, value)) +
  geom_vline(xintercept = as.Date(c("2020-04-01", "2020-07-01")), color = "gray20") +
  geom_area(fill = si_lblue,
            color = si_blue,
            alpha = .7) +
  facet_wrap(~sub_region_1) +
  scale_y_continuous(label = scales::percent) +
  labs(x = NULL, y = NULL,
       title = "SOUTH AFRICA MOBILITY CHANGES",
       subtitle = "change from baseline",
       caption = "Source: Google Mobility Report") +
  si_style() 