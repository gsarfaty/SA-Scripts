library(tidyverse)
library(extrafont)
library(glitr)
library(gisr)
library(scales)
library(here)
library(readxl)
library(sf)
library(ICPIutilities)
library(patchwork)
library(RColorBrewer)


# DATA ----------------------------------------------------------------------------------------
covid_cases<-read_csv(here("COVID", "Provincial_COVID_Data_Nov20.csv")) %>% 
  mutate(cumulative_case_per100k=(cumulative_cases/population)*100000,
         cases_7day_per100k=(cases_seven_day_average/population)*100000) %>% 
  mutate(province=case_when(
    province=="KwaZulu Natal" ~ "KwaZulu-Natal",
    TRUE ~ province
  ))

dir_geo <- "C:/Users/gsarfaty/Documents/GIS/DATA/Boundaries"
dir_terr <- "C:/Users/gsarfaty/Documents/GIS/DATA/Topography"


gis_5_sfc <- list.files(dir_geo, pattern = ".*_5_.*.shp$", recursive = T, full.names = T) %>%
  set_names(basename(.) %>% str_remove("_.*.shp$")) %>%
  map(read_sf)

zaf1 <- get_adm_boundaries("ZAF", adm_level = 1, geo_path = dir_geo) %>%
  st_as_sf() %>%
  select(country = name_0, province = name_1)


MER<-read_msd(here("Processed_Files/MSD_genie","msd_fy17to20_2020-09-09_attributes.txt"))


Q3loss<-MER %>% 
  filter(indicator=="TX_CURR",
         fiscal_year=="2020",
         standardizeddisaggregate=="Total Numerator",
         snuprioritization %in% c("2 - Scale-Up: Aggressive","1 - Scale-Up: Saturation")) %>% 
  group_by(psnu,psnuuid) %>% 
  summarize_at(vars(targets:cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  mutate(ltfu=case_when(
    qtr3 >0 & qtr2 >0 ~ (qtr3-qtr2)/qtr2))


# Timeseries - Small multiples 7 day avg new cases per 100k  -------------------------------------------------------
cases_per100k<-covid_cases %>% 
  mutate(province = factor(province,levels = c("Western Cape", "Eastern Cape","Gauteng","Free State", 
                                               "KwaZulu Natal",
                                               "Northern Cape","North West",
                                               "Mpumalanga", "Limpopo"))) %>% 
  filter(!province=="NA") %>% 
  ggplot(aes(date, cases_7day_per100k)) +
  geom_vline(xintercept = as.Date(c("2020-04-01", "2020-07-01")), color = "gray20") +
  geom_area(fill = si_lblue,
            color = si_blue,
            alpha = .7) +
  facet_wrap(~ province, ncol = 3) +
  scale_y_continuous(label = scales::label_comma()) +
  labs(x = NULL, y = NULL,
       title = "SOUTH AFRICA COVID-19",
       subtitle = "7 day avg of new cases per 100k",
       caption = "Source:https://mediahack.co.za/datastories/coronavirus/provinces/") +
  si_style_nolines() 


print(cases_per100k)

ggsave("SA_COVID19_CasesPer100K_byProvince_nov20.png")

# Timeseries - Small multiples cumulative cases per 100k -------------------------------------------------------

cumulative_cases_100k<-covid_cases %>% 
  mutate(province = factor(province,levels = c("Western Cape", "Eastern Cape","Gauteng","Free State", 
                                               "KwaZulu Natal",
                                               "Northern Cape","North West",
                                               "Mpumalanga", "Limpopo"))) %>% 
  filter(!province=="NA") %>% 
  ggplot(aes(date, cumulative_case_per100k)) +
  geom_vline(xintercept = as.Date(c("2020-04-01", "2020-07-01")), color = "gray20") +
  geom_area(fill = si_lblue,
            color = si_blue,
            alpha = .7) +
  facet_wrap(~ province, ncol = 3) +
  scale_y_continuous(label = scales::label_comma()) +
  labs(x = NULL, y = NULL,
       title = "SOUTH AFRICA COVID-19",
       subtitle = "Cumulative Cases Per 100k",
       caption = "Source:https://mediahack.co.za/datastories/coronavirus/provinces/") +
  si_style_nolines() 


print(cumulative_cases_100k)

ggsave("SA_COVID19_CumulativeCasesPer100k_byProvince.png")


# MAP CASES & LOSSES -------------------------------------------------------------------------------------
zaf_covid_geo<-st_as_sf(zaf1) %>%
  left_join(covid_cases, by = c("province" = "province"))

zaf_mer_geo<-st_as_sf(gis_5_sfc$SouthAfrica) %>%
  left_join(Q3loss, by = c("uid" = "psnuuid"))


map_cumulative_cases<-terrain_map(countries = "South Africa", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zaf_covid_geo %>% filter(date==as.Date("2020-06-30")), aes(fill = cumulative_case_per100k), lwd = .2, color = grey10k) +
  geom_sf(data = zaf1, fill = NA, lwd = .2, color = grey30k) +
  scale_fill_viridis_c(option="magma", alpha=0.9, direction = -1)+
  si_style_map()+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle(label = "",
          subtitle = "Cumulative Cases Per 100k - June 30th")


map_losses<-terrain_map(countries = "South Africa", terr_path = dir_terr, mask = TRUE) +
  geom_sf(data = zaf_mer_geo %>% filter(!is.na(ltfu)), aes(fill = ltfu), lwd = .2, color = grey10k) +
  geom_sf(data = zaf1, fill = NA, lwd = .2, color = grey30k) +
  # geom_sf_text(data = zaf_mer_geo %>% filter(!is.na(ltfu)), aes(label = psnu, color = grey10k, size = 3))+
  scale_fill_gradient2(
    low="#7b3294",
    mid="#f7f7f7",
    high="#008837",
    labels=percent)+
  si_style_map()+
  theme(
    legend.position =  "bottom",
    legend.key.width = ggplot2::unit(1, "cm"),
    legend.key.height = ggplot2::unit(.5, "cm"))+
  ggtitle(label = "",
          subtitle = "% growth/loss in TX_CURR")




(map_cumulative_cases + map_losses) +
plot_layout(widths = c(1,1))+
  plot_annotation(
    title="South Africa | FY20Q3",
    caption = "Source: FY20Q3 genie")



ggsave("SA_COVID19vsLoss_Map.png")