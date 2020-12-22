
library(glamr)

#cumulative covid cases per 100 k by end of q3
covid<-covid_cases %>% 
  filter(date==as.Date("2020-06-30"))


#district curr
district<-df %>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year %in% c("2020"),
         priority_district=="YES",
         DSP=="YES")%>% 
  group_by(fiscal_year,snu1,psnu,shortname,DSPID,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>%
  mutate(shortname=case_when(
    DSPID=="81902_kz eThekwini Metropolitan Municipality" ~ "kz eThekwini - USAID",
    DSPID=="18481_kz eThekwini Metropolitan Municipality" ~ "kz eThekwini - CDC",
    TRUE ~ shortname
  )) %>% 
  group_by(snu1,shortname) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE) %>% 
  ungroup() %>% 
  mutate(ach=round(cumulative/targets*100,0),
         change_q3=qtr3-qtr2,
         prct_change=round(change_q3/qtr2*100,1),
         province=case_when(
            str_detect(snu1,"Eastern") ~ "Eastern Cape",
            str_detect(snu1,"Free State") ~ "Free State",
            str_detect(snu1,"Northern") ~ "Northern Cape",
            str_detect(snu1, "Limpopo") ~ "Limpopo",
            str_detect(snu1, "North West") ~ "North West",
            str_detect(snu1, "Mpumalanga") ~ "Mpumalanga",
            str_detect(snu1, "KwaZulu-Natal") ~ "KwaZulu-Natal",
            str_detect(snu1, "Western") ~ "Western Cape",
            str_detect(snu1, "Gauteng") ~ "Gauteng"
         )) %>% 
  left_join(covid,by="province") %>% 
  filter(!shortname=="kz eThekwini")


# VIZ
district_plot<-district %>% 
  mutate(province = factor(province,levels = c("Western Cape", "Eastern Cape","Gauteng","Free State", 
                                               "KwaZulu-Natal",
                                               "Northern Cape","North West",
                                               "Mpumalanga", "Limpopo"))) %>% 
  ggplot(aes(x=psnu,y=prct_change, fill=province)) +
  geom_bar(stat="identity")+
  scale_y_continuous()+
  scale_fill_viridis_d(option="magma")+
  si_style()


print(district_plot)


#write
write_tsv(district, "Quarterly Reviews//district.txt", na="")