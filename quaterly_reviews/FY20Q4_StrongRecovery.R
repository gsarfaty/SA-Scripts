library(extrafont)
library(tidyverse)
library(ICPIutilities)
library(here)
library(glitr)
library(scales)
library(patchwork)
library(formattable)
library(gt)
library(glamr)

# MER

df<-read_msd(here("Processed_Files/MSD_genie", "msd_fy17to20_2020-11-17_attributes.txt"))


# Data - proxy linkage by metro ---------------------------------------------------------------------
annual_growth_metros <-df%>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year %in% c("2019","2020"),
         DSP=="Yes",
         HIGHBURDEN=="YES")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="cumulative") %>% 
  spread(period,val) %>% 
  mutate(fy20growth=(FY20-FY19)/(FY19),
  label=case_when(
    short_name=="kz eThekwini MM" ~ paste0(short_name," ",agency_lookback),
    TRUE ~ short_name))

fy_growth<-df %>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year %in% c("2019","2020"),
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="cumulative") %>% 
  spread(period,val) %>% 
  mutate(fy20growth=(FY20-FY19)/(FY19),
         label=case_when(
           short_name=="kz eThekwini MM" ~ paste0(short_name," ",agency_lookback),
           TRUE ~ short_name))
  
  
growth<-df %>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year=="2020",
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period %in% c("FY20Q3","FY20Q4")) %>% 
  spread(period,val) %>% 
  mutate(q4_growth=(FY20Q4-FY20Q3)/(FY20Q3),
         label=case_when(
           short_name=="kz eThekwini MM" ~ paste0(short_name," ",agency_lookback),
           TRUE ~ short_name
         ),
         flag=case_when(
           q4_growth >.02 & agency_lookback=="USAID" ~ "USAID",
           q4_growth >.02 & agency_lookback=="HHS/CDC" ~ "CDC",
           TRUE ~ "NO"
         ),
         top10=case_when(
           q4_growth >.019 ~ "YES",
           TRUE ~ "NO")) %>% 
  arrange(desc(q4_growth))

prinf(growth)

write_tsv(growth,here("Quarterly Reviews","FY20Q4_growth.txt"))


NN<-df %>% 
  filter(indicator %in% c("TX_NET_NEW"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year=="2020",
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period %in% c("FY20Q4"))

prinf(NN)

NN_psnu<-df %>% 
  filter(indicator %in% c("TX_NET_NEW"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year=="2020",
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period %in% c("FY20Q4")) %>%
  mutate(label=case_when(
    short_name=="kz eThekwini MM" ~ paste0(short_name," ",agency_lookback),
    TRUE ~ short_name)) %>% 
  arrange(desc(val))

prinf(NN_psnu)


partner_ach<-df %>% 
  filter(indicator %in% c("HTS_TST_POS","TX_NEW","TX_CURR","TX_NET_NEW"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year=="2020",
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type %in% c("cumulative","targets")) %>% 
  spread(period_type,val) %>% 
  mutate(ach=(cumulative/targets))
  


# Viz - NN & New trend -----------------------------------------------------------

#Q4 growth
viz<-growth %>%
  ggplot(aes(y =reorder(label,q4_growth), 
             x = q4_growth,
             fill=agency_lookback))+
  scale_x_continuous(labels = scales::percent_format(accuracy=1))+
  geom_col()+
  scale_fill_manual(values=c(usaid_lightblue,usaid_blue))+
  geom_vline(xintercept = .02, linetype="dotted")+
  annotate(geom = "text", x = 0, y = 28.05, #determine text placement on coordinate plane
           label = "USAID", #what you want your text to say
           hjust="left", size=5, color="white", fontface="bold",family="Source Sans Pro")+
  annotate(geom = "text", x = 0, y = 21.05, #determine text placement on coordinate plane
           label = "CDC", #what you want your text to say
           hjust="left", size=5, color=usaid_black, fontface="bold", family="Source Sans Pro")+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text = element_text(size=14),
        legend.position = "none")
  
print(viz)
  

ggsave(here("Quarterly Reviews","FY20Q4_growth.png"),
       width=9, height=5, dpi=300, units="in")


#annual growth - metros
metro_fy20_growth_viz<-annual_growth_metros %>%
  ggplot(aes(x =reorder(label,-fy20growth), 
             y = fy20growth,
             fill=agency_lookback))+
  scale_y_continuous(labels=percent)+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c(usaid_lightblue,usaid_blue))+
  geom_vline(xintercept = .02, linetype="dotted")+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

print(metro_fy20_growth_viz)


ggsave(here("Quarterly Reviews","FY20_annual_growth_metros.png"))

#Table
growth_table<- growth %>%  
  select(label,FY20Q3,FY20Q4,q4_growth) %>% 
  arrange(-q4_growth) %>% 
  gt()

growth_table<-growth_table %>% 
  cols_label(label="District", q4_growth="Q4 Growth") %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>% 
  opt_table_lines(extent = "default") %>% 
  fmt_number(
    columns = vars(FY20Q3),
    decimals=0) %>% 
  fmt_number(
    columns = vars(FY20Q4),
    decimals=0) %>% 
  fmt_percent(
    columns=vars(q4_growth),
    placement="right",
    decimals=1) %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(style = cell_fill(color = "#91cf60"),
            locations = cells_body(
              columns = vars(q4_growth),
              rows = q4_growth >= .021)) %>% 
  tab_style(style = cell_fill(color = "#ffffbf"),
            locations = cells_body(
              columns = vars(q4_growth),
              rows = q4_growth <.021)) %>% 
  tab_style(style = cell_fill(color = "#fc8d59"),
            locations = cells_body(
              columns = vars(q4_growth),
              rows = q4_growth <0))


growth_table


#annual growth table
fy_growth_table<-fy_growth %>% 
  select(label,FY19,FY20,fy20growth) %>% 
  arrange(-fy20growth) %>% 
  gt() %>% 
  cols_label(label="District", fy20growth="FY20 Growth") %>% 
  tab_style(
    style = list(
      cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>% 
  opt_table_lines(extent = "default") %>% 
  fmt_number(
    columns = vars(FY19),
    decimals=0) %>% 
  fmt_number(
    columns = vars(FY20),
    decimals=0) %>% 
  fmt_percent(
    columns=vars(fy20growth),
    placement="right",
    decimals=1) %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)) %>% 
  tab_source_note(md("**Data**: TX_CURR | 2020-11-17 genie")) %>% 
  tab_style(
    style = cell_borders(
      sides = "right",
      weight = px(1.5),
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )) %>% 
  tab_style(style = cell_fill(color = "#91cf60"),
            locations = cells_body(
              columns = vars(fy20growth),
              rows = fy20growth >= .041)) %>% 
  tab_style(style = cell_fill(color = "#ffffbf"),
            locations = cells_body(
              columns = vars(fy20growth),
              rows = fy20growth <.0411)) %>% 
  tab_style(style = cell_fill(color = "#fc8d59"),
            locations = cells_body(
              columns = vars(fy20growth),
              rows = fy20growth <0))


fy_growth_table

#NN column
NN_viz<-NN %>% 
  ggplot(aes(x=reorder(agency_lookback,-val), 
             y=val,
             fill=agency_lookback))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c(usaid_lightblue,usaid_blue))+
  scale_y_continuous(labels=comma)+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

print(NN_viz)

ggsave(here("Quarterly Reviews","FY20Q4_NN.png"),
       width=3, height=4, dpi=300, units="in")