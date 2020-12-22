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
recovery <-df%>% 
  filter(indicator %in% c("TX_CURR"),
         standardizeddisaggregate %in% c("Total Numerator"),
         fiscal_year %in% c("2020"),
         DSP=="Yes")%>% 
  group_by(fiscal_year,agency_lookback,Partner_lookback,short_name,indicator) %>% 
  summarise_at(vars(targets:cumulative), sum, na.rm = TRUE)%>% 
  ungroup() %>% 
  reshape_msd(clean=TRUE) %>% 
  filter(period_type=="results") %>% 
  spread(period,val) %>% 
  mutate(q3loss=(
    case_when(FY20Q3 < FY20Q2 ~ "YES",
              TRUE ~ "NO")),
    recovery=case_when(
      q3loss=="YES" & FY20Q4 >FY20Q2 ~ "YES",
      q3loss=="NO" ~ "N/A",
      TRUE ~ "NO"),
    label=case_when(
      short_name=="kz eThekwini MM" ~ paste0(short_name," ",agency_lookback),
      TRUE ~ short_name
    ))


recovery_tally<-recovery %>% 
  group_by(q3loss,recovery) %>% 
  tally()


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
  mutate(ach=(FY20-FY19)/(FY19),
         label=case_when(
           short_name=="kz eThekwini MM" ~ paste0(short_name," ",agency_lookback),
           TRUE ~ short_name))

# VIZ --------------------------------------------------------------------------------------
# recovery table
recovery_table<-recovery %>% 
  select(label,Partner_lookback,FY20Q1,FY20Q2,FY20Q3,FY20Q4,q3loss,recovery) %>% 
  arrange(desc(q3loss),desc(recovery),desc(Partner_lookback)) %>% 
  gt() %>% 
  cols_label(label="District",Partner_lookback="Partner",q3loss="Q3 Loss?", recovery="Q4 Recovery?") %>% 
  tab_style(
    style = list(
    cell_text(weight = "bold")),
    locations = cells_column_labels(everything())) %>% 
  opt_table_lines(extent = "default") %>%
  gt::cols_align(
    align = "center",
    columns = vars(`q3loss`, `recovery`)) %>% 
  fmt_number(
    columns = vars(FY20Q1),
    decimals=0) %>%
  fmt_number(
    columns = vars(FY20Q2),
    decimals=0) %>%
  fmt_number(
    columns = vars(FY20Q3),
    decimals=0) %>% 
  fmt_number(
    columns = vars(FY20Q4),
    decimals=0) %>% 
  tab_options(
    column_labels.border.top.color = "white",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "black",
    table_body.hlines.color = "white",
    table.border.bottom.color = "white",
    table.border.bottom.width = px(3)) %>% 
  tab_source_note(md("**Data**: TX_CURR | 2020-11-17 genie | recovery: Q4 is > Q2")) %>% 
  tab_style(
    cell_fill(color=grey20k),
    locations=cells_body(
      rows=label %in% c("ec Alfred Nzo DM", "ec Buffalo City MM", "kz Harry Gwala DM")
    )
  ) %>%
  tab_style(
    style=list(
      cell_borders(
        sides = "all",
        color="black",
        weight=px(2))),
    locations=list(
      cells_body()
    )
  )
  tab_style(
    style = list(
      cell_fill(color = scales::alpha("#c43d4d", 0.7)),
      cell_text(color = "white", weight = "bold")),
    locations = list(
      cells_body(
        columns = 7,
        rows = `q3loss`=="YES"))) %>%
  tab_style(
    style = list(
      cell_fill(color = scales::alpha("#287c6f", 0.7)),
      cell_text(color = "white", weight = "bold")
    ),
    cells_body(
      columns = 8,
      rows = `recovery` =="YES"
    )) %>% 
  tab_style(
    style=cell_text(weight="bold"),
    locations=list(
    cells_body(
      columns=2,
    rows= label %in% c("ec Alfred Nzo DM", "ec Buffalo City MM", "kz Harry Gwala DM")
  )))


recovery_table


gt::gtsave(recovery_table,here("Quarterly Reviews/Self_assessment","FY20Q4_recovery_MatCH_3.png"))


#fy20 growth table
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
    decimals=0) %>% 
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
              rows = fy20growth >= .035)) %>% 
  tab_style(style = cell_fill(color = "#ffffbf"),
            locations = cells_body(
              columns = vars(fy20growth),
              rows = fy20growth <.035)) %>% 
  tab_style(style = cell_fill(color = "#fc8d59"),
            locations = cells_body(
              columns = vars(fy20growth),
              rows = fy20growth <0))


fy_growth_table


gt::gtsave(fy_growth_table,here("Quarterly Reviews/Self_assessment","FY20Q4_recovery_MatCH_3.png"))
