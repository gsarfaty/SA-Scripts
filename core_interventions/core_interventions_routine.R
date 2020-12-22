# Core Interventions Processing
# G.Sarfaty
# USAID


library(here)
library(tidyverse)
library(readxl)
library(ICPIutilities)

memory.limit(size=500000)


# Function - get most recent file -----------------------------------
latest_file = function(fpattern, fpath) {
  f = list.files(pattern=fpattern, path=fpath, full.names=TRUE)
  f = file.info(f)
  rownames(f)[which.max(f$mtime)] 
}


# Globals  ------------------------------------
ci_folder<-here("Core_Interventions")
siyenza_folder<-here("Siyenza")
mer<-here("Processed_Files/MSD_Genie")
hfr<-here("HFR")
context<-here("ContextFiles")

Ci_files<-list.files(ci_folder,pattern=".xlsx")
syz_files<-list.files(ci_folder,pattern="Phuthuma")
mer_file<-latest_file(".txt",mer)
hfr_file<-latest_file(".xlsx",hfr)
syz_context<-list.files(context,pattern="FY 21")

# Read in CI Data ------------------------------------------------------------------------
ci_df<-here("Core_Interventions", Ci_files) %>% 
  map(~ read_excel(.x, sheet = which(str_detect(excel_sheets(.x), "Full_Data"))))%>%
  reduce(bind_rows)

ci_YN<-ci_df %>% 
  filter(!ValueYesNo=="(blank)") %>% 
  mutate(val=case_when(
    `Yes OrgUnit DistinctCount` > 0 ~ "1",
    `No OrgUnit DistinctCount`>0 ~ "0",
    is.na(`Yes OrgUnit DistinctCount`) & is.na(`No OrgUnit DistinctCount`) ~ ""
  )) %>% 
  select(-c(`Yes OrgUnit DistinctCount`,`No OrgUnit DistinctCount`,
            `Total OrgUnit DistinctCount`,`Sum of ValueNumeric`,ValueYesNo)) %>% 
  mutate(val=as.numeric(val))

ci_val<-ci_df %>% 
  filter(ValueYesNo=="(blank)") %>% 
  select(-c(`Yes OrgUnit DistinctCount`,`No OrgUnit DistinctCount`,`Total OrgUnit DistinctCount`)) %>% 
  rename(val=`Sum of ValueNumeric`) %>% 
  select(-ValueYesNo) 
  

ci_bound<-bind_rows(ci_YN,ci_val)

rm(ci_df,ci_YN,ci_val)

# Read in MER Data -----------------------------------------------------------------------
mer_df<-read_msd(mer_file)%>%
  filter(indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR"),
         standardizeddisaggregate=="Total Numerator",
         fiscal_year=="2020",
         fundingagency=="USAID",
         DSP=="Yes") %>% 
  select(orgunituid,psnu,mech_code,sitename,indicator,Siyenza_StartDate,qtr1,qtr2,qtr3,cumulative,targets) %>% 
  group_by(orgunituid,mech_code,psnu,sitename,indicator,Siyenza_StartDate) %>% 
  summarize_at(vars(cumulative),sum,na.rm=TRUE) %>% 
  ungroup() %>% 
  rename(orgunit=sitename) %>%
  mutate(indicator=paste0(indicator,"_",)) %>% 
  spread(indicator,cumulative)

# Read in HFR ----------------------------------------------------------------------------
hfr_df<-read_excel(hfr_file, sheet="Sheet1", skip=18) %>% 
  mutate(indicator=case_when(
    indicator=="TX_CURR_28" ~ "TX_CURR",
    TRUE ~ indicator
  )) %>% 
  rename(val=`Sum of Value`)

hfr_snapshot<-hfr_df %>% 
  filter(indicator %in% c("HTS_TST", "TX_CURR"),
         End_Date==as.Date("2020-10-30") | End_Date==as.Date("2020-10-31")) %>%  #test was only reported monthly
  select(-Start_Date, -End_Date)

hfr_cumulative<-hfr_df %>% 
  filter(indicator %in% c("HTS_TST_POS", "TX_NEW")) %>% 
  group_by(psnu,orgunit,mech_code,partner,indicator) %>% 
  summarize_at(vars(val),sum,na.rm=TRUE)

hfr_df_bound<-bind_rows(hfr_snapshot,hfr_cumulative)

rm(hfr_df,hfr_cumulative,hfr_snapshot)

# Siyenza Sites Flag
siyenza_Sites<-read_excel(here("ContextFiles",syz_context)) %>% 
  mutate(Siyenza="YES") %>% 
  rename(orgunit=facility) %>% 
  select(orgunit,Siyenza)


# Combine ----------------------------------------------------------------------------------

#-----------------------------------------
final<-bind_rows(ci_bound,hfr_df_bound)  

final<-ci_bound %>% 
  full_join(siyenza_Sites, by="orgunit") 
  
final<-final %>% 
  spread(indicator, val, drop=FALSE)

filename<-paste("core_interventions", "_long", Sys.Date(), ".txt", sep="")

write_tsv(final, file.path(here("Core_Interventions"),filename,na=""))

