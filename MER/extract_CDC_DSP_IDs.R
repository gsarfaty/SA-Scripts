library(readxl)
library(tidyverse)
library(here)


#READ CDC DASH DATA
dsp<-read_excel(here("ContextFiles", "CDC_Dash_20200914.xlsx"), sheet="Data", skip=2,
                col_types="text")


# GET DISTINCT SET OF DSP DETAILS TO MERGE AS ATT.
dsp_n<-dsp %>% 
  select(MechanismID,DSPID,DSP,DSP_lookback,mechID_lookback,Partner_lookback,`Agency lookback`) %>% 
  distinct() 




# WRITE FILE

filename<-paste("DSP_attributes", Sys.Date(), ".txt", sep="_")

write_tsv(dsp_n, file.path(here("ContextFiles"),filename,na=""))