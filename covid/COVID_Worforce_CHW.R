library(tidyverse)
library(readxl)
library(stringr)


HWF<-read_excel("FY20\\FY20Q2\\From team\\COVID-19 Workforce and CHW Activity Tracker 20200420.xlsx", sheet="Workforce")

HWF_wide<-HWF %>% 
  gather(period,val,`3/26/2020`:`4/29/2020`) %>% 
  spread(`Data element`,val)



write_tsv(HWF_wide,"Processed_Files\\HWF\\HWF_COVID_wide_2020610.txt",na="")