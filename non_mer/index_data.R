library(tidyverse)
library(readxl)

memory.limit(size=500000)

##read in file w attributes as base##
df<-read_tsv("FY20Q1\\merged_files\\msd_genie_fy17to20_20200214_attributes_v18Feb.txt",
             col_types = c(.default = "c"))

index<-df %>% 
  filter(standardizeddisaggregate %in% c("1:Age/Sex/IndexCasesOffered","2:Age/Sex/IndexCasesAccepted","3:Age Aggregated/Sex/Contacts"),
         indicator=="HTS_INDEX",
         fiscal_year=="2020",
         fundingagency=="USAID") %>% 
  spread(standardizeddisaggregate,qtr1)



write_tsv(index,"indextestingdata_USAIDfy20q1.txt",na="")




####GP & KZN Sites##############
sites<-df %>% 
  filter(snu1uid %in% c("BBbedoFVZXY","n2MDD1blKzk"),
         fiscal_year %in% c("2019","2020"),
         standardizeddisaggregate=="Total Numerator")



write_tsv(sites, "GP_KZN_Sites_FY19FY20.txt",na="")
