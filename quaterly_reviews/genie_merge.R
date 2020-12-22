library(tidyverse)
library(readxl)
library(ICPIutilities)

memory.limit(size=500000)

#GENIE FILES#
df1<-read_msd("Genie\\fy20q2\\Genie_SITE_IM_South_Africa_1_20200526.txt", save_rds = FALSE, remove_txt = FALSE)
df2<-read_msd("Genie\\fy20q2\\Genie_SITE_IM_South_Africa_2_20200526.txt", save_rds = FALSE, remove_txt = FALSE)
df3<-read_msd("Genie\\fy20q2\\Genie_SITE_IM_South_Africa_3_20200526.txt", save_rds = FALSE, remove_txt = FALSE)
df4<-read_msd("Genie\\fy20q2\\Genie_SITE_IM_South_Africa_4_20200526.txt", save_rds = FALSE, remove_txt = FALSE)
df5<-read_msd("Genie\\fy20q2\\Genie_SITE_IM_South_Africa_5_20200526.txt", save_rds = FALSE, remove_txt = FALSE)


genie_combined<-bind_rows(df1,df2,df3,df4,df5)
rm(df1,df2,df3,df4,df5)


#trim genie to only fy20 if it is not already in that format
genie_combined <- genie_combined %>% 
  filter(fiscal_year %in% c("2020", "2021"))


#MSD & bind to genie
msd17_19<-read_msd("MSD\\fy19q4c\\MER_Structured_Datasets_Site_IM_FY17-20_20191220_v2_1_South Africa.txt",
                  save_rds = FALSE, remove_txt = FALSE)

msd17_19<-msd17_19 %>%
  filter(fiscal_year %in% c("2017","2018","2019"))

genie_msd<-genie_combined %>% 
  bind_rows(msd17_19)

rm(genie_combined,msd17_19)

saveRDS(genie_msd,"Processed_Files\\msd_genie_fy17to2020_20200526.RDS")


##CONTEXT FILES & RSD 
dsp<-read_excel("ContextFiles\\UserFriendly_PartnerName_DSPcolumn.xlsx") %>% 
  rename(mech_code=MechanismID,
         DSP=DSP_18_19)

ethk<-read_excel("ContextFiles\\eThekwiniSiteShifts.xlsx") %>% 
  filter(Transitionstat=="USAIDtoCDC") %>% 
  rename(fy19q1_sitetransition=Transitionstat,
         orgunituid=Facilityuid) %>% 
  select(orgunituid,fy19q1_sitetransition)

siyenza<-read_excel("ContextFiles\\siyenza_att_uid_20200318.xlsx", sheet="Sheet1") %>% 
  rename(orgunituid=orgunit_internal_id) %>% 
  select(-c(facility))


factype<-read_excel("ContextFiles\\Facility_Type.xlsx") %>% 
  rename(facility=Facility)



#MSD/Genie/Context Merge ##
final<-genie_msd %>% 
  left_join(dsp, by="mech_code") %>% 
  left_join(ethk,by="orgunituid") %>% 
  left_join(factype, by="facility") %>% 
  left_join(siyenza, by="orgunituid")

rm(dsp,ethk,factype,genie_msd,siyenza)



#Write file#
write_tsv(final,"Processed_Files\\msd_genie_fy17to20_20200526_attributes.txt",na="")


