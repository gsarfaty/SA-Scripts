library(tidyverse)
library(readxl)
library(here)
library(extrafont)
library(glitr)

#COJ DEEP DIVE
Anova<-"Anova Review of NDOH Report v4_GS.xlsx"

df<-read_xlsx(here("Quarterly Reviews/Partner Deep Dives", Anova),sheet = "COJ_Close_GS", skip=3) %>% 
  rename(Q2=`Sum of Q2 Result`,
         Q3=`Sum of Q3 Tier Result from DOH`,
         Q3_NN=`Sum of Q3 Net_New`,
         days_closed=`Sum of How many days of clinic closure during Q3?`) %>% 
  mutate(curr_size=case_when(
    Q3 <500 ~ "Less than 500",
    Q3 =500 & Q3 <1000 ~ "500 to 999",
    Q3 =100 & Q3 <2500 ~ "1,000-2,499",
    Q3 =2500 & Q3 <4000 ~ "2,500 to 3,499",
    Q3 =3500 & Q3 <5000 ~ "3,500 to 4,999",
    Q3 =5000 & Q3 <10000 ~ "5,000 to 9,999",
    Q3 >1000 ~ "10,000+"
  ))




## SCATTER PLOT -- NN & Facility Clossure

scatter<-df %>% 
  filter(`Negative Net_New?` %in% c("Yes","No")) %>% 
  filter()
  ggplot(aes(x=Q3, y=days_closed,
             color=`Negative Net_New?`)) + 
  geom_point(size=4) +
  si_style_ygrid()

print(scatter)


## HISTOGRAM -- CURR & Facility Closure
neworder<-c("Less than 500","500 to 999","1,000-2,499",
         "2,500 to 3,499","3,500 to 4,999","5,000 to 9,999",
         "10,000+")
  
histo<-df %>% 
  filter(`Negative Net_New?` %in% c("Yes","No"),
         `Tier vs SYNCH` =="Tier < SYNCH") %>% 
  arrange()
  ggplot(arrange(transform(df,
    curr_size=factor(curr_size,levels=neworder)),curr_size))+
  geom_histogram(size=4) +
  facet_wrap(~curr_size)+
  si_style_ygrid()

print(histo)
