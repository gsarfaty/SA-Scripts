


# SCATTER PLOT --------------------------------------------------------------------------------------------------------
pitc_risk_vis<-pitc_fac %>% 
  group_by(period,psnu.x,risk) %>% 
  summarize_at(vars(HTS_TST:HTS_TST_POS),sum,na.rm=TRUE) %>% 
  mutate(yield=HTS_TST_POS/HTS_TST) %>% 
  ggplot(aes(x=risk,y=yield, fill=risk))+
  geom_col()+
  scale_fill_manual(values=c(golden_sand,scooter))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits=c(0,.08))+
  geom_text(aes(label=scales::percent(yield,accuracy=0.5)), vjust=-0.25,family="Gill Sans MT", size=3)+
  si_style_nolines()+
  labs(caption="FY21 PITC yields + Dec 31 core interventions data")+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  facet_wrap(~psnu.x)
           
  
print(pitc_risk_vis)



ggsave(here("Data Use/HTS","PITC_Risk.png"),
       scale=1.5,width=10, height=4, dpi=300, units="in")
       
       
 # BAR CHART -----------------------------------------------------------------------------------------------------------
 tb_stat_vis<-TB_STAT %>%
  filter(period=="FY20") %>% 
  ggplot(aes(x =reorder(psnu, -TB_Cov), 
             y = TB_Cov,
             fill=flag))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1),
                     limits=c(0,1))+
  geom_col(width=.6)+
  # geom_hline(yintercept = .90, linetype="dashed")+
  scale_fill_manual(values=c(old_rose,grey40k))+
  labs(title="",
       caption="APR20; TB_STAT/TB_STAT_D")+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=10, angle = 45,hjust=1,vjust=1),
        legend.position = "none")

print(tb_stat_vis)


ggsave(here("Data Use/HTS","FY20_TB_STAT_district.png"),
       width=10, height=4, dpi=300, units="in")
       
       
# LINE GRAPH ---------------------------------------------------------------------------------------------------------
pict_pmtct_trend_viz<-pitc_dist%>%
  filter(!psnu %in% c("City of Tshwane","eThekwini","uMgungundlovu")) %>% 
  mutate(psnu=case_when(
    psnu=="City of Cape Town" ~ "Cape Town",
    psnu=="City of Johannesburg" ~ "CoJ",
    TRUE ~ psnu
  )) %>% 
  ggplot(aes(x =period, 
             y = yield,
             group=modality))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1))+
  geom_line(aes(color=modality))+
  # geom_point(aes(color=modality))+
  scale_color_si("scooter")+
  labs(title="",
       caption="PITC vs PMTCT yield")+
  facet_wrap(~psnu,
             ncol=5)+
  si_style_yline()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size=7, angle = 45,hjust=1,vjust=1),
        legend.position = "top")

print(pict_pmtct_trend_viz)


ggsave(here("Data Use/HTS","PITC v PMTCT.png"),
       scale=1.5, width=6, height=6, dpi=300, units="in")

# JITTER PLOT ----------------------------------------------------------------------------------------------

viz<-coj %>% 
  filter(period %in% c("FY20Q3","FY20Q4")) %>% 
  mutate(period=factor(period,levels=c("FY20Q4","FY20Q3"))) %>%
  ggplot(aes(y=period,
             x=val,
             fill=NN_cat))+
  geom_jitter(shape=21,color="white",size=3)+
  scale_fill_manual(values=c(grey60k,usaid_blue))+
  geom_vline(xintercept = 0,linetype="dotted")+
  labs(x="",
       y="")+
  si_style_nolines()+
  theme(legend.position = "none")
  

print(viz)

ggsave(here("Quarterly Reviews","coj_NN_site_v1.1.png"))
