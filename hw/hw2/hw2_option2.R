
#Original#################################################
ggplot()+
  geom_line(aes(x=Year,y=Production),
            data=quarterly_mean,size=1,
            color="blue",alpha=0.5)+
  geom_line(aes(x=Year,y=Production),
            data=date_production,size=0.5,
            color="pink",alpha=0.6)+
  scale_color_manual(name = "Computation Method",
                     values = c("Quarterly","Weekly"))+
  scale_y_continuous(label = scales::comma)+
  labs(x="Year",y="Thousand Barrels Per Day")+
  ggtitle("Production of Crude Oil In the U.S.(Week/Quarter)",subtitle="From January in 1983 to February in 2020, Thousand Barrels Per Day")+
  theme_clean()+
  theme(plot.title = element_text(size = 10,
                                  hjust=0.5,face = "bold"))+
  theme(plot.subtitle=element_text(size=8, hjust=0.5,
                                   face="italic", color="navy"))+
  theme(legend.position= "bottom")


