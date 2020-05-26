

library(ggplot2)

load("Jan2020Flight.Rdata")
names(delay)
str(delay)

delay2<-delay %>% select(ORIGIN_STATE_NM,OP_UNIQUE_CARRIER,dep_hour,dep_min) 
head(delay2) 

delay_sum<- delay2 %>% 
  group_by(OP_UNIQUE_CARRIER)%>%
  summarise(num_flights=n()) 
#  spread(key=OP_UNIQUE_CARRIER,value=num_flights)

ggplot(delay_sum, aes(x=OP_UNIQUE_CARRIER, y=num_flights)) + 
  geom_bar(stat="identity") + 
  labs(x="Carrier", y="Number of Flights", 
       title="Number of Flights Per Carrier")
#===========================================================
delay3<-delay2 %>% 
  mutate(dp_time=dep_hour+dep_min/60) %>%  group_by(ORIGIN_STATE_NM)
unique(delay3)
# %>%
#         summarise(med_dptime=median(dp_time,na.rm=TRUE))
#coordinateflip()
#x=reorder(pass in 4 var?) 
#aes(x = fct_reorder(Species, Sepal.Width, fun = median, .desc =TRUE), y = Sepal.Width

new_order <- with(delay3, reorder(ORIGIN_STATE_NM,dp_time, median , na.rm=T))
head(delay3)
ggplot(delay3)+
  geom_boxplot(aes(x=new_order, y=dp_time))+coord_flip()+
  labs(y="Departure time",x="State",
       title="Boxplots  Departure Time for Each State")+
  theme(axis.text.y = element_text(size=8))




ggplot(delay3)+
  geom_boxplot(aes(x=fct_reorder(ORIGIN_STATE_NM,dp_time,fun =median,na.rm=TRUE,.desc=TRUE), y=dp_time))
              
               