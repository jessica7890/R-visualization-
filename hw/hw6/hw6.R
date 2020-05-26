#install.packages("gifski")
#install.packages("transformr")
library(openintro)
library(gganimate)
library(maps)
library(tranformr)
library(dplyr)

setwd("C:/Users/user/Desktop/2020spring/504 data visualization/hw/hw6")

US <- read_csv("us-states.csv")
US$state<- tolower(US$state)

#filter the date only upto  March 21st
US<-US %>% dplyr::filter(date<"2020-03-22") %>% 
  select(date,state,cases) 

# State boundary data
state_outlines <- map_data("state") 
head(state_outlines)
state_outlines$region<- tolower(state_outlines$region)


#leftjoin state_outlines and US by state name
all_covid<-left_join(state_outlines,US,
                      by=c("region"="state")) %>% arrange(date)
head(all_covid)

#identify the center point of each state (long,lat)and create abbreviation for each state.
st_abbr<-all_covid%>% group_by(region) %>%
  summarise(long=mean(range(long)),
    lat=mean(range(lat))) %>% 
  mutate(abbr=state2abbr(region))
head(st_abbr)


#entire period############################################################
library(ggthemes)
min(all_covid$cases,na.rm=TRUE)#1
max(all_covid$cases,na.rm=TRUE)#7102
head(all_covid)

str(all_covid)

#create map using geom_polygon
covidplot<-ggplot() +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cases),data=all_covid) + 
  theme_map() +  
  scale_fill_continuous(low="lightblue", high="darkblue",
                       trans='log10', limits=c(1,8000))+
    labs(fill="Number of Confirmed Cases",
       title=" Updated Date: ", 
       subtitle="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html")+
  theme(legend.position="bottom",
        plot.title = element_text(size = 15),
        plot.subtitle = element_text(size = 12))

covid_Mar21<-covidplot+
  geom_text(aes(x=long,y=lat,label=abbr),data=st_abbr)+ 
  geom_path(aes(x=long, y=lat, group=group), data=state_outlines)+
  transition_time(date)+
  ggtitle('Updated Date:{frame_time}')+
  ease_aes('linear') 

 animate(covid_Mar21,
         nframe=length(unique(all_covid$date)),
         fps=4,
         width=700,
         height=500)
        
 
 anim_save("hw6.gif")