# ----Day26--Week 09----#
#      Google Maps      #
#-----------------------#
library(tidyverse)
#install.packages("ggmap")
library(ggmap)

## Run the folloiwng code to view the information about enabling google service in R. 
?register_google 

## You need to enable the "Maps Static API".
#  A credit card information needs to be provided. 
#  You will get $200.00 in free usage for Maps, Routes, 
#  and Places every month. (google charges $2/1K requests, 
#  that is 100K free requests per month)
#  View more information:
#  https://console.cloud.google.com/apis/library/static-maps-backend.googleapis.com?q=map&id=d375a354-744d-4a9c-bf52-4893091a057d&project=august-circle-272420&folder&organizationId

## API key is a key to access the google service. 
#  Refer to the following website to get an API Key
#  https://developers.google.com/maps/documentation/javascript/get-api-key

register_google(key="[your key]")
?get_googlemap
ohio_ggmap <- get_googlemap(c(-82.9071,40.4173),zoom=7) #longitude and latitude

# or you can use the following code to save the key to a variable
api_key="[your key]"
ohio_ggmap <- get_googlemap(c(-82.9071,40.4173),key=api_key)

# 2. Basic Google Maps
########################
?ggmap
ggmap(ohio_ggmap)

## (2.1) Recall the following county map of Ohio
ohio_map_data <- map_data("county",region="ohio")
ggplot(aes(x=long, y=lat), data=ohio_map_data) + 
  geom_path(aes(group=group))

## (2.2) Display it on top of the google map
# The ggmap() function makes a ggplot object, so we can do standard ggplot stuff to it.
ggmap(ohio_ggmap,base_layer=
        ggplot(aes(x=long,y=lat),data=ohio_map_data)) +
  geom_path(aes(group=group))

# 3. Add  data on top of the map
############################################
## (3.1) Read in COVID-19 data
#  This website contains COVID-19 daily report
#  https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
coviddata <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv")
head(coviddata)
ohio_covid <- coviddata %>% 
  filter(Province_State=="Ohio")

## (3.2) Add the number of confirmed cases to the map
ggmap(ohio_ggmap,base_layer=
        ggplot(aes(x=long,y=lat),data=ohio_map_data)) +
  geom_path(aes(group=group))+
  geom_text(aes(x=Long_,y=Lat,label=Confirmed),data=ohio_covid %>% filter(Confirmed>0))


## (3.3) Adjust the design of the graph
?attr
bounds <- as.numeric(attr(ohio_ggmap, "bb"))
bounds
library(ggthemes)
ggmap(ohio_ggmap,base_layer=
        ggplot(aes(x=long,y=lat),data=ohio_map_data)) +
  geom_path(aes(group=group),color="dark grey")+
  geom_text(aes(x=Long_,y=Lat,label=Confirmed),
            family="Times",color="darkgreen",
            fontface="bold",size=5,data=ohio_covid %>% filter(Confirmed>0))+
  coord_map(ylim=c(38.1,42),xlim=c(-85.2,-80))+
  theme_map()


# This website contians many more examples with google maps.
# https://www.littlemissdata.com/blog/maps

