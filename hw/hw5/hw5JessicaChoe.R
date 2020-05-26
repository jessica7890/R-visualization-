
#==================================================================#
# ---- 1. The following is a stating code for the USDA data ------ #
#==================================================================#

library(tidyverse)
library(readxl)
library(ggthemes)
setwd("C:/Users/user/Desktop/2020spring/504 data visualization/hw/hw5")

# Download the data (which is the excel data with name “Data Download” from the website with a last uploaded date 3/27/2018)
# https://www.ers.usda.gov/data-products/food-environment-atlas/data-access-and-documentation-downloads/


## local foods data from "LOCAL" tab in excel sheets

  
local_food <- read_excel(path="DataDownload.xls", sheet="LOCAL") %>% 
  select(FIPS, State, County, FRESHVEG_FARMS12, ORCHARD_ACRES12, 
         GHVEG_FARMS12, GHVEG_SQFTPTH12, FMRKT16, FMRKTPTH16, DIRSALES_FARMS12)
head(local_food,4)
# Note: postal abbreviations on state names

## FIPS codes for State and county (unabbreviated) from "Supplemental Data - County" tab
county_fips <- read_excel(path="DataDownload.xls", sheet="Supplemental Data - County") %>% 
  select(FIPS, State, County)
head(county_fips,2)

## County boundary data
county_outlines <- map_data("county") 
head(county_outlines)

# ----Start your code below ------ #

#Change the letter of the county/state name into the same format
county_fips$State<- str_to_lower(county_fips$State)
county_fips$County<- str_to_lower(county_fips$County)
local_food$County<- str_to_lower(local_food$County)

#Check if there are strange values.
unique(county_outlines$region)
unique(county_fips$State)
head(county_outlines$region)

#Leftjoin local food and county fips by "FIPS" in order to get the state full name.
local_food_combined<-left_join (local_food,county_fips,by="FIPS")
head(local_food_combined)

head(local_food_combined$County.x)
head(local_food_combined$County.y)

#Leftjoin county outlines and localfood_combined by the names of counties. 
all_local_food<-left_join(county_outlines,local_food_combined,
                          by=c("region"="State.y","subregion"="County.y"))
unique(all_local_food$DIRSALES_FARMS12)
class(all_local_food$DIRSALES_FARMS12)

#Apply log10
all_local_food <- all_local_food %>% 
    mutate(DIRSALES_FARMS12=log10(DIRSALES_FARMS12))
  unique(all_local_food$DIRSALES_FARMS12)
#Geom ploygon
hw5.1<-
  ggplot(all_local_food) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=DIRSALES_FARMS12)) +
  theme_map() +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  labs(fill="Number of Farms",
       title="Number of Farms with Direct Sales, 2012",
       caption="Data source: USDA Food Environment Atlas, 3/27/2018")+
  theme(legend.position=c(0.8, 0.05),
        plot.title = element_text(hjust = 0.4,size = 15))
  
#Save the image file
 ggsave(hw5.1,file="hw5_1.png",
       width = 6, height = 4)

#==================================================================#
# --- 2. The following is a stating code for the COVID-19 data --- #
#==================================================================#
# The following data is the 03-29-2020 data, you may use a more updated data from the github page.
# https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports
# To use the data directly, click the dataset name, then click the "Raw" on the upper right side of the dataset. 
# Then you may copy the url and replace the url below to read in the data.

coviddata <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-29-2020.csv")
## State boundary data
state_outlines <- map_data("state") 
head(state_outlines)
# ----Start your code below ------ #


#Change the first letter of the region into the Capital letter. 
state_outlines$region<- str_to_title(state_outlines$region)

#Check if there are strange values.
unique(coviddata$Province_State)

#Filter the country by US and arrange by State name
coviddata<-coviddata %>%  dplyr::filter(Country_Region=="US")%>%
  arrange(Province_State)

#Calculate the total number of cases for each state.(version1)
class(coviddata$Confirmed)
coviddata1<-coviddata %>%group_by(Province_State) %>%
  summarise(Confirmed_State=sum(as.numeric(Confirmed)))
head(coviddata1,3)

#Calculate the Confirmed cases for each state (version2)
#(sum of cases of counties within each state.)
#We can use both ver 1 and 2 but here I am going to use version 1 and keet the 2nd one as reference.
#coviddata2<-coviddata %>%group_by(Province_State) %>%
#  mutate(Confirmed_State=sum(as.numeric(Confirmed)))
#head(coviddata2)


#leftjoin state_outlines,coviddata1 by state names
all_covid<-left_join(state_outlines,coviddata1,
                     by=c("region"="Province_State")) %>% 
  select(long,lat,group,order,region,Confirmed_State)
head(all_covid,3)

#create map using geom_polygon
#hw5.2<-
  ggplot(all_covid) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Confirmed_State)) +
  theme_map() +
  scale_fill_gradient(low="lightblue", high="darkblue") +
  labs(fill="Confirmed Cases",
       title="Number of Confirmed Cases of COVID as of 3/29/2020", 
       caption="Data source: 2019 Novel Coronavirus COVID-19 by Johns Hopkins CSSE 3/29/2020")+
  theme(legend.position=c(0.8, 0.05),
        plot.title = element_text(size = 12))

#Save the image file
ggsave(hw5.2,file="hw5_2.png",
        width = 6,height = 4)
