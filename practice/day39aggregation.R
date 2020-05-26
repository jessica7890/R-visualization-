# ----Day37--Week 13------------#
# App based on aggregation plot #
#-------------------------------#
library(shiny)
library(tidyverse)
library(lubridate)
library(ggthemes)
# Copied from day 28 #

## Aggregation Based Plot ##
###############################
load("Jan2020Flight.Rdata")

# From the Federal Airlines Administration (FAA), flight records for every U.S.
# air flight for the month of January 2020. The original CSV file is 80MB. 
# The `.RData` file has been trimmed and is in R format.
# The original data can be obtained here for those interested:
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# End of copy
###############################


# find top 10 busiest airports :
busiest <- delay %>%
  group_by(ORIGIN_CITY_NAME) %>%
  summarize(count=n() ) %>%
  arrange(desc(count)) %>% head(10) 

# aggregate by airport
time_airport_stats <- delay %>%
  mutate(wday = wday(FL_DATE, label=T)) %>%
  group_by(FL_DATE,ORIGIN,ORIGIN_CITY_NAME) %>%
  summarize(avg_dep_delay = mean(DEP_DELAY, na.rm=T),
            sd_dep_delay=sd(DEP_DELAY,na.rm=T),
            count=n(),
            bad_delays=quantile(DEP_DELAY,.9,na.rm=T), #defined the bad delay
            early_times = quantile(dep_hour+dep_min/60, .1, na.rm=T)) %>%
  as.data.frame()

# prepare the dataset needed.
# We need the state outlines data: esay: map_data("state")
# We need the location of the airports: search for such data in google
# Refer to https://openflights.org/data.html     download the airports.dat
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header=FALSE)

airports <- airports %>%
  select(V5, V7, V8) %>%
  rename(AirportCode = V5,
         lat=V7,
         long=V8)

airport_all <- left_join(time_airport_stats, airports,
                         by=c("ORIGIN"="AirportCode")) %>%
  filter(count>10)

save(busiest,time_airport_stats,airport_all,
     file="Jan2020FlightAggregation.RData")

