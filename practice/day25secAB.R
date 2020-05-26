# ----Day25-- Week 09------------#
# Choropleth map with Shapefiles #
#--------------------------------#
library(tidyverse)
library(rgdal)

# 1. Read in Shapefiles
#########################
ohio_map <- readOGR("tl_2018_39_unsd.shp") 
# Technically, ggplot can work with this type of data directly
library(ggthemes)
ggplot(ohio_map) +
  geom_path(aes(x=long, y=lat, group=group)) +
  coord_quickmap() +
  theme_map()

# 2. Choropleth map using shapefile
#####################################

## Task 1: Find another data that contains some meaningful data.
###################################################################
# For example, if people are interested in the enrollment infromation, can 
# Search: "ohio enrollment by school district" on google
# click on the first website. Ohio Dedepartment of Education
# http://education.ohio.gov/Topics/Data/Frequently-Requested-Data/Enrollment-Data
# oct_hdcnt_fy19.xls
library(readxl)
?read_excel
enrollment <- read_excel("oct_hdcnt_fy19.xls", sheet="fy19_hdcnt_dist")
head(enrollment)
names(enrollment)
# Interested in Grade_12 enrollment headcount?
enrollment <- enrollment %>% 
  select(DIST_IRN,DIST_NAME,COUNTY,GRADE_12)


## Task 2: Prepare the map data that can be joined with the enrollment data
###########################################################################
# What are the things that needs to be known?
# Longitude, latitude, some variable that can join with the enrollment data?
# format: data frame
?tidy #Turn an object into a tidy tibble
# install.packages("broom")
library(broom)
tidy(ohio_map)
ohio_map@data
head(tidy(ohio_map)) # Look at the variable "id", type: character
head(ohio_map@data) # Look at the row names 0,1,2,..., etc
# These two values above should match.

## We can join these two datasets together.
ohio_map_tidy <- tidy(ohio_map)
ohio_map_data <- ohio_map@data


# Put an id that goes 0 to (n-1) number of groups for the ohio_map@data, make it as a character variable.
ohio_map_data <- ohio_map_data %>% 
  mutate(id = as.character(0:(nrow(ohio_map_data)-1)))
head(ohio_map_data)
# Now combine the two datasets by "id"
ohio_map_combined <- left_join(ohio_map_tidy,ohio_map_data,by="id") 
head(ohio_map_combined)


## Task 3: Combine the map data with the enrollment data
###########################################################################
head(enrollment)
head(ohio_map_combined) #Maybe do not need all variables, select some that's necessary.
ohio_map_combined <- ohio_map_combined %>% 
  select(long,lat,order,group,id,NAME)
head(ohio_map_combined) 

# Rearrange the data, to see the observations clearly.
ohio_map_combined %>% 
  arrange(NAME)

enrollment %>% 
  arrange(DIST_NAME)

# One extra step: Remove the " School District" from ohio_map_combined variable "NAME"
ohio_map_combined <- ohio_map_combined %>% 
  mutate(NAME2=str_sub(NAME,end=-17))


map_all <- left_join(ohio_map_combined, enrollment,by=c("NAME2"="DIST_NAME"))
head(map_all)
unique(map_all$GRADE_12) # Look for any value that does not make sense.
as.numeric(unique(map_all$GRADE_12))


map_all <- map_all %>% 
  mutate(Grade12enroll=as.numeric(GRADE_12))

ggplot(map_all) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=Grade12enroll)) +
  theme_map() +
  scale_fill_gradient(low="gray99", high="darkgreen") +
  labs(fill="Enrollment Headcount",
       title="Ohio Grade 12 Enrollment Headcount for Fall 2018",
       subtitle="Data Source: Ohio Department of Education")+
  theme(legend.position=c(0.8, 0.05),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))