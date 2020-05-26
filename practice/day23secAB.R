# ----Day23--Week 08-------------#
# Choropleth map with Shapefiles #
#--------------------------------#
library(tidyverse)

####################################################
## 1. Heatmap with approximate geographic layouts ##
####################################################
## using statebins package
#install.packages("statebins")
library(statebins)
?statebins_continuous #Check the format requirement 

states_stats <- read.csv("http://kmaurer.github.io/documents/data/StateStatsBRFSS.csv")
# Downloaded from Dr. Maurer's github page. 
head(states_stats)
str(states_stats)

# alaska --> Alaska 
# Needs state names with capitalized first letter or as postal abbreviation
?str_to_title
states_stats$StateName <- str_to_title(states_stats$StateName)

head(states_stats)

# make binned state plot
statebins_continuous(states_stats, 
                     state_col = "StateName",
                     value_col = "avgHealth")

# Error Message
states_stats$StateName
states_stats$StateName[9]<-"District of Columbia"

statebins_continuous(states_stats, 
                     state_col = "StateName",
                     value_col = "avgSleep")


?statebins # By default, there're 5 bins available
statebins(states_stats,
          state_col = "StateName",
          value_col = "avgSleep") 

statebins(states_stats,
          state_col = "StateName",
          value_col = "avgSleep",
          breaks=7,labels=1:7)

# More examples:
# http://rstudio-pubs-static.s3.amazonaws.com/332155_761cb4672f7644d290084eca9c195ed5.html

###################
## 2. Shapefiles ##
###################

# 2.1 Read in Shapfile
########################
#install.packages("rgdal")
library(rgdal)

help(package=rgdal)
# https://cran.r-project.org/web/packages/rgdal/rgdal.pdf

## What is a shapefile?
#   A shapefile is a simple, nontopological format for storing the geometric 
#   location and attribute information of geographic features. 
#   Geographic features in a shapefile can be represented by points, 
#   lines, or polygons (areas). The workspace containing shapefiles may also 
#   contain dBASE tables, which can store additional attributes that can be 
#   joined to a shapefile's features.

## Let's download the shapefile of Ohio's school districts from
#  https://catalog.data.gov/dataset/tiger-line-shapefile-2018-state-ohio-current-unified-school-districts-shapefile-state-based
#  In the middle of the page: Downloads & Resources --> Shapefile Zip File
#  Unzip into a folder you know
#  The folder contains "tl_2018_39_unsd.shp", we need all the files in the same folder
#  We will use this file today. 

# Set working directory appropriately
# OGR: OpenGIS Simple Features Reference Implementation
?readOGR #reads an OGR data source and layer into a suitable Spatial vector object.
setwd("C:/Users/user/Desktop/2020spring/504 data visualization")
ohio_map <- readOGR("tl_2018_39_unsd.shp") 
class(ohio_map)
?proj4string

proj4string(ohio_map)
# +proj=longlat: what type of project is used. longlat: regular longitude/latitude projection
# 
# +ellps=GRS80: Geodetic Reference System 1980, it is a geodetic reference system consisting 
#               of a global reference ellipsoid and a gravity field model.
#               https://en.wikipedia.org/wiki/Geodetic_Reference_System_1980
# 
# WGS84:         The World Geodetic System (WGS) is a standard for use in cartography, geodesy, 
#                and satellite navigation including GPS. The latest version is WGS 1984, or WGS84.
#                https://en.wikipedia.org/wiki/World_Geodetic_System
#
# +towgs84:      an additional that allows to transform our datum to
#                one of the most commonly used datums, WGS84. 
#
#                Datum shifts can be approximated by 3 parameter spatial translations (in geocentric space), 
#                or 7 parameter shifts (translation + rotation + scaling).
#                In our case, we're not making the transform, so we have +towgs84=0,0,0,0,0,0,0.
#                http://proj.maptools.org/gen_parms.html
#
# +no_defs: ensures that no defaults are read from the defaults files. Sometimes they cause suprising problems.

# Take a look at the data by looking at the environment panel
glimpse(ohio_map) #helps to figure out how to extract the information



# 2.2 Maps with shapefiles
############################
# Technically, ggplot can work with this type of data directly
library(ggthemes)
ggplot(ohio_map) +
  geom_path(aes(x=, y=, group=)) +
  coord_quickmap() +
  theme_map()

# 2.3 Choropleth map using shapefile
#####################################


## Task 1: Find another data that contains some meaningful data.
###################################################################
# For example, if people are interested in the enrollment infromation, can 
# Search: "ohio enrollment by school district" on google
# click on the first website. Ohio Dedepartment of Education
# http://education.ohio.gov/Topics/Data/Frequently-Requested-Data/Enrollment-Data
library(readxl)
?read_excel
enrollment <- read_excel("", sheet="")
head(enrollment)
names(enrollment)
# Interested in Grade_12 enrollment headcount?
enrollment <- enrollment %>% 
  select()


## Task 2: Prepare the map data that can be joined with the enrollment data
###########################################################################
# What are the things that needs to be known?
# Longitude, latitude, some variable that can join with the enrollment data?
# format: data frame
?tidy #Turn an object into a tidy tibble
#install.packages("broom")
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
  mutate(id = )
head(ohio_map_data)

# Now combine the two datasets by "id"
ohio_map_combined <- left_join() 
head(ohio_map_combined)


## Task 3: Combine the map data with the enrollment data
###########################################################################
head(enrollment)
head(ohio_map_combined) #Maybe do not need all variables, select some that's necessary.
ohio_map_combined <- ohio_map_combined %>% 
  select()
head(ohio_map_combined) 

# Rearrange the data, to see the observations clearly.
ohio_map_combined %>% 
  arrange(NAME)

enrollment %>% 
  arrange(DIST_NAME)

# One extra step: Remove the " School District" from ohio_map_combined variable "NAME"
ohio_map_combined <- ohio_map_combined 


map_all <- left_join(ohio_map_combined, enrollment)
head(map_all)
unique(map_all$GRADE_12) # Look for any value that does not make sense.
as.numeric(unique(map_all$GRADE_12))


map_all <- map_all %>% 
  mutate(Grade12enroll=as.numeric(GRADE_12))

ggplot(map_all) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=)) +
  theme_map() +
  scale_fill_gradient(low="gray99", high="darkgreen") +
  labs(fill="Enrollment Headcount",title="Ohio Grade 12 Enrollment Headcount for Fall 2019",
       subtitle="Data Source: Ohio Department of Education")+
  theme(legend.position=c(0.8, 0.05),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
