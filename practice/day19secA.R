# -----Day19--Week 07-----#
#       Maps Intro2
#-------------------------#
library(tidyverse)

#1. Choropleth Maps ---Display data on the map
################################################
# Choropleth maps are thematic maps in which different areas are colored 
# or shaded in accordance with the value of a statistical variable being 
# represented in the map.
# Definition in https://en.wikipedia.org/wiki/Choropleth_map
# (1) Make a Choropleth Map
############################
# Get the world map
library(maps)
world_map_data <- map_data("world")

#install.packages("gapminder")
library(gapminder)
?gapminder
head(gapminder)

country2007 <- gapminder %>%
  filter(year==2007) 
head(world_map_data)
head(country2007)

str(world_map_data)
str(country2007)
#And let's merge our map data frame with the gapminder data. 
# Join two tbles together https://dplyr.tidyverse.org/reference/join.html
# left_join: return all rows from x, and all columns from x and y. 
#            Rows in x with no match in y will have NA values in the new columns. 
#            If there are multiple matches between x and y, 
#            all combinations of the matches are returned.
world_map_all <- left_join(world_map_data,country2007,
                           by=c("region"="country"))
  
# geom_polygon: Polygons are very similar to paths (as drawn by geom_path()) 
#                except that the start and end points are connected and the 
#                inside is coloured by fill.
gdpmap <- ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group,fill=gdpPercap),
                data=world_map_all)
gdpmap

library(ggthemes)
gdpmap+
  theme_map() 

# (2) Check and fix issues with the graph
##########################################
## Question: Anything wrong with this graph?
## Many country's information is missing, for example
## United States, Russia,United Kingdom, etc.

## ? Can we display the boarder lines?

unique(world_map_data$region) #USA,Russia, UK
unique(country2007$country)#United States, no Russia information, United Kingdom 

country2007 <- gapminder %>%
  filter(year==2007) %>%
  mutate(country=plyr::revalue(country,c("United States"="USA",
                                         "United Kingdom"="UK"))) 
?plyr::revalue  
#If x is a factor, the named levels of the factor will be replaced with the new values.
#syntax: c("old value"="new value")

country2007$country
# This is tedious. We want more efficient/standard way to do this

# Question: is there a standard country name in R?
# Documentation for gapminder reveals that there is a country_codes data frame with ISO-3
# https://cran.r-project.org/web/packages/gapminder/gapminder.pdf
?country_codes
head(country_codes)
country2007_ISO3 <- gapminder %>%
  filter(year==2007) %>% 
  left_join(country_codes,by="country")
head(country2007_ISO3)


#install.packages("countrycode") 
# It convert country names and country codes
library(countrycode)
?countrycode()
?codelist
world_map_data$iso3 <- countrycode(world_map_data$region,
                                   origin = 'country.name',
                                   destination = 'iso3n')
# Look at the errors:
# country code: https://unstats.un.org/unsd/tradekb/knowledgebase/country-code
# Virgin Islands: https://en.wikipedia.org/wiki/Virgin_Islands
# Join the two datasets by iso country code again
world_map_all_iso3 <- left_join(world_map_data, country2007_ISO3, 
                                by=c("iso3"="iso_num"))
head(world_map_all_iso3)

# (3) An updated map
####################
ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group, fill=gdpPercap),
               data=world_map_all_iso3)+
  theme_map() 

# (4) Compare with the previous map, any finding?
############################################################
# str_detect: detect a substring
# Wild card symbol'.'(dot): represent any letter including special characters. 
# '*'(asterisk): match any preceeding characters zero or more times
# https://blog.exploratory.io/filter-with-text-data-952df792c2ba
?str_detect

world_map_data %>% 
  filter(str_detect(region, "Sud.n")) %>% 
  select(region) %>% 
  unique()

world_map_data %>% 
  filter(str_detect(region, "Su.n")) %>% 
  select(region)



world_map_data %>% 
  filter(str_detect(region, "Su.*")) %>% 
  select(region) %>% 
  unique()

## Note: We now seem to have an issue with sudan 
# ISO codes may have updated after sudan split into sudan and south sudan after 2010 civil war 
# map_data("world") provides 2013 border outlines, gapminder data is from 2007

# It is likely that our original merge was successful by name but was not shading in 
# the territory in what is now south sudan 
# Shifting borders and changing nations adds challenging complexity to map drawing

# (5) Change the appearance of the graph
################################################
ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group, fill=gdpPercap),
               data=world_map_all_iso3)+
  coord_quickmap() + 
  theme_map()+
  scale_fill_gradient(low="honeydew1", high="palegreen4", name="") +   
  labs(title="2007 GDP per capita (US$, inflation-adjusted)", 
       subtitle="Data source: Gapminder.org (2007)")+
  theme(legend.position = c(0.05,0.15),
        plot.title=element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))
