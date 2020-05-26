# -----Day18--Week 06-----#
#       Maps Intro
#-------------------------#
library(tidyverse)

############### Materialsl related with Day 17 ############### 
# (1) Aesthestics: grouping
# By default, the group is set to the interaction of all discrete variables in the
# plot.
# https://ggplot2.tidyverse.org/reference/aes_group_order.html

# (2) Save the plot
library(reshape2)
head(french_fries)
ff_subj_tall <- french_fries %>% 
  gather("flavor", "score", potato:painty) %>%
  group_by(subject, time, flavor, treatment) %>%
  summarise(avgScore = mean(score, na.rm=TRUE))

mytheme <- theme(panel.background = element_rect(fill="white"),
                 panel.grid.major = element_line(color="gray85"),
                 panel.grid.minor = element_line(color="gray95"),
                 plot.title = element_text(size=14, color="darkblue"),
                 axis.title.y = element_text(color="darkblue"))

# Change the oil variable from 1,2,3 to "Oil 1", "Oil 2", "Oil 3
ff_subj_tall$oil <- paste("Oil", ff_subj_tall$treatment)
glimpse(ff_subj_tall)

ggplot(aes(x=time, y=avgScore), data=ff_subj_tall) +
  geom_line(aes(group=subject)) +
  stat_smooth(aes(group=1,color=flavor),method="loess",span=2,se=FALSE,size=2)+
  facet_grid(flavor ~ oil)+
  scale_y_continuous(breaks=seq(0,16,by=4),
                     limits=c(0,16)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2"), guide="none") +
  #guides(color=FALSE) 
  labs(x="Week", y="Average Scores by Subjects \n(0-16 scale)",
       title="Changing Flavor Profiles for Each Subject Over \n 10 Weeks with LOESS Smoothers") +
  mytheme

ggsave("FrenchFries.png",dpi=600,height=8,width=5, units="in")
# ggsave() is a convenient function for saving a plot. It defaults to saving the last plot that you displayed, using the size of the current graphics device. 
# It also guesses the type of graphics device from the extension.
################ End of Day 17 Material ############### 



######## Maps #########
#######################

# 1. Map Introduction #
#######################
#install.packages('maps')
library(maps)

?map_data()
state_map_data <- map_data("state")
head(state_map_data)
tail(state_map_data)
# 
# long:  longitude. Things to the west of the prime meridian are negative, down to -180, 
#        and things to the east of the prime meridian run from 0 to positive 180.
# lat:   latitude.
# order: This just shows in which order ggplot should "connect the dots"
#        region and subregion tell what region or subregion a set of points surrounds.
# group: ggplot2's functions can take a group argument which controls
#        (amongst other things) whether adjacent points should be connected by lines. 
#        If they are in the same group, then they get connected, but if they are in different 
#        groups then they don't. Essentially, having two points in different groups means that 
#        ggplot "lifts the pen" when going between them.
# https://eriqande.github.io/rep-res-eeb-2017/map-making-in-R.html

# (1) Make an outline of the states in the United States
#########################################################
# Try to use geom_line to connect the longitude and latitude information.
ggplot() + 
  geom_line(aes(x=long,y=lat,group=group),data=state_map_data)
  

# What's wrong with the graph?
# geom_line() connects observations in order of the variable on the x axis
# geom_path() connects them in the order in which they appear in the data
ggplot() + 
  geom_path(aes(x=long,y=lat,group=group),data=state_map_data)


# (2) Work with other maps
###########################
## ohio data by county
head(map_data("county"))
unique(map_data("county")$region)
ohio_map_data <- map_data("county",region="ohio")
head(ohio_map_data)
tail(ohio_map_data)
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=ohio_map_data)

## world map
world_map_data <- map_data("world")
head(world_map_data)
tail(world_map_data)
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=world_map_data)


# (3) Projections
#############
# coord_map projects a portion of the earth, which is approximately spherical, 
# onto a flat 2D plane using any projection defined by the mapproj package. 
# https://ggplot2.tidyverse.org/reference/coord_map.html
nz_map_data <- map_data("nz")
head(nz_map_data)

#install.packages("mapproj")
library(mapproj)
ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=nz_map_data)+
  coord_quickmap() # With the aspect ratio approximation


ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=nz_map_data)+
  coord_map() # With correct mercator projection


ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=nz_map_data)+
  coord_map("azequalarea", orientation = c(-36.92, 174.6, 0))
# default is c(90, 0, mean(range(x)))

ggplot() + 
  geom_path(aes(x=long, y=lat, group=group), data=world_map_data) + 
  coord_map(projection = "ortho",orientation = c(-90,0,0))
# orthographic projection

# 2. Choropleth Maps ---Display data on the map
################################################
# Choropleth maps are thematic maps in which different areas are colored 
# or shaded in accordance with the value of a statistical variable being 
# represented in the map.
# Definition in https://en.wikipedia.org/wiki/Choropleth_map
# (1) Make a Choropleth Map
############################
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
head(world_map_all)
  
# geomo_polygon: Polygons are very similar to paths (as drawn by geom_path()) 
#                except that the start and end points are connected and the 
#                inside is coloured by fill.
gdpmap <- ggplot() +

gdpmap

library(ggthemes)
gdpmap+
  theme_map() 

# (2) Check and fix issues with the graph
##########################################
## Question: Anything wrong with this graph?

## 
unique(world_map_data$region) #
unique(country2007$country) #

country2007 <- gapminder %>%
  filter(year==2007) %>%
  mutate() 
?plyr::revalue  
#If x is a factor, the named levels of the factor will be replaced with the new values.
#syntax: c("old value"="new value")

country2007$country
# This is tedious. We want more efficient/standard way to do this

# Question: is there a standard country name in R?

# Documentation for gapminder reveals that there is a country_codes data frame with ISO-3
# https://cran.r-project.org/web/packages/gapminder/gapminder.pdf
?country_codes
country2007_ISO3 <- gapminder %>%
  filter(year==2007) %>% 
 
head(country2007_ISO3)


#install.packages("countrycode") 
# It convert country names and country codes
library(countrycode)
?countrycode()
?codelist
world_map_data$iso3 <- countrycode()
# Look at the errors:
# country code: https://unstats.un.org/unsd/tradekb/knowledgebase/country-code
# Virgin Islands: https://en.wikipedia.org/wiki/Virgin_Islands
# Join the two datasets by iso country code again
world_map_all_iso3 <- left_join(world_map_data, country2007_ISO3, 
                                )
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
  select(region)

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

# 2. Change the appearance of the graph
################################################
ggplot() +
  geom_polygon(aes(x=long,y=lat,group=group, fill=gdpPercap),
               data=world_map_all_iso3)+
  coord_quickmap() + 
  theme_map()+
  scale_fill_gradient(low="honeydew1", high="palegreen4", name="") +   
  labs(title="2007 GDP per capita (US$, inflation-adjusted)", subtitle="Data source: Gapminder.org (2007)")
