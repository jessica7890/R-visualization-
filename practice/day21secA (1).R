# ----Day21--Week 07------#
#  More Maps, Shapefiles #
#-------------------------#
library(tidyverse)
library(maps)

#------    Data Preparation -----------#
world_map_data <- map_data("world")
library(gapminder)
country2007 <- gapminder %>%
  filter(year==2007) 
##############################
# 1. Nesting data with tidyr #
##############################
## What if we store the data a little differently, put all outline in single row next to state name?
##   - problem: we have never put a complex object into a data cell before, but tibble can handle it
world_data <- world_map_data %>%
  group_by(region) %>%
  nest()
world_data$data[[2]] #  outlines
test <- world_map_data %>% 
  filter(region=="Afghanistan")

head(world_data$data[[which(world_data$region=="Turkey")]])
?which
head(test)
head(world_data$data[[2]])

world_data$data[[2]]$long # looks like values are rounded, but they arn't

world_all <- left_join(world_data,country2007, by=c("region"="country"))
head(world_all)
## The gain here is that the storage size is much smaller without the redundancy of 
## having the gapminder values repeated for each boundary outline. 

?unnest
head(unnest(world_all))
head(unnest(world_all,cols="data"))

library(ggthemes)

lifeExpmap <- ggplot() + 
  geom_polygon(aes(x=long, y=lat,
                   fill=lifeExp, group=group),
               data=unnest(world_all,cols=c(data)))+
  coord_map(xlim=c(-180,180)) + # What's wrong with it? 
  # issue with the coord_map(), see https://stackoverflow.com/questions/30360830/map-in-ggplot2-visualization-displaying-bug/30463740#30463740
  theme_map()+
  theme(legend.position = c(0.05,0.1))

  
lifeExpmap
#######################################
# 2. Adding layer of labels to states #
#######################################
?sort
sort(unique(world_all$lifeExp))


world_lab <- world_all %>%
  filter(lifeExp>81.702) %>% 
  unnest(data) %>% #"data", c(data)
  summarise(life=round(lifeExp[1],1),#round(var,#of decimal points)
            long=mean(range(long)),
            lat=mean(range(lat))) 
?range
world_lab

lifeExpmap+
  geom_text(aes(x=long,y=lat,label=life),data=world_lab)

##################################################
# 3. Heatmap with approximate geographic layouts #
##################################################
## using statebins package
#install.packages("statebins")
library(statebins)
?statebins
?statebins_continuous

states_stats <- read.csv("http://kmaurer.github.io/documents/data/StateStatsBRFSS.csv")
# Downloaded from Dr. Maurer's github page. 
str(states_stats)

# Needs state names with capitalized first letter or as postal abbreviation
states_stats$StateName <- str_to_title(states_stats$StateName)

# make binned state plot
statebins_continuous(states_stats, 
                     state_col = "",
                     value_col = "")
# More examples:
# http://rstudio-pubs-static.s3.amazonaws.com/332155_761cb4672f7644d290084eca9c195ed5.html
