# ----Day33--Week 11----#
#       Animation       #
#-----------------------#
library(tidyverse)
library(ggplot2)
setwd("C:/Users/user/Desktop/2020spring/504 data visualization")
# 1. Get the data (discussed on day 32)
#########################################
# An animated chart displays several chart states one after the other.
# You can get a sense of different animated graphs here: https://www.r-graph-gallery.com/animation.html
# Please watch this video and we will try to create a simialr graph. 
# Hans Rosling: 200 years in 4 minutes - BBC News
# https://www.youtube.com/watch?v=Z8t4k0Q8e8Y&feature=youtu.be

# We can get the data from https://www.gapminder.org/data/, which we have used it in previous class.

# Income
income <- read_csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv")

# Life expectancy
life <- read_csv("life_expectancy_years.csv")

# Population
pop <- read_csv("population_total.csv")

# Demographic location
library(readxl)
geographics <- read_excel("Data Geographies - v1 - by Gapminder.xlsx",
                          sheet=2)

head(income)
head(life)
head(pop)
head(geographics)


# 2. Prepare the data for plot
#################################
# Change data from "wide" to "long"
income.tall <- income %>%
  gather(key=Year, value=Income,-country)
head(income.tall)  
income.tall %>% arrange(country)

life.tall <- life %>%
  gather(key=Year, value=LifeExp,-country)
  head(life.tall)

pop.tall <- pop %>%
  gather(key=Year, value=Population,-country)
  head(pop.tall)

# Combine them all
all <- left_join(pop.tall,life.tall,by=c("country","Year")) %>% 
  left_join(income.tall,by=c("country","Year")) 
  
# Subset the geographics data
head(geographics)
region <- geographics %>% 
  select(name,four_regions)
head(all)
head(region)
all <- all %>%
  left_join(region,by=c("country"="name"))
head(all)
tail(all)


# 3. Make a bubble plot for a specific year
###############################################
# Get the range of the x axis and y axis
min(all$Income)
min(alln$Income,na.rm=TRUE) #245
max(alln$Income,na.rm=TRUE) #179000
min(alln$LifeExp, na.rm=TRUE) # 1.01
max(alln$LifeExp, na.rm=TRUE) #94.8
min(all$Population, na.rm=TRUE) #645
max(all$Population, na.rm=TRUE) # 1650000000


all2019 <- all %>%
  filter(Year == "2019")
head(all2019)
min(all2019$Population, na.rm=TRUE) #815


ggplot(all2019) +
  geom_point(aes(x=Income, y=LifeExp, size=Population, color=four_regions),
             alpha=0.5) +
  scale_y_continuous(limits=c(0,100)) + 
  scale_x_log10(limits=c(200,200000)) +
  labs(y="Life Expentency", x="Income (log 10 scale)") +
  scale_color_discrete(name="Continent",
                       labels=c("Africa", "Americas", "Asia", "Europe") ) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_size_continuous(range=c(1,10),guide=FALSE,
                        limits=c(500,1650000000))
#scale_size_continuous guide=FALSE will remove the legend for size


# 4. Make animated plot
###########################
library(gganimate)
# https://gganimate.com/articles/gganimate.html
# https://cran.r-project.org/web/packages/gganimate/gganimate.pdf

# Use the data "all" directly, and see whether it will give you the result.
alln <- all %>%
  mutate(Year = as.numeric(Year)) %>% 
  filter(Year<=2019) 

ggplot(alln) +
  geom_point(aes(x=Income, y=LifeExp, size=Population, color=four_regions),
             alpha=0.5) +
  scale_y_continuous(limits=c(0,90)) + 
  scale_x_log10(limits=c(200,200000)) +
  labs(y="Life Expentency", x="Income (log 10 scale)") +
  scale_color_discrete(name="Continent",
                       labels=c("Africa", "Americas", "Asia", "Europe") ) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_size_continuous(range=c(1,10),guide=FALSE,
                        limits=c(500,1440000000))+
  transition_time(Year)
#?transition_states

class(all$Year)

# 5. Add title and change some specific setting 
###################################################
my.animation <- ggplot(alln) +
  geom_point(aes(x=Income, y=LifeExp, size=Population, color=four_regions),
             alpha=0.5) +
  scale_y_continuous(limits=c(0,90)) + 
  scale_x_log10(limits=c(200,200000)) +
  labs(y="Life Expentency", x="Income (log 10 scale)") +
  scale_color_discrete(name="Continent",
                       labels=c("Africa", "Americas", "Asia", "Europe") ) +
  theme_bw() + 
  theme(legend.position="bottom",
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 10)) + 
  scale_size_continuous(range=c(1,10),guide=FALSE,
                        limits=c(500,1440000000))+
  transition_time(Year)+
  ggtitle('Year:{as.integer(frame_time)}')+
  ease_aes('cubic-in-out') #ease_aes('linear')

# ease_aes: it needs to decide how the change from one value to another should progress. 
# This is a concept called easing. The default easing is linear, but others can be used,


?animate
animate(my.animation,
        nframe=length(unique(alln$Year)),
        fps=4,
        width=700,
        height=600,
        res=200)
# The animate() function documentation describes how to specify how the animation is rendered
# Label variables: frame_time. Gives the time that the current frame corresponds to.
# fps: Each image represents a frame, so if a video is captured and played back at 24fps, 
#      that means each second of video shows 24 distinct still images.
# res: set the resolution for each frame.

# 6. Check if there's anything wrong with the data
###################################################



