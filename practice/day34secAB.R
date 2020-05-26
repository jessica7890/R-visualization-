# ----Day34--Week 12----#
# Animation, shiny app  #
#-----------------------#
library(tidyverse)
library(gganimate)

################
# 1. Animation #
################
setwd("C:/Users/user/Desktop/2020spring/504 data visualization")
# (1.1) Prepare the data -- Day 33 material
###################################################
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
income.tall <- income %>%
  gather(key=Year, value=Income,-country)
head(income.tall)  
income.tall %>% arrange(country)

head(life)
life.tall <- life %>%
  gather(key=Year, value=LifeExp,-country)
head(life.tall)

pop.tall <- pop %>%
  gather(key=Year, value=Population,-country)
head(pop.tall)

all <- left_join(pop.tall,life.tall,by=c("country","Year")) %>% 
  left_join(income.tall,by=c("country","Year")) 

# Subset the geographics data
region <- geographics %>% 
  select(name,four_regions)

all <- all %>%
  left_join(region,by=c("country"="name"))

alln <- all %>%
  mutate(Year = as.numeric(Year)) %>% 
  filter(Year<=2019) 

# (1.2). Add title and change some specific setting 
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
        legend.title = element_text(size = 4),
        legend.text = element_text(size = 4)) + 
  scale_size_continuous(range=c(1,10),guide=FALSE,
                        limits=c(500,1440000000))+
  transition_time(Year)+
  ggtitle('Year:{as.integer(frame_time)}')+
  ease_aes('cubic-in-out') #ease_aes('linear')


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

# (1.3). Check if there's anything wrong with the data
###################################################
unique(alln$four_regions)

test <- alln %>% 
  filter(is.na(four_regions)*1==1)
test

################
# 2. Shiny Apps#
################
# Pleae visit https://shiny.rstudio.com/tutorial/ to view the first 37 minutes of the video. 
# The first 26 minutes introduced how to create a shiny app.
# The 26-37 minutes introduce how to share a shiny app to the Rstudio cloud. 


# Some of the information in the video is a little bit different from the current Rstudio version, 
# for example you can always see a "Run App" button on upper side of the screen.
#
# Find out more about building applications with Shiny here: http://shiny.rstudio.com/
# Options in the input functions and others, see 
#  -https://shiny.rstudio.com/reference/shiny/1.4.0/
#  -the shinny cheat sheet: https://shiny.rstudio.com/articles/cheatsheet.html

