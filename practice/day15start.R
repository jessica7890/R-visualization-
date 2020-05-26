# -------Day15--Week 05-----------------------------------#
#   Gestalt principles, Fluctuation Diagrams, Mosaic Plot
#---------------------------------------------------------#
library(tidyverse)

## 1. Gestalt Principles  ##
############################
## Poximity
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)

## Similarity
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species),data=iris)


ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,shape=Species),data=iris)


## Enclosure
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)+

  
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)+

  
## Cluosure
library(readxl)
stardata <- read_excel("stardata.xlsx")

ggplot()+
  geom_point(aes(x=x,y=y),data=stardata)+
  theme_void()
# http://robertgrantstats.co.uk/drawmydata.html This is a website you
# can draw any pictur eusing dot, then download the dataset.

## Continuity
ggplot(aes(x = clarity),data = diamonds)+
  geom_bar()+
  theme_void()+  
  geom_text(aes(label=clarity),stat="count",
            position=position_stack(vjust=0.5),
            color="white")

## Connection
?economics
head(economics)
library(lubridate)

## we'll plot the variable unemploy (number of unemployed 
## in thousands) by date (x-axis).
ggplot(aes(x=date,y=unemploy),data=economics)+
  geom_point()

## Mark the data by decades
new <- economics %>% 
  mutate() 
new
ggplot(aes(x=date,y=unemploy,shape=),data=new)+
  geom_point()


## Example of Decluttering
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species),data=iris)+
  

## https://felixfan.github.io/ggplot2-remove-grid-background-margin/


## 2. Fluctuation Diagrams  ##
##############################

## Related with Day 13 heatmap and the scatterplot.
## Same idea as heatmap, but let the size, not color, 
## determine the intensity
## In a scatteplot, the size of the point reflects the intensity/freqency

## Note: If x & y were numeric variables, this is called
## a "bubble plot" (will be discussed in the future)
## More informaiton about bubble plot: 
## https://www.data-to-viz.com/graph/bubble.html

##--------------------------##
## Summary table from Day 13
summary <- diamonds %>%
  group_by(clarity, cut) %>%
  summarise(n=n())
summary
##---------------------------##


ggplot() + 
  geom_point(aes(x=clarity, y=cut, size=), 
             shape=, data=summary) + ## can customize
  scale_size_continuous() + ## range specifies size of squares
  theme_bw()

## 3. Mosaic Plot  ##
#####################
## A mosaic plot is a graphical display of the cell 
## frequencies of a contingency table in which the area
## of boxes of the plot are proportional to the cell 
## frequencies of the contingency table.

## Review ##
##----------------------------------------------------##
## Stacked bar graph in Day 6
ggplot()+
  geom_bar(aes(x=cut, fill=clarity), position ="fill",
           data=diamonds)
##----------------------------------------------------##

##----------------------------------------------------##
## Summary2 table similar in Day 13
## conditional clarity proportions by cut
summary2 <- summary %>%
  group_by(cut) %>%
  mutate(cutcount= sum(n)) %>%
  mutate(prop= n/cutcount) %>%
  ungroup() %>%
  arrange(cut)
summary2
##----------------------------------------------------##

#install.packages("ggmosaic")
library(ggmosaic)
help(package="ggmosaic")

ggplot() +
  geom_mosaic(aes(weight=, x=, fill=), data=summary2)
?product ## a wrapper for a list
class(diamonds$cut)
class(product(diamonds$cut))


## I can also use geom_mosiac on the raw data, not the counted frequencies

ggplot() +
  geom_mosaic(aes(x=, fill=), data=diamonds)


ggplot() +
  geom_mosaic(aes(x=product(color), fill=cut), data=diamonds)

## The mosaic plot can show more types of relationships
## https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html

ggplot() +
  geom_mosaic(aes(x=product(), fill=), data=diamonds)
