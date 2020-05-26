# -------Day14--Week 05-------#
#     Design principles 2     #
#-----------------------------#
library(tidyverse)

##################################################
## 1. Heatmatp related with Day 13

## View overall correlation heatmap
## https://boxuancui.github.io/DataExplorer/index.html
install.packages("DataExplorer")
library(DataExplorer)
plot_correlation(diamonds)
##################################################


## 2. Slopgraph  ##
###################
## Drug Poisoning Mortality in the United States, 1999-2017
## https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/

## (1) Data Preparation
drug_poison <- read_csv("NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv")
data0717 <- drug_poison %>% 
  filter((Year==2007|Year==2017)&State!="United States") %>% 
  mutate(Yearo=ordered(Year)) %>% 
  select(State,Yearo,`Age-adjusted Rate`)
  head(data0717)
  
data0717

## (2) Slopegraph
ggplot() + 
  geom_line(aes(x=Yearo, y=get("Age-adjusted Rate"),group=State),data=data0717)
# get(): Return the value of a named object
ggplot() + 
  geom_line(aes(x=Yearo, y=`Age-adjusted Rate`,group=State),data=data0717)+
  theme_minimal()

## (3) Clean up the plot and add appropriate information
colnames(data0717)=c("State","Year","Rate")

ggplot() + 
  geom_line(aes(x=Year, y=Rate, group=State), data=data0717) +
  labs(y="Age Adjusted Death Rate per 100,000 Population",
       title="Drug Poisoning Mortality") + 
  theme_minimal()+
  coord_cartesian(xlim=c(1.3,1.3))

## (4) Highlight Ohio?

data0717ind<- data0717 %>% 
  mutate(IsOhio=ifelse(State=="Ohio","Ohio",""))
data0717ind

ggplot() + 
  geom_line(aes(x=Year, y=Rate, group=State, color=Isohio), data=data0717ind)+
  scale_color_manual(values=c("grey","red"), name="State") +
  geom_text(aes(x=Year,y=Rate, label=Isohio),data=filter(data0717ind,Year==2017))
    labs(y="Age Adjusted Death Rate per 100,000 Population",
       title="Drug Poisoning Mortality") + 
  theme_minimal()+
  coord_cartesian(xlim=c(1.5,1.5))



## 3. Gestalt principles  ##
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
  geom_rect(aes(xmin=4.5,xmax=5.6,ymin=3.25,ymax=4),color="red",alpha=0)

ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width),data=iris)+
  geom_rect(aes(xmin=4.5,xmax=5.6,ymin=3.25,ymax=4),alpha=0.2)

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

# we'll plot the variable unemploy (number of unemployed 
# in thousands) by date (x-axis).
ggplot(aes(x=date,y=unemploy),data=economics)+
  geom_point()

#Mark the data by decades
new <- economics %>% 
  mutate(year=year(date),
         decade=factor(round(year/10) * 10)) 
new
ggplot(aes(x=date,y=unemploy,shape=decade),data=new)+
  geom_point()+
  geom_line()


## Example of Decluttering
ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species),data=iris)+
  theme_bw()+
  theme(panel.border)

# https://felixfan.github.io/ggplot2-remove-grid-background-margin/
