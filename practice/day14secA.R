# -------Day14--Week 05-------#
#     Design principles 2     #
#-----------------------------#
library(tidyverse)

##################################################
## 1. Heatmatp related with Day 13

## View overall correlation heatmap
## https://boxuancui.github.io/DataExplorer/index.html
#install.packages("DataExplorer")
library(DataExplorer)
plot_correlation(diamonds)
##################################################


## 2. Slopgraph  ##
###################
## Drug Poisoning Mortality in the United States, 1999-2017
## https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/

## (1) Data Preparation
drug_poison <- read_csv("C:/Users/user/Desktop/2020spring/504 data visualization/NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv")
data0717 <- drug_poison %>% 
  filter(Year==2007|Year==2017,State!="United States") %>% 
  mutate(Yearo=ordered(Year)) %>%
  select(State,Yearo,`Age-adjusted Rate`)
  
  
data0717
head(data0717)
tail(data0717)

## (2) Slopegraph
ggplot() + 
  geom_line(aes(x=Yearo, y=`Age-adjusted Rate`,group=State),data=data0717)

ggplot() + 
  geom_line(aes(x=Yearo, y=get("Age-adjusted Rate"),group=State),data=data0717)

# get(): Return the value of a named object

## (3) Clean up the plot and add appropriate information
colnames(data0717)=c("State","Year","Rate")

ggplot() + 
  geom_line(aes(x=Year, y=Rate, group=State), data=data0717) +
  labs(y="Age Adjusted Death Rate per 100,000 Population",
       title="Drug Poisoning Mortality") + 
  theme_minimal()+
  coord_cartesian(xlim=c(1.5,1.5))

## (4) Highlight Ohio?
data0717ind <- data0717 %>% 
  mutate(IsOhio=ifelse(State=="Ohio","Ohio",""))

data0717ind

ggplot() + 
  geom_line(aes(x=Year, y=Rate, group=State,color=IsOhio), data=data0717ind) +
  scale_color_manual(values=c("grey","red"),name="State")+
  geom_text(aes(x=Year,y=Rate,label=IsOhio),
            hjust=-0.05,color="blue", size=2,
            data=filter(data0717ind,Year==2017))+
  labs(y="Age Adjusted Death Rate per 100,000 Population",
       title="Drug Poisoning Mortality") + 
  theme_minimal()+
  coord_cartesian(xlim=c(1.5,1.5))

