# -------Day13--Week 05------------------#
# Design principals, other types of plots#
#----------------------------------------#
library(tidyverse)

## 1. Table and Heatmap  ##
###########################

## In day 06, we talked about the following information
table(diamonds$clarity, diamonds$cut)

## Create similar summary table in tidyverse
summary<-diamonds %>% 
  group_by(clarity,cut) %>% 
  summarise(n=n()) #%>% 
  #spread(key=cut,value=n)

summary

summary %>% 
  ggplot() +
  geom_tile(aes(x=clarity,y=cut,fill=n),color="white") + 
  scale_fill_gradient(low="white",high="blue") +
  labs(fill ="Frequency")

## Or can display the density of clarity for each cut type 

summary2 <- diamonds %>%
  group_by(clarity, cut) %>%
  summarise(n=n()) %>%
  group_by(cut) %>%
  mutate(sum.n=sum(n)) %>%
  mutate(n2=n/sum.n) %>%
  ungroup() %>%
  select(clarity, cut, n2)

summary2 %>% spread(cut,n2)


summary2 %>%
  ggplot(aes(clarity, cut)) +
  geom_tile(aes(fill=n2), colour = "white") +
  scale_fill_gradient(low="white",high="blue") +
  labs(fill = "Density")


## View overall correlation heatmap
## https://boxuancui.github.io/DataExplorer/index.html
#install.packages("DataExplorer")
library(DataExplorer)
plot_correlation()


## 2. Slopgraph  ##
###################
## Drug Poisoning Mortality in the United States, 1999-2017
## https://www.cdc.gov/nchs/data-visualization/drug-poisoning-mortality/

## (1) Data Preparation
drug_poison <- read_csv("NCHS_-_Drug_Poisoning_Mortality_by_State__United_States.csv")
data0717 <- drug_poison %>% 

data0717

## (2) Slopegraph
ggplot() + 
  geom_line()
# get(): Return the Value of a Named Object

## (3) Clean up the plot and add appropriate information
colnames(data0717)=c("State","Year","Rate")
library(ggforce)


## (4) Highlight Ohio?