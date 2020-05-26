#Day07 (week3)
library(tidyverse)

###1. Multiple Histograms with proportional representation
######################################################################
ggplot()+
  geom_histogram(aes(x=carat, y=..count..),binwidth=.1, data=diamonds)+
  facet_grid(cut ~ .)
#..density.. is a special variable in ggplot, can also try ..count..

###2. Add numbers on top of graphs
#################################
#2.1 Bargraph
ggplot()+
  geom_bar(aes(x=cut),data=diamonds)+
  geom_text(aes(x=cut,label=..count..),data=diamonds,
            stat="count",vjust=1)
#label: the value you want to display, can be ..count.., can be ..density..
#vjust: adjust where to put the numbers. 
#https://www.gl-li.com/2017/08/18/place-text-at-right-location/

##stacked bargraph
ggplot()+
  geom_bar(aes(x=cut,fill=clarity),data=diamonds)+
  geom_text(aes(x=cut,label=..count..,group=clarity),data=diamonds,
            stat="count",vjust=1)

##adjust the display of the numbers
ggplot()+
  geom_bar(aes(x=cut,fill=clarity),data=diamonds)+
  geom_text(aes(x=cut,label=..count..,group=clarity),data=diamonds,
            stat="count",position=position_stack(vjust=0.5),size=3)
#position=: change the positions of where you want to put the number.
#size: how large the number is


#Question???
#aes(x=cut), data=diamonds appear multiple times. Any easier way to code?
ggplot(aes(x=cut),data=diamonds)+
  geom_bar(aes(fill=clarity))+
  geom_text(aes(label=..count..,group=clarity),
            stat="count",position=position_stack(vjust=0.5),size=2)

#2.2 Histogram
ggplot(aes(x=price),data=diamonds)+
  geom_histogram()+
  stat_bin(aes(label=..count..),geom="text")
#geom: The geometric object to use display the data. e.g. "bar","point","line"
#stat_bin: bin data, something related with what information you want to display, etc


##not happy with the display, maybe too many bins?
ggplot(aes(x=price),data=diamonds)+
  geom_histogram(binwidth = 3000)+
  stat_bin(aes(label=..count..),geom="text",
           binwidth = 3000)

##move the number to other locations? maybe in the middle of the bars?
##also take a look at the x-axis, the numbers do not match with the bins.
ggplot(aes(x=price),data=diamonds)+
  geom_histogram(binwidth = 3000)+
  stat_bin(aes(label=..count..),geom="text",
           binwidth = 3000,position=position_stack(vjust=0.5))+
  scale_x_continuous(breaks=seq(0,max(diamonds$price),3000))


##color mark it by another variable

ggplot(aes(x=price),data=diamonds)+
  geom_histogram(aes(fill=cut),binwidth = 3000)+
  stat_bin(aes(label=..count..,group=cut),geom="text",
           binwidth = 3000,position=position_stack(vjust=0.5))+
  scale_x_continuous(breaks=seq(0,max(diamonds$price),3000))

#################################
###3. Data cleaning and manipulation
###################################

#3.1 What is dplyr
###################
#d: dataframe, The precursor to dplyr was called plyr. 
#The 'ply' in plyr comes from an expansion/refining of the various "apply" functions in R as part of the "split-apply-combine" model/strategy. 
#Here's a good slideset that provides more insight into the plyr name:
#https://www.slideshare.net/hadley/plyr-one-data-analytic-strategy

#3.2 Day 06 material continued--Diamonds data
##############################################

##(1) How many of the diamonds have clarity type VS1 or VVS2?
##What's the 70th percentile of the diamonds carat for those diamonds?
diamonds %>% 
  dplyr::filter(clarity=="VS1"|clarity=="VVS2")%>%
  summarize(percent_70=quantile(carat,probs=0.7))


diamonds %>% 
  dplyr::filter(clarity=="VS1"&cut=="Fair")
#3.3 Tennis data
##############################################
#This is tour-level qualifying and challenger 
#main-draw matches data for year 2019.
#Refer the description from the website:
#https://github.com/JeffSackmann/tennis_atp

##(1) Read the data into R
tennis <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2019.csv")
#look at the error message
#WC: wild card

tennis2 <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2019.csv",
                   col_types=cols(
                     loser_seed=col_character()))
class(tennis2$loser_seed)          

arrange(desc(mean.price))

# or download from Canvas, then load into R
setwd("---Set the path to your dataset here---")
tennis <- read_csv("atp_matches_qual_chall_2019.csv")

head(tennis,10)
tail(tennis,10)

ggplot() +
  geom_point(aes(x=winner_age, y=winner_rank_points),data=tennis) +
  theme_bw()
#--------------------------------------------------------------------------
##(2) Data Exploration and Cleaning

## Too many variables, focus on a few
ts <- select(tennis, tourney_id:surface, tourney_date, 
             match_num, winner_id,
             winner_name, winner_hand,winner_age,
             loser_id,loser_name, loser_hand,loser_age,
             minutes,winner_rank,winner_rank_points,
             loser_rank,loser_rank_points)


##(3) Create an indicator column, specifying whether the
## game is an upset. (A person with a higher rank
## lose the same)




##(4) What if I want to count how many games each player win?



##(5) Who had the most number of wins? Reorder data



##(6) Let's explore the performance for the player
## who has the most win in 2019 four-level qualifying
## and challenger main-draw matches
## Create a table showing the number 




##(6) What if I want to automate the process (4), (5), (6)
##for other years of data, without need to run the code separately



#############################################
##Your Turn---Diamonds data
##What type of cut and clarity combination have
#the highest 3 mean price?



