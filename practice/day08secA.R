#Day08
library(tidyverse)
#######################################
#Material related with day07
#Bar located on center of some number
######################################
ggplot(aes(x=price),data=diamonds)+
  geom_histogram(binwidth = 3000,boundary=0,closed="left")+
  stat_bin(aes(label=..count..),geom="text",
           binwidth = 3000,position=position_stack(vjust=0.5),boundary=0,closed="left")+
  scale_x_continuous(breaks=seq(0,max(diamonds$price), 3000))
#boundary=0: indicates you want to display the histogram beginning from 0.
#closed="right" or "left": specifies if a number occurs on the boundary, e.g. 3000, will it be put in the left
#bar, or the right bar. right:(0,3000]   left:[0,3000)
#Refer to https://ggplot2.tidyverse.org/reference/geom_histogram.html


#######################################
#Data cleaning and manipulation
#######################################

#1. Tennis data
##############################################
#This is tour-level qualifying and challenger 
#main-draw matches data for year 2019.
#Refer the description from the website:
#https://github.com/JeffSackmann/tennis_atp

##(1) Read the data into R
tennis <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2019.csv")
#look at the error message
#WC: wild card
class(tennis)
tennis$loser_seed[3000:3332]
tennis2 <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2019.csv",
                    col_types = cols(
                      loser_seed = col_character()))

# or download from Canvas, then load into R
#setwd("---Set the path to your dataset here---")
tennis <- read_csv("atp_matches_qual_chall_2019.csv")

head(tennis,10)
tail(tennis,10)

ggplot() +
  geom_point(aes(x=winner_age, y=winner_rank_points),data=tennis) +
  theme_bw()
#--------------------------------------------------------------------------
##(2) Data Exploration and Cleaning

str(tennis)
## Too many variables, focus on a few
ts <- select(tennis, tourney_id:surface, tourney_date, 
             match_num, winner_id,
             winner_name, winner_hand,winner_age,
             loser_id,loser_name, loser_hand,loser_age,
             minutes,winner_rank,winner_rank_points,
             loser_rank,loser_rank_points)

select(tennis,-tourney_id:-loser_rank)
##(3) Create an indicator column, specifying whether the
## game is an upset. (A person with a higher rank
## lose the same)
ts_new <- mutate(ts,upset=1*((winner_rank-loser_rank)>0))
ts_new[,18:19]
head(ts_new)
ts_new_s <- select(ts_new,-tourney_id:-minutes)
head(ts_new_s,6)
#How many upset matches do I have?
summarise(ts_new_s,sum(upset))
summarise(ts_new_s,sum(upset,na.rm=TRUE))
#na.rm: remove the missing values in calculation.

##(4) What if I want to count how many games each player win?
head(ts)
ts_win_grouped <- group_by(ts,winner_name)
ts_win_grouped
ts_win <- summarise(ts_win_grouped,n_win=n())
ts_win
##(5) Who had the most number of wins? Reorder data
ts_win <- arrange(ts_win,desc(n_win))
head(ts_win)


##(6) Let's calculate the average total time of a game, and the number
##of games that the player who has the most number of wins. had on different surface.
winfirst <- filter(ts,winner_name=="James Duckworth"|loser_name=="James Duckworth")
winfirst_grouped <- group_by(winfirst,surface)
summarise(winfirst_grouped,ave_minutes=mean(minutes),n_game=n())



##(7) What if I want to automate the process (4), (5), (6)
##for other years of data, without need to run the code separately
##short cut for %>% : "ctrl"+"shift"+"M"
##a. find the player
ts %>% group_by(winner_name) %>% 
  summarise(n_win=n()) %>% 
  arrange(desc(n_win)) %>% 
  top_n(1,n_win) %>% 
  select(1,1)

#top_n(x, n, wt), x is the data, n is the number of 
#rows to return, wt is the variable to use for ordering.
#if not specified, defaults to the last variable in x.

##b. do the computation
#paste0: Concatenate vectors after converting to character.
#paste0(player_selected,collapse = ", ")


####################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
#(1) Diamonds data
##What type of cut and clarity combination have
#the highest 3 mean price?

####################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



#2. Ohio BRFSS data
##############################################
#The Behavioral Risk Factor Surveillance System (BRFSS) is the nation’s premier
#system of health-related telephone surveys that collect state data about U.S. 
#residents regarding their health-related risk behaviors, chronic health conditions,
#and use of preventive services.
#--------------------------------------------------
#(1) Preparing the data
#Download the 2018 Behavioral Risk Factor Survalence System (BRFSS) as 101M SAS Export File
#from CDC Website: https://www.cdc.gov/brfss/annual_data/annual_2018.html
#file name: 2018 BRFSS Data (SAS Transport Format) [ZIP 101 MB]
#What is a .xpt file: https://www.loc.gov/preservation/digital/formats/fdd/fdd000464.shtml

#install.packages("SASxport")
library(SASxport)
setwd("---Set the path to your dataset here---")
dat <- read.xport("LLCP2018.XPT ") #make sure there's a space after XPT
head(dat)
dim(dat)

#Too many variables and observations
#refer to the codebook (156 pages):https://www.cdc.gov/brfss/annual_data/2018/pdf/codebook18_llcp-v2-508.pdf
#Only focus on Ohio.
ohio_brfss <- dat %>% filter(X.STATE == 39) #why there's a X. not as what's in the codebook?
write.csv(ohio_brfss, file="ohioBRFSS2018.csv")
#-----------------------------------------------------------

oh_brfss <- read_csv("ohioBRFSS2018.csv")
oh_brfss <- read_csv("https://raw.githubusercontent.com/qyuan20/Datasets-DataVis/master/ohioBRFSS2018.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
### Manipulation of the BRFSS data:
# 0. Take a look at code book for the PYSHLTH and MENTHLTH variables, page 17 and 18.
# 1. Narrow down to a small number of columns, {IMONTH IDAY PHYSHLTH MENTHLTH}.
# 2. Convert the "None" for Phyiscal and Mental health issues to 0 days.
# 3. Keep only complete records for values for health responses (drop "missing", "don't know",etc.).
# 4. Display the first 6 lines of the final data.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#Hint for task 2.
#ifelse(test, yes, no)
#$test:an object which can be coerced to logical mode.
#yes:return values for true elements of test.
#no:return values for false elements of test.
x="A"
ifelse(x=="B",1,0)
x=4
ifelse(x==6,1,0)


