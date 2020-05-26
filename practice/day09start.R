#Day09--Week 03------------------#
#Data cleaning and manipulation  #
#--------------------------------#
#-------------------------------------------------------------------------------------------------------------------
## Materials from day08
#-------------------------------------------------------------------------------------------------------------------
#1. Tennis data
##############################################
#This is tour-level qualifying and challenger 
#main-draw matches data for year 2019.
#Refer the description from the website:
#https://github.com/JeffSackmann/tennis_atp

##(1) Read the data into R
library(tidyverse)
tennis <- read_csv("https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_qual_chall_2019.csv")

##(2) Data Exploration and Cleaning
ts <- select(tennis, tourney_id:surface, tourney_date, 
             match_num, winner_id,
             winner_name, winner_hand,winner_age,
             loser_id,loser_name, loser_hand,loser_age,
             minutes,winner_rank,winner_rank_points,
             loser_rank,loser_rank_points)

##(3) Create an indicator column, specifying whether the
## game is an upset. (A person with a higher rank
## lose the same)
ts_new <- mutate(ts, upset =1*((winner_rank-loser_rank)>0))
head(ts_new)
ts_new_s <- select(ts_new,-tourney_id:-minutes)
summarise(ts_new_s,sum(upset)) #What's wrong with it?
summarise(ts_new_s,nupset=sum(upset,na.rm=TRUE))
#na.rm:remove the missing values in calculation

##(4) What if I want to count how many games each player win?
ts_win_grouped <- group_by(ts, winner_name)
ts_win_grouped
ts_win <- summarise(ts_win_grouped,
                    n_win=n())

##(5) Who had the most number of wins? Reorder data
ts_win <- arrange(ts_win, desc(n_win))
head(ts_win)

##(6) For the player that has the most number of wins,
#Let's calculate (1) average total time of a game; (2) the total number
##of games that the player had on different surface.
winfirst <- filter(ts, winner_name=="James Duckworth"|loser_name=="James Duckworth")
winfirst_grouped <- group_by(winfirst,surface)
summarise(winfirst_grouped,
          avg_minutes = mean(minutes),
          n_game=n())

#-------------------------------------------------------------------------------------------------------------------
#End of Materials from day08
#-------------------------------------------------------------------------------------------------------------------


##(7) What if I want to automate the process (4), (5), (6)
##for other years of data, without need to run the code separately
##short cut for %>% : "ctrl"+"shift"+"M"

##a. find the player
player_selected <- ts %>% 
  group_by(winner_name) %>% 
  summarise(n_win=n()) %>% 
  arrange(desc(n_win)) %>% 
  top_n(1,n_win) %>% #select the top number of rows.
  select(1) #Or, select(winner_name) 
player_selected

#Note the error introduced on day 08.
#select can only choose which variable to be selected.  
#top_n(x, n, wt), x is the data, n is the number of 
#rows to return, wt is the variable to use for ordering.
#if not specified, defaults to the last variable in x.

#----------------------------------------------------------------------


##b. do the computation
#change dataframe to characters.
class(player_selected)
as.character(player_selected)
player_selected
ts %>% filter(ts, winner_name==as.character(player_selected)|
                loser_name==as.character(player_selected)) %>% 
  group_by(surface) %>% 
  summarise(avg_minutes =mean(minutes),n_game=n())

winfirst

winfirst_grouped <- group_by(winfirst,surface)
summarise(winfirst_grouped,
          avg_minutes = mean(minutes),
          n_game=n())

#2. Diamonds data
####################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
##(1) What type of cut and clarity combination have
#the highest mean price?
ls(diamonds)

diamonds %>%
  group_by(cut,clarity) %>% 
  summarise(mean.price=mean(price)) %>% 
  arrange(desc(mean.price)) %>% 
  ungroup() %>% 
  top_n(1)#IF do not specify which variable to work with, it will select according to the last variable.
#???ungroup()?????
  arrange(desc(n_win)) %>% 
  top_n(1,n_win) %>% #select the top number of rows.
  ####################################################################################
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(2) There're 8 clarity types, after searching for the name of diamonds types, I find:
#"SI1" and "SI2"  ---> Slightly included (SI)
#"VS1", "VS2"     ---> Very Slightly included (VS)
#"VVS1", "VVS2"   ---> Very, Very Slightly included (VVS)
#Can I change the clarity type veraiable values according to the above assignment?
#e.g. change "SI1" and "SI2" values to "SI", just keep the 
#other clarity types the same.

#Use ifelse(), syntax: ifelse(test, yes,no)
#test: logic statement
#yes: the value you want to return if the logic statement is true
#no: the value you want to return if the logic statement is false
diamonds %>% 
  mutate(newvar=ifelse((clarity=="SI1"|clarity=="SI2"),1,0))

test=diamonds %>% 
  mutate(newvar=ifelse((clarity=="SI1"|clarity=="SI2"),"SI",clarity))
test
#problem: all other categories appear as 5,4,6, seems in the ifelse statement, lost the labels in the factors\

#change the clarity variable to character first, then do the assignment. 
test=diamonds %>% 
  mutate(newvar=ifelse((clarity=="SI1"|clarity=="SI2"),
                       "SI",as.character(clarity)))
test

test$newvar
class(test$newvar)

#change the new variable to be ordered factor
test=diamonds %>% 
  mutate(newvar=ordered(ifelse((clarity=="SI1"|clarity=="SI2"),
                               "SI",as.character(clarity))))
test
class(test$newvar)
test$newvar
diamonds$clarity
levels(test$newvar)
levels(diamonds$clarity)
#?Anthying wrong?
#The order of the levels does not match with the orginal clarity data.
#We can specify the levels exiplictly, to overcome the problem. 
test=diamonds %>% 
  mutate(newvar=ordered(ifelse((clarity=="SI1"|clarity=="SI2"),
                               "SI",as.character(clarity)),
                               levels=c("I1","SI","VS2","VS1","VVS2","VVS1","IF")))
test
class(test$newvar)
test$newvar#you need to specify the orders of the data
levels(diamonds$clarity)



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
setwd("C:\Users\user\Desktop\2020spring\502 data visualization\In-class assignment")
dat <- read.xport("LLCP2018.XPT ") #make sure there's a space after XPT
head(dat)
dim(dat)

#Too many variables and observations
#refer to the codebook (156 pages):https://www.cdc.gov/brfss/annual_data/2018/pdf/codebook18_llcp-v2-508.pdf
#Only focus on Ohio.
ohio_brfss <- dat %>% filter(X.STATE == 39) #why there's a X. not as what's in the codebook?
write.csv(ohio_brfss, file="ohioBRFSS2018.csv")
#-----------------------------------------------------------

#(2) Read data in R
oh_brfss <- read_csv("https://raw.githubusercontent.com/qyuan20/Datasets-DataVis/master/ohioBRFSS2018.csv")
#or
oh_brfss <- read_csv("ohioBRFSS2018.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
### Manipulation and plot of the BRFSS data:
# 0. Take a look at code book for the PYSHLTH and MENTHLTH variables, page 17 and 18.
# 1. Narrow down the data to a small number of columns, {IMONTH IDAY PHYSHLTH MENTHLTH}.
# 2. Convert the "None" for Phyiscal and Mental health issues to 0 days. 
# 3. Keep only complete records for values for health responses (drop "missing", "don't know",etc.).
# 4. Display the first 6 lines of the cleaned data.
# 5. Make a plot of individual measurements for physical and mental health, 
#    use appropriate method to show the relationship more clearly, and avoid 
#    some problems with just using the basic plot.  
# 6. Create monthly summaries for both mental and physical health, 
#    show the result in a table.
# 7. Plot the above calculated monthly summaries relationship
#    in one plot, where each month is distinguished by color.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#1
library(tidyverse)
oh_selected <-select(oh_brfss,IMONTH, IDAY, PHYSHLTH, MENTHLTH)
head(oh_selected)

#2
oh_selected$PHYSHLTH
oh_selected$MENTHLTH
class(oh_selected$MENTHLTH)

oh_PHYSHLTH2=oh_selected %>% 
  mutate(PHYSHLTH2=ifelse((PHYSHLTH==88),0,PHYSHLTH))

head(oh_PHYSHLTH2)
Oh_none_removed=oh_PHYSHLTH2 %>% 
  mutate(MENTHLTH2=ifelse((MENTHLTH==88),0,MENTHLTH))
head(Oh_none_removed)

#3. we need PHYSHLTH from 0 to 30, same range for MENTHLTH, so anything below 31 can be filtered in this data 
oh_filtered <-Oh_none_removed%>%
  dplyr::filter(PHYSHLTH2< 31 & MENTHLTH2< 31)

oh_cleaned<-select(oh_filtered,IMONTH, IDAY,PHYSHLTH2,MENTHLTH2)
oh_cleaned
#4.
head(oh_cleaned,6)
#5
ggplot(aes(x=PHYSHLTH2,y=MENTHLTH2),data=oh_cleaned) +
  geom_count(alpha=0.3)+
  geom_smooth( method=lm )+
  labs(x="Number of Days Physical Health Bad",
       y="Number of Days Mental Health Bad", 
  title="The Scatter Plot:Bad Physical and Bad Mental Health")+
  theme(plot.title = element_text(size = 10))

#6. Create monthly summaries for both mental and physical health, 
#    show the result in a table.
head(oh_cleaned,3)

oh_summary=oh_cleaned %>% group_by(IMONTH) %>% 
  summarise(mean.physical=mean(PHYSHLTH2),sd.physical=sd(PHYSHLTH2),
            mean.mental=mean(MENTHLTH2),sd.mental=sd(MENTHLTH2))
colnames(oh_summary)[1]<-"Month" 
#change column name IMONTH to Month for better visualization
oh_summary
#7. Plot the above calculated monthly summaries relationship
#in one plot, where each month is distinguished by color.
ggplot() +
  geom_point(aes(x=mean.physical,y=mean.mental,color=Month),
             data=oh_summary,alpha=0.5)+
  labs(x="Monthly Average of Bad Physical Days",
       y="Monthly Average of Bad Mental Days",
 title="The Scatter Plot:Average Physical and Mental Days")+
  theme(plot.title = element_text(size = 10))
