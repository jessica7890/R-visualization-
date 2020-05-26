# --------------Day28--Week 10-------------------#
# Multivariate Displays, Aggregation Based Plots #
#------------------------------------------------#

library(tidyverse)
## 1. Scatterplot Matrix ##
###########################
# Scatterplot matrix explores all pairwise relationships quickly
# make many pairwise plots and organize into grid of scatterplots
# install.packages("GGally")
library(GGally)
?ggscatmat
library(ISLR)
College
glimpse(College)
# Can directly function on the whole dataset.
# ggscatmat(College) 
ggscatmat(College, columns=, color = "")+
  theme_classic() #This is a ggplot object, can change the theme

## 2. Parallel Coordinate Plot ##
#################################
# Organize many numeric axes in parallel (instead of orthogonal)
# Connect observation values with line across all axes
?ggparcoord
ggparcoord(College, columns=2:10)

ggparcoord(College, columns=2:10, groupColumn=,
           alpha=)

## Display Miami in a different color




ggparcoord(, columns=2:10, groupColumn=1,
           alpha=0.6)+
  scale_color_manual(values=c("pink", "pink4", "black")) 
# Since R display the graphs in order, so need to change the order
# of the observations. 



## 3. Aggregation Based Plot ##
###############################
load("Jan2020Flight.Rdata")
head(delay)
glimpse(delay)


# # 
# From the Federal Airlines Administration (FAA), flight records for every U.S.
# air flight for the month of August 2019. The original CSV file is 80MB. 
# The `.RData` file has been trimmed and is in R format.
# The original data can be obtained here for those interested:
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time

#-----------------------------------------------
# ### Unaggregated plots (run at your own risk)
# 
# # scatterplot of Departure Delays vs Arrival Delays
# # (Don't add alpha blending. It will probably break your computer.
# #  Since there are too many points need to be displayed.)
# ggplot()+
#   geom_point(aes(x=DEP_DELAY,y=ARR_DELAY), data=delay)
# 
# # Time vs Arrival Delays
# ggplot()+
#   geom_point(aes(x=FL_DATE,y=ARR_DELAY), data=delay)
#------------------------------------------------

# (3.1) Some Concepts about Aggregation
# Q1: What is aggregation? 
# A: 
#------------------------------------------------

# Q2: Many of the plots we already know rely on aggregation: Brainstorm which ones?
# A: 
#------------------------------------------------

# Q3: What are those plots used for?
# A: Common goal of all of these plots to create a geometric objects 
# with attributes based on summaries instead of raw data
#------------------------------------------------

# Histograms do binning on one dimension



# (3.2) Binned Scatterplots 
# (heatmap-style 2-dimensional histogram)
ggplot()+

  
  
  
  theme_bw()+
  scale_fill_continuous(low="gray90",
                        high="gray10",
                        trans="log10")+
  labs(x="Departure Delay (Scheduled - Actual)",
       y="Arrival Delay (Scheduled - Actual)",
       title="Relationship between Arrival and Departure Delays")



# Your turn: Create the following plots
# (easy) barplot showing the number of flights per carrier 


# (hard) boxplots of departure hour per state. The state names should have format "Ohio",
# "Indiana", etc. Each boxplots shold lay horizontally and they are ordered 
# by decending median departure hour

# Hint:
# (1) You will need to create a numeric version of the department time, `dep_hour+dep_min/60`.
# (2) Look into using the `reorder` function in R, you need to pass in four variables to 
# get it to work correctly, including `na.rm=TRUE`.
