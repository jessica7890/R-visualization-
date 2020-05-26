# -------Day11--Week 04----------#
# Tidyr, line plots, time, 
# long/wide format data conversion#
#--------------------------------#

######################
# GDP data from Day10
######################
# The following is a FRED (Federal Reserved Bank Ecnomonic Data)
# https://fred.stlouisfed.org/series/ND000334Q
library(tidyverse)
library(ggthemes)
gdpdata <- read_csv("ND000334Q.csv")
options(pillar.sigfig = 8)  #how many decimal points to display
gdpdata

# 1. Make a line plot (annual average)
#########################################

## (Step 1): Compute the average annual GDP
#-----------------------------------------
gdpdata %>% 
  group_by(DATE) %>% 
  summarize(mean(ND000334Q))
# What's wrong with my calculation? 
class(gdpdata$DATE)
# What is "Date" type?

# Working with Dates
#####################
# Dates are represented as the number of days since 1970-01-01, 
# with negative values for earlier dates.
today <- Sys.Date() #returns the current date
today
class(today) 
date() #returns current date and time

# format() function
# It formats and R object for pretty printing.
format(today,"%A,%B-%b-%Y")
# Refer to this for specific date formats.
# https://www.statmethods.net/input/dates.html

# The following example is to show the date is stored in R as integer values clearly.
strDates <- c("01/05/2019","02/19/2020")
strDates
strDates[2]-strDates[1]
dates <-as.Date(strDates,"%m/%d/%Y")  ## Convert character string to date variable.
class(dates)
dates
dates[2]-dates[1]

# Want to work on more things about dates.
# R lubridate package--contains functions to work with date and time.
########################
library(tidyverse)
install.packages("lubridate")
library(lubridate)
# https://lubridate.tidyverse.org/ refer to the descriptions and cheatsheet
is.Date(today)
is.Date(gdpdata$DATE)

as_date(12345)
as_date(-12345)
hms::as_hms(32)

# a hms is a time stored as the number of seconds since 00:00:00
# hms is a package inside lubridate.

"2020Feb19"
ymd("2020Feb19") # By default, it uses "-" to link these numbers.
mdy_hms("02042020 13:30:25")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
# Make the line plot using annual GDP data.
## (Step 1): Generate a summarized data containing the average annual GDP
gdpdata$year<-year(gdpdata$DATE)
head(gdpdata)
avgGDP<-gdpdata %>% group_by(year) %>% 
  summarise(mean.annualGDP=mean(ND000334Q))
avgGDP
#professor's answer below
gdp_annual<-gdpdata %>% 
  mutate(Year=year(DATE)) %>% 
  group_by(Year) %>% 
  summarise(meanGDP=mean(ND000334Q))


## (Step 2): Make the line plot
ggplot() +  
  geom_line(aes(x=Year, y=meanGDP), data=gdp_annual,size=1.6) +
  labs(x="Year", y="Billions of Chained 2012 Dollars")+
  ggtitle("Annual Average Real Gross Domestic Product ", 
          subtitle ="From 2002 to 2019, not seasonaly adjusted")+
  theme_few()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



# 2. Wide and Long format data 
#########################################
# From wikipedia (https://en.wikipedia.org/wiki/Wide_and_narrow_data)

# Wide data (unstacked data)
# Each different data variable in a separate column.
# 
# Person	Age	  Weight	Height
# Bob   	32	  128	    180
# Alice	  24	  86	    175
# Steve	  64	  95	    165
#
# ---------------------------------------------------
#
# Long (Narrow, stacked data)
# One column containing all the values and 
#another column listing the context of the value
# 
# Person	Variable	Value
# Bob	    Age	      32
# Bob   	Weight	  128
# Bob	    Height	  180
# Alice 	Age	      24
# Alice	  Weight	  86
# Alice	  Height	  175
# Steve	  Age	      64
# Steve	  Weight	  95
# Steve	  Height	  165
# This is often easier to implement; 
# addition of a new field does not require any changes to the structure of the table, however it can be harder for people to understand.



# 3. Change long-format data to wide-format
#########################################

head(gdpdata)
# the gdpdata is long-format data, but we need to add more information

# 3.1 Add the year, month, quarter information. 
########################################

gdpdata2 <- gdpdata %>% 
  mutate(Year=year(DATE),Month=month.abb(DATE),
         Quarter=paste("Quarter",quarter(DATE)))
gdpdata2
paste("Quarter",3,"more")
paste0("Quarter",3)
# paste: Concatenate vectors after converting to character.
#        syntax: paste (…, sep = " ", collapse = NULL)
# paste0: is equivalent to paste(…, sep = "", collapse), 
#         slightly more efficiently.
# month.abb: the three-letter abbreviations for the English month names
head(gdpdata2)


# 3.2 Use the spread() to do the change
# spread(): long-to-wide
# distributes a pair of key:value columns into a field of cells. 
# The unique values of the key column become the column names of the field of cells.
########################################

?spread()
#spread(data,key,value)
# We need to figure out what is key and what is value
# A key value pair is a simple way to record information. 
# key: explains what the information describes;
# value: contains the actual information.
# Question: in the above long format data, which column contains the key, which column contains the value?
# What about the gdp data?

gdpdata2%>%
  spread(key=, value=)

gdp_wide_month <- gdpdata2%>%
  select() %>% 
  spread()

# this is the same as
gdpdata2%>%
  select() %>% 
  spread()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
# Use the gdpdata2, create a table gdp_wide_quarter with head as follows:
# # A tibble: 6 x 5
# Year Quarter1 Quarter2 Quarter3 Quarter4
# <dbl>  <dbl>  <dbl>    <dbl>    <dbl>
# 1  2002 3264.061 3363.525 3401.8   3463.678
# 2  2003 3342.362 3432.912 3501.294 3602.561
# 3  2004 3486.566 3578.905 3617.1   3723.813
# 4  2005 3605.815 3703.346 3768.864 3834.484
# 5  2006 3737.475 3841.314 3836.379 3923.089
# 6  2007 3792.335 3897.638 3922.29  4013.767






#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# 3.3 Take a look at gdp_wide_month, any problem?
# reorder the columns
gdp_wide_month
gdp_wide_month <- gdp_wide_month[,c("Year","Jan","Apr","Jul","Oct")]


# 4. Change wide-format to long-format
#########################################
# gather(): wide-to-long
# gather() collects a set of column names and places them into a single “key” column. 
# It also collects the cells of those columns and places them into a single value column. 
#########################################

# 4.1 Change gdp_wide_month to long format
#########################################
?gather()
# (1) The following codes are equivalent
gdp_wide_month %>% 
  gather(key="", value="", ) 



#This is the same as
gdp_long <- 
  
  
head(gdp_long)
#compare with
head(gdpdata)
# Question: gdp_long looks very similar to gdpdata, but not exactly. 
# Can we edit it to look exactly the same?

# (2) Add the DATE variable
gdp_long2 <- gdp_long %>%
  mutate(Date.string = paste0(Year, "/", Month, "/01"))
head(gdp_long2)


gdpdata

