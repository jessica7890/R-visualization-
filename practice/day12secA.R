library(tidyverse)
library(lubridate)
gdpdata <- read_csv("ND000334Q.csv")
options(pillar.sigfig = 8)  

########################################
## 1. Edit the data, add year, month, quarter information. 
########################################
gdpdata2 <- gdpdata %>%
  mutate(Year=year(DATE),
         Month=month.abb[month(DATE)],
         Quarter=paste("Quarter",quarter(DATE)))
head(gdpdata2)

gdpdata %>% 
  mutate(Year=str_sub(DATE,1,4),
         Month=month(DATE, label=TRUE, abbr=FALSE),
         Month2=str_sub(DATE,-5,-4),
         Quarter=paste("Quarter",quarter(DATE),sep="?@"))

# str_sub is from the stringr package. 
# syntax: str_sub(string, start=, end=)
# It can be used to substring some characters.
# str_trim sometimes is useful to remove the spaces before or after



########################################
## 2. Change Data from Long to Wide format
########################################

# spread(): long-to-wide
# distributes a pair of key:value columns into a field of cells. 
# The unique values of the key column become the column names of the field of cells.
########################################
?spread()
gdpdata2
gdpdata2%>%
  spread(key=Month, value=ND000334Q)

gdp_wide_month <- gdpdata2%>%
  select(Year,Month,ND000334Q) %>% 
  spread(key=Month, value=ND000334Q)

# this is the same as
gdpdata2%>%
  select(-DATE,-Quarter) %>% 
  spread(key=Month, value=ND000334Q)


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

gdp_wide_quarter <- gdpdata2 %>% 
  select(Year,Quarter,ND000334Q) %>% 
  spread(key=Quarter,value=ND000334Q)
#<- df[ df$v1 != " " , ]
gdp_wide_quarter
names(gdp_wide_quarter) <- str_replace_all(names(gdp_wide_quarter),c(" "="")) #c(" "="", ","="")
names(gdp_wide_quarter)
head(gdp_wide_quarter)
#str_replace_all is in the stringr package
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Take a look at gdp_wide_month, any problem?
# reorder the columns
gdp_wide_month


# 3. Change Data from Wide to Long format
#########################################

# gather() collects a set of column names and places them into a single "key" column. 
# It also collects the cells of those columns and places them into a single value column. 
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

# Change the character date to Date type


gdpdata


# 4. More Date/Time information
###############################
# Portable Operating System Interface (POSIX) is a family 
# of standards specified by the IEEE Computer Society for 
# maintaining compatibility between operating systems.

# POSIX data type(date+time observation)
now = Sys.time()
class(now)
time <- ymd_hms("2010-12-13 15:30:30")
time
class(time)
is.Date(time)

# https://garthtarr.github.io/meatR/datetime.html
# These objects store the number of seconds (for POSIXct) or 
# days (for Date) since January 1st 1970 at midnight.
# When printed on the console, they are displayed in a 
# human-readable format along the ISO 8601 standard:


# Change characters to POSIX time #
#---------------------------------#
# https://www.gormanalysis.com/blog/dates-and-times-in-r-without-losing-your-sanity/
class("2020-02-21 08:10:05")
x <- as.POSIXct("2020-02-21 08:10:05")
x
class(x)
#But the following has problem
x <- as.POSIXct("Feb/01/19, 05-30-00")
x
#Specify the format to read in
y=as.POSIXct("Feb/01/19 05-30-00", format = "%b/%d/%y %H-%M-%S")
y
as.POSIXct("Feb/01/19", format = "%b/%d/%y")
x-y


# Easier way using functions in lubridate package  #
#--------------------------------------------------#
day<-"21/02/2020"
parse_date_time(day,orders="dmy")
# parse_date_time always assigns the "UTC" time zone,
# "UTC" stand for "Coordinated Universal Time" and is the 
# time at the 0 meridian; somewhat similar to Greenwich Mean
# Time or "GMT".
class(parse_date_time(day,orders="dmy"))
as.Date(parse_date_time(day,orders="dmy")) 
# if you don't want to display the time zone.

parse_date_time("25-05-17 20-10-05", orders = "dmy HMS")


# Change time zone to data
x <- ymd_hms("2020-02-20 20:10:05", tz="America/New_York")
x
class(x)

