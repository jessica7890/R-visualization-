#Day10--Week 04------------------#
#Data manipulaiton, Tidyr, line plots#
#--------------------------------#
#-------------------------------------------------------------------------------------------------------------------
##############################################
#1. Day09 Inclass Assignment
##############################################

#(1) Read data in R
library(tidyverse)
oh_brfss <- read_csv("https://raw.githubusercontent.com/qyuan20/Datasets-DataVis/master/ohioBRFSS2018.csv")
#Or
#oh_brfss <- read_csv("ohioBRFSS2018.csv")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
### Manipulation of the BRFSS data:
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
#    in one plot, where each month is distinguished by color

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Q0-Q3
oh_clean <- oh_brfss %>%
  select(IDATE, IMONTH, IDAY, PHYSHLTH, MENTHLTH) %>%       ## Select relevant columns
  mutate(PHYSHLTH = ifelse(PHYSHLTH==88,0,PHYSHLTH),        ## convert "none", 88, to zeros
         MENTHLTH = ifelse(MENTHLTH==88,0,MENTHLTH)) %>%
  dplyr::filter(PHYSHLTH <= 30, MENTHLTH <=30)   
#filter(PHYSHLTH <= 30 & PHYSHLTH>=0, MENTHLTH <=30 & MENTHLTH >=0)   

## Remove the "don't know" and "missing"
# Q4
head(oh_clean)   ## Looks correct

# Q5
#Start with the following scatter plot
ggplot()+
  geom_point(aes(x=PHYSHLTH,y=MENTHLTH), data=oh_clean)+
  theme_classic() +
  labs(x="Number of Days with poor Physical Health",
       y="Number of Days with poor Mental Health",
       title="Results from Behavioral Risk Factor Surveillance System",
       subtitle="State of Ohio, 2018")

ggplot()+
  geom_point(aes(x=PHYSHLTH,y=MENTHLTH), alpha=0.1,data=oh_clean)+
  theme_classic() +
  labs(x="Number of Days with poor Physical Health",
       y="Number of Days with poor Mental Health",
       title="Results from Behavioral Risk Factor Surveillance System",
       subtitle="State of Ohio, 2018")

# There're many options for themes. 
# Refer to 

ggplot()+
  geom_count(aes(x=PHYSHLTH,y=MENTHLTH),data=oh_clean)+
  theme_bw() +
  labs(x="Number of Days with poor Physical Health",
       y="Number of Days with poor Mental Health",
       title="Results from Behavioral Risk Factor Surveillance System",
       subtitle="State of Ohio, 2018")

# There are multiple observations for many PHYSHLTH and MENTHLTH combinations.
# also, the values are discrete (can only hit integer number of days),
# We can add a little bit of 'jittering' and alpha scaling

# geom_jitter
# It adds a small amount of random variation to the location of each point,
# and is a useful way of handling overplotting caused by discreteness in smaller datasets.
ggplot()+
  geom_jitter(aes(x=PHYSHLTH,y=MENTHLTH),alpha=.2, data=oh_clean)+
  theme_bw() +
  labs(x="Number of Days with poor Physical Health",
       y="Number of Days with poor Mental Health",
       title="Results from Behavioral Risk Factor Surveillance System",
       subtitle="State of Ohio, 2018")

# I include both jittering and alpha-scaling on the points
# In doing so, I can see where clusters appear to form.
# For example, many people report 0 days of poor mental and physical health
# and many report 30 days of both.  With these two features, you can clearly
# see this behavior.

# Q6
oh_monthly <- oh_clean %>%
  group_by(IMONTH) %>%                      ## Group by the month
  summarize(avg_phys = mean(PHYSHLTH),      ## calculate the 
            avg_ment = mean(MENTHLTH))      ## monthly averages
oh_monthly

# Q7
ggplot() + 
  geom_point(aes(x=avg_phys, y=avg_ment, color=IMONTH), 
             data=oh_monthly, size=1.75) +
  labs(x="Average Number of Days with Poor Physical Health",
       y="Average Number of Days with Poor Mental Health",
       title="Results from Behavioral Risk Factor Surveillance System",
       subtitle="State of Ohio, 2018",
       color="Month") +
  theme_bw()


##############################################
#2. Learning to work with tidyr (and lineplots!)
##############################################
# 2.1 tidyr?
#############
# The goal of tidyr is to help you create tidy data. 
# Tidy data is data where:
#  Every column is variable.
#  Every row is an observation.
#  Every cell is a single value.

# The tidyr package (part of tidyverse) essentially contains two functions
#   spread() -- Spreads key-value pairs across multiple columns
#   gather() -- combines multiple columns into key-value pairs


# 2.2 The GDP data
##################
# The following is from FRED (Federal Reserved Bank Ecnomonic Data)
# https://fred.stlouisfed.org/series/ND000334Q
# Download it directly from the website, or from canvas.
setwd("C:/Users/user/Desktop/2020spring/504 data visualization")
gdpdata <- read_csv("ND000334Q.csv")

gdpdata
gdpdata$ND000334Q
options(pillar.sigfig = 8)  
## Sometimes the numbers after the decimal points are
## not displayed. Can adjust the length you want to see
## pillar.sigfig: The number of significant digits that 
## will be printed and highlighted, default: 3
gdpdata

# 2.3 Make a line plot (quarterly)
######################
#install.packages("ggthemes")
library(ggthemes) ## To add some additional themes

ggplot() +  
  geom_line(aes(x=DATE, y=ND000334Q), data=gdpdata) +
  labs(x="Year", y="Billions of Chained 2012 Dollars")+
  ggtitle("Quarly Real Gross Domestic Product", 
          subtitle ="From 2002 to 2019, not seasonaly adjusted")+
  theme_economist()

# Add title/subtitle: https://www.datanovia.com/en/blog/ggplot-title-subtitle-and-caption/
# theme_economist(): refer to https://www.datanovia.com/en/blog/ggplot-themes-gallery/#theme_economist
# Chained dollars is a method of adjusting real dollar 
# amounts for inflation over time, so as to allow comparison of figures from different years.


# 2.4 Make a line plot (annual average)
#########################################

## (Task 1): Compute the average annual GDP
#-----------------------------------------
gdpdata %>% 
  group_by(DATE) %>% 
  summarize(mean(ND000334Q))
# What's wrong with my calculation?
class(gdpdata$DATE)
# Dates are represented as the number of days since 1970-01-01, 
# with negative values for earlier dates.
library(lubridate)
today <- Sys.Date()
today
is.Date(today)
?is.Date
# Refer to this for specific date formats.
# https://www.statmethods.net/input/dates.html



# R lubridate package--contains functions to work with date and time.
# install.packages("lubridate")
library(lubridate)
# https://lubridate.tidyverse.org/ refer to the descriptions and cheatsheet



## (Task 2): Make the line plot
#-----------------------------------------
ggplot() +  
  geom_line(aes(x=Year, y=meangdp), data=gdp_annual) +
  theme_economist() + 
  labs(x="Year", y=" Billions of Chained 2012 Dollars")+
  ggtitle("Annually Real Gross Domestic Product", 
          subtitle = "From 2002-01-01 to 2019-10-01, not seasonally adjusted")

head(gdpdata)
# the gdpdata is called tall-format data


# 2.5 Change long-format data to wide-format
# spread() = tall-to-wide
#########################################
# (1) Add the month, quarter information. 
gdpdata2 <- gdpdata %>% 
  mutate(Year=,
         Month=,
         Quarter=)
paste("Quarter",3)
paste0("Quarter",3)
# paste: Concatenate vectors after converting to character.
#        syntax: paste (…, sep = " ", collapse = NULL)
# paste0: is equivalent to paste(…, sep = "", collapse), 
#         slightly more efficiently.
# month.abb: the three-letter abbreviations for the English month names;


# (2) Use the spread() to do the change
?spread()
gdpdata2%>%
  spread(Month, ND000334Q)

gdp_wide_month <- gdpdata2%>%
  select(Year,Month,ND000334Q) %>% 
  spread(Month, ND000334Q)


# this is the same as
gdpdata2%>%
  select(-DATE,-Quarter) %>% 
  spread(Month, ND000334Q)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##Your Turn##
#Change data from long to wide in terms of quarter

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# (3) Take a look at gdp_wide_month, any problem?
# reorder the columns
gdp_wide_month


# 2.6 Change wide-format to long-format
# gather() = wide-to-tall
#########################################
?gather()
# (1) The following codes are equivalent
gdp_tall <- gdp_wide_month %>% 
  gather() 
gdp_tall




#Now very close to the gdpdata, but not exactly. 

# (2) Add the DATE variable
gdpdata


