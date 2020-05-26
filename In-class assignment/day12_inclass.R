library(tidyverse)
library(lubridate)
gdpdata <- read_csv("C:/Users/user/Desktop/2020spring/502 data visualization/ND000334Q.csv")
options(pillar.sigfig = 8)  

########################################
## 1. Edit the data, add year, month, quarter information. 
########################################
gdpdata2 <- gdpdata %>%
  mutate(Year=year(DATE),
         Month=month.abb[month(DATE)],
         Quarter=paste("Quarter",quarter(DATE)))
head(gdpdata2)


########################################
## 2. Change Data from Long to Wide format
########################################

# spread(): long-to-wide
# distributes a pair of key:value columns into a field of cells. 
# The unique values of the key column become the column names of the field of cells.


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
gdp_wide_quarter<-gdpdata2%>%
  select(Year,Quarter,ND000334Q) %>% 
  spread(key=Quarter,value=ND000334Q)

gdp_wide_quarter$'Quarter 1'
names(gdp_wide_quarter) <-str_replace_all(names(gdp_wide_quarter),c(" "=""))#c(" "="", ","="")
head(gdp_wide_quarter)