# -------Day16--Week 06-----------------------------------#
#   Fluctuation Diagrams, Mosaic Plot, Chisq test
#---------------------------------------------------------#

library(tidyverse)

## 1. Fluctuation Diagrams  ##
##############################

## Related with Day 13 heatmap and the scatterplot.
## Same idea as heatmap, but let the size, not color, 
## determine the intensity
## In a scatteplot, the size of the point reflects the intensity/freqency

## Note: If x & y were numeric variables, this is called
## a "bubble plot" (will be discussed in the future)
## More informaiton about bubble plot: 
## https://www.data-to-viz.com/graph/bubble.html

##--------------------------##
## Summary table from Day 13
summary <- diamonds %>%
  group_by(clarity, cut) %>%
  summarise(n=n())
summary
##---------------------------##


ggplot() + 
  geom_point(aes(x=clarity, y=cut, size=n), 
             shape=15, data=summary) + ## can customize
  scale_size_continuous(range=c(1,4)) + ## range specifies size of squares
  theme_bw()

## 2. Mosaic Plot  ##
#####################
## A mosaic plot is a graphical display of the cell 
## frequencies of a contingency table in which the area
## of boxes of the plot are proportional to the cell 
## frequencies of the contingency table.

## Review ##
##----------------------------------------------------##
## Stacked bar graph in Day 6
ggplot()+
  geom_bar(aes(x=cut, fill=clarity), position ="fill",
           data=diamonds)
##----------------------------------------------------##

##----------------------------------------------------##
## Summary2 table similar in Day 13
## conditional clarity proportions by cut
summary2 <- summary %>%
  group_by(cut) %>%
  mutate(cutcount= sum(n)) %>% #group_by(cut)->sum by cut
  mutate(prop= n/cutcount) %>% #same reason. prop by cut
  ungroup() %>%
  arrange(cut)
summary2
##----------------------------------------------------##

#install.packages("ggmosaic")
library(ggmosaic)
help(package="ggmosaic")

ggplot() +
  geom_mosaic(aes(weight=, x=, fill=), data=summary2)

?product ## a wrapper for a list
class(diamonds$cut)
class(product(diamonds$cut))

## I can also use geom_mosiac on the raw data, not the counted frequencies

ggplot() +
  geom_mosaic(aes(x=product(cut), fill=clarity), data=diamonds)
## The mosaic plot can show more types of relationships
## https://cran.r-project.org/web/packages/ggmosaic/vignettes/ggmosaic.html

ggplot() +
  geom_mosaic(aes(x=product(cut), fill=cut), data=diamonds)+
  coord_flip()

## 3. Chi-Squared Test of Independence ##
###########################

####################
## Pearson's Chi-square test review
?chisq.test()
# https://en.wikipedia.org/wiki/Chi-squared_test
# https://math.hws.edu/javamath/ryan/ChiSquare.html

## Are clarity and cut of diamonds "independent"?
summary
## I need a contigency table to do the test

claritycut_mat <- summary %>% 
  spread(key=,value=)
chisq.test(claritycut_mat)
# Error!
claritycut_mat # First column contains text, not numbers.
chisq.test()

# If I want to keep the information of which row is which clarity
rownames(claritycut_mat) <- claritycut_mat$clarity
# Warning message
# While a tibble can have row names (e.g., when converting 
# from a regular data frame), they are removed when subsetting
# with the [ operator. 

# It still works as of now but will be removed in future

# Solution: You can always coerce to a plain data frame via 
# as.data.frame() before setting row names. The plan is to 
# eventually get rid of row names entirely, and encourage 
# use of key columns instead; the deprecation warning will 
# become an error eventually.

# Supplement reading material: working with rownames in tibble:
# https://tibble.tidyverse.org/reference/rownames.html

claritycut_mat <- summary %>% 
  spread(cut,n) 

rownames(claritycut_mat) <- claritycut_mat$clarity

claritycut_mat <- claritycut_mat %>%
  select(-clarity)

claritycut_mat

chisq.test(claritycut_mat)
## We rejct the null hypothesis and find clarity and cut 
## are dependent (p-value < 2.2*10^(-16) on 28 degrees 
## of freedom)