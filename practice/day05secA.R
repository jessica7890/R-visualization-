#Day05# geom_col,geom_bar, geom density, stat="identity",coord_flip(),
#stat="summary",fun.y=mean %>% , summary (group_by, summarise),

######################################
#1.Visualizing frequency/amount--Bar graph
##(Fundamentals of datavis: Chapter 6)
##########################################

###1.1 Visualizing frequency
######################################
##(1) work with summarized data
#summarized data-geom_col
library(tidyverse)
table(diamonds$clarity)
clarity_df <- data.frame(table(diamonds$clarity))
str(clarity_df)
clarity_df#it's the data already counted for us. 

ggplot() + 
  geom_col(aes(x=Var1,y=Freq), data=clarity_df)+
  labs(x="Clarity",y="Frequency")

#summarized data-geom_bar
#Or, you can specify the "identity" option in geom_bar.
#Since here, you're telling R which is the variable you're interested with, which
#is the value you want to display.
ggplot() + 
  geom_bar(aes(x=Var1, y=Freq),
           data=clarity_df,
           stat="identity") +
  labs(x="Clarity",y="Frequency")

##(2) Want to display the bars horizontally?
ggplot() + 
  geom_bar(aes(x=Var1, y=Freq),
           data=clarity_df,
           stat="identity") +
  labs(x="Clarity",y="Frequency")+
  coord_flip()

###1.2 Visualizing amount
##########################
##(1) Directly through geom_bar
#Visualize the mean price for each type of cut?
ggplot() +
  geom_bar(aes(x=cut,y=price),
           stat="summary",fun.y=mean,data=diamonds)

#median price per clarity type?
ggplot() +
  geom_bar(aes(x=clarity,y=price),
           stat="summary",fun.y=median,data=diamonds)

##(2) Create the summary data first, then plot
#The pipe, %>%, comes from the magrittr package, pronounce it as "then"
#Behind the scenes, x %>% f(y) turns into f(x, y), 
#and x %>% f(y) %>% g(z) turns into g(f(x, y), z) and so on. 

#group_by: group the data according to one/more variables 
#grouping doesn't change how the data looks (apart from listing how it's grouped):
diamonds2 <- diamonds %>% group_by(clarity)
head(diamonds)  
head(diamonds2)
# Groups:   clarity [5] means how many groups the displayed data has covered. 
clarity_summary <- 
  diamonds2 %>%
  summarise(clarity.median=median(price),
            clarity.mean=mean(price),
            n=n())

#compare the data
clarity_df
clarity_summary

#compare the graphs
#graph previously generated
ggplot() +
  geom_bar(aes(x=clarity,y=price),data=diamonds,
           stat="summary",fun.y=median)
#graph with the new data
ggplot() +
  geom_bar(aes(x=clarity,y=clarity.median),
           data=clarity_summary,stat="identity")


##########################################
#2.Visualizing distribution
##(Fundamentals of datavis: Chapter 7)
##########################################

##2.1 Histograms
##########################
?geom_histogram
ggplot() +
  geom_histogram(aes(x=price), data=diamonds)

## Note the warning 'bins=30' is the default. 
#We can improve (the appearance) of
## histograms by changing the binwidth parameter

#change color/binwidth:
ggplot() +
  geom_histogram(aes(x=price),
                 fill="purple", 
                 binwidth=100, 
                 data=diamonds)
#Follow similar rule before specify aesthestic options inside and outside aes()

#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#!# Your Turn - iris data 
?iris
head(iris)
# 1) histograms for petal length Faceted by species
ggplot() +
  geom_histogram(aes(x=Petal.Length),bins=50,data=iris)+
  facet_grid(Species~.)#(.~Species)

# 2) histograms for petal length filled by species
ggplot() +
  geom_histogram(aes(x=Petal.Length,fill=Species),bins=50,data=iris)


#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&

# An alternative to the histogram is a density plot

##2.2 Density plots
##########################
#kernel density estimates
?geom_density
#Please refer to the following for definition
#https://en.wikipedia.org/wiki/Kernel_density_estimation
ggplot()+
  geom_density(aes(x=carat),data=diamonds)

#Default bandwidth is the standard deviation of the smoothing kernel
#change bandwidth/size of the line (try adjust=0.2,1,3,6)
ggplot()+
  geom_density(aes(x=carat),size=1,adjust=.2,data=diamonds)
# How does this work?

# Using line instead of bars like histogram has advantage of stackability
ggplot()+
  geom_density(aes(x=carat, color=cut),
               size=1.5, adjust=1,
               data=diamonds)
