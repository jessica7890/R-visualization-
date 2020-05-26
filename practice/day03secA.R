######################################################
##Day 03 Factor variables, Plots  
#factor, ordinal, geom_point, geom_count
######################################################
#Factor variables
#################
x <- c(1,2,4,2,4,2,5,3,2,3,4,1)
class(x)
factor(x)
factor(x,labels=c("A","B","C","D","E"))

#Ordinal
color.vec <- c("Red","Blue","Orange","Blue","Red")
color.ord <- ordered(color.vec)
color.ord
color.ord <- ordered(color.vec,levels=c("Red","Blue","Orange"))
color.ord

#Advantages of using factor variables
# (1) Useful in statistical models, DF can be computed correctly.
# (2) Useful in different types of graphics
# (3) Storing string variables as factor variables is a more efficient use of memory. 

############################
##      Part 2: Plot      ##
############################
library(tidyverse)
?diamonds #look for the help file
diamonds
head(diamonds)
#tibble: morden version of data frames, they tweak some older behaviours to make life a little easier.
#refer to https://tibble.tidyverse.org/ for more details
head(diamonds, n=16)#typeof carat stores the internal storage mode of an object
tail(diamonds)
glimpse(diamonds)#This is like a transposed version of print(): 
#columns run down the page, and data runs across. This makes it possible to see every column in a data frame
str(diamonds)

##Scatter plot##
################
## Relationship between carat & price?

#ggplot2: a system for declaratively creating graphics.
#"gg": Grammar of Graphics. 
#You provide the data, tell ggplot2 how to map variables to aesthetics, 
#what graphical primitives to use, and it plots the data.

# first, start with a blank canvas
ggplot()

# Second, add a layer of geometric features to the blank canvas
# via "aesthetic mapping". Specify values through "aes()"
ggplot() +
  geom_point(aes(x=carat,y=price),data=diamonds)

## geom_point: add points
## potential problem:overplotting, summarise # of points at each location 
#and display that in some way, using geom_count()
#refer more here:https://ggplot2.tidyverse.org/reference/geom_point.html
#and https://ggplot2.tidyverse.org/reference/geom_count.html
ggplot() +
  geom_count(aes(x=carat,y=price),data=diamonds)
#geom_count:different size of dots depending of the size of n

## Add additional aesthetic options
#(1) color
ggplot() +
  geom_point(aes(x=carat,y=price),data=diamonds,color="red")

ggplot() +
  geom_point(aes(x=carat,y=price,color="blue"),data=diamonds)

#waht's wrong
#you can map (inside aes) a varialbe of your data to an aesthetic, e.g. (aes(color=cut))
#you can set (outside aes, but inside a geom element) an aesthetic to a constant value, e.g. "blue"
ggplot() +
  geom_point(aes(x=carat,y=price,color=cut),data=diamonds)

#if you don't like the color, you can also specify it
ggplot() +
  geom_point(aes(x=carat,y=price,color=cut),data=diamonds)+
  scale_color_manual(values=c("red","orange","yellow","green","blue"))
#Search for other color schemes
# REG triplet/hexadecimal format

#(2) transparancy scales
ggplot() +
  geom_point(aes(x=carat,y=price),data=diamonds,alpha=0.3)

# 1: not transparant, usually used to 
#down-weight less important observations

# (3) labels
#adding labels for the axes can also be regarded as a layer
ggplot() +
  geom_point(aes(x=carat,y=price),data=diamonds,alpha=0.3)+
  labs(x="Carat",y="Price in US Dollars")


#(4) Positions of the legends
