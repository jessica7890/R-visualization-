#DAY 2 Introduction to data types#
############################
## Part 1: Basic R stuff ###
############################
#1. Calculator and other stuff
2+3*4
log(34)
exp(3)
# You may use # to comment your R script. 
# If you want to comment/uncomment a few lines together, use Ctrl+Shift+C (Window) or Command+Shift+C (Mac).

#2. Assignment 
#You can assign numbers, characters, etc to an object in R.
#Assignment is done via the <-operator (shortcut: Alt" + "-" (windows) and "Option"+"-" (mac))
#Run your code: Ctrl+enter(windows) Alt+enter (mac)
x <- 23
x
xx <- x^2
xx
test <- "Math"
test
test.score <- 93.24
test.score
# Acceptable name may be made out of letters, numbers, and the period(.) symbol, 
#but names must start with a letter.
test2 <- 2
#2test <- 3

#vectors
y <- c(1,3,4,5,3) #via c() function
yy <- seq(from=0,to=6,by=0.1)
yy[10]
mean(y)
sd(y)
# R uses the notation `[n]' at the beginning 
# of each output line to indicate which entries are shown on that line.
test.subjects <- c("Economics","Math","English")
test.subjects

#3. Different data types
#(1) vector-numeric, character,logic, etc
class(x)
z <- 1:4
class(z)
w <- c("a","b","c")
w
class(w)
h <- c(TRUE,FALSE,TRUE)
h
class(h)
#create a vector type by yourself
treatment <- c("A","B","C","A","B","C")
class(treatment)
class(treatment) <- "group"
class(treatment)
#(2) Matrix
mat <- matrix(c(seq(1,17,1),rep(2,3)),nrow=4)
mat
mat2 <- matrix(c(seq(1,17,1),rep(2,3)),nrow=4,byrow=TRUE)
mat2
mat2 <- matrix(c(seq(1,17,1),rep("2",3)),nrow=4,byrow=TRUE)
mat2

#(3) Arrary-can have multiple dimenions, 
#but generally similar like matrix
array1 <- array(seq(1,30,1),c(2,3,5))
array1
array2 <- array(1:3,c(2,4))
array2
#(4) Data frame
# Similar as matrix, Different columns can have different characters
test.data <- data.frame(Subjects=c("Economics","Math","English"),
                        Registered=c(TRUE,TRUE,FALSE),
                        Score=c(89,93,NA))
test.data

abc<-data.frame(sex=c("male","female"),
                score= c(99,100))
abc
class(test.data)
str(test.data)
class(test.data$Registered)
class(test.data$Subjects)
class(test.data$Score)
#Question: what is a factor?

#(5) Factor -refer to this website for more details
#Berkeley:https://www.stat.berkeley.edu/~s133/factors.html
# UCLA: https://stats.idre.ucla.edu/r/modules/factor-variables/
# Factor variables are categorical variables that can be either numeric or string.
#categorical variable: a.without order --nominal -- Red, Blue, Orange
#                      b.with order --  ordinal -- Freshman Sophomore, Junior,Senior
#                      

#Nominal
color.vec <- c("Red","Blue","Orange","Blue","Red")
class(color.vec)
color.fac <- factor(color.vec)
color.fac
#the levels are ordered according to the alphabetical order of 
#all the elements appeared in color.vec. Want to change?
factor(color.vec,levels=c("Red","Blue","Orange"))

#Ordinal


#Advantages of using factor variables
# (1) Useful in statistical models, DF can be computed correctly.
# (2) Useful in different types of graphics
# (3) Storing string variables as factor variables is a more efficient use of memory. 

############################
##      Part 2: Plot      ##
############################
library(tidyverse)
?diamonds #look for the help file
head(diamonds)
#tibble: morden version of data frames, they tweak some older behaviours to make life a little easier.
#refer to https://tibble.tidyverse.org/ for more details
head(diamonds, n=10)#typeof carat stores the internal storage mode of an object
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


# Second, add a layer of geometric features to the blank canvas
# via "aesthetic mapping". Specify values through "aes=()"

## geom_point: add points
## potential problem:overplotting, summarise # of points at each location 
#and display that in some way, using geom_count()
#refer more here:https://ggplot2.tidyverse.org/reference/geom_point.html
#and https://ggplot2.tidyverse.org/reference/geom_count.html



## Add additional aesthetic options
#color



#transparancy scales

# 1: not transparant, less than 1: transparant, usually used to 
#down-weight less important observations

## adding labels for the axes can also be regarded as a layer

