#Day 06#
##########################################
#1.Visualizing distribution---continued
##(Fundamentals of datavis: Chapter 7)
##########################################
# copied from Day05
head(iris)
colnames(iris)=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width","Species")

ggplot()+
  geom_point(aes(x=Sepal.Length,y=Sepal.Width,color=Species),data=iris)+
  theme_bw()+panel.grid.minor = element_blank()
p1 <- ggplot(mtcars, aes(wt, mpg)) +
  geom_point()
p1 + theme(axis.line = element_line(size = 3, colour = "grey80"))
p1 + theme(axis.line = element_line(colour = "black"),panel.grid.major=element_line(color="lightblue"),panel.grid.minor = element_blank())

+axis.line = element_line(colour = "black")
  panel.border = element_blank()

?iris
library(tidyverse)
# 2) histograms for petal length filled by species
str(iris)
ggplot() +
  geom_histogram(aes(x=Petal.Length,fill=Species),bins=50,data=iris)

##1.2 Density plots
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
  geom_density(aes(x=carat),size=1,adjust=1,data=diamonds)
#adjust: bigger-> smoother , size: thickness

# Using line instead of bars (e.g. histogram) has advantage of stackability
ggplot()+
  geom_density(aes(x=carat,color=cut),
               size=1.5, adjust=1,
               data=diamonds)

#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#Your Turn - iris data again
# 1) Density plots for petal length filled by species with alpha scaling of 0.5
ggplot()+
  geom_density(aes(x=Petal.Length,fill=Species),data=iris,
               alpha=0.5)


## You should see a clear overlap in petal lenghts between the versicolor and 
## virginica Iris plants
#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&

##1.3 Boxplot
##########################
#Median, hinge: (Q1, Q3), whiskers: (Q1-1.5*IQR, Q3+1.5*IQR) 
# IQR=Q3-Q1
# Simple boxplot for price
ggplot()+
  geom_boxplot(aes(y=price), data=diamonds)

# side-by-side for each clarity category
ggplot()+
  geom_boxplot(aes(x=clarity, y=price), data=diamonds) 
# ggplot()+
#   geom_density(aes(x=price), data=diamonds)+
#  facet_grid()

#Any problems with the data?

# Options: scale, color, fill, orientation
ggplot()+
  geom_boxplot(aes(x=clarity, y=log10(price)), data=diamonds) +
  coord_flip() +
  labs(x="Cut Quality", 
       y="Price (log10USD)",
       title="Boxplot of prices by clarity")


#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&
#Your Turn - iris data 
# 2) Make a boxplot for petal length by species
str(iris)
ggplot()+
  geom_boxplot(aes(x=Species, y=log10(Petal.Length)), data=iris) 
coord_flip() +
  labs(x="species", 
       y="Petal length (log10)",
       title="Boxplot of prices by clarity")



#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&#&


##1.4 Violin plots
####################################################
#(like side-by-side boxplot with reflected density plots)
#Violin plots are similar to box plots, except that they also 
#show the probability density of the data at different values, 
#usually smoothed by a kernel density estimator.
ggplot()+
  geom_violin(aes(x=cut, y=carat),
              color="red", fill="lightblue",
              data=diamonds) +
  labs(y="Carat Weight", x="Cut Quality",
       title="Violin Plots of Diamond Weights by Cut Quality")+
  coord_flip()+
  theme_bw()
#color: controls the color of the border, fill controls what 
#fill: controls what color fills the violine
#theme_bw(): change the theme of the background

# Combine violins with boxplots
ggplot()+
  geom_boxplot(aes(x=cut, y=carat), width=.1,
               color="black", fill="red",
               data=diamonds)+
  geom_violin(aes(x=cut, y=carat),
              color="black", fill="lightblue",
              data=diamonds) +
  
  theme_bw() +
  labs(y="Carat Weight", x="Cut Quality")+
  coord_flip()

#Anything wrong? How to fix it?
ggplot()+
  geom_violin(aes(x=cut, y=carat),
              color="black", fill="lightblue",
              data=diamonds) +
  geom_boxplot(aes(x=cut, y=carat), width=.1,
               color="black", fill="white",
               data=diamonds)+
  theme_bw() +
  labs(y="Carat Weight", x="Cut Quality")+
  coord_flip()


#You can also customize the graph by chaning color, scale, fill, etc
#scale_fill_manual() for box plot, bar plot, violin plot, dot plot, etc

##########################################

#2 Displays for Bivariate Categorical Data
##(Fundamentals of datavis: Chapter 6)
##########################################

#Explore the data
table(diamonds$clarity, diamonds$cut)
str(diamonds$clarity)

##2.1 Grouped bargraphs (similar to faceting)
#############################################

ggplot()+
  geom_bar(aes(x=cut, fill=clarity), 
           stat="count",
           position ="dodge",
           data=diamonds)
#you may also omit the stat="count"
#"dodge" display the graph side by side.

ggplot()+
  geom_bar(aes(x=clarity, fill=clarity), 
           data=diamonds, color="black") +
  facet_grid(cut ~ . )#, scale="free_y")
#"free_y" means that each row of facets can set its y-axis 
#independent of the other rows. 


##2.2 Stacked bargraphs 
#############################
#show breakdown of a second categorical variable within each bar.
# Stacked-Bargraph 
ggplot()+
  geom_bar(aes(x=cut, fill=clarity), 
           stat="count",position="stack",
           data=diamonds)

# Proportionally Stacked-Bargraph
ggplot()+
  geom_bar(aes(x=cut, fill=clarity), 
           stat="count",position ="fill",
           data=diamonds)
#calculate the proportion (clarity.cut/ cut) for each value of cut,
#and to do that it uses the count statistic,
#but the actual value represented by the cumulative height of
#the different color bars is not a count.


##########################################
#3 Some data manipulation--more next Monday
##########################################
#How many of the diamonds have clarity type VS1 or VVS2?
#What's the 70th percentile of carat for those diamonds?
# diamonds %>% 
#   dplyr::filter(clarity=="VS1"|clarity=="VVS2")%>%
#   summarize(quantile())#explore by yourself

#What type of cut and clarity combination have
#the highest 3 mean price?