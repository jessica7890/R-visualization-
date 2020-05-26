#Day04#
######################################
#1. Somthing related with last Friday
######################################
# (1) Colors
#scale_color_manual can be used for lines and points
# Refer for this for more details about the colors 
#https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
library(tidyverse)
ggplot() +
  geom_point(aes(x=carat,y=price,color=cut),data=diamonds) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442"))

# (2) Add title
ggplot() +
  geom_point(aes(x=carat, y=price,color=clarity), 
             data=diamonds) +
  labs(x="Carat", y="Price in US Dollars")+
  ggtitle("The scatter plot of Price (US$) and Carat")

?Orange
ggplot() +
  geom_point(aes(x=age, y=circumference), data=Orange)+
  labs(x="Age (day since 1968/12/31)", y="Circumference (mm)",
       title="Scatter plot of age and circumstance for Orange dataset")

#How about color-mark it by tree?
head(orange)
ggplot() +
  geom_point(aes(x=age, y=circumference, color=Tree), data=Orange)+
  labs(x="Age (day since 1968/12/31)", y="Circumference (mm)",
       title="Scatter plot of age and circumstance for Orange dataset")
# ???with "": just ignore it by default pink without"":  search for variables
#????below???Any question?
Orange$Tree2 <- ordered(Orange$Tree,levels=c(1,2,3,4,5))
Orange
Orange$Tree2
ggplot() +
  geom_point(aes(x=age, y=circumference, color=Tree), data=Orange)+
  labs(x="Age (day since 1968/12/31)", y="Circumference (mm)",
       title="Scatter plot of age and circumstance for Orange dataset")

######################################
#2. More on scatter plot
######################################
#(1) size/shape of point, Legends
#http://www.sthda.com/english/wiki/ggplot2-point-shapes

ggplot() +
  geom_point(aes(x=carat, y=price,color=cut),data=diamonds, size=2,shape=3) +
  labs(x="Carat", y="Price in US Dollars")+
  ggtitle("The scatter plot of Price (US$) and Carat colored by cut")+
  theme(legend.position = "bottom")#"top"

#size - (default: 0.5) diameter of the point
#shape - (default: 16=dot) the shape of the point

#Change name of the legend
ggplot() +
  geom_point(aes(x=carat, y=price,color=cut),data=diamonds, size=2,shape=3) +
  labs(x="Carat", y="Price in US Dollars",color="diamonds cut")+
  ggtitle("The scatter plot of Price (US$) and Carat colored by cut")+
  theme(legend.position = "bottom")#"top"

#(2) Display Layout panels in a grid
#https://plot.ly/ggplot2/facet_grid/
#facet_grid(var_vertical~var_horizontal)

#Vertical direction
ggplot() +
  geom_point(aes(x=carat, y=price,color=cut),data=diamonds, size=2,shape=3) +
  labs(x="Carat", y="Price in US Dollars",color="diamonds cut")+
  ggtitle("The scatter plot of Price (US$) and Carat colored by cut")+
  theme(legend.position = "bottom")+#"top"
  facet_grid(cut~.)

#Horizontal direction
ggplot() +
  geom_point(aes(x=carat, y=price,color=cut),data=diamonds, size=2,shape=3) +
  labs(x="Carat", y="Price in US Dollars",color="diamonds cut")+
  ggtitle("The scatter plot of Price (US$) and Carat colored by cut")+
  theme(legend.position = "bottom")+#"top"
  facet_grid(.~cut)

ggplot() +
  geom_point(aes(x=carat, y=price,color=cut),data=diamonds, size=2,shape=3) +
  labs(x="Carat", y="Price in US Dollars",color="diamonds cut")+
  ggtitle("The scatter plot of Price (US$) and Carat colored by cut")+
  theme(legend.position = "bottom")+#"top"
  facet_grid(.~clarity)


##########################################
#3.Visualizing frequency/amount--Bar graph (added later)
##(Fundamentals of datavis: Chapter 6)
##########################################

##3.1 Visualizing frequency
##########################
#(1) Make the graph
head(diamonds)
## What should we do with the "clarity" variable?
##   It is qualitative -- Make a bargraph--geom_bar
?geom_bar

ggplot() +
  geom_bar(aes(x=clarity),data=diamonds)
#can just specify what variable you're interested with. 
#geom_bar will display the groups in the x axis and y is the
#frequency of the group.

#(2) How is it made?
##Do I have other way to create the same graph?
table(diamonds$clarity)
clarity_df <- data.frame(table(diamonds$clarity))
clarity_df
str(clarity_df)
ggplot

# it's the summary dataset
ggplot( ) +
  geom_bar(aes(x=Var1),data=clarity_df)
# There are 8 levels, each appeared once.
ggplot( ) +
  geom_col(aes(x=var1,y=Freq),data=clarity_df)

#(3) Want to display the bars horizontally?



##3.2 Visualizing amount
##########################
#Visualize the median price for each type of cut?

