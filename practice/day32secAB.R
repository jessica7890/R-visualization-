# ----Day32--Week 11----#
#   Create network data #
#   Intro to animation  #
#-----------------------#
library(tidyverse)
### 1. Create a network data ###
################################
# install.packages("network")
# install.packages("sna")
library(network)
library(sna)

# (1.1) Generate the Adjacency matrix
######################################
?rgraph #generate bernoulli random graphs
randomgraph <- rgraph(10,mode="graph",tprob=0.2)
randomgraph
# "digraph" for directed data, "graph" for undirected data,
# by default it is mode="digraph", the matrix is not symmetirc
randomgraph <- rgraph(10,tprob=0.2) 
randomgraph
class(randomgraph)

# (1.2) Construct the network
##############################
?network #Construct, coerce to, test for and print network objects.
n <- network(randomgraph, directed = FALSE)
n # list the information in a graph. 
class(n)
sum(randomgraph) # If node 1, 2 and 2, 1 are all 1, then it's counted as 1 edge. 


# (1.3) Add vertex attributes
##############################
# add a categorical vertex attributes, called "family", with values "a","b" or "c" 
# add a continuous vertex attribute, called "importance", with values 1, 2, 3/ 
?set.vertex.attribute #set vertic attribute, (x, attrname, value)
?letters
letters[1:4]
set.vertex.attribute(n,"family",sample(letters[1:3],10,replace=TRUE))
# similarly, can use this
#n %v% "family" <- sample(letters[1:3], 10, replace = TRUE)

set.vertex.attribute(n,"importance",sample(1:3,10,replace=TRUE))
# similarly, can use this
#n %v% "importance" <- sample(1:3, 10, replace = TRUE)

# (1.4) Add edge attributes
##############################
# add a categorical edge attribute called "type", which is set to either "x", "y" or "z",
# and a continuous vertex attribute called "day", which is set to either 1, 2 or 3.
?network.edgecount #count how many edges in a network object
n
e <- network.edgecount(n)
set.edge.attribute(n, "type", sample(letters[24:26], e, replace = TRUE))
set.edge.attribute(n, "day", sample(1:3, e, replace = TRUE))

# (1.5) Make a network plot using ggnetwork
############################################
#install.packages("ggnetwork")
library(ggnetwork)
ggnetwork(n)
head(ggnetwork(n))
tail(ggnetwork(n))

ggplot(n, aes(x =x , y =y , xend =xend , yend = yend)) +
  geom_edges(aes(linetype=type),color="gray50") +
  theme_blank()

ggplot(n, aes(x =x , y =y , xend =xend , yend = yend)) +
  geom_edges(aes(linetype=type),color="gray50") +
  geom_nodes(aes(color=family, size=importance))
  theme_blank()

# Because ggplot2 follows Wilkinson’s grammar of graphics, it accepts 
# only one color scale. In the example above, that scale is mapped to a 
# vertex attribute, but it could have also been mapped to an edge attribute. 
# Mapping a color to both a vertex attribute and an edge attribute will create 
# a single color scale that incorrectly merges both attributes into one:
  


# Add the vertex names in the plot
  ggplot(n, aes(x =x , y =y , xend =xend , yend = yend)) +
    geom_edges(aes(linetype=type),color="black") +
    geom_nodes(color="black", size=8)+
    geom_nodetext(aes(color=family, label=LETTERS[vertex.names]),
                  fontface="bold")+
    theme_blank()
  


### 2. Animation ###
####################
# An animated chart displays several chart states one after the other.
# You can get a sense of different animated graphs here: https://www.r-graph-gallery.com/animation.html
# Please watch this video and we will try to create a simialr graph. 
# Hans Rosling: 200 years in 4 minutes - BBC News
# https://www.youtube.com/watch?v=Z8t4k0Q8e8Y&feature=youtu.be

# (2.1) Get the data
#####################
# We can get the data from https://www.gapminder.org/data/, which we have used it in previous class.

# Income
income <- read_csv("income_per_person_gdppercapita_ppp_inflation_adjusted.csv")

# Life expectancy
life <- read_csv("life_expectancy_years.csv")

# Population
pop <- read_csv("population_total.csv")

# Demographic location
library(readxl)
geographics <- read_excel("Data Geographies - v1 - by Gapminder.xlsx",
                          sheet=2)

head(income)
head(life)
head(pop)
head(geographics)

### Referecnes ###
#####################
#https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html
#https://www.jessesadler.com/post/network-analysis-with-r/

