# ----Day27--Week 09----#
# Multivariate Displays #
#-----------------------#
library(tidyverse)
# install.packages("ISLR")
library(ISLR)
?College

# College data consists of 777 colleges.
head(College)
names(College)

## 1. Bubble Plot ##
####################
# Extends a scatterplot into 3 dimensions by utilizing 
# the size of the point to determine a third variable

## (1.1) Basic bubble plot
ggplot() + 
  geom_point(aes(x=Expend, y=Grad.Rate, 
                 size=Enroll),alpha=0.4,data=College)

# Question: anything wrong in the plot?
max(College$Grad.Rate)
which(College$Grad.Rate==118)
College[which(College$Grad.Rate==118),]

ggplot() + 
  geom_point(aes(x=Expend, y=Grad.Rate, 
                 size=Enroll),alpha=0.4,data=College)+
  scale_y_continuous(limits = c(0,100))

## (1.2) Add a fourth variable's information
ggplot() + 
  geom_point(aes(x=Expend, y=Grad.Rate, 
                 size=Enroll,color=Private),alpha=0.4,data=College)+
  scale_y_continuous(limits = c(0,100))

## (1.3) Let's hightlight Miami on the plot
# Maybe do not give alpha scale for Miami
head(College)
names(College)
rownames(College)
# Change private to have value "Yes","No" and "Miami University"
College$Private 
#This is a two level factor, want to add a nother level "Miami"

# Which row corresponds to Miami University?
College %>% 
  mutate(School=rownames(College)) %>% 
  filter(str_detect(School,"Miami"))

College2 <- College %>% 
  mutate(School=rownames(College),
         Private=ifelse(School=="Miami University at Oxford","Miami",as.character(Private)),
         Private=factor(Private,levels = c("No","Yes","Miami")))
  
class(College2$Private)
College2$Private

a <- factor (c("a", "b", "c", "b", "c", "b", "a", "c", "c"))
# What if I try to change an element to a new value, like "d"?
a[3] <- "d"
# However it's okay to set elements to values that are already levels
a[3] <- "a"
a

ggplot(data=College2) + 
  geom_point(aes(x=Expend, y=Grad.Rate, 
                 size=Enroll,color=Private,alpha=Private))+
  scale_y_continuous(limits = c(0,100))+
  scale_color_manual(values=c("#0072B2", "#D55E00", "black"),
                     labels=c("Private", "Public", "Miami University"),
                     name="School Type")+
  scale_alpha_manual(values=c(0.3,0.4,1),
                     labels=c("Private", "Public", "Miami University"),
                     name="School Type")+
  labs(y="Graduate Rate", x="Expenditures (thousands of $)",size="Enrollment") + 
  theme_bw() +
  theme(legend.position="bottom")


## (1.4) Extend scatterplots to more dimensions in data
# (a). We may plot composite scores from multiple variables (sums, rates, etc)
# F.Undergrad+P.Undergraduate

# (b). Facet to create small mutliples 
min(College$Books)
max(College$Books)
?cut
College3 <- College %>%
  mutate(total_undergrads = F.Undergrad + P.Undergrad,
         book_groups = cut(Books,breaks=c(0,500,700,2500),
                           labels=c("Under $500","$500-700","Over $700")))

ggplot()+
  geom_point(aes(x=Outstate, y=Grad.Rate, 
                 size=total_undergrads,
                 color=Private),
             alpha=.2,
             data=College3)+
  scale_y_continuous(limits=c(0,100))+
  facet_grid(.~book_groups)


## 2. Scatterplot Matrix ##
###########################
# Scatterplot matrix explores all pairwise relationships quickly
# make many pairwise plots and organize into grid of scatterplots
# install.packages("GGally")
library(GGally)
?ggscatmat
# Can directly function on the whole dataset.
# ggscatmat(College) 
ggscatmat(College, columns=, color = "")+
  theme_classic() #This is a ggplot object, can change the theme

## 3. Parallel Coordinate Plot ##
#################################
# Organize many numeric axes in parallel (instead of orthogonal)
# Connect observation values with line across all axes
?ggparcoord
ggparcoord(College, columns=2:10)

ggparcoord(College, columns=2:10, groupColumn=,
           alpha=)

## Display Miami in a different color
ggparcoord(College3, columns=2:10, groupColumn=,
           alpha=0.6)+
  scale_color_manual(values=c("pink", "pink4", "black")) 
# Does not display very well






ggparcoord(, columns=2:10, groupColumn=1,
           alpha=0.6)+
  scale_color_manual(values=c("pink", "pink4", "black")) 
# Since R display the graphs in order, so need to change the order
# of the observations. 