# -------Day17--Week 07------------------------#
#   Rescalling, annotations, themes
#----------------------------------------------#
library(tidyverse)

# Questions from Day 16 #
#########################
# # Why some numbers in the tibble have underline?
# Groups of three digits are now underlined, starting with the fourth before/after
# the decimal point. This gives a better idea of the order of magnitude of the numbers.
# https://www.tidyverse.org/blog/2018/03/pillar-1-2-1/
diamonds %>%
  group_by(clarity, cut) %>%
  summarise(n=n())%>%
  group_by(cut) %>%
  mutate(cutcount= sum(n)) %>%
  mutate(prop= n/cutcount) %>%
  ungroup() %>%
  arrange(cut)

# French Fries Dataset #
########################

# 1. Data description #
#french_fries
#---------------------#
# Data is from the reshape2 package
library(reshape2)
?french_fries
head(french_fries)
test <- french_fries %>% arrange(subject)
# 10 time points, Different subjects may participate in all or some of the studies.
# Each subject takes two batches of test.

# 2. Prelinminary plots #
#-----------------------#
# (1) Lineplot of average potato flavor over time, one line per person
# Data: Each person at each time point will have an observation
# (average potato score across the 3 treatments and 2 replicates(batches))
ff_subj <- french_fries %>% 
  group_by(subject,time) %>%
  summarise(potatoavg = mean(potato,na.rm = TRUE)) 
ff_subj

french_fries %>% 
  filter(subject==15,time==5)

unique(ff_subj$subject)
length(unique(ff_subj$subject)) 

ggplot() +
  geom_line(aes(x=time, y=potatoavg, group=subject), data=ff_subj) 

# (2) Lineplot of overall average potato flavor over time
# Data: Each time point has an average score
ff_time <- french_fries %>% 
  group_by(time) %>%
  summarise(potatoavg = mean(potato, na.rm=TRUE))
ff_time
ggplot()+
  geom_line(aes(x=time, y=potatoavg,group=1), data=ff_time)

# Put them together
ggplot() +
  geom_line(aes(x=time, y=potatoavg, group=subject), data=ff_subj)+
  geom_line(aes(x=time, y=potatoavg,group=1), data=ff_time)


# Add some features to it

ggplot() +
  geom_line(aes(x=time, y=potatoavg, group=subject), color="tan",data=ff_subj) + 
  geom_line(aes(x=time, y=potatoavg, group=1),color="dark red",size=1.5,data=ff_time)+
  theme_bw()+
  labs(x="Week",y="Potato Flavor Score",
       title="'Potato-y' Flavor Scores of Fries over Time")+
  scale_y_continuous(breaks = seq(0,16,by=4),limits = c(0,16))+
  annotate(geom="text",x=5.5,y=7.8,label="Potato-y Average",
           size=7,color="dark red",angle=-13)+
  annotate(geom="rect",xmin=3,xmax=5,ymin=7,ymax=9,alpha=0.2)
# annotate(): create an annotation layer
# https://ggplot2.tidyverse.org/reference/annotate.html

# 3. Display Extra Information #
#------------------------------#
# (1) Add the oil type and flavor information to the graph, facet?

# Convert to tall format where flavor and score variables replace 5 flavor columns
# then find average for each subject for each flavor category and each treatment, at each week
head(french_fries)
ff_subj_tall <- french_fries %>% 
  gather("flavor", "score", potato:painty) %>%
  group_by(subject, time, flavor, treatment) %>%
  summarise(avgScore = mean(score, na.rm=TRUE))
head(ff_subj_tall,10)
ggplot() +
  geom_line(aes(x=time, y=avgScore, group=subject), data=ff_subj_tall) +
  facet_grid(flavor ~ treatment )

# (2) Add a "smoother" for each flavor and oil treatment combination
ggplot(aes(x=time, y=avgScore), data=ff_subj_tall) +
  geom_line(aes(group=subject)) +
  stat_smooth(aes(group=1,color=flavor),method = "loess",
              span=5,se=FALSE)+
  facet_grid(flavor ~ treatment )
?stat_smooth
# LOESS: locally estimated scatterplot smoothing. It fit local polynomial regression.

# span=: Controls the amount of smoothing for the default loess smoother. 
#        Smaller numbers produce wigglier lines, larger numbers produce smoother lines.
# se: Display confidence interval around smooth? (TRUE by default, see level to control.)
# Refer to https://ggplot2.tidyverse.org/reference/geom_smooth.html


# (3) Edit the theme, polish up the graphs.
# RGB: red green blue
# The red, green and blue use 8 bits each, which have integer values from 0 to 255. 
# This makes 256*256*256=16777216 possible colors. https://www.rapidtables.com/web/color/RGB_Color.html

# hexadecimal colors (HEX) https://www.developintelligence.com/blog/2017/02/rgb-to-hex-understanding-the-major-web-color-codes/
# Color wheels: https://www.canva.com/colors/color-wheel/
# Recall the website from Day 3: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/

# Define our own theme
mytheme <- theme(panel.background = element_rect(fill="white"),
                 panel.grid.major = element_line(color="gray85"),
                 panel.grid.minor = element_line(color="gray95"),
                 plot.title = element_text(size=10, color="darkblue"),
                 axis.title.y = element_text(color="blue"))

# Change the oil variable from 1,2,3 to "Oil 1", "Oil 2", "Oil 3
ff_subj_tall$oil <- paste("Oil", ff_subj_tall$treatment)
glimpse(ff_subj_tall)

ggplot(aes(x=time, y=avgScore), data=ff_subj_tall) +
  geom_line(aes(group=subject)) +
  stat_smooth(aes(group=1,color=flavor),method="loess",span=2,se=FALSE,size=1)+
  facet_grid(flavor ~ oil)+
  scale_y_continuous(breaks=seq(0,16,by=4),
                     limits=c(0,16)) +
  scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73",
                              "#F0E442", "#0072B2"),guide="none") +
  labs(x="Week", y="Average Scores by Subjects \n(0-16 scale)",
       title="Changing Flavor Profiles for Each Subject Over \n 10 Weeks with LOESS Smoothers") +
  mytheme

?scale_color_manual
?guides()
# guides(): set or remove the legend for a specific aesthetic
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software#guides-set-or-remove-the-legend-for-a-specific-aesthetic
# https://ggplot2.tidyverse.org/reference/guides.html

ggsave("FrenchFries.png",dpi=,height=,width=, units="")
# ggsave() is a convenient function for saving a plot. It defaults to saving the last plot that you displayed, using the size of the current graphics device. 
# It also guesses the type of graphics device from the extension.