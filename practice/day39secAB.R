# ----Day37--Week 13------------#
# App based on aggregation plot #
#-------------------------------#
library(shiny)
library(tidyverse)
library(lubridate)
library(ggthemes)
# Copied from day 28 #

## Aggregation Based Plot ##
###############################
load("Jan2020Flight.Rdata")

# From the Federal Airlines Administration (FAA), flight records for every U.S.
# air flight for the month of January 2020. The original CSV file is 80MB. 
# The `.RData` file has been trimmed and is in R format.
# The original data can be obtained here for those interested:
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# End of copy
###############################


# find top 10 busiest airports :
busiest <- delay %>%
  group_by(ORIGIN_CITY_NAME) %>%
  summarize(count=n() ) %>%
  arrange(desc(count)) %>% head(10) 

# aggregate by airport
time_airport_stats <- delay %>%
  mutate(wday = wday(FL_DATE, label=T)) %>%
  group_by(FL_DATE,ORIGIN,ORIGIN_CITY_NAME) %>%
  summarize(avg_dep_delay = mean(DEP_DELAY, na.rm=T),
            sd_dep_delay=sd(DEP_DELAY,na.rm=T),
            count=n(),
            bad_delays=quantile(DEP_DELAY,.9,na.rm=T), #defined the bad delay
            early_times = quantile(dep_hour+dep_min/60, .1, na.rm=T)) %>%
  as.data.frame()


# Create a line plot of the top 10 busiest airports, and highlight the airport user specified.

# Make the y labels flexible
plot_ylabs=c(avg_dep_delay="Average Departure Delay (minutes)",
  sd_dep_delay="Std. Dev. of Departure Delay (minutes)",
  bad_delays="Bad Delays \n(90th percentile for delay in minutes)")

top_airports <- filter(time_airport_stats,
                       ORIGIN_CITY_NAME %in% busiest$ORIGIN_CITY_NAME,
                       FL_DATE >= ymd("2020-01-05"),
                       FL_DATE <= ymd("2020-01-24"))

select_airport <- filter(top_airports,ORIGIN_CITY_NAME %in% c("Atlanta, GA","New York, NY"))

# Make a line plot of the top 10 busiest airport, then highlight the one you selected.
ggplot()+
  geom_line(aes_string(x="FL_DATE",y="avg_dep_delay",
                       group="ORIGIN_CITY_NAME"),
            data=top_airports)+
  geom_line(aes_string(x="FL_DATE",y="avg_dep_delay",
                       group="ORIGIN_CITY_NAME",
                       color="ORIGIN_CITY_NAME"),
            data=select_airport,size=2)+
  theme_bw() +
  labs(title="Flights in January 2020", x="Date",
       #y="Std. Dev. of Departure Delay (minutes)",
       y=plot_ylabs["avg_dep_delay"],
       caption="Source: Bureau of Transportation Statistics")+
  guides(color=guide_legend("City"))


# (1) For some cities, there're multiple airports
delay %>% filter(ORIGIN_CITY_NAME=="New York, NY")

# (2) Assign color to 10 cities
library(RColorBrewer)
# display.brewer.all(colorblindFriendly = T)

#?brewer.pal
plotcolors <- brewer.pal(10,name="Paired")
#plotcolors
names(plotcolors) <- sort(busiest$ORIGIN_CITY_NAME)
#plotcolors["Denver, CO"]

# (3) Add other plots, how to display them better? 
# Probably multiple tabs.

# (4) Want to create a map, which uses center of circles to represt an airport
# location, the size of circle to represent how large the statistic is. Then users
# can select a specific statistic, a date range and view the plot.
# 
# 
# prepare the dataset needed.
# We need the state outlines data: esay: map_data("state")
# We need the location of the airports: search for such data in google
# Refer to https://openflights.org/data.html     download the airports.dat
airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header=FALSE)

airports <- airports %>%
  select(V5, V7, V8) %>%
  rename(AirportCode = V5,
         lat=V7,
         long=V8)

airport_all <- left_join(time_airport_stats, airports,
                         by=c("ORIGIN"="AirportCode")) %>%
  filter(count>10)


# subset the data, then make the plot
airport_map_data <- airport_all %>%
  filter(!str_sub(ORIGIN_CITY_NAME,
                  str_length(ORIGIN_CITY_NAME)-1,
                  str_length(ORIGIN_CITY_NAME) ) %in% c("AK", "HI", "PR", "VI") ) %>%
  filter( FL_DATE >= ymd("2020-01-01"),
          FL_DATE <= ymd("2020-01-31")) %>%
  group_by(ORIGIN, ORIGIN_CITY_NAME, long, lat) %>%
  summarize(avg_dep_delay = weighted.mean(avg_dep_delay,weights=count,na.rm=T),
            sd_dep_delay = weighted.mean(sd_dep_delay,weights=count,na.rm=T),
            bad_delays =  weighted.mean(bad_delays,weights=count,na.rm=T),
            count = sum(count))
#?weighted.mean

ggplot() +
  geom_path(aes(x=long, y=lat, group=group), data=map_data("state"),
            color="gray50") +
  geom_point(aes_string(x="long", y="lat", fill="avg_dep_delay",
                        size="count"), color="gray30", shape=21,
             data=airport_map_data) +
  coord_map() +
  theme_map() +
  theme(legend.position="bottom")+
  scale_fill_gradient2(plot_ylabs["avg_dep_delay"],low="darkblue", mid="white",high="darkred",
                       midpoint = 0)+
  guides(size=guide_legend(order=1))







### Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel(title = "Your App Title Here!"),
  sidebarLayout(
    
    # Sidebar typically used to house input controls
    sidebarPanel(
      # put your input controls here!
      selectInput(inputId="stat", label="Select a Departure Delay Statistic",
                  choices=c("Average Departure Delay"="avg_dep_delay",
                            "Std. Dev. of Departure Delay"="sd_dep_delay",
                            "Bad Delays (90th perc.)"="bad_delays")),
      
      conditionalPanel(condition="input.tabs=='TimePlots'",
      selectInput(inputId="city", label="Select Airport to Highlight",
                  choices=busiest$ORIGIN_CITY_NAME,multiple=TRUE)),
      
      dateRangeInput(inputId = "dates", label="Pick date range",
                     #start="2020-01-01",end="2020-01-19")
                     start=mdy("01/1/20"),end=mdy("01/31/20"),
                     min="2020-01-01",
                     max="2020-01-31",
                     format="mm/dd/yy")
    ) , 
    
    # Main panel typically used to display outputs
    mainPanel(
      #plotOutput(outputId="timeplot")
      tabsetPanel(id="tabs",
        tabPanel("TimePlots", plotOutput(outputId="timeplot")), 
        tabPanel("Maps", plotOutput(outputId="mapplot"))
        #tabPanel("Summary", verbatimTextOutput("summary")), 
        #tabPanel("Table", tableOutput("table"))
      )
    )
    # Multiple tabs, can refer to https://shiny.rstudio.com/articles/layout-guide.html
    
    
  )
)


### Define server behavior for application here
server <- function(input, output) {
  
  output$timeplot <- renderPlot({
    top_airports <- filter(time_airport_stats,
                           ORIGIN_CITY_NAME %in% busiest$ORIGIN_CITY_NAME,
                           FL_DATE >= min(ymd(input$dates)),
                           FL_DATE <= max(ymd(input$dates))) %>% 
                    group_by(FL_DATE,ORIGIN_CITY_NAME) %>% 
                    summarise(avg_dep_delay=mean(avg_dep_delay,na.rm=TRUE),
                              sd_dep_delay=mean(sd_dep_delay,na.rm=TRUE),
                              bad_delays=max(bad_delays,na.rm=TRUE),
                              count=sum(count))
    
    select_airport <- filter(top_airports,ORIGIN_CITY_NAME %in% input$city)
    
    # Make a line plot of the top 10 busiest airport, then highlight the one you selected.
    ggplot()+
      geom_line(aes_string(x="FL_DATE",y=input$stat,
                           group="ORIGIN_CITY_NAME"),
                data=top_airports)+
      geom_line(aes_string(x="FL_DATE",y=input$stat,
                           group="ORIGIN_CITY_NAME",
                           color="ORIGIN_CITY_NAME"),
                data=select_airport,size=2)+
      scale_color_manual(values=plotcolors[input$city])+
      theme_bw() +
      labs(title="Flights in January 2020", x="Date",
           #y="Std. Dev. of Departure Delay (minutes)",
           y=plot_ylabs[input$stat],
           caption="Source: Bureau of Transportation Statistics")+
      guides(color=guide_legend("City"))
    
  })
  
  
  output$mapplot <- renderPlot({
    # subset the data, then make the plot
    airport_map_data <- airport_all %>%
      filter(!str_sub(ORIGIN_CITY_NAME,
                      str_length(ORIGIN_CITY_NAME)-1,
                      str_length(ORIGIN_CITY_NAME) ) %in% c("AK", "HI", "PR", "VI") ) %>%
      filter( FL_DATE >= min(ymd(input$dates)),
              FL_DATE <= max(ymd(input$dates))) %>%
      group_by(ORIGIN, ORIGIN_CITY_NAME, long, lat) %>%
      summarize(avg_dep_delay = weighted.mean(avg_dep_delay,weights=count,na.rm=T),
                sd_dep_delay = weighted.mean(sd_dep_delay,weights=count,na.rm=T),
                bad_delays =  weighted.mean(bad_delays,weights=count,na.rm=T),
                count = sum(count))
    #?weighted.mean
    
    stat_map <- ggplot() +
      geom_path(aes(x=long, y=lat, group=group), data=map_data("state"),
                color="gray50") +
      geom_point(aes_string(x="long", y="lat", fill=input$stat,
                            size="count"), color="gray30", shape=21,
                 data=airport_map_data) +
      coord_map() +
      theme_map() +
      theme(legend.position="bottom")
    
    if(input$stat=="avg_dep_delay"){
      stat_map+scale_fill_gradient2(plot_ylabs[input$stat],low="darkblue", mid="white",high="darkred",
                           midpoint = 0)+
      guides(size=guide_legend(order=1))
    }
    else if(input$stat=="sd_dep_delay"){
      stat_map+scale_fill_gradient(plot_ylabs[input$stat],low="white", high="darkgreen")
    }
    else{
      stat_map+scale_fill_gradient(plot_ylabs[input$stat],low="white", high="darkblue")
    }
    
    
  })
  
  
  
  
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)


# Conditoinal panel
# https://shiny.rstudio.com/gallery/conditionalpanel-demo.html