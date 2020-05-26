# ----Day37--Week 13------------#
# App based on aggregation plot #
#-------------------------------#
library(shiny)
library(tidyverse)
library(lubridate)
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
  group_by(FL_DATE,ORIGIN_CITY_NAME) %>%
  summarize(avg_dep_delay = mean(DEP_DELAY, na.rm=T),
            sd_dep_delay=sd(DEP_DELAY,na.rm=T),
            count=n(),
            bad_delays=quantile(DEP_DELAY,.9,na.rm=T), #defined the bad delay
            early_times = quantile(dep_hour+dep_min/60, .1, na.rm=T)) %>%
  as.data.frame()


# Create a line plot of the top 10 busiest airports, and highlight the airport user specified.

# %in% operator: check if an element belongs to a vector
c(1,6) %in% seq(1,5)
seq(1,5) %in% c(1,6)

# == operator
1:2 == seq(1,5)
1:2 == seq(1,6)
c(1,2,1,2,1,2)==seq(1,6)
seq(1,6)==1:2

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
      
      selectInput(inputId="city", label="Select Airport to Highlight",
                  choices=busiest$ORIGIN_CITY_NAME,multiple=TRUE),
      
      dateRangeInput(inputId = "dates", label="Pick date range",
                     #start="2020-01-01",end="2020-01-19")
                     start=mdy("01/1/20"),end=mdy("01/31/20"),
                     min="2020-01-01",
                     max="2020-01-31",
                     format="mm/dd/yy")
    ) , 
    
    # Main panel typically used to display outputs
    mainPanel(
      plotOutput(outputId="timeplot")
    )
    
  )
)


### Define server behavior for application here
server <- function(input, output) {
  
  output$timeplot <- renderPlot({
    top_airports <- filter(time_airport_stats,
                           ORIGIN_CITY_NAME %in% busiest$ORIGIN_CITY_NAME,
                           FL_DATE >= min(ymd(input$dates)),
                           FL_DATE <= max(ymd(input$dates)))
    
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
      theme_bw() +
      labs(title="Flights in January 2020", x="Date",
           #y="Std. Dev. of Departure Delay (minutes)",
           y=plot_ylabs[input$stat],
           caption="Source: Bureau of Transportation Statistics")+
      guides(color=guide_legend("City"))
    
  })
  
}

### specify the ui and server objects to be combined to make App
shinyApp(ui=ui, server=server)