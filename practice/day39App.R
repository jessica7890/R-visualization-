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
#load("Jan2020Flight.Rdata")
load("Jan2020FlightAggregation.RData")
# From the Federal Airlines Administration (FAA), flight records for every U.S.
# air flight for the month of January 2020. The original CSV file is 80MB. 
# The `.RData` file has been trimmed and is in R format.
# The original data can be obtained here for those interested:
# https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236&DB_Short_Name=On-Time
# End of copy
###############################



# Make the y labels flexible
plot_ylabs=c(avg_dep_delay="Average Departure Delay (minutes)",
             sd_dep_delay="Std. Dev. of Departure Delay (minutes)",
             bad_delays="Bad Delays \n(90th percentile for delay in minutes)")

# (2) Assign color to 10 cities
library(RColorBrewer)
# display.brewer.all(colorblindFriendly = T)

#?brewer.pal
plotcolors <- brewer.pal(10,name="Paired")
#plotcolors
names(plotcolors) <- sort(busiest$ORIGIN_CITY_NAME)
#plotcolors["Denver, CO"]



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