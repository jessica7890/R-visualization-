 library( shiny )
 library(tidyverse)
 library(readxl)
 library(lubridate)
 library( ggplot2 )
 library(scales)
 setwd("C:/Users/user/Desktop/2020spring/504 data visualization/project/group project")
 
 load("allcovid.Rdata")
 
 myvar <- c("Confirmed Cases"="Confirmed",
            "Death total"="Deaths",
            "Recovered Cases"="Recovered")
 
 
 ui <- fluidPage(
    
    titlePanel("2018 US Diabetes Description: "), 
    
    tabsetPanel(id="tabs",
    tabPanel("Maps", 
             sidebarPanel(selectInput(inputId = "yvar", 
                                      label="Choose a response variable", 
                                      choices = myvar),
                          
                          sliderInput(inputId="num",
                                      label="Choose the number of states with the highest cases:",
                                      value=10, min = 1, max = 30)
             ),
             mainPanel(plotOutput(outputId = "map"))
    ),
    
    
    tabPanel("BoxPlot",
             sidebarPanel(checkboxGroupInput(
               "Region", "Region:",
               c("South" = "South",
                 "West" = "West",
                 "Midwest" = "Midwest",
                 "North East"="North East"
               ),
               inline = TRUE,
               selected = c("Midwest","West") ),
               
               radioButtons(
                 "Additional",
                 "Additional Plot:",
                 c( "Violin" = "Violin", "Jitter" = "Jitter" ),
                 inline = TRUE
               ),
               
               selectInput(inputId="var",
                           label="Select a variable",
                           choices = c("Incident Rate"="Incident_Rate",
                                       "Testing Rate" = "Testing_Rate",
                                       "Mortality Rate" = "Mortality_Rate"),
                           selected = "Testing_Rate")),
             mainPanel(plotOutput( outputId="distPlot"),textOutput("selected_var"))
    )
    
    )
 )

 
 server <-  function( input, output ) {
   
    output$map <- renderPlot({
       
       # Make a base map.  
       p <- ggplot(data=all_covid) +
          geom_polygon(aes_string(x="long", y="lat", fill=input$yvar, group="group"),
                       alpha=0.5)+
          geom_path(aes(x=long, y=lat, group=group), size=0.3) +
          scale_fill_continuous(low="mistyrose", high="red4", trans="log10") +
          theme_map() 
       
       # Update data according to what users entered  
       if (input$yvar=="Confirmed") {
          most <- covid2 %>% filter(!is.na(Confirmed)) %>% 
             arrange(desc(Confirmed)) %>% head(input$num)
          
       } else if (input$yvar=="Deaths") {
          most <- covid2 %>% filter(!is.na(Deaths)) %>% 
             arrange(desc(Deaths)) %>% head(input$num)
          
       } else {
          most <- covid2 %>% filter(!is.na(Recovered)) %>% 
             arrange(desc(Recovered)) %>% head(input$num)
       }
       
       # Update the number of displayed states with the highest number of cases. 
       p +
          geom_point(data=most, aes_string(x="Long_", y="Lat", size=input$yvar), color="gold") +
          labs(fill=names(myvar)[myvar==input$yvar],
               size=names(myvar)[myvar==input$yvar],
               title=paste(names(myvar)[myvar==input$yvar], "in US"),
               caption="Source:The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)")+
          theme(legend.direction = "horizontal",
                legend.position=c(0,0),
                legend.title = element_text(size = 12),
                plot.title = element_text(size = 25),
                plot.subtitle = element_text(size = 12))
       
    })
    
    
   df <- reactive( {
     return( subset( covid_cleaned, region  %in% input$Region ) )
   } )
   
   output$distPlot <- renderPlot( {
     
     if (input$var == "Testing_Rate") {
       var <- "Testing Rate "
       sub<-"(per 100,000 Persons)"
     } 
     else if (input$var == "Mortality_Rate") {
       var <- "Mortality Rate "
       sub<-"(per Number of Confirmed Cases*100)"
     } 
     else if (input$var == "Incident_Rate") {
       var <- "Incident Rate "
       sub<-"(Confirmed cases per 100,000 Persons)"
     } 
     else {var <-input$var }
      
     #put decimal point for every 1000 for y axis
     options(scipen=10000000)
     
     #If Violin is selected.
     if ( input$Additional == "Violin" ) {
       ggplot( data=df(), aes_string(x="region", y=input$var) ) +
         geom_violin( color="black", fill="steelblue")+
         geom_boxplot( color="black", fill="white", 
                       outlier.color="yellow",width=.07,na.rm=T)+
         scale_y_continuous(labels = comma)+
         guides( fill=FALSE )+theme_bw()+ylab("")+xlab("") +
         labs(title= paste(var),subtitle=sub,
              caption="Source:The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)")+
         theme(  plot.title = element_text(size =25),
                 axis.text.x = element_text(size=11,face="bold"),
                 axis.text.y = element_text(size=11),
                 plot.subtitle = element_text(size = 12)
         )
     }
     
     #If Jittery plot is selected 
     else {
       ggplot( data=df(), aes_string(x="region", y=input$var) ) +
         geom_boxplot( color="black", fill="skyblue", 
                       width=.1,na.rm=T)+
         scale_y_continuous(labels = comma)+
         geom_jitter(width = 0.3)+
         guides( fill=FALSE )+theme_bw()+ylab("")+xlab("") +
         labs(title= paste(var),subtitle=sub,caption="Source:the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)")+
         theme(  plot.title = element_text(size =25),
                 axis.text.x = element_text(size=11,face="bold"),
                 axis.text.y = element_text(size=11),
                 plot.subtitle = element_text(size = 12)
         )
       
       
     }
     
   } )
 } 
 shinyApp(ui = ui, server = server)
