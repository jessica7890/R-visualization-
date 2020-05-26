#https://www.cdc.gov/nchs/nhis/nhis_2018_data_release.htm
library( shiny )
library(tidyverse)
library(ggmosaic)
library(readxl)
library(lubridate)
library(ggplot2)
#Data reading and cleaning
setwd("C:/Users/user/Desktop/2020spring/504 data visualization/project/graduate project/")
file <- read.csv("samadult.csv")

adult<-file %>% select(DIBEV1,ASICPUSE,ASISLEEP,BMI,DEP_1,TIRED_1 )#,TIRED_1

colnames(adult)<-c("diabetes","computer","sleephr",
                   "BMI","depressed","tired")

adult <-adult %>%  filter(diabetes==1|diabetes==2) %>%
                  filter(computer<5) %>% 
                  filter(sleephr<15) %>% 
                filter(BMI<5000) %>% 
                filter(depressed<6) %>% 
              filter(tired<5) %>%
  mutate(diabetes=factor(diabetes,levels=c(1,2),labels=c("Diabetes","No diabetes"))) %>% 
  mutate(computer=factor(computer,levels=c(1,2,3,4),labels=c("Never","Somedays","Most days","Everyday"))) %>% 
  mutate(depressed=factor(depressed,levels=c(1,2,3,4,5),labels=c("Daily","Weekly","Monthly","A few times","Never"))) %>% 
  mutate(tired=factor(tired,levels=c(1,2,3,4),labels=c("Never","Some","Most","Everyday"))) %>% 
  mutate(BMI=BMI/100,na.rm=T)
colors <- colorRampPalette(c("steelblue", "yellow", "black", "orange","red"))(5)

# str(adult)
####################################################################

#Overall histogram of BMI of people
ggplot(adult, aes(BMI)) +
  geom_histogram(bins =30, fill = "steelblue3",
                 colour = "grey30")+
  labs(y="", x="",
       caption="Data Source: 2018 National Health Interview Survey",
       title="Distribution of BMI of Adult in the US 2018 ")+
  theme(  plot.title = element_text(size = 12,face="bold"),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11))+
  theme_bw()

#Distribution of BMI

ggplot( data=adult,aes(x=diabetes, y=BMI) ) +
  geom_violin( color="black", fill="steelblue")+
  geom_boxplot( color="black", fill="white",
                width=.2,na.rm=T)+ylab("")+xlab("")+
  theme_bw() +
  labs(y="", x="",
       caption="Data Source: 2018 National Health Interview Survey",
       title="Distribution of BMI of Adult (With or Without Diabetes) ")+
  theme(  plot.title = element_text(size = 12,face="bold"),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11))+
  coord_flip()
#Diabetic-> more often depressed(Mentally)
ggplot() +
  geom_mosaic(aes(x=product(diabetes), fill=depressed),data=adult)+
  labs(x="",y="",
       title=" How often Are You Depressed in a year ?",
       caption="Data Source: 2018 National Health Interview Survey")+
  theme(legend.position = "none",plot.title = element_text(size = 12,face="bold"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11))+
  scale_fill_manual(values=colors)+
  theme_bw()+
  theme(legend.position = "none")

#More often Tired(Physically)
ggplot() +
  geom_mosaic(aes(x=product(diabetes), fill=tired),data=adult)+
  labs(x="",y="",
       title=" How Often Physically Tired in 3 months?",
       caption="Data Source: 2018 National Health Interview Survey")+
  theme(legend.position = "none",plot.title = element_text(size = 12,face="bold"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11))+
  scale_fill_manual(values=colors)+
  theme_bw()+
  theme(legend.position = "none")


#Diabetic-> less time in front of computer
ggplot() +
  geom_mosaic(aes(x=product(diabetes), fill=computer),data=adult)+
  labs(x="",y="",
       title=" Frequency of using computer ",
       caption="Data Source: 2018 National Health Interview Survey")+
  theme(legend.position = "none",plot.title = element_text(size = 12,face="bold"),
        axis.text.x = element_text(size=11),
        axis.text.y = element_text(size=11))+
  scale_fill_manual(values=colors)+
  theme_bw()+
theme(legend.position = "none")


#Distribution of Sleep hour
ggplot( data=adult,aes(x=diabetes, y=sleephr) ) +
  geom_violin( color="black", fill="steelblue")+
  geom_boxplot( color="black", fill="white",
                width=.07,na.rm=T)+ylab("")+xlab("")+
  theme_bw() +
  labs(y="", x="",
       caption="Data Source: 2018 National Health Interview Survey",
       title="Distribution of Sleep hour of Adult  ")+
  theme(  plot.title = element_text(size = 12,face="bold"),
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11))+
  coord_flip()




########################################

ui <- fluidPage(
  
  titlePanel( title="Boxplot of People with Diabetes in the United States" ),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "Region", "Region:",
        c("South" = "South",
          "West" = "West",
          "Midwest" = "Midwest",
          "North East"="North East"
        ),
        inline = TRUE,
        selected = c("Midwest","West")
      ),
      
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
                  selected = "Testing_Rate"),
    ),
    
    mainPanel(
      textOutput("selected_var"),
      plotOutput( "distPlot" )
    )
  )
) 

library( ggplot2 )
library( shiny )
library(scales)
#Testing_Rate - Total number of people tested per 100,000 persons.
#Mortality_Rate - Number recorded deaths * 100/ Number confirmed cases.
#Incident_Rate - confirmed cases per 100,000 persons.
#https://www.michigan.gov/coronavirus/0,9753,7-406-98163_98173---,00.html

server <-  function( input, output ) {
  
  df <- reactive( {
    return( subset( covid3, region  %in% input$Region ) )
  } )
  
  output$distPlot <- renderPlot( {
    
    if (input$var == "Testing_Rate") {
      var <- "Testing Rate (per 100,000 Persons)"
    } 
    else if (input$var == "Mortality_Rate") {
      var <- "Mortality Rate (per Number of Confirmed Cases*100)"
    } 
    else if (input$var == "Incident_Rate") {
      var <- "Incident Rate (Confirmed cases per 100,000 Persons)"
    } 
    else {var <-input$var }
    
    options(scipen=10000000)
    
    if ( input$Additional == "Violin" ) {
      ggplot( data=df(), aes_string(x="region", y=input$var) ) +
        geom_violin( color="black", fill="steelblue")+
        geom_boxplot( color="black", fill="white", 
                      outlier.color="yellow",width=.07,na.rm=T)+
        scale_y_continuous(labels = comma)+
        guides( fill=FALSE )+theme_bw()+ylab("")+xlab("") +
        labs(title= paste("Selected variable:",var),caption="Source:The Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)")+
        theme(  plot.title = element_text(size =15,face="bold"),
                axis.text.x = element_text(size=11,face="bold"),
                axis.text.y = element_text(size=11)
        )
    }
    else {
      ggplot( data=df(), aes_string(x="region", y=input$var) ) +
        geom_boxplot( color="black", fill="skyblue", 
                      width=.1,na.rm=T)+
        scale_y_continuous(labels = comma)+
        geom_jitter(width = 0.3)+
        guides( fill=FALSE )+theme_bw()+ylab("")+xlab("") +
        labs(title= paste("Selected variable:",var),caption="Source:the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)")+
        theme(  plot.title = element_text(size =15,face="bold"),
                axis.text.x = element_text(size=11,face="bold"),
                axis.text.y = element_text(size=11)
        )
      
      
    }
    
  } )
} 
shinyApp(ui = ui, server = server)