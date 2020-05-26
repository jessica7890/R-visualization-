# ----Day36--Week 12----#
#     Shiny App-iris2   #
#-----------------------#
# https://shiny.rstudio.com/tutorial/

library(tidyverse)
library(shiny)
library(gridExtra)

myvar <- c("Sepal Length (cm)" = "Sepal.Length", 
           "Sepal Width (cm)" = "Sepal.Width", 
           "Petal Legnth (cm)" = "Petal.Length",
           "Petal Width (cm)" = "Petal.Width")
# 1. Built in Shiny app examples
#################################
# There're many built in shiny code examples available, for example:
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/#Go%20Further
# runExample("01_hello") 
# runExample("10_download")

# Extend the current shinny app to flexibly choose respose variable.
# Choose predictor variable and then make a scatterplot. 
# 2. More on building a shiny app
##################################
# The layout of those examples are different from what we created for day34.

ui <- fluidPage(
  titlePanel("Iris Data Explorer"), 
  
  sidebarLayout(
    #position="right",
    sidebarPanel(
      selectInput(inputId="yvar",
                  label="Choose a response variable",
                  choices = myvar),
      
      
      # Input() functions
      sliderInput(inputId="bins",
                  label="Choose the number of bins:",
                  value=30,min = 1,max = 50),
      
      
      uiOutput(outputId="xvar"),
      
      checkboxInput(inputId="speciesSelected", 
                    label="Color by Species", 
                    value = FALSE),
      
      div(img(src="iris.png",height=120,width=360),style="text-align: center;")    
    ),
    mainPanel(
      # Output() functions
      plotOutput(outputId = "hist"),
      plotOutput(outputId = "scatter")
    )
  )
  # 
  
  
)

# You can view more advanced layout options in:
# https://shiny.rstudio.com/articles/layout-guide.html


server <- function(input, output) {
  output$xvar <- renderUI({
    selectInput(inputId = "xvar",
                label="Choose a predictor variable",
                choices = myvar[myvar!=input$yvar])
  })
  
  
  output$hist <- renderPlot({
    p1 <- ggplot(data=iris)+
      geom_histogram(aes_string(x=input$yvar),bins=input$bins)+
      theme_bw()+
      #labs(x="Sepal Legnth (cm)",
      #     title="The histogram of Sepal Length in Iris Data")
      labs(x=names(myvar)[myvar==input$yvar],
           title=paste("Distribution of"))
    p1
  })
    
  output$scatter <- renderPlot({
    if(input$speciesSelected==TRUE){
      p2 <- ggplot(iris)+
        geom_point(aes_string(x=input$xvar,
                              y=input$yvar,
                              color="Species"))
    }else{
      p2 <- ggplot(iris)+
        geom_point(aes_string(x=input$xvar,
                              y=input$yvar))
    }
    
    p2 <- p2+
      labs(x=names(myvar)[myvar==input$xvar],
           y=names(myvar)[myvar==input$yvar],
           title=paste("Relationship of",
                       names(myvar)[myvar==input$xvar],
                       "with",
                       names(myvar)[myvar==input$yvar]))+
      theme_bw()
    p2
    #grid.arrange(p1,p2,nrow=2) #from gridExtra
  })
}

# conditionalPanel

# Run the application 
shinyApp(ui = ui, server = server)