# ----Day35--Week 12----#
#      Shiny App        #
#-----------------------#
# https://shiny.rstudio.com/tutorial/

library(tidyverse)
library(shiny)

myvar <- c("Sepal Length (cm)" = "Sepal.Length", 
  "Sepal Width (cm)" = "Sepal.Width",
  "Petal Length (cm)" = "Petal.Length",
  "Petal Width (cm)"="Petal.Width")


# 1. Built in Shiny app examples
#################################
# There're many built in shiny code examples available, for example:
# https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/#Go%20Further
#runExample("01_hello") 
#runExample("10_download")

# Extend the current shiny app to flexibly choose the var for histogram (Response var)
# Add the predictor variable choice
# Add the scatter plot, and probably can let user choose whether to color-mark it. 


# 2. More on building a shiny app
##################################
# The layout of those examples are different from what we created for day34.

ui <- fluidPage(
  titlePanel("Iris Data Explorer"), 
  
   sidebarLayout(
     #position="right",
     sidebarPanel(
       selectInput(inputId = "yvar", 
                   label="Choose a response variable", 
                   choices = myvar), 
                   #selected = "Sepal.Width"),
       
       
       # Input() functions
       sliderInput(inputId="bins",
                   label="Choose the number of bins:",
                   value=30,min = 1,max = 50),
       
       
       
       div(img(src="iris.png",height=120,width=360),style="text-align: center;")
     ),
     mainPanel(
       # Output() functions
       plotOutput(outputId = "hist")
     )
   ) 



)

# You can view more advanced layout options in:
# https://shiny.rstudio.com/articles/layout-guide.html



server <- function(input, output) {
  output$hist <- renderPlot({
    ggplot(data=iris)+
      geom_histogram(aes_string(x=input$yvar),bins=input$bins)+
      theme_bw()+
      #labs(x="Sepal Legnth (cm)",
      #     title="The histogram of Sepal Length in Iris Data")
      labs(x=names(myvar)[myvar==input$yvar],
           title=paste("Distribution of",names(myvar)[myvar==input$yvar]))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)