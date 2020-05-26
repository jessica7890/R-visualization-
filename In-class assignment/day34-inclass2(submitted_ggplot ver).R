library(shiny)
library(ggplot2)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # App title ----
  titlePanel("Day34- In class   :Jessica Choe"),
    # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
   sidebarPanel(
      # Input: Slider for the number of bins 
      sliderInput(inputId = "bins",
                  label = "Number of bins:",
                  min = 1, max = 50, value = 30),
      radioButtons("var",
                "Which variable do you want to know about?",
                choices = c("Sepal Length" = "Sepal.Length",
                            "Sepal Width" = "Sepal.Width",
                            "Petal Length" = "Petal.Length",
                            "Petal Width" = "Petal.Width"),
                selected = "Sepal.Length")),

   # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram 
      plotOutput(outputId = "distPlot")
    )
))
     
  

# Define server logic required to draw a histogram 
server <- function(input, output) {
      output$distPlot <- renderPlot({
#Put different lables for x for each selection.  
        if (input$var == "Sepal.Length") {
          xlabel <- "Sepal Length"
        } 
        else if (input$var == "Sepal.Width") {
          xlabel <- "Sepal Width"
        }        
        else if (input$var == "Petal.Length") {
          xlabel <- "Petal Length"
        }
        else {xlabel <-input$var }
        # else if (input$var == "Petal.Width") {
        #   xlabel <- "Petal Width"
        # }
# create histogram using ggplot +geom_histogram
      ggplot(iris, aes_string(input$var)) +
        geom_histogram(bins = input$bins,
                       fill = "steelblue3",
                       colour = "grey30") +  
          theme_minimal()+
          xlab(xlabel)+
          ggtitle("Histogram of Iris Data")+  
          theme(  plot.title = element_text(size = 16,face="bold"),
                  axis.text.x = element_text(size=12))
                  
    })}
# Create Shiny app
shinyApp(ui = ui, server = server)


