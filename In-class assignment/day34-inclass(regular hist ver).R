library(shiny)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Day34- In class   :Jessica Choe"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
    # Sidebar panel for inputs ----
        sidebarPanel(
            # Input: Slider for the number of bins ----
            sliderInput(inputId = "bins",
                        label = "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
            ),
    # Main panel for displaying outputs ----
        mainPanel(
            # Output: Histogram ----
            plotOutput(outputId = "distPlot")
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    output$distPlot <- renderPlot({
        x    <- iris$Sepal.Length
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col = "Blue", border = "white",
             xlab = "Sepal Length(cm)",ylab = "Count",
             main = "Histogram of Sepal Length in Iris Data")
    })
}
# Create Shinyapp
shinyApp(ui = ui, server = server)