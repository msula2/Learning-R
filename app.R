# Install and load the shiny package if you haven't already
# install.packages("shiny")
# library(shiny)
library(katex)
# Define the UI for the app
ui <- fluidPage(
  titlePanel("Simple Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("npoints", "Number of points:", min = 10, max = 100, value = 50)
    ),
    mainPanel(
      p("Hello")
      plotOutput("scatterplot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  # Generate random data based on the number of points selected by the user
  data <- reactive({
    n <- input$npoints
    data.frame(x = rnorm(n), y = rnorm(n))
  })
  
  # Render the scatter plot based on the generated data
  output$scatterplot <- renderPlot({
    plot(data()$x, data()$y, main = "Scatter Plot", xlab = "X-axis", ylab = "Y-axis")
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)
