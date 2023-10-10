#Setup
library(shiny)

#Define UI
ui <- fluidPage(titlePanel("Basic Demo"),
                sidebarLayout(
                  sidebarPanel(
                    h2("My favourite things"),
                    checkboxGroupInput(
                      inputId = "fav_things",
                      label = "What are your favourite things ?",
                      choices = c("Coding", "Cycling", "Cooking")
                    ),
                    actionButton(
                      inputId = "count_fav_things",
                      label = "Count",
                      icon = icon("calculator")
                    )
                  ),
                  mainPanel(
                    textOutput(outputId = "n_fav_things"),
                    p("This is a very basic demo."),
                    tags$img(
                      src = "https://debruine.github.io/shinyintro/images/logos/shinyintro.png",
                      width = "100px",
                      height = "100px"
                    )
                  )
                )
               )

#Define server logic
server <- function(input, output, session){
  #count favorite things
  observeEvent(input$count_fav_things, {
    n <- length(input$fav_things)
    count_text <- sprintf("You have %d favourite things", n)
    output$n_fav_things <- renderText(count_text)
  })
  
}

#Run the application
shinyApp(ui = ui, server = server)