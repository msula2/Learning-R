#Setup
library(shiny)

#Define UI
ui <- fluidPage(titlePanel("Addition Demo"),
               verticalLayout(
                   numericInput(
                     inputId = "first_num",
                     label = "First number",
                     value = 0
                   ),
                   numericInput(
                     inputId = "second_num",
                     label = "Second number",
                     value = 0
                   ),
                   actionButton(
                     inputId = "calc_sum",
                     label = "Add Numbers"
                   ),
                   textOutput(outputId = "sum_of_nums")
                 )
            )

#Define server logic
server <- function(input, output, session){
  observeEvent(input$calc_sum, {
    n <- input$first_num + input$second_num
    sum_text <- sprintf("The sum of the two numbers is: %d", n)
    output$sum_of_nums = renderText(sum_text)
    
  })
}

#Run the application
shinyApp(ui = ui, server = server)