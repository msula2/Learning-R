#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(
    title = "Pet Store",
    # puts sidebar toggle on right
    titleWidth = "calc(100% - 44px)"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Homepage",
        tabName = "home_tab",
        icon = icon("house")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home_tab",
        column(
          width = 10,
          box(
            width = 12,
            title = h3("Welcome to the pet store!"),
            p("Our pet store allows you to create your own pet. 
              You can choose from a range of domestic or wild animals 
              and tinker with their appearance, personalities and traits")
          ),
          box(
            width = 12,
            title = "Create",
            collapsible = TRUE,
            status = "warning",
            column(
              width = 8,
              textInput("name", label = "What is your pet's name?", value = "", placeholder = "Name", width = "100%"),
              selectInput("type", "Choose type of animal", c("Cat", "Dog", "Bird", "Spider", "Snake"), width = "100%"),
              actionButton(
                inputId = "create",
                label = "Create",
                icon = icon("plus")
              )
            ),
            column(
              width = 4,
              imageOutput("pet_image")
            )
            
          )
          
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  v <- reactiveValues(
    data = data.frame(Name = character(), Type = factor(), Index = numeric()),
    count = 1
  )
  observeEvent(input$create, {
    new_row <- data.frame(
      Name = input$name,
      Type = input$type,
      Index = v$count
    )
    v$data <- rbind(v$data, new_row)
    v$count <- v$count + 1
    print(v$data)
    
  })
  output$pet_image <- renderImage({
    image_path <- paste("images/", ".jpg", sep = input$type)
    list(src = image_path,
         width = 150,
         height = 150,
         alt = "A pet")
  }, deleteFile = FALSE)

  
}

# Run the application 
shinyApp(ui = ui, server = server)
