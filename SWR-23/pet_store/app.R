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
library(dplyr)
library(ggthemes)

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
      ),
      uiOutput("bins")
    )
  ),
  dashboardBody(
    tags$style(HTML(".main-footer { display: none; }")),
    tabItems(
      tabItem(
        tabName = "home_tab",
        column(
          width = 6,
          box(
            width = 12,
            title = "Create",
            collapsible = TRUE,
            status = "warning",
            column(
              width = 8,
              textInput("name", label = "What is your pet's name?", value = "", placeholder = "Name", width = "100%"),
              selectInput("type", "Choose type of animal", c("Cat", "Dog", "Bird", "Spider", "Snake"), width = "100%"),
              radioButtons("gender",
               label = "Choose gender",
               choices = c("Male", "Female"),
               selected = character(0),
               inline = TRUE),
              sliderInput("age",
               label = "Age",
                min = 0,
                max = 10,
                value = 0,
                step = 1,
                width = "100%"),
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
          
        ),
        column(
          width = 6,
          tabBox(
            width = 12,
            title = "Statistics",
            tabPanel(
              title = "Table",
              DT::dataTableOutput("data_table",
               width = "100%",
               height = "auto")
            ),
            tabPanel(
              title = "Graphs",
              selectInput("plot_type", 
                label = "Plot by", 
                choices = c("Type", "Gender", "Age"),
                selected = NULL,
                width = "100%"),
              plotOutput("plots")
            )
          )
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v <- reactiveValues(
    data = data.frame(Name = character(), Type = factor(), Gender = factor(), Age = numeric()),
    count = 1,
    data_type = NULL
  )
  observeEvent(input$create, {
    new_row <- data.frame(
      Name = input$name,
      Type = input$type,
      Gender = input$gender,
      Age = input$age
    )
    v$data <- rbind(v$data, new_row)
    v$count <- v$count + 1
    
    updateTextInput(session, "name", value = "")
    updateSelectInput(session, "type", selected = "")
    updateRadioButtons(session, "gender", selected = character(0))
    updateSliderInput(session, "age", value = 0)
    
  })
  
  output$data_table <- DT::renderDataTable({
    v$data
  }, options = list(pageLength = 10))
  
  output$bins <- renderUI({
    if (input$plot_type == "Age"){
      sliderInput("width_bin",
        label = "Bin Width",
        min = 0,
        max = 10,
        value = 1,
        step = 1,
        width = "100%")
    }
    
  })
  
  output$pet_image <- renderImage({
    image_path <- paste("images/", ".jpg", sep = input$type)
    list(src = image_path,
         width = 100,
         height = 100,
         alt = "A pet")
  }, deleteFile = FALSE)
  output$plots <- renderPlot({
    if (nrow(v$data)){
      if (input$plot_type != "Age"){
        ggplot(v$data, aes(x = .data[[input$plot_type]], fill = Type)) +
          geom_bar() +
          scale_x_discrete(
            # change axis title
            name = paste(input$plot_type, "of Pet", " ") 
          ) + 
          scale_y_continuous(
            name = "", # remove axis title
            breaks = seq(0, 10, by = 1),
            # remove the space above and below the y-axis
            expand = expansion(add = 0)
          ) +
          # add a title      
          ggtitle(paste("Pets by", input$plot_type, " ")) +
          theme_classic()  
      }
      else{
        ggplot(v$data, aes(x = .data[[input$plot_type]])) +
          geom_histogram(binwidth = input$width_bin, 
             boundary = 0, 
             fill = "white", 
             color = "black") +
          scale_x_continuous(name = "Age",
             breaks = seq(0, 10, 1)) +
          theme_classic() 
          
      }
      
    }
      
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
