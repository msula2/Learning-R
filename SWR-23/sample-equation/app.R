library(shinyjs)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(katex)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Sample Equations",
    titleWidth = "calc(100% - 44px)"
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(
        "Line Equation",
        tabName = "line_tab",
        icon = icon("chart-line")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
      tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous")
    ),
    tags$style(HTML(".main-footer { display: none; } a {display: inline;}")),
    tabItems(
      tabItem(
        tabName = "line_tab",
        column(
          width = 6,
          box(
            width = 12,
            title = "About",
            collapsible = TRUE,
            p(
              "A line equation is a fundamental concept in geometry and mathematics that 
              represents a straight, unending path between two points in a two-dimensional 
              space. It is often used to model and describe relationships between two 
              variables, such as time and distance. Its equation is given below"
            ),
            helpText(
              HTML(katex_html(
                "y = mx + c", 
                displayMode = TRUE, 
                preview = FALSE,
                include_css = TRUE,
                output = "html")
              )
            ),
            selectizeInput("variable", label = "Choose a variable to learn more about it", choices = NULL),
            uiOutput("about_variable"),
            textOutput("result")
          )
        ),
        column(
          width = 6,
          uiOutput("change_values"),
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
              selectizeInput("plot_type", label = "Plot by:", choices = NULL),
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
    data = data.frame(y = numeric(), m = numeric(), x = numeric(), c = numeric())
  )
  
  var_ids <- list("y", "m", "x", "c");
  var_texts <- list("y", "m", "x","c")
  vars <- setNames(var_ids, var_texts)
  
  var_ids_plot <- list("m", "x", "c");
  var_texts_plot <- list("m", "x","c")
  vars_plot <- setNames(var_ids_plot, var_texts_plot)
  
  
  updateSelectizeInput(
    session, "variable",
    choices = vars,
    options = list(render = I("
    {
    item:   function(item, escape) { 
      var html = katex.renderToString(item.label);
      return '<div>' + html + '</div>'; 
    },
    option: function(item, escape) { 
      var html = katex.renderToString(item.label);
      return '<div>' + html + '</div>'; 
    }
    }
                              "))
  )
  
  updateSelectizeInput(
    session, "plot_type",
    choices = vars_plot,
    options = list(render = I("
    {
    item:   function(item, escape) { 
      var html = katex.renderToString(item.label);
      return '<div>' + html + '</div>'; 
    },
    option: function(item, escape) { 
      var html = katex.renderToString(item.label);
      return '<div>' + html + '</div>'; 
    }
    }
                              "))
  )
  output$about_variable <- renderUI({
    var <- input$variable
    if (var == "y"){
      definition <- "is the dependent variable, also known as the response variable."
    }
    else if (var == "m"){
      definition <- "is the slope and it is a measure of the steepness. It describes how much 'y' changes for every unit change in 'x'."
    }
    else if (var == "x"){
      definition <- "is the independent variable. The independent variable is the variable that you can manipulate or change to observe its effect on the dependent variable 'y'."
    }
    else{
      definition <- "is y-intercept. It is the value of the dependent variable 'y' when the independent variable 'x' is equal to zero. It represents the point where the line, which is defined by the equation, intersects or crosses the y-axis on a graph."
    }
    box(
      width = 12,
      title = "Definition",
      collapsible = TRUE,
      p(
        HTML(katex_html(var, displayMode = FALSE)),
        definition
      ),
    )

  })
  
  output$change_values <- renderUI({
    fluidRow(
      box(
        width = 12,
        title = "Change Values",
        collapsible = TRUE,
        column(width = 6,
               sliderInput("m", HTML(katex_html("m", displayMode = FALSE)), min = 0, max = 5, value = 1, step = 1)
        ),
        column(width = 6,
               sliderInput("x", HTML(katex_html("x", displayMode = FALSE)), min = 0, max = 10, value = 1, step = 1)
        ),
        column(width = 6,
               sliderInput("c", HTML(katex_html("c", displayMode = FALSE)), min = -2, max = 2, value = 0, step = 1)
        ),
        column(width = 6,
               actionButton(
                 style = "margin: 40px;",
                 inputId = "submit",
                 label = "Submit",
                 icon = icon("plus")
               )
        )
      )
      
    )
    
  })
  
  observeEvent(input$submit, {
    new_row <- data.frame(
      m = input$m,
      x = input$x,
      c = input$c,
      y = input$m * input$x + input$c
    )
    v$data <- rbind(v$data, new_row)
    
  })
  
  output$data_table <- DT::renderDataTable({
    v$data
  }, options = list(pageLength = 10))
  
  output$plots <- renderPlot({
    if (nrow(v$data)){
      ggplot(v$data, aes(x = .data[[input$plot_type]], y = y)) +
        geom_point(colour = "dodgerblue", size = 5
                   ) + 
        scale_x_continuous(name = input$plot_type) +
        scale_y_continuous(name = "Value of y") +
        theme_classic() 
    
      }
        
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
