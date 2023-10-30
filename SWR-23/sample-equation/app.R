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
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  var_ids <- list("y", "m", "x", "c");
  var_texts <- list("y", "m", "x","c")
  vars <- setNames(var_ids, var_texts)
  
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
  output$about_variable <- renderUI({
    var <- input$variable
    if (var == "y"){
      definition <- "is the dependent variable, also known as the response variable."
    }
    else if (var == "m"){
      definition <- "is the slope and it is a measure of the steepness. It describes how much 'y' changes for every unit change in 'x'."
      max <- 5
      min <- 0
      value <- 1
      if (!is.null(input$m)){
        value <- input$m
      }
    }
    else if (var == "x"){
      definition <- "is the independent variable. The independent variable is the variable that you can manipulate or change to observe its effect on the dependent variable 'y'."
      max <- 7
      min <- -7
      value <- 3
      if (!is.null(input$x)){
        value <- input$x
      }
    }
    else{
      definition <- "is y-intercept. It is the value of the dependent variable 'y' when the independent variable 'x' is equal to zero. It represents the point where the line, which is defined by the equation, intersects or crosses the y-axis on a graph."
      max <- 4
      min <- -4
      value <- 0
      if (!is.null(input$c)){
        value <- input$c
      }
    }
    if (var == "y"){
      box(
        width = 12,
        title = "Definition",
        collapsible = TRUE,
        p(
          HTML(katex_html(var, displayMode = FALSE)),
          definition
        ),
      )      
    }
    else{
      box(
        width = 12,
        title = "Definition",
        collapsible = TRUE,
        p(
          HTML(katex_html(var, displayMode = FALSE)),
          definition
        ),
        sliderInput(var,
                    label = "Change the value",
                    min = min,
                    max = max,
                    value = value,
                    step = 1,
                    width = "100%")
      )
    }

  })
  
  output$result <- renderText({
    y <- input$m * input$x + input$c
    sprintf("Output is %s", y)
  })  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
