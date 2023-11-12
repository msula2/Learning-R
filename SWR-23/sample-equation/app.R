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
      ),
      menuItem(
        "Penman-Monteith Equation",
        tabName = "pm_tab",
        icon = icon("droplet")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
      tags$link(rel="stylesheet", href="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.css", integrity="sha384-9tPv11A+glH/on/wEu99NVwDPwkMQESOocs/ZGXPoIiLE8MU/qkqUcZ3zzL+6DuH", crossorigin="anonymous"),
      tags$script(src="https://cdn.jsdelivr.net/npm/katex@0.10.0-beta/dist/katex.min.js", integrity="sha384-U8Vrjwb8fuHMt6ewaCy8uqeUXv4oitYACKdB0VziCerzt011iQ/0TqlSlv8MReCm", crossorigin="anonymous")
      ),
      tags$script(HTML('
        $(document).ready(function() { 
        $(".info").tooltip(); 
        });
    ')),
    tags$style(HTML(".main-footer { display: none; } a {display: inline;} .tooltip-inner {background-color: #336699; color: #ffffff;border-radius: 5px; } .tooltip.bs-tooltip-top .tooltip-arrow::before {border-top-color: #336699;}")),
    tabItems(
      tabItem(
        tabName = "line_tab",
        column(
          width = 4,
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
            uiOutput("about_variable")
          )
        ),
        column(
          width = 4,
          uiOutput("change_values")
        ),
        column(
          width = 4,
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
              plotOutput("plots")
            )
          )
        )
      ),
      tabItem(
        tabName = "pm_tab",
        column(
          width = 6,
          box(
            width = 12,
            title = "About",
            collapsible = TRUE,
            p(
              "The Penman-Monteith equation is a widely used method for calculating 
              the evapotranspiration (ET) of water from the land surface into the atmosphere. 
              Evapotranspiration is the combined process of evaporation, which is the loss 
              of water from the land surface directly into the atmosphere, and transpiration, 
              which is the release of water vapor from plants through their leaves."
            ),
            helpText(
              HTML(katex_html(
                "\\lambda{ET} = \\frac{ \\Delta (R_n - G) + \\rho_a c_p \\frac{(e_s - e_a)}{r_a} }{\\Delta + \\gamma(1 + \\frac{r_s}{r_a})}",
                displayMode = TRUE, 
                preview = FALSE,
                include_css = TRUE,
                output = "html")
              )
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
  
  var_ids_plot <- list("m", "x", "c", "d", "e");
  var_texts_plot <- list("m", "x","c", "d", "e")
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
    box(
      width = 12,
      title = "Set Arguments",
      collapsible = TRUE,
      selectizeInput("plot_type", label = "Plot by:", choices = NULL),
      div(
        style = "display: flex; justify-content: space-evenly;",
        textInput("min", 
                  label = "Minimum",
                  value = "", 
                  width = "30%"
        ),
        textInput("max", 
                  label = "Maximum",
                  value = "",
                  width = "30%"
        )
      ),
      uiOutput("default_values")
      
    )
    
  })
  
  output$default_values <- renderUI({
    inputs <- lapply(names(vars_plot), function(var_id) {
      if (var_id != input$plot_type) {
        label_html <- paste0(
          katex_html(vars_plot[[var_id]], displayMode = FALSE),
          sprintf('<i class="fa-regular fa-circle-question info" style="color: #c0bfbc; margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', var_id)
        )
        return(
          column(
            width = 6,
            textInput(var_id,
                      label = HTML(label_html),
                      value = "",
                      width = "100%"
            )    
          )
        )
      }
    })
    
    fluidRow(inputs)
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
