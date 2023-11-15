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
              title = "Graph",
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
    data = data.frame(y = numeric(), m = numeric(), x = numeric(), c = numeric()),
    x = NULL,
    m = NULL,
    c = NULL,
    y = NULL
  )
  
  var_ids <- list("y", "m", "x", "c");
  var_texts <- list("y", "m", "x","c")
  vars <- setNames(var_ids, var_texts)
  
  var_ids_plot <- list("m", "x", "c");
  var_texts_plot <- list("m", "x","c")
  vars_plot <- setNames(var_ids_plot, var_texts_plot)
  

  vars_def <- list("m" = "Gradient is recommended between 2 - 5","x" = "X value is recommended between 1 -10","c" = "Intercept is recommended between 1-5")
  
  
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
      uiOutput("range_values"),
      uiOutput("default_values"),
      actionButton(
        style = "float: right;",
        inputId = "submit",
        label = "Submit",
        icon = icon("check")
      )
      
    )
    
  })
  output$range_values <- renderUI({
    def <- vars_def[[input$plot_type]]
    label_html <- paste0(
      katex_html(input$plot_type, displayMode = FALSE),
      sprintf('<i class="fa-regular fa-circle-question info" style="color: #c0bfbc; margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
    )
    div(
      style = "display: flex; align-items: center; justify-content: space-evenly;",
      textInput("min", label = "", value = "", width = "30%"),
      div(HTML(katex_html("\\leq", displayMode = FALSE))),
      div(HTML(label_html)),
      div(HTML(katex_html("\\leq", displayMode = FALSE))),
      textInput("max", label = "", value = "", width = "30%")
    )
  })
  output$default_values <- renderUI({
    inputs <- lapply(names(vars_plot), function(var_id) {
      if (var_id != input$plot_type) {
        def <- vars_def[[var_id]]
        label_html <- paste0(
          katex_html(vars_plot[[var_id]], displayMode = FALSE),
          sprintf('<i class="fa-regular fa-circle-question info" style="color: #c0bfbc; margin-left: 5px;" data-toggle="tooltip" data-placement="right" title="%s"></i>', def)
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
    input_values <- seq(input$min, input$max, length.out = 10)
    output_var <- "y"
    
    inputs <- setdiff(var_ids_plot, input$plot_type)
    for (inp in inputs){
      v[[inp]] <- as.numeric(input[[inp]])
    }
    range <- setdiff(var_ids_plot, inputs)
    
    
    for (r in range) {
      v[[r]] <- input_values
    }
    
    v$y <- v$m * v$x + v$c
    
    # Initialize an empty data frame to store the results
    result_df <- data.frame(matrix(ncol = length(vars), nrow = length(input_values)))
    colnames(result_df) <- names(vars)
    
    for (j in seq_along(vars)) {
      if (names(vars)[j] == input$plot_type) {
        result_df[, names(vars)[j]] <- input_values
      } else if (names(vars)[j] == output_var) {
        result_df[, names(vars)[j]] <- v$y
      } else {
        result_df[, names(vars)[j]] <- input[[names(vars)[j]]]
      }
    }
    
    print(result_df)
    
    # Assign result_df to v$data
    v$data <- result_df
    
  })
  output$plots <- renderPlot({
    if (nrow(v$data)) {
      ggplot(v$data, aes(x = .data[[input$plot_type]], y = y)) +
        geom_line(color = "dodgerblue", size = 2) +  # Use geom_line instead of geom_point
        scale_x_continuous(name = input$plot_type) +
        scale_y_continuous(name = "y") +
        theme_classic()
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
