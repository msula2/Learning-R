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
      ),
      menuItem(
        "Dogs",
        tabName = "dogs_tab",
        icon = icon("dog")
      ),
      menuItem(
        "Cats",
        tabName = "cats_tab",
        icon = icon("cat")
      ),
      menuItem(
        "Birds",
        tabName = "birds_tab",
        icon = icon("crow")
      ),
      menuItem(
        "Spider",
        tabName = "spiders_tab",
        icon = icon("spider")
      )
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "home_tab",
        verticalLayout(
          h2("Welcome to the pet store!"),
          p("Our store hosts a range of pets from cute cats to scary snakes. Learn more about each pet by clicking their respective tab")
        )
      ),
      tabItem(
        tabName = "dogs_tab",
        column(
          width = 6,
          box(
            width = 12,
            h2("About"),
            p("There's nothing quite like the love and excitement a puppy brings to a home. 
              Our pet store offers a heartwarming selection of adorable puppies, each as unique as a snowflake. 
              Whether you're looking for a spirited playmate, a devoted guardian, or a cuddly lap dog, we're here to help you find your furry soulmate.")
          ),
          box(
            width = 12,
            h2("Tell us what you think about them:"),
            sliderInput("dog_slider",
                        label = "Willngness to keep a dog as a pet",
                        min = 0,
                        max = 10,
                        value = 0,
                        step = 1,
                        width = "100%"),
            selectInput("dog_select", 
                        label = "Do you think dogs are: ", 
                        choices = list("", 
                                       "Friendly" = "friendly", 
                                       "Non-Friendly" = "non-friendly"),
                        selected = NULL,
                        width = "100%"),
            actionButton(
              inputId = "submit_dog",
              label = "Submit",
              icon = icon("check")
            )
          )
        ),
        column(
          width = 6,
          plotOutput("sliderPlotDog")
          
        )
        
      ),
      tabItem(
        tabName = "cats_tab",
        column(
          width = 6,
          box(
            width = 12,
            h2("About"),
            p("Cats, known for their grace, independence, and charm, bring joy to countless households around the world.
              With their soft purrs and gentle meows, they effortlessly melt our hearts.")
          ),
          box(
            width = 12,
            h2("Tell us what you think about them:"),
            sliderInput("cat_slider",
                        label = "Willngness to keep a cat as a pet",
                        min = 0,
                        max = 10,
                        value = 0,
                        step = 1,
                        width = "100%"),
            selectInput("cat_select", 
                        label = "Do you think cats are: ", 
                        choices = list("", 
                                       "Friendly" = "friendly", 
                                       "Non-Friendly" = "non-friendly"),
                        selected = NULL,
                        width = "100%"),
            actionButton(
              inputId = "submit_cat",
              label = "Submit",
              icon = icon("check")
            )
          )
        ),
        column(
          width = 6,
          plotOutput("sliderPlotCat")
          
        )
      ),
      tabItem(
        tabName = "birds_tab",
        column(
          width = 6,
          box(
            width = 12,
            h2("About"),
            p("From the sweet melodies of canaries to the intelligent antics of parrots, our diverse selection of birds represents the beauty and diversity of the avian world. 
              Our mission is to connect these wonderful feathered friends with loving homes, where they can bring joy, laughter, and a touch of nature's wonder to your life.")
          ),
          box(
            width = 12,
            h2("Tell us what you think about them:"),
            sliderInput("bird_slider",
                        label = "Willngness to keep a bird as a pet",
                        min = 0,
                        max = 10,
                        value = 0,
                        step = 1,
                        width = "100%"),
            selectInput("bird_select", 
                        label = "Do you think birds are: ", 
                        choices = list("", 
                                       "Friendly" = "friendly", 
                                       "Non-Friendly" = "non-friendly"),
                        selected = NULL,
                        width = "100%"),
            actionButton(
              inputId = "submit_bird",
              label = "Submit",
              icon = icon("check")
            )
          )
        ),
        column(
          width = 6,
          plotOutput("sliderPlotBird")
          
        )
      ),
      tabItem(
        tabName = "spiders_tab",
        column(
          width = 6,
          box(
            width = 12,
            h2("About"),
            p("Arachnids have long been shrouded in myths and legends, but their unique allure and distinct beauty make them a truly extraordinary choice for pet enthusiasts")
          ),
          box(
            width = 12,
            h2("Tell us what you think about them:"),
            sliderInput("spider_slider",
                        label = "Willngness to keep a spider as a pet",
                        min = 0,
                        max = 10,
                        value = 0,
                        step = 1,
                        width = "100%"),
            selectInput("spider_select", 
                        label = "Do you think spiders are: ", 
                        choices = list("", 
                                       "Friendly" = "friendly", 
                                       "Non-Friendly" = "non-friendly"),
                        selected = NULL,
                        width = "100%"),
            actionButton(
              inputId = "submit_spider",
              label = "Submit",
              icon = icon("check")
            )
          )
        ),
        column(
          width = 6,
          plotOutput("sliderPlotSpider")
          
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  slider <- reactiveValues(vals = c(0,0,0,0,0), labels = c("Dogs", "Cats", "Scorpions", "Snakes", "Birds"))
  
  observeEvent(input$submit_dog, {
    selected_index <- which(slider$labels == "Dogs")
    if (length(selected_index) > 0) {
      slider$vals[selected_index] <- input$dog_slider
    }
    data <- data.frame(Name = slider$labels,
                       Score = slider$vals)
    
    output$sliderPlotDog <- renderPlot({
      ggplot(data, aes(x = Name, y = Score)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Willingness to keep pet", x = "Name", y = "Willingness") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme_classic()
    })
  })
    
  observeEvent(input$submit_cat, {
    selected_index <- which(slider$labels == "Cats")
    if (length(selected_index) > 0) {
      slider$vals[selected_index] <- input$cat_slider
    }
    data <- data.frame(Name = slider$labels,
                       Score = slider$vals)
    
    output$sliderPlotCat <- renderPlot({
      ggplot(data, aes(x = Name, y = Score)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Willingness to keep pet", x = "Name", y = "Willingness") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme_classic()
    })
  })
  
  observeEvent(input$submit_bird, {
    selected_index <- which(slider$labels == "Birds")
    if (length(selected_index) > 0) {
      slider$vals[selected_index] <- input$bird_slider
    }
    data <- data.frame(Name = slider$labels,
                       Score = slider$vals)
    
    output$sliderPlotBird <- renderPlot({
      ggplot(data, aes(x = Name, y = Score)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Willingness to keep pet", x = "Name", y = "Willingness") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme_classic()
    })
  })
  
  observeEvent(input$submit_spider, {
    selected_index <- which(slider$labels == "Spiders")
    if (length(selected_index) > 0) {
      slider$vals[selected_index] <- input$spider_slider
    }
    data <- data.frame(Name = slider$labels,
                       Score = slider$vals)
    
    output$sliderPlotBird <- renderPlot({
      ggplot(data, aes(x = Name, y = Score)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(title = "Willingness to keep pet", x = "Name", y = "Willingness") +
        scale_y_continuous(breaks = seq(0, 10, by = 1)) +
        theme_classic()
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
