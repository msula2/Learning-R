#Setup
library(shiny)
library(shinydashboard)

#UI
ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Basic Template", 
                                    titleWidth = "calc(100% - 44px)"
                                    ),
                    dashboardSidebar(
                      #The sidebar contains a sidebarMenu which needs a unique id  and a list of menu items
                      #Any other element can also be added to the sidebar such as we add a link using tags$a
                      sidebarMenu(id = "tabs",
                                  menuItem(
                                    "Collapsable Tabs",
                                    tabName = "collapse_tab",
                                    icon = icon("canadian-maple-leaf")
                                  ),
                                  menuItem(
                                    "Nested Tabs",
                                    tabName = "nested_tab",
                                    icon = icon("canadian-maple-leaf")
                                  ),
                                  menuItem(
                                    "Rows Tab",
                                    tabName = "rows_tab",
                                    icon = icon("canadian-maple-leaf")
                                  ),
                                  menuItem(
                                    "Columns Tab",
                                    tabName = "cols_tab",
                                    icon = icon("canadian-maple-leaf")
                                  )
                      )
                      
                    ),
                    #If we want to use js functions we need to specify using shinyjs::useShinyjs, we can then include the head to link custom css and js files
                    dashboardBody(shinyjs::useShinyjs(),
                                  tags$head(
                                    #link to files
                                    tags$link(rel = "stylesheet",
                                              type = "text/css",
                                              href = "custom.css"),
                                    tags$script(src = "custom.js")
                                  ),
                                  tabItems(
                                    tabItem(tabName = "collapse_tab",
                                            box(
                                              title = "Personal Info",
                                              collapsible = TRUE,
                                              textInput("given", "Given Name"),
                                              textInput("surname", "Surname"),
                                              selectInput("pet", "What is your favourite pet?", c("cats", "dogs", "ferrets")),
                                              
                                            ),
                                            box(
                                              title = "Biography",
                                              collapsible = TRUE,
                                              textAreaInput("bio", NULL, height = "100px", placeholder = "Breif bio")
                                            )
                                          ),
                                    tabItem(tabName = "nested_tab",
                                            tabBox(
                                              title = "Test Yourself - 1",
                                              tabPanel("Question", "What function creates tabBox contents"),
                                              tabPanel("Answer", "tabPanel()")
                                            ),
                                            tabBox(
                                              title = "Test Yourself - 2",
                                              side = "right",
                                              selected = "Question",
                                              tabPanel("Answer", "selected"),
                                              tabPanel("Question", "What attributes changes the default tab?")
                                            )
                                            
                                      ),
                                    tabItem(tabName = "rows_tab",
                                            fluidRow(
                                              box("A", title = "2x100", width = 2, height = 100),
                                              box("B", title = "1x100", width = 1, height = 100),
                                              box("C", title = "2x200", width = 2, height = 200),
                                              box("D", title = "3x300", width = 3, height = 300),
                                              box("E", title = "4x100", width = 4, height = 100),
                                              box("F", title = "5x100", width = 5, height = 100),
                                              box("G", title = "7x100", width = 7, height = 100)
                                            )
                                     ),
                                  tabItem(tabName = "cols_tab",
                                          column(width = 6,
                                                 box("A", title = "12x100", width = 12, height = 100),
                                                 box("B", title = "6x100", width = 6, height = 100),
                                                 box("C", title = "6x200", width = 6, height = 200)
                                          ), 
                                          column(width = 4,
                                                 box("D", title = "12x300", width = 12, height = 300),
                                                 box("E", title = "12x100", width = 12, height = 100)
                                          ),
                                          column(width = 2,
                                                 box("F", title = "12x100", width = 12, height = 100),
                                                 box("G", title = "12x100", width = 12, height = 100)
                                          ) 
                                    )
                                    
                                  )
                              )
                    )

#Server configuration
server <- function(input, output, sessions){}

#Run the application
shinyApp(ui = ui, server = server)