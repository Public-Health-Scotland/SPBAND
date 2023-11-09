library(shiny)
library(shinydashboard)

# HOW TO USE THIS DASHBOARD ----

instructions <- 
  
  tabPanel(title = "How to use this dashboard",
           value = "instructions",
           
           fluidRow(
             
             br(),
             
             br(),
             
             p("Click the + to open the sections below for help", 
               style = "text-align: right;"),
             
             box(title = p(strong("Navigation and filtering")),
                 status = "primary",
                 width = 12,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 
                 column(10,
                        p("This dashboard has been developed with simplicity in mind. The ", strong("left-hand navigation menu"), " lists all the content available and will expand and collapse when a sub-menu with an arrow to the right is selected."
                        )
                 )
             )
           )
  )

# HOW DO WE IDENTIFY PATTERNS IN THE DATA? ----

patterns <- 
  
  tabPanel(title = "How do we identify patterns in the data?",
           value = "patterns",
           
           fluidRow(
             
             br(),
             
             box(solidHeader = TRUE,
                 width = 12,
                 
                 p(strong("Run charts"), "have been used to show time series data for many of the measures in this dashboard. Run charts use a series of rules to help identify important change in the data. These are the ones we used for these charts:"
                 ),
                 
                 tags$ul(
                   tags$li(class= "bullet-points",
                           strong("Shifts:"), " Six or more consecutive data points above or below the centreline. Points on the centreline neither break nor contribute to a shift (marked as a yellow line on charts)."
                   ),
                   
                   tags$li(class= "bullet-points",
                           strong("Trends:"), " Five or more consecutive data points which are increasing or decreasing. An observation that is the same as the preceding value does not count towards a trend (marked as green highlights on charts)."
                   )
                 )
             )
           )
  )

ui <- dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  dashboardSidebar(
    sidebarMenu(id = "topics",
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      menuItem("Pregnancy",
               menuSubItem("Gestation at booking",
                       tabName = "gestation_at_booking")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              
              fluidRow(
    
                h1("Welcome to the Scottish Pregnancy, Births and Neonatal Data dashboard",
                   class = "smaller--h1"
                ),
                
                hr(),
                
                tabBox(title = "Home",
                       
                       # The id lets us use input$tabset00 on the server to find the current tab
                       id = "tabset00",
                       width = 12,
                       
                       instructions,
                       
                       patterns
                       
                ) # tabBox, actionButton('switchtab', 'Switch tab')
              )
      ),
      
      tabItem(tabName = "gestation_at_booking",
              
              fluidRow(
                tabBox(title = "Average gestation at booking",
                       
                       # The id lets us use input$tabset12 on the server to find the current tab
                       id = "tabset12",
                       width = 12,
                       
                       # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "gest_at_booking_overview",
                    
                    fluidRow(
                      column(12,
                             p(textOutput("gest_at_booking_small_multiples_title"
                             )
                             ),
                             
                             br()
                             
                      )
                    )
           ),
                       
                       # Individual board
           
           tabPanel(title = "Individual board", #value = "gest_at_booking_board",
                    
                    fluidRow(
                      column(12,
                             p(textOutput("gest_at_booking_runcharts_title"
                             ),
                             
                             verbatimTextOutput("tabset1Selected")
                             ),
                             
                             actionButton('switchtab1', 'Switch tab1')
                             # 
                             # actionButton('switchtab2', 'Switch tab2')
                             
                      )
                    )
           )
                )
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
    output$tabset1Selected <- renderText({
      input$tabset00
  })
    
    observeEvent(input$switchtab1, {
    newtab <- switch(input$tabset00, "instructions" = "patterns")

    # updateTabsetPanel(getDefaultReactiveDomain(),
    #                   "tabset00", # home
    #                   "patterns")
    updateTabsetPanel(session, "tabset00", newtab)
  })

  # observeEvent(input$switchtab2, {
  #   newtab <- switch(input$topics, "home" = "gestation_at_booking", "gestation_at_booking" = "home")
  #   updateTabItems(session, "topics", newtab)
  # })
  
  #observe(print(paste0("Selected Tabset = ", Selected$Tabset)))
  #observe(print(paste0("newtab = ", newtab)))
}

shinyApp(ui, server)