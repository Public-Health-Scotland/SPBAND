# TITLE ----

# appears top left

dashboardtitle <- tags$a(href = "https://www.publichealthscotland.scot/",
                         target="_blank",
                         tags$imag(src = "phs-logo.png",
                                   alt = "Public Health Scotland logo",
                                   width = 120)
)

# HEADER BAR ----

# forces dashboard name to top right

header <- dashboardHeader(
  title = dashboardtitle,
  #titleWidth = 290,
  tags$li(class = "dropdown",
          tags$p("SPBAND v 1.0") # this is the LIVE dashboard - comment out as appropriate - and secure if PRA!
          # tags$p("SPBAND_PRA v 1.0") # this is the PRA dashboard
  )
)

# MENU ----

topicmenu <- sidebarMenu(
  id = "topics",
  menuItem("Home",
           tabName = "home",
           icon = icon("info-circle", verify_fa = FALSE) %>% rem_aria_label()
           ),
  menuItem("Pregnancy",
           icon = icon("person-pregnant", verify_fa = FALSE) %>% rem_aria_label(),
           menuSubItem("Gestation at booking",
                       tabName = "gestation_at_booking",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           )
  ) %>% rem_menu_aria_label()
)

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

# SIDEBAR ----

sidebar <- dashboardSidebar(#width = 280,
  useShinyjs(),
  accessible_menu(topicmenu),
  uiOutput("organisationControl"), # Board of Residence/Treatment
  uiOutput("hbnameControl"), # Board name
  uiOutput("dateControl"), # FY/CY
  hidden(
    textInput(inputId = "topics",
              label = "",
              value = "home") # forces input$topics to initialise as "home" to make filters appear correctly
  )
  #uiOutput("subgroupControl") # Age group/SIMD/Ethnicity - not currently used
  #textOutput("mytext") # for testing
)

# HOME ----

home <- tabItem(
  tabName = "home",
  
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
           
    ) # tabBox
    
  ) # fluidRow
  
) # tabItem ("home")

# GESTATION AT BOOKING ----

gestation_at_booking <- tabItem(
  tabName = "gestation_at_booking",
  
  fluidRow(
    tabBox(title = "Average gestation at booking",
           
           # The id lets us use input$tabset12 on the server to find the current tab
           id = "tabset12",
           width = 12,
           
           # Individual board
           
           tabPanel(title = "Individual board", #value = "gest_at_booking_board",
                    
                    fluidRow(
                      column(12,
                             p(textOutput("gest_at_booking_runcharts_title"
                             )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Average gestation at booking (based on completed weeks of
                               pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_booking_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gest_at_booking_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data refreshed on ", pretty_refresh_date),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Antenatal Booking Collection",
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ",
                             
                             actionLink(inputId = "link_to_patterns", 
                                        label = tags$u("How do we identify patterns in the data?")
                                        ),
                             " section on the Home page."
                               ),

                             p("The black dots connected by a line in the chart above show the average (mean) gestation at which women booked for their antenatal care (based on gestation at booking measured in completed weeks of pregnancy), for each month, from Apr 2019 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the overall average (median) of the mean gestation at booking each month over the period Apr 2019 to Feb 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             ),
                             
                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             )
                      )
                      
                    ) # fluidRow
                    
           ) # tabPanel("gest_at_booking_board")
    )
  )
)

# BODY ----

body <- dashboardBody(
  
  use_theme(mytheme), # <-- use the theme to change colours
  tags$head(includeCSS("www/styles.css")),
  
  tabItems(
    home,
    gestation_at_booking
  ) # tabItems
  
) # dashboardBody


ui <- 
  
  tagList( #needed for shinyjs
    #useShinyjs(),  # Include shinyjs
    tags$style("@import url(https://use.fontawesome.com/releases/v6.0/css/all.css);"),
    tags$head(
      HTML(
        "<html lang='en'>"),
      tags$link(rel="shortcut icon",
                href="favicon_phs.ico"), # Icon for browser tab
      tags$title("Scottish Pregnancy, Births and Neonatal Dashboard")
    ),
  
  dashboardPage(
  
  header,
  
  sidebar,
  
  body
  
  )
  
  )

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  #cdata <- session$clientData
  
  Selected <- reactiveValues(HBType = "RESIDENCE",
                             HBName = "Scotland",
                             Date = "2020/21",
                             Subgroup = "Age group",
                             Measure_cat = "all caesarean births",
                             Gestation = "under 32 weeks",
                             Nicename = "under 32 weeks")
  
  observeEvent(input$organisation, Selected$HBType <- input$organisation)
  
  observeEvent(input$hbname, Selected$HBName <- input$hbname)
  
  observeEvent(input$date, Selected$Date <- input$date)
  
  observeEvent(input$gestation, Selected$Gestation <- input$gestation)

  observeEvent(input$link_to_patterns, {
    updateTabsetPanel(session,
                      "tabset00",
                      "patterns")
  })
  
  # this observeEvent sets the current tabset back to the first tabPanel when a new tabset is selected from the
  # menu - this is needed to trigger the filter selections correctly
  
  observeEvent(input$topics,

               if (input$topics %in% names(tabnames)) {

                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset00", # home
                 #                   "instructions") 
                 
                 # removed as this was switching back to "instructions" if you clicked on another tab too soon after opening the dashboard - should not affect the workings as filters are not dependent on the tab selected here

                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset01", # multi_indicator_overview
                 #                   "Board comparison")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset10", # pregnancies_booked
                 #                   "Individual board")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset11", # terminations
                 #                   "Individual board")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset12", # gestation_at_booking
                                   "Board comparison")

                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset13", # gestation_at_termination
                 #                   "Board comparison")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset20", # location_of_ex_pre_term
                 #                   "Scotland")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset21", # inductions
                 #                   "Board comparison")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset22", # type_of_birth
                 #                   "Board comparison")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset23", # perineal_tears
                 #                   "Board comparison")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset24", # gestation_at_birth
                 #                   "Board comparison")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset25", # stillbirths
                 #                   "Scotland")
                 # 
                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset26", # apgar_scores
                 #                   "Board comparison")

                 Selected$Tabset <- "Board comparison" # forces reset for HBname filter where there is a Board comparison tab
               }
  )
  
  observe(print(paste0("Selected Tabset = ", Selected$Tabset)))
  observeEvent(input$link_to_patterns, print(Selected$Tabset))

  # observe(print(paste0("Measure_cat = ", Selected$Measure_cat)))
  observe(print(paste0("Home: ", input$tabset00)))
  
  observeEvent(input$tabset00, Selected$Tabset <- input$tabset00)
  # observeEvent(input$tabset01, Selected$Tabset <- input$tabset01)
  # observeEvent(input$tabset10, Selected$Tabset <- input$tabset10)
  # observeEvent(input$tabset11, Selected$Tabset <- input$tabset11)
  observeEvent(input$tabset12, Selected$Tabset <- input$tabset12)
  # observeEvent(input$tabset13, Selected$Tabset <- input$tabset13)
  # observeEvent(input$tabset20, Selected$Tabset <- input$tabset20)
  # observeEvent(input$tabset21, Selected$Tabset <- input$tabset21)
  # observeEvent(input$tabset22, Selected$Tabset <- input$tabset22)
  # observeEvent(input$tabset23, Selected$Tabset <- input$tabset23)
  # observeEvent(input$tabset24, Selected$Tabset <- input$tabset24)
  # observeEvent(input$tabset25, Selected$Tabset <- input$tabset25)
  # observeEvent(input$tabset26, Selected$Tabset <- input$tabset26)
  # observeEvent(input$tabset31, Selected$Tabset <- input$tabset31)  # testing whether can jump to a tabset
  
  # select ORGANISATION (RESIDENCE or TREATMENT)
  
  output$organisationControl <- renderUI({ 
    hidden(
      radioButtons(
        inputId = "organisation",
        label = "View analyses by Board of",
        choiceNames = list("Residence", "Treatment"),
        choiceValues = list("RESIDENCE", "TREATMENT"),
        selected = "RESIDENCE",
        inline = FALSE
      )
    )
  })
  
  # determines whether the ORGANISATION filter should show or not
  
  observe({
    toggleElement(id = "organisation",
                  condition = (input$topics %in% show_org &
                                 Selected$Tabset != "About this measure")
    )
  })
  
  # select hbname
  
  output$hbnameControl <- renderUI({
    hidden(
      pickerInput(
        #session = session,
        inputId = "hbname",
        label = "Select Board",
        choices = HBnames,
        selected = "Scotland",
        options = pickerOptions(size = 10), # shows 10 boards and a scroll bar - will drop up and not get hidden?
        choicesOpt = list(
          style = rep("color: #3F3685;", 15) # PHS-purple text
        )
      )
    )
  })
  
  # determines whether the hbname filter should show or not
  
  observe(toggleElement(id = "hbname",
                        condition = ((input$topics %in% show_HBname &
                                        Selected$Tabset != "About this measure") |
                                       (input$topics %in% show_HBname2 &
                                          !Selected$Tabset %in% c("Board comparison", "About this measure"))
                        )
  )
  )
  
  # select date (financial year or calendar year)
  
  output$dateControl <- renderUI({ 
    hidden(
      pickerInput(
        inputId = "date",
        label = "Choose time period",
        choices = factor_labels_year,
        selected = "2022",
        width = "fit",
        choicesOpt = list(
          style = rep("color: #3F3685;",
                      length(factor_labels_year))
        )
      )
    )
  })
  
  # determines whether the date filter should show or not
  
  observe(
    toggleElement(id = "date",
                  condition = (input$topics == "multi_indicator_overview")
    )
  )
  
  # this section tells the app where to find the code for each tab
  
  source("Antenatal booking/Average gestation at booking small multiples.R", local = TRUE)
  
  source("Antenatal booking/Average gestation at booking runcharts.R", local = TRUE)
  
  source("Antenatal booking/Average gestation at booking download data.R", local = TRUE)
  
}

shinyApp(ui, server)  
