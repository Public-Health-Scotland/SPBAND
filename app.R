# source global settings
source("global.R")

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
          # comment out version as appropriate - and secure if PRA or TEST!
          #tags$p("SPBAND v 1.6") # this is the LIVE dashboard 
          #tags$p("SPBAND_test v 1.6") # this is the TEST dashboard 
          tags$p("SPBAND_PRA v 1.6") # this is the PRA dashboard
  )
)

# MENU ----

topicmenu <- sidebarMenu(
  id = "topics",
  menuItem("Home",
           tabName = "home",
           icon = icon("info-circle", verify_fa = FALSE) %>% rem_aria_label()
           ),
  menuItem("Multi indicator overview",
           tabName = "multi_indicator_overview",
           icon = icon("tachometer-alt", verify_fa = FALSE) %>% rem_aria_label()
  ),
  menuItem("Pregnancy",
           icon = icon("person-pregnant", verify_fa = FALSE) %>% rem_aria_label(),
           menuSubItem("Number of pregnancies booked", 
                       tabName = "pregnancies_booked",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Number of terminations",
                       tabName = "terminations",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Gestation at booking",
                       tabName = "gestation_at_booking",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Gestation at termination",
                       tabName = "gestation_at_termination",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           )
  ) %>% rem_menu_aria_label(),
  menuItem("Births and babies",
           icon = icon("baby", verify_fa = FALSE) %>% rem_aria_label(),
           menuSubItem("Location of extremely pre-term births",
                       tabName = "location_of_ex_pre_term",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Induction of labour",
                       tabName = "inductions",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Type of birth",
                       tabName = "type_of_birth",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Third- and fourth-degree perineal tears",
                       tabName = "perineal_tears",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Gestation at birth: pre- and post-term births",
                       tabName = "gestation_at_birth",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Stillbirths and infant deaths",
                       tabName = "stillbirths",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           ),
           menuSubItem("Apgar scores",
                       tabName = "apgar_scores",
                       icon = shiny::icon("angle-double-right") %>% rem_aria_label()
           )
  ) %>% rem_menu_aria_label(),
  # menuItem("Neonatal",
  #          icon = icon("hand-holding-medical", verify_fa = FALSE) %>% rem_aria_label(),
  #          menuSubItem("Median corrected gestational age at discharge from neonatal care",
  #                      tabName = "median_cga_30_32",
  #                      icon = shiny::icon("angle-double-right") %>% rem_aria_label()
  #          ),
  #          menuSubItem("Admissions to a neonatal unit by level of care",
  #                      tabName = "gestation_by_BAPM_LOC",
  #                      icon = shiny::icon("angle-double-right") %>% rem_aria_label()
  #          )
  # ) %>% rem_menu_aria_label(),
  menuItem("Infant feeding",
           tabName = "infant_feeding",
           icon = icon("person-breastfeeding", verify_fa = FALSE) %>% rem_aria_label()
  ) %>% rem_menu_aria_label()

)

# HOW TO USE THIS DASHBOARD ----

instructions <- 
  
  tabPanel(title = "How to use this dashboard",
           value = "instructions",
           
           fluidRow(
             
             br(),
             
             br(),
             
             p("Click +/- to open and close the sections below", 
               style = "text-align: right;"),
             
             box(title = p(strong("Navigation and filtering")),
                 status = "primary",
                 width = 12,
                 collapsible = TRUE,
                 collapsed = FALSE,
                 
                 column(10,
                        p("This dashboard has been developed with simplicity in mind. The ", strong("left-hand navigation menu"), " lists all the content available and will expand and collapse when a sub-menu with an arrow to the right is selected."
                        ),
                        
                        p("The left-hand menu can also be hidden (or revealed) by clicking on this toggle button", img(src = "toggle.png", alt = ""), " at the top of the screen. This can be useful if you are viewing charts on a small computer screen."
                        ),
                        
                        p("Data for individual measures are available in the ", strong("Pregnancy"), " and ", strong("Births and  babies"), " sub-menus."
                        ),
                        
                        p("Each measure has an ", strong("About this measure"), " tab which is visible when the measure is selected."
                        )
                 ),
                 
                 column(2,
                        img(src = "nav_menu.png", alt = "The left-hand navigation menu.", width = "70%", height = "70%"
                        ),
                        
                        br(),
                        
                        br()
                        
                        ),
                 
                 column(10,
                        p("The ", strong("selection filters"), " are under the navigation menu. Filter values are persistent, which means that the same selections will be in place regardless of the measure chosen."
                        ),
                        
                        p("Most measures are available by Health Board of Residence (based on home postcode) and by Health Board of Treatment (based on location of care). Individual Boards can be selected unless a measure is only available for Scotland."
                        ),
                        
                        p("Where applicable, time periods can be selected (by calendar year – Jan-Dec, and by financial year – Apr-Mar). Time periods are only relevant to the ", strong("Multi indicator overview"), " and will not be visible when viewing other content."
                        )
                 ),
                 
                 column(2,
                        img(src = "filter.png", alt = "The selection filters.", width = "70%", height = "70%"
                        )
                        
                 )
                 
             ) # box "Navigation and filtering"
             
             %>% rem_button_aria_label(), 
             
             box(title = p(strong("There are three ways to view data")),
                 status = "primary",
                 width = 12,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 
                 column(12,
                        tags$ol(
                          tags$li(class = "bullet-points", "Multi indicator overview"), 
                          tags$li(class = "bullet-points", "Board comparison"), 
                          tags$li(class = "bullet-points", "Individual Board")
                        )
                 ),
                 
                 column(12,
                        p("The ", strong("Multi indicator overview"), " shows a selection of the Core measures. All NHS Boards can be compared at once in the ", strong("Board comparison"), " tab. When a Board is selected in the filter panel its values appear as green dots; Scotland values are shown as black dots; the remaining Boards values are shown as light grey dots. The selected Board values can be compared against Scotland in the table shown on the ", strong("Individual Board"), " tab."
                        ),
                        
                        p("Individual measures are available in the ", strong("Pregnancy"), " and ", strong("Births and babies"), "sub-menus. Most measures will have ", strong("Board comparison"), " and ", strong("Individual Board"), " tabs. Those that are only available for Scotland will have a ", strong("Scotland"), " tab instead."
                        ),
                        
                        p("Where applicable, ", strong("Board comparison"), " tabs show simple time series charts for all NHS Boards in a grouped layout. These are usually shown on the same scale to allow easy comparison over the same time periods. Where necessary, Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) may have a different y-axis to allow the mainland Boards charts to be easier to read. Note there are no individual charts available for the Island Boards for the ‘Gestation at termination’ measure as small numbers are disclosive. If an Island Board is selected in the filter panel the values relating to this measure will be aggregated values for NHS Orkney, NHS Shetland and NHS Western Isles combined. The charts default to show data by NHS Board of Residence but the filter is available to switch to NHS Board of Treatment." 
                        ),
                        
                        p(strong("Individual Board"), " tabs show a more detailed time series chart (‘Number of pregnancies booked’, ‘Number of terminations’) or run chart (all other measures excluding the ‘Location of extremely pre-term births’ and ‘Stillbirths and infant deaths’). The charts default to show data for Scotland but the filters are available to change the content. Some measures also have a ‘context’ chart below the run charts. These show time series of counts of the data, for example, singleton live births at any gestation by type of birth and all live births."
                        )
                 )
                 
             ) # box "There are three ways to view data"
             
             %>% rem_button_aria_label(),
             
             box(title = p(strong("Notes on particular measures")),
                 status = "primary",
                 width = 12,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 
                 column(12,
                        p("The ", strong("Location of extremely pre-term births"), " measure has a ", strong("Scotland"), " tab which shows a control chart. Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts by clicking the tab ", strong("How do we identify patterns in the data?"), " There are no filters applicable to this chart."
                        ),
                        
                        p("The ", strong("Stillbirths and infant deaths"), " measure also has a ", strong("Scotland"), " tab. This shows simple time series of the relevant rates for stillbirths, neonatal deaths, extended perinatal deaths, post-natal deaths, and infant deaths. Please see the related ", strong("About this measure"), " for more information about these terms. There are no filters applicable to this chart."
                        )
                 )
                 
             ) # box "Notes on particular measures"
             
             %>% rem_button_aria_label(), 
             
             box(title = p(strong("Copying charts and downloading data")),
                 status = "primary",
                 width = 12,
                 collapsible = TRUE,
                 collapsed = TRUE,
                 
                 column(12,
                        p("To make a ", strong("copy"), " of any content it is recommended that a snipping tool is used as this will ensure all titles and footnotes can be included in the snapshot. For Windows, use the Windows logo key + Shift + S; for Apple Mac use Command + Shift + 5."
                        ),
                        
                        p("All charts, including the ", strong("Multi indicator overview"), " have their associated data available to ", strong("download"), " by clicking this button", img(src = "download_button.png", alt = ""     
                        ),
                        
                        "Data are now available in an accessible Excel file format which contains metadata for the relevant variables. The same data are available from either download button within an measure."
                        )
                 )
                 
             ) # box "Copying charts and downloading data"
             
             %>% rem_button_aria_label(), 
             
             box(title = p(strong("Tell us what you think")),
                 status = "primary",
                 width = 12,
                 collapsible = FALSE,
                 collapsed = FALSE,
                 
                 column(12,
                        p("The version of the dashboard available today is still in development and is subject to changes and refinements in the future. Contact ", tags$a(href = "mailto:phs.matneodatahub@phs.scot", tags$u("phs.matneodatahub@phs.scot")), "for more information or to provide feedback."
                        )
                 )
                 
             ) # box "Tell us what you think"
             
             #%>% rem_button_aria_label()
             
           ) # fluidRow
           
  ) # tabPanel("How to use this dashboard")

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
                   tags$li(class = "bullet-points",
                           strong("Shifts:"), " Six or more consecutive data points above or below the centreline. Points on the centreline neither break nor contribute to a shift (marked as a yellow line on charts)."
                   ),
                   
                   tags$li(class = "bullet-points",
                           strong("Trends:"), " Five or more consecutive data points which are increasing or decreasing. An observation that is the same as the preceding value does not count towards a trend (marked as green highlights on charts)."
                   ),
                   
                   tags$li(class = "bullet-points",
                           strong("Too many or too few runs:"), " A run is a sequence of one or more consecutive observations on the same side of the centreline. Any observations falling directly on the centreline can be ignored. If there are too many or too few runs (i.e. the median is crossed too many or too few times) that is a sign of something more than random chance."
                   ),
                   
                   tags$li(class = "bullet-points",
                           strong("Astronomical data point:"), " A data point which is distinctly different from the rest. Different people looking at the same graph would be expected to recognise the same data point as astronomical (or not)."
                   )
                 ),
                 
                 br(),
                 
                 p("The", strong("Location of extremely pre-term births"), "charts are ", strong("control charts."), "These charts have additional ‘control limits’ to indicate how much random variation we would expect to see by chance. They also use slightly different rules for", strong("shifts"), "and", strong("trends."), "The five rules we use are:"
                 ),
                 
                 tags$ul(
                   tags$li(class = "bullet-points",
                           strong("Outliers:"), " Data points outside the limits marked by the control limits."
                   ),
                   
                   tags$li(class = "bullet-points",
                           strong("Shifts:"), " Eight or more consecutive data points above or below the centreline.",
                   ),
                   
                   tags$li(class = "bullet-points",
                           strong("Trends:"), " Six or more consecutive data points which are increasing or decreasing."
                   ),
                   
                   tags$li(class = "bullet-points",
                           strong("Outer one–third:"), " Two out of three consecutive data points which sit between the control and warning limits."
                   ),
                   
                   tags$li(class = "bullet-points",
                           strong("Inner one-third:"), " 15 or more consecutive data points that lie close to the centreline."
                   )
                 ),
                 
                 p("The type of chart used depends on the type of data involved (which statistical distribution we think it follows). For the ", strong("Location of extremely pre-term births"), " measure P charts are presented."
                 ),
                 
                 br(),
                 
                 p("Further information on these methods of presenting data can be found in this ",
                   
                   tags$a(
                     href = "https://webarchive.nrscotland.gov.uk/20231129152542mp_/https://www.isdscotland.org/Health-Topics/Quality-Indicators/Statistical-Process-Control/_docs/Statistical-Process-Control-Tutorial-Guide-180713.pdf",
                     tags$u("guide to statistical process control charts (PDF)."),
                     class = "externallink",
                     target = "_blank"
                   )
                 )
                 
             ) # box
             
           ) # fluidRow
           
  ) # tabPanel("How do we identify patterns in the data?")

# BACKGROUND ----

background <- 
  
  tabPanel(title = "Background",
           value = "background",
           
           fluidRow(
             
             br(),
             
             box(solidHeader = TRUE,
                 width = 12,
                 
                 br(),
                 
                 p("The data displays on this ", strong("Scottish Pregnancy, Births and Neonatal Data (SPBAND) Dashboard"), " have been developed in response to commitment 67 in ",
                   
                   tags$a(
                     href = "https://www.gov.scot/publications/best-start-five-year-forward-plan-maternity-neonatal-care-scotland/",
                     tags$u("The Best Start: five year plan for maternity and neonatal care"),
                     class = "externallink",
                     target = "_blank"
                   ),
                   
                   " that:  ‘national level maternity and neonatal dashboards should be developed to facilitate benchmarking and reduce variations in care’."
                 ),
                 
                 p("During 2019 the ",
                   
                   tags$a(
                     href = "https://www.perinatalnetwork.scot/data/",
                     tags$u("MatNeo Data Hub"),
                     class = "externallink",
                     target = "_blank"
                   ),
                   
                   " compiled an initial list of CORE maternity measures by exploring what measures are used in a variety of local and national dashboards."
                 ),
                 
                 p("Given the extensive work done in England through a Delphi process to derive 14 monthly Clinical Quality Improvement Metrics or ", strong("CQIMS,"), " the concepts included in the CQIMs measures were used as a starting point for further engagement on establishing core measures of pregnancy, births, and neonatal care quality relevant to multiple audiences (policy makers, planners, clinical staff, the public) and suitable for a publicly accessible dashboard."
                 ),
                 
                 p("A longer list of potential measures was discussed in November 2019 with a short life working group. This working group contained representation from Heads of Midwifery, Clinical Directors of Obstetrics, the Scottish Perinatal Network, and the Best Start Implementation Programme. Through a voting exercise, we selected a set of measures to concentrate on initially. These CORE measures are listed in the ",
                   
                   tags$a(
                     href = "https://docs.google.com/spreadsheets/d/1iAcRF8gc1-k7341JygofiSUmsvmKJ_OxUPyE07XVTPU/edit#gid=747368373",
                     tags$u("Topics Index"),
                     class = "externallink", 
                     target = "_blank"
                   ),
                   
                   " produced by the MatNeo Data Hub."
                 ),
                 
                 p("During 2020, in response to concerns about the Wider Impacts of COVID-19, many of these CORE measures were made available on a dashboard showing ",
                   
                   tags$a(
                     href =  "https://scotland.shinyapps.io/phs-covid-wider-impact/",
                     tags$u("COVID-19 wider impacts on the health care system."),
                     class = "externallink",
                     target = "_blank"
                   )
                 ),
                 
                 p("The ‘Pregnancy’ and ‘Births and babies’ sections of the Wider Impacts dashboard were updated for the last time in September 2023, so this post-COVID ", strong("Pregnancy, Births and Neonatal Data"), " dashboard for Scotland is now the primary source of PHS Maternity and Neonatal data visualisations. It offers alternative data views, including small multiple time series charts, to allow comparison (for a particular measure) across Health Board areas, and a multi indicator overview to display multiple measures simultaneously for all Health Board areas."
                 )
                 
             ) # box
             
           ) # fluidRow
           
  ) # tabPanel("Background")

# VERSION ----

version <- 
  
  tabPanel(title = "Version",
           value = "version",
           
           uiOutput("version_panel")
           
  ) # tabPanel("Version")

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
           
           patterns,
           
           background,
           
           version
           
    ) # tabBox
    
  ) # fluidRow
  
) # tabItem ("home")

# MULTI INDICATOR OVERVIEW ----

multi_indicator_overview <- tabItem(
  tabName = "multi_indicator_overview",
  
  fluidRow(
    tabBox(title = "Multiple indicator overview",
           
           # The id lets us use input$tabset01 on the server to find the current tab
           id = "tabset01",
           width = 12,
           
           # "bullet" chart tab
           
           tabPanel(title = "Board comparison",
                    
                    fluidRow(
                      column(12,
                             textOutput("multi_indicator_chart_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Compare Boards by measure: hover your mouse over the dots to see individual Board values"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("multi_indicator_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(plotlyOutput("multi_indicator_chart",
                                                  height = "40em"
                             )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p("* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for  ‘Average gestation at termination’ are based on the data for those three Boards combined.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("^ Shortened label for clarity. Full label is: % of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation who had a third- or fourth-degree perineal tear.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12, 
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Antenatal Booking Collection, Termination of Pregnancy Submissions Scotland (ToPSS), SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual",
                               
                               tags$a(
                                 href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland/",
                                 tags$u("Antenatal booking in Scotland,"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               
                               " ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                                 tags$u("Termination of pregnancy statistics"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               
                               "and ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " reports.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("Board comparison")
           
           # table tab
           
           tabPanel(title = "Individual Board",
                    
                    fluidRow(
                      column(12,
                             textOutput("multi_indicator_table_title"
                             ), 
                             
                             br()
                             
                      ),
                      
                      column(1, offset = 10,
                             downloadButton("multi_indicator_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      )
                    ), # fluidRow
                    
                    br(),
                    
                    fluidRow(
                      column(11,
                             loading(DTOutput("mytable"
                             )
                             ),
                             br()
                             
                      ),
                      
                      column(12,
                             p("* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for  ‘Average gestation at termination’ are based on the data for those three Boards combined.",
                               class = "notes-style"
                             )                             
                      ),
                      
                      column(12,
                             p("^ Shortened label for clarity. Full label is: % of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at between 37-42 weeks gestation who had a third- or fourth-degree perineal tear.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12, 
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Antenatal Booking Collection, Termination of Pregnancy Submissions Scotland (ToPSS), SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual",
                               
                               tags$a(
                                 href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland/",
                                 tags$u("Antenatal booking in Scotland,"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               
                               " ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                                 tags$u("Termination of pregnancy statistics"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               
                               "and ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " reports.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ) # tabPanel ("Individual Board")
           
    ) # tabBox ("Multi indicator overview")
    
  ) # fluidRow
  
) # tabItem ("multi_indicator_overview")

# NUMBER OF PREGNANCIES BOOKED ----

pregnancies_booked <- tabItem(
  tabName = "pregnancies_booked",
  
  fluidRow(
    tabBox(title = "Number of pregnancies booked",
           
           # The id lets us use input$tabset10 on the server to find the current tab
           id = "tabset10",
           width = 12,
           
           # Time series
           
           tabPanel(title = "Individual Board", #value = "bookings_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("bookings_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Number of pregnancies booked for antenatal care"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("bookings_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(plotlyOutput("bookings_runcharts", # time series not runchart
                                                  height = "30em"
                             )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Antenatal Booking Collection.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual ",
                               
                               tags$a(
                                 href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland/",
                                 tags$u("Antenatal booking in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                            
                            " publication.",
                            class = "notes-style"),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("The black dots connected by a line in the chart above show the number of pregnancies booked for antenatal care, for each month from Apr 2019 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the average (median) number of pregnancies booked for antenatal care each month over the period Apr 2019 to Feb 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             ),
                             
                             p("Numbers of pregnancies booked for antenatal care by gestation bands (under 10 weeks, between 10 and 12 weeks, and 13 weeks and over) are available in the download file."
                               )
                      ) # column
                      
                    ) # fluidRow
                    
           ), # tabPanel("bookings_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "bookings_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Number of pregnancies booked for antenatal care",
                               class = "about-this-measure-title"
                             ),
                             
                             br()                          
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("The ",
                            
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/pregnancy/your-antenatal-care/your-booking-appointment-booking-visit",
                              tags$u("‘booking’ appointment (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " is the first main appointment a woman has with her local maternity service once she knows she is pregnant. At the booking appointment, women are assessed by a midwife who can then tailor the subsequent care they receive during their pregnancy to their particular preferences and needs. Women are encouraged to book before they are 13 weeks pregnant, and ideally before they are 10 weeks pregnant."
                          ),
                          
                          p("Counting the numbers of pregnancies booked for antenatal care is useful in estimating the number of women who will require maternity care and also the numbers of those who should be offered antenatal screening and vaccinations."
                          ),
                          
                          p("Further information based on Antenatal Booking Collection (ABC) data is available from the annual ",
                            tags$a(
                              href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland/",
                              tags$u("Antenatal booking in Scotland"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " publication."
                          )
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Antenatal Booking Collection."
                          ),
                          
                          p("The antenatal booking data presented are based on the ", strong("Antenatal Booking Collection"), "initially established as a rapid response to COVID-19. Data are collected from the clinical information system - BadgerNet Maternity (most NHS boards) or TrakCare Maternity (NHS Lothian) - used by the midwives who ‘book’ a pregnant woman for maternity care. The booking appointment is the first planned and structured contact a midwife has with a pregnant woman, to assess her history and needs so that local maternity services can provide further care such as an early pregnancy scan and antenatal screening tests. The booking appointment can also give women further  information about how they can keep themselves and their baby healthy during pregnancy, and to help them plan labour and birth."
                          ),
                          
                          p("Historic data from April 2019 were also collected as a ‘catch-up’ extract in order to identify all women who were currently pregnant during the COVID-19 period. This was either from the same source or, in NHS Ayrshire & Arran, NHS Tayside and NHS Highland, from the systems in use before the introduction of BadgerNet Maternity."
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                            ),
                          
                          p("Data are shown for up to and including the most recent month for which records are considered near complete. Data for the most recent month should be viewed as provisional. Data for the whole time period shown will be refreshed every time the dashboard page is updated, and data for the most recent months are likely to change slightly as additional or updated records are added."
                          )
                          
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("bookings_datasource")
           
    ) # tabBox ("Number of pregnancies booked")
    
  ) # fluidRow
  
) # tabItem("pregnancies_booked")

# NUMBER OF TERMINATIONS ----

terminations <- tabItem(
  tabName = "terminations",
  
  fluidRow(
    tabBox(title = "Number of terminations",
           
           # The id lets us use input$tabset11 on the server to find the current tab
           id = "tabset11",
           width = 12,
           
           # Time series
           
           tabPanel(title = "Individual Board", #value = "terminations_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("terminations_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Number of terminations"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("terminations_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(plotlyOutput("terminations_runcharts", # time series not runchart
                                                  height = "30em"
                             )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"                             
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Termination of Pregnancy Submissions Scotland (ToPSS).",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                                 tags$u("Termination of pregnancy statistics"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " publication.",
                               class = "notes-style"),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("The black dots connected by a line in the chart above show the number of terminations of pregnancy, for each month, from Jan 2017 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the average (median) number of terminations of pregnancy each month over the period Jan 2017 to Feb 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("terminations_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "terminations_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Number of terminations",
                               class = "about-this-measure-title"
                             ),
                             
                             br()                          
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("Monitoring the number of terminations of pregnancy is useful for service planners to understand changes in demand and need for termination services."
                          ),
                          
                          p("Further information is available from the PHS annual report ",
                            
                            tags$a(
                              href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                              tags$u("Termination of pregnancy statistics"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "where other data tables and charts are available."
                          )
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Termination of Pregnancy Submissions Scotland (ToPSS)."
                          ),
                          
                          p("The Abortion (Scotland) Regulations 1991 set out the arrangements under which a doctor who has terminated a pregnancy must give notice of the termination to the Chief Medical Officer (CMO), as required by the Abortion Act 1967. For any terminations taking place from 1 May 2022 onwards, the 1991 Regulations were replaced by the Abortion (Scotland) Amendment Regulations 2021. The 2021 Regulations include: the removal of the prescribed notification form (commonly known as the ‘yellow form’); changing the deadline for giving notice of a termination from ‘within 7 days’ of the termination to ‘before the fifteenth day of the calendar month immediately following the calendar month in which the practitioner terminated the pregnancy’; and a simpler notification to be sent to the CMO."
                          ),
                          
                          p("Data regarding each termination should also be provided directly to PHS for the collation of termination statistics.  In addition to an aggregate notification to the CMO, local service providers record details about the termination on the PHS web-based Termination of Pregnancy Submissions Scotland (ToPSS) system. These details should be saved on the system within 30 days of a termination taking place."
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent month for which records are considered near complete. Data for the most recent month should be viewed as provisional. Data for the whole time period shown will be refreshed every time the dashboard page is updated, and data for the most recent months are likely to change slightly as additional or updated records are added."
                          )
                          
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("terminations_datasource")
           
    ) # tabBox ("Number of terminations")
    
  ) #fluidRow
  
) # tabItem("terminations")

# GESTATION AT BOOKING ----

gestation_at_booking <- tabItem(
  tabName = "gestation_at_booking",
  
  fluidRow(
    tabBox(title = "Average gestation at booking",
           
           # The id lets us use input$tabset12 on the server to find the current tab
           id = "tabset12",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "gest_at_booking_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_booking_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Average gestation at booking (based on completed weeks of pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_booking_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("gest_at_booking_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12, 
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Antenatal Booking Collection.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual ",
                               
                               tags$a(
                                 href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland/",
                                 tags$u("Antenatal booking in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               
                               " publication.",
                               class = "notes-style")
                      )
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_booking_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board", #value = "gest_at_booking_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_booking_runcharts_title"
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
                                            height = "35em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Antenatal Booking Collection.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual ",
                               
                               tags$a(
                                 href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland/",
                                 tags$u("Antenatal booking in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               
                               " publication.",
                               class = "notes-style"),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                               ),

                             p("The black dots connected by a line in the chart above show the average (mean) gestation at which women booked for their antenatal care (based on gestation at booking measured in completed weeks of pregnancy), for each month, from Apr 2019 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data:",
                               
                               tags$ul(
                                 tags$li(class= "bullet-points",
                                         "A blue line shows the overall average (median) of the mean gestation at booking each month over the period Apr 2019 to Feb 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                                         ),
                                 uiOutput("gest_at_booking_footnote"
                                          ),
                                 tags$li(class= "bullet-points",
                                         "A magenta line shows a post-pandemic median - the overall average (median) of the mean gestation at booking each month in the two-year post-pandemic period (from July 2022 to June 2024).  The magenta line is dashed where the post-pandemic average is projected outside that time range."
                                         ),
                                 )
                               ),

                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             ),
                             
                             p("Numbers of pregnancies booked for antenatal care by gestation bands (under 10 weeks, between 10 and 12 weeks, and 13 weeks and over) are available in the download file for the ‘Number of pregnancies booked’ measure.")
                      )
                      ) # fluidRow
                    
           ), # tabPanel("gest_at_booking_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "gest_at_booking_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Average gestation at booking",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("The ",
                            
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/pregnancy/your-antenatal-care/your-booking-appointment-booking-visit",
                              tags$u("‘booking’ appointment (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " is the first main appointment a woman has with her local maternity service once she knows she is pregnant. At the booking appointment, women are assessed by a midwife who can then tailor the subsequent care they receive during their pregnancy to their particular preferences and needs. Women are encouraged to book before they are 13 weeks pregnant, and ideally before they are 10 weeks pregnant. Early access to antenatal care supports promotion of healthier pregnancies and identification and optimal management of risks for poorer maternal and infant health outcomes."
                          ),
                          
                          p("Measuring the average gestation at booking helps service providers consider and, where required, improve service accessibility."
                          ),
                          
                          p("Further information based on Antenatal Booking Collection (ABC) data is also available from the annual ",
                            tags$a(
                              href = "https://www.publichealthscotland.scot/publications/antenatal-booking-in-scotland/",
                              tags$u("Antenatal booking in Scotland"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " publication."
                          )
                          
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Antenatal Booking Collection."
                          ),
                          
                          p("The antenatal booking data presented is based on the ", strong("Antenatal Booking Collection"), "initially established as a rapid response to COVID-19. Data on completed weeks of pregnancy are collected from the clinical information system - BadgerNet Maternity (most NHS boards) or TrakCare Maternity (NHS Lothian) - used by the midwives who ‘book’ a pregnant woman for maternity care.  The booking appointment is defined as the first planned and structured contact a midwife has with a pregnant woman, to assess her history and needs so that local maternity services can provide further care such as an early pregnancy scan and antenatal screening tests. The booking appointment can also give women further information about how they can keep themselves and their baby healthy during pregnancy, and to help them plan labour and birth."
                          ),
                          
                          p("Historic data from April 2019 were also collected as a ‘catch-up’ extract in order to identify all women who were currently pregnant during the COVID-19 period. This was either from the same source or, in NHS Ayrshire & Arran, NHS Tayside and NHS Highland, from the systems in use before the introduction of BadgerNet Maternity."
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent month for which records are considered near complete. Data for the most recent month should be viewed as provisional. Data for the whole time period shown will be refreshed every time the dashboard page is updated, and data for the most recent months are likely to change slightly as additional or updated records are added."
                          )
                          
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("gest_at_booking_datasource")
           
    ) # tabBox ("Average gestation at booking")
    
  ) # fluidRow
  
) # tabItem ("gestation_at_booking")

# GESTATION AT TERMINATION ----

gestation_at_termination <- tabItem(
  tabName = "gestation_at_termination",
  
  fluidRow(
    tabBox(title = "Average gestation at termination",
           
           # The id lets us use input$tabset13 on the server to find the current tab
           id = "tabset13",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "gest_at_termination_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_termination_small_multiples_title"
                             ),
                             
                             br()
                      ),
                      
                      column(10,
                             p("Average gestation at termination (based on completed weeks of pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_termination_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("gest_at_termination_small_multiples_mainland",  
                                            height = "32em"
                               )
                             )
                      ),
                      
                      br(),
                      
                      br(),  
                      
                      column(4,
                             loading(
                               plotlyOutput("gest_at_termination_small_multiples_island",  
                                            height = "16em"
                               )
                             )
                      ),
                      
                      column(7,
                             br(),
                             
                             br(),
                             
                             p("* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for ‘Average gestation at termination’ are based on the data for those three Boards combined.",
                               class = "notes-style"
                             ),
                             
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             ),
                             
                             p("Source: Public Health Scotland - Termination of Pregnancy Submissions Scotland (ToPSS).",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                                 tags$u("Termination of pregnancy statistics"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " publication.",
                               class = "notes-style"
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_termination_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board", #value = "gest_at_termination_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_terminaton_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Average gestation at termination (based on completed weeks of pregnancy)"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("gest_at_termination_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gest_at_termination_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(textOutput("gest_at_termination_runcharts_footnote1") %>%
                                 tagAppendAttributes(style = "font-size:14px;
                                                   text-align: left;")
                             )
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - Termination of Pregnancy Submissions Scotland (ToPSS).",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the annual ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                                 tags$u("Termination of pregnancy statistics"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " publication.",
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                             ),
                             
                             p("The black dots connected by a line in the chart above show the average (mean) gestation at which pregnancies were terminated (based on gestation at termination measured in completed weeks of pregnancy), for each month, from Jan 2017 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data:",
                               
                               tags$ul(
                                 tags$li(class= "bullet-points",
                                         "A blue line shows the overall average (median) of the mean gestation at termination each month over the period Jan 2017 to Feb 2020 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the pre-pandemic average is projected outside that time range."
                                         ),
                                 tags$li(class= "bullet-points",
                                         "A magenta line shows a post-pandemic median - the overall average (median) of the mean gestation at termination each month in the two-year post-pandemic period (from July 2022 to June 2024).  The magenta line is dashed where the post-pandemic average is projected outside that time range."
                                         ),
                                 )
                               ),

                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_termination_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "gest_at_termination_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Average gestation at termination",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("Standard 10 of Healthcare Improvement Scotland’s current ",
                            
                            tags$a(
                              href = "https://www.healthcareimprovementscotland.scot/publications/sexual-health-standards/",
                              tags$u("Sexual Health Standards"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " states that ‘Women, trans and non-binary people who become pregnant can access safe, timely and person-centred abortion care services’. Monitoring average gestation at termination supports assessment of access to timely service provision."
                          ),
                          
                          p("Further information is available from the PHS annual report ",
                            
                            tags$a(
                              href = "https://publichealthscotland.scot/publications/termination-of-pregnancy-statistics/",
                              tags$u("Termination of pregnancy statistics"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "where other data tables and charts are available."
                          )
                          
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Termination of Pregnancy Submissions Scotland (ToPSS)."
                          ),
                          
                          p("Up to April 2022: data were derived from the Notifications of Abortion to the Chief Medical Officer (CMO) for Scotland under the Abortion (Scotland) Regulations 1991. Notifications should be submitted to the CMO within 7 days of a termination."
                          ),
                          
                          p("From May 2022: details about the termination are recorded locally by boards on the PHS web-based Termination of Pregnancy Submissions Scotland (ToPSS) system. These details should be saved on the system  within 30 days of a termination taking place."
                          ),
                          
                          p("The Abortion (Scotland) Regulations 1991 set out the arrangements under which a doctor who has terminated a pregnancy must give notice of the termination to the Chief Medical Officer (CMO), as required by the Abortion Act 1967. For any terminations taking place from 1 May 2022 onwards, the 1991 Regulations were replaced by the Abortion (Scotland) Amendment Regulations 2021. The 2021 Regulations include: the removal of the prescribed notification form (commonly known as the ‘yellow form’); changing the deadline for giving notice of a termination from ‘within 7 days’ of the termination to ‘before the fifteenth day of the calendar month immediately following the calendar month in which the practitioner terminated the pregnancy’; and a simpler notification to be sent to the CMO."
                          ),
                          
                          p("Data regarding each termination should also be provided directly to PHS for the collation of termination statistics. In addition to an aggregate notification to the CMO, local service providers record details about the termination on the PHS web-based Termination of Pregnancy Submissions Scotland (ToPSS) system. These details should be saved on the system within 30 days of a termination taking place."
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent month for which records are considered near complete. Data for the most recent month should be viewed as provisional. Data for the whole time period shown will be refreshed every time the dashboard page is updated, and data for the most recent months are likely to change slightly as additional or updated records are added."
                          )
                          
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("gest_at_termination_datasource")
           
    ) # tabBox ("Average gestation at termination")
    
  ) # fluidRow
  
) # tabItem ("gestation_at_termination")

# PRE-TERM BIRTHS ----

location_of_ex_pre_term <- tabItem(
  tabName = "location_of_ex_pre_term",
  
  fluidRow(
    tabBox(title = "Location of extremely pre-term births",
           
           # The id lets us use input$tabset20 on the server to find the current tab
           id = "tabset20",
           width = 12,
           
           # Control chart and context chart
           
           tabPanel(title = "Scotland", #value = "pre-term_births_control_chart",
                    
                    fluidRow(
                      column(12,
                             textOutput("extremely_preterm_control_chart_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of births at 22-26 weeks gestation resulting in a live born baby that occurred in a hospital with a neonatal intensive care unit (NICU) on site"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("extremely_preterm_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("extremely_preterm_control_chart",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style",
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("As births at 22-26 weeks gestation are relatively rare events in Scotland, the percentage of these births that occur in a hospital with a neonatal intensive care unit on site will fluctuate over time just by chance. We have therefore used ‘control charts’ to present the percentages above."
                             ),
                             
                             p("Control charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                             ),
                             
                             p(tags$div(
                               HTML(paste0(
                                 "The dots joined by a solid black line in the chart above show the percentage of births between 22", tags$sup("+0"), " and 26", tags$sup("+6"), " weeks gestation inclusive that occurred in a hospital with a neonatal intensive care unit on site, for quarters from Jan-Mar 2018 onwards."
                               )
                               ) # HTML
                             ) # div
                             ),
                             
                             p("The other lines - centreline, and control and warning limits - are there to help show how unexpected any observed changes are."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a dashed  blue line shows the overall percentage of births at 22-26 weeks gestation resulting in a live born baby that occurred in a hospital with a neonatal intensive care unit on site over the entire time period."
                             ),
                             
                             p("Control and warning limits take into consideration the random variation that would be expected by chance, and help us decide when values are unexpectedly low or high and require further investigation."
                             ),
                             
                             p("Due to the small number of births at this very early gestation, data are only shown at all Scotland level."
                             ),
                             
                             hr()
                             
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of births at 22-26 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("extremely_preterm_context_chart",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("pre-term_births_control_chart")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "pre-term_births_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Location of extremely pre-term births",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("Babies born pre-term (at least 3 weeks before their due date) are at increased risk of health problems compared to babies born at term (around their due date). The earlier in pregnancy a baby is born, the higher the risk."
                          ),
                          
                          p(tags$div(
                            HTML(paste0("There is evidence that the outcomes of extremely pre-term babies (here defined as those born between ", "22", tags$sup("+0"), " and 26", tags$sup("+6"), " weeks gestation inclusive) are influenced by where they are born. Extremely pre-term babies are more likely to survive and be healthy if they are born in a hospital that has an on-site neonatal intensive care unit. In addition, extremely pre-term babies cared for in larger neonatal intensive care units (those caring for high numbers of very unwell babies) have better outcomes than babies cared for in smaller units."
                            )
                            )# HTML
                          ) # div
                          ),
                          
                          p("Reflecting this evidence, the British Association of Perinatal Medicine",
                            
                            tags$a(
                              href = "https://www.bapm.org/resources/80-perinatal-management-of-extreme-preterm-birth-before-27-weeks-of-gestation-2019",
                              tags$u("recommends (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " that when a woman is thought to be at imminent risk of extremely pre-term delivery she should be transferred to a maternity unit in a hospital with an on-site neonatal intensive care unit to allow her baby (or babies in the case of a multiple pregnancy of twins or more) to be born in the safest place. In addition, whilst the overall number of neonatal units is not changing in Scotland, the number of units that are classed as ",
                            
                            tags$a(
                              href = "https://www.bapm.org/resources/296-optimal-arrangements-for-neonatal-intensive-care-units-in-the-uk-2021",
                              tags$u("neonatal intensive care units (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "(also known as level III units, that is those able to provide the most complex, specialist care) is reducing over time in line with ",
                            
                            tags$a(
                              href = "https://www.gov.scot/publications/five-year-forward-plan-maternity-neonatal-services-neonatal-intensive-care-options-appraisal-report/",
                              tags$u("national policy (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "to concentrate expertise and improve babies’ outcomes."
                          ),
                          
                          p("Further information on ",
                            
                            tags$a(
                              href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                              tags$u("Births in Scotland,"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            " based on SMR02 data, is also available in PHS annual reports."
                          ),
                          
                          p("The ",
                            
                            tags$a(
                              href = "https://www.rcpch.ac.uk/work-we-do/clinical-audits/nnap",
                              tags$u("National Neonatal Audit Programme (NNAP)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "also monitor this measure but Scottish Units did not participate in NNAP during 2021."
                          )
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Scottish Morbidity Record (SMR02) - Maternity Inpatient and Day Case."
                          ),
                          
                          p("The data used for the ‘location of extremely pre-term births’ measure come from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."
                          ),
                          
                          p(tags$div(
                            HTML(
                              paste0("For ‘location of extremely pre-term births’, SMR02 records for episodes of care that resulted in the birth of one or more live born babies between 22", tags$sup("+0"), " and 26", tags$sup("+6"), " weeks gestation inclusive have been used. The charts show the total number of these episodes and the number and percentage of these episodes that occurred in a hospital that had a neonatal intensive care unit (NICU) on site at the time of the birth. An episode is allocated to a quarter based on the date the woman was discharged from hospital after giving birth. Due to the small number of births at this very early gestation, data are only shown for Scotland."
                              )
                            ) # HTML
                          ) # div
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Information on which hospitals have had a neonatal intensive care unit on site over the time period of interest (from January 2018), and associated dates, has been provided by the ",
                            
                            tags$a(
                              href = "https://www.perinatalnetwork.scot/",
                              tags$u("Scottish Perinatal Network (external website)."),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "For the purpose of this measure, since January 2018, the following hospitals in Scotland have been considered to have a NICU on site for the period specified:",
                            
                            tags$ul(
                              tags$li(class= "bullet-points",
                                      "A111H University Hospital Crosshouse (up until end September 2019 only) (Best Start early adopter since Oct 2019)"),
                              tags$li(class= "bullet-points",
                                      "F704H Victoria Hospital, Kirkcaldy (up until end September 2019 only) (Best Start early adopter since Oct 2019)"),
                              tags$li(class= "bullet-points",
                                      "F705H Victoria Maternity Unit, Kirkcaldy (up until end September 2019 only) (same location as F704H)"),
                              tags$li(class= "bullet-points",
                                      "G108H The Princess Royal Maternity Unit"),
                              tags$li(class= "bullet-points",
                                      "G405H Queen Elizabeth University Hospital"),
                              tags$li(class= "bullet-points",
                                      "G513H Royal Hospital for Children (same location as G405H)"),
                              tags$li(class= "bullet-points",
                                      "L308H University Hospital Wishaw"),
                              tags$li(class= "bullet-points",
                                      "N101H Aberdeen Royal Infirmary"),
                              tags$li(class= "bullet-points",
                                      "N161H Aberdeen Maternity Hospital (same location as N101H)"),
                              tags$li(class= "bullet-points",
                                      "S314H Royal Infirmary of Edinburgh at Little France"),
                              tags$li(class= "bullet-points",
                                      "T101H Ninewells Hospital")
                            )
                          ),
                          
                          p("The following approach has been used to classify episodes as occurring in a hospital with a NICU on site. The location of birth as recorded on SMR02 was compared to the above list of hospitals to determine whether the birth occurred at a hospital with a NICU on site. Admissions following home births or after delivery in another hospital or any births occurring en-route to hospital were not counted as a birth in a hospital with a NICU on site. In the calculation of the percentage of births occurring in a hospital with a NICU on site, admissions following home births or births occurring en-route to hospital are excluded from the numerator but are included in the denominator. Admissions after delivery in another hospital are excluded from both numerator and denominator. There were no births where location of birth was recorded as unknown or missing."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters is likely to change slightly as additional SMR02 records are submitted to PHS."
                          ),
                          
                          p(tags$div(
                            HTML(
                              paste0("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2019 to 31 March 2020, live births recorded on SMR02 represented 98.8% of the live births registered by law with National Records of Scotland. In addition, the recording of specific data items allowing identification of births between 22", tags$sup("+0"), " and 26", tags$sup("+6"), " weeks gestation, and the location of these births, is very complete. Since January 2018, the recording of gestation at birth on SMR02 was known and within the range 18-44 weeks in 99.7% of births. Location of birth was recorded in 100% of episodes. The latest ",
                                     
                                     tags$a(
                                       href = "https://webarchive.nrscotland.gov.uk/20231129152352/http:/www.isdscotland.org/Products-and-Services/Data-Quality/docs/20191023-Assessment-of-SMR02-Data-Scotland-2017-2018.pdf",
                                       tags$u("Data Quality Assessment Report (PDF)"),
                                       class = "externallink",
                                       target = "_blank"
                                     ),
                                     
                                     " found gestation to be accurately recorded in 95% of episodes although there was no information provided on the accuracy of the location of delivery data item."
                              )
                            ) #HTML
                          ) #div
                          ),
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("pre-term_births_datasource")
           
    ) # tabBox("Location of extremely pre-term births")
    
  ) # fluidRow
  
) # tabItem ("location_of_ex_pre_term")

# INDUCTIONS ----

inductions <- tabItem(
  tabName = "inductions",
  
  fluidRow(
    tabBox(title = "Induction of labour",
           
           # The id lets us use input$tabset21 on the server to find the current tab
           id = "tabset21",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "induction_of_labour_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("inductions_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton live births at 37-42 weeks gestation that followed induction of labour"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("inductions_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("inductions_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                                                          p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("induction_of_labour_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board", #value = "induction_of_labour_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("inductions_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton live births at 37-42 weeks gestation
                                that followed induction of labour"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("inductions_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("inductions_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                             ),
                             
                             p(tags$div(
                               HTML(
                                 paste0("The black dots connected by a line in the chart above show the percentage of singleton live births at 37", tags$sup("+0"), " to 42", tags$sup("+6"), " weeks gestation that followed induction of labour, for each quarter from Jan-Mar 2017 onwards."
                                 )
                               ) # HTML
                             ) # div
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the average (median) percentage of births that followed induction of labour over the period Jan-Mar 2017 to Oct-Dec 2019 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             ),
                             
                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             ),
                             
                             hr()
                             
                      )
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton live births at 37-42 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("inductions_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("induction_of_labour_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "induction_of_labour_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Induction of labour",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p(
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/getting-ready-for-the-birth/induced-labour",
                              tags$u("‘Induction of labour’ (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " is when a woman is given a medical intervention to start her labour, when medically indicated, rather than waiting for labour to start spontaneously."
                          ),
                          
                          p("The majority of women will spontaneously go into labour between 37 and 42 weeks of pregnancy, in the period known as ‘term’."
                          ),
                          
                          p("However, there are many reasons why an earlier birth may be recommended. The most common reason for early induction of labour is to prevent stillbirth when baby’s growth is compromised."
                          ),
                          
                          p("Recent initiatives to reduce stillbirth recommend induction of labour if there is concern about the baby’s wellbeing. However, inducing labour may lead to births that require further intervention, so it is important to monitor rates of induction."
                          ),
                          
                          p("The ",
                            
                            tags$a(
                              href = "https://maternityaudit.org.uk/",
                              tags$u("National Maternity and Perinatal Audit"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "recommends that national bodies such as NHS England, the Scottish Government, the Welsh Government, the Royal College of Obstetrics and Gynaecology (RCOG) and the Royal College of Midwives (RCM) should work together to review the need for guidance and standards to reduce variation in key aspects of maternity care, including induction of labour and modes of birth."
                          ),
                          
                          p("Having this measure available will contribute towards the work highlighted above."
                          ),
                          
                          p("Further information based on SMR02 data is available from the PHS annual ",
                            
                            tags$a(
                              href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                              tags$u("Births in Scotland"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "report where other data tables and charts are available."
                          )
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Scottish Morbidity Record (SMR02) - Maternity Inpatient and Day Case."
                          ),
                          
                          p("The data used for the ‘induction of labour’ measure come from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."
                          ),
                          
                          p(
                            
                            tags$div(
                              HTML(
                                paste0("For ‘induction of labour’, SMR02 records for episodes of care for singletons (i.e. one baby, not twins or more) born alive between 37", tags$sup("+0"), " to 42", tags$sup("+6"), " weeks gestation inclusive have been used. Only records with a known induction status are included in the denominator when calculating percentages. A birth is allocated to a quarter based on the date the woman was discharged from hospital after giving birth."
                                )
                              ) # HTML
                            ) # div
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters is likely to change slightly as additional SMR02 records are submitted to PHS."
                          ),
                          
                          p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2021 to 31 March 2022, live births recorded on SMR02 represented 97.6% of the live births registered by law with National Records of Scotland. In addition, the recording of gestation at birth, and of whether the birth followed induction of labour, is very complete. For the period 1 April 2021 to 31 March 2022, gestation was recorded on >99.9% of SMR02 records relating to singleton live births, and whether the birth followed induction of labour was recorded on 99.6% of records relating to singleton live births at 37-42 weeks gestation."
                          )
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("induction_of_labour_datasource")
           
    ) # tabBox("Induction of labour")
    
  ) # fluidRow
  
) # tabItem ("inductions")

# TYPE OF BIRTH ----
# previously METHOD OF DELIVERY

type_of_birth <- tabItem(
  tabName = "type_of_birth",
  
  fluidRow(
    tabBox(title = "Type of birth",
           
           # The id lets us use input$tabset22 on the server to find the current tab
           id = "tabset22",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "type_of_birth_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("type_of_birth_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p(htmlOutput("type_of_birth_small_multiples_sub_title"
                             )
                             )
                      ),
                      
                      column(1,
                             downloadButton("type_of_birth_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(10,
                             loading(
                               plotlyOutput("type_of_birth_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(2,
                             br(),
                             uiOutput("typeofbirthControl"
                             )
                      ),
                      
                      # column(12,
                      #        p(textOutput("Borders_caesarean_footnote1") %>%
                      #            tagAppendAttributes(style = "font-size:14px;
                      #                              text-align: left;")
                      #        )
                      # ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("type_of_birth_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board",  #value = "type_of_birth_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("type_of_birth_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton live births at any gestation that were"
                             ),
                             
                             br()
                      ),
                      
                      column(1, 
                             downloadButton("type_of_birth_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("type_of_birth_runcharts",
                                            height = "50em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      # column(12,
                      #        p(textOutput("Borders_caesarean_footnote2") %>%
                      #            tagAppendAttributes(style = "font-size:14px;
                      #                              text-align: left;")
                      #        )
                      # ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                             ),
                             
                             p("The black dots connected by a line in the charts above show the percentage of births by each type of birth, for each quarter, from Jan-Mar 2017 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the average (median) percentage of each type of birth over the period Jan-Mar 2017 to Oct-Dec 2019 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             ),
                             
                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             ),
                             
                             hr()
                             
                      )
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton live births at any gestation by type of birth"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("type_of_birth_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      # column(12,
                      #        p(textOutput("Borders_caesarean_footnote3") %>%
                      #            tagAppendAttributes(style = "font-size:14px;
                      #                              text-align: left;")
                      #        )
                      # ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style" 
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("type_of_birth_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "type_of_birth_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Type of birth",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("The ",
                            
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/assisted-birth",
                              tags$u("‘type of birth’ (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "refers to the way a baby is born. Different types of birth (sometimes previously called ‘method of delivery’ or ‘mode of delivery’) include spontaneous vaginal birth, assisted vaginal birth (including vaginal birth by forceps or ventouse, or vaginal breech birth), or a caesarean section (an operation to deliver the baby through an incision in the mother’s abdomen). A caesarean birth can be planned (sometimes called elective; planned in advance and provided before labour has started) or unplanned (sometimes called emergency, usually but not always provided after labour has started)."
                          ),
                          
                          p("A substantial rise in ‘obstetric intervention’ (i.e. instrumental and caesarean births) has been seen in most developed countries since the 1970s. Caesarean sections are effective in saving maternal and infant lives, when they are required for medically indicated reasons. Risks associated with caesarean births include wound infection and anaesthetic risks. Further information can be found in the World Health Organisation (WHO)",
                            
                            tags$a(
                              href = "https://www.who.int/publications/i/item/WHO-RHR-15.02",
                              tags$u("statement on caesarean section rates (external website)."),
                              class = "externallink",
                              target = "_blank"
                            )
                          ),
                          
                          p("This variation in rates of operative deliveries (i.e. assisted and caesarean deliveries) between countries was highlighted in the National Maternity and Perinatal Audit (NMPA)",
                            
                            tags$a(
                              href = "https://maternityaudit.org.uk/FilesUploaded/Ref315%20NMPA%20clinical%20report%202021_v1.1.pdf",
                              tags$u("Clinical Report 2021 - version 1.1 (PDF on external website)."),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "This report (the most recent that includes Scottish data) looks at births in the year 2017/18. For full term singleton babies born, there were differences in the proportions of women across England, Scotland and Wales who had spontaneous, instrumental, or caesarean births. In this report, Scotland had higher rates for both planned and unplanned caesarean births and lower rates of spontaneous vaginal birth when compared to England and Wales."
                          ),
                          
                          p("It is because of these variations in practice, and the potential health consequences for both mother and baby, that the type of birth is recorded, and the numbers of operative interventions are measured and published as national statistics in the annual",
                            
                            tags$a(
                              href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                              tags$u("Births in Scotland"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            " report."
                          )
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Scottish Morbidity Record (SMR02) - Maternity Inpatient and Day Case."
                          ),
                          
                          p("The data used for the ‘type of birth’ measure come from the ‘Mode of Delivery’ field on the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."
                          ),
                          
                          p("For ‘type of birth’, SMR02 records for episodes of care for singletons (i.e. one baby, not twins or more) born alive at any gestation have been used. Only records where type of birth is known are included in the denominator when calculating percentages. Type of birth has been categorised as",
                            
                            tags$ul(
                              tags$li(class= "bullet-points",
                                      "spontaneous vaginal"),
                              tags$li(class= "bullet-points",
                                      "assisted vaginal (including forceps, ventouse, and vaginal breech)"),
                              tags$li(class= "bullet-points",
                                      "planned (i.e. elective) caesarean"),
                              tags$li(class= "bullet-points",
                                      "unplanned (i.e. emergency) caesarean")
                            )
                          ),
                          
                          p("A birth is allocated to a quarter based on the date the woman was discharged from hospital after giving birth."
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters are likely to change sightly as additional SMR02 records are submitted to PHS."
                          ),
                          
                          p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2021 to 31 March 2022, live births recorded on SMR02 represented 97.6% of the live births registered by law with National Records of Scotland. In addition, the recording of type of birth (method of delivery) is very complete. For the period 1 April 2021 to 31 March 2022, type of birth (method of delivery) was recorded on 99.9% of SMR02 records relating to singleton live births." 
                          )
                      ) # box
                    ) # fluidRow
                    
           ) # tabPanel("type_of_birth_datasource")
    ) # tabBox("Type of birth")
  ) # fluidRow
) # tabItem ("type_of_birth")

# PERINEAL TEARS ----

perineal_tears <- tabItem(
  tabName = "perineal_tears",
  
  fluidRow(
    tabBox(title = "Perineal tears",
           
           # The id lets us use input$tabset23 on the server to find the current tab
           id = "tabset23",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "tears_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("tears_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation who had a third- or fourth-degree perineal tear"
                             )
                      ),
                      
                      column(1,
                             downloadButton("tears_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("tears_small_multiples",  
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("tears_overview")
           
           # Individual Board
           
           tabPanel("Individual Board", #value = "tears_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("tears_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation who had a third- or fourth-degree perineal tear"
                             )
                      ),
                      
                      column(1,
                             downloadButton("tears_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("tears_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12, 
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                             ),
                             
                             p(tags$div(
                               HTML(
                                 paste0("The black dots connected by a line in the chart above show the percentage of women giving birth vaginally to a singleton baby (born alive or stillborn) with a cephalic presentation at between 37", tags$sup("+0"), " to 42", tags$sup("+6"), " weeks gestation who had a third- or fourth-degree perineal tear, for each quarter from Jan-Mar 2017 onwards."
                                 )
                               ) # HTML
                             ) # div
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the average (median) percentage of women who had a third- or fourth-degree perineal tear, over the period Jan-Mar 2017 to Oct-Dec 2019 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             ),
                             
                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             ),
                             
                             hr()
                             
                      )
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation at 37-42 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("tears_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             )
                      )
                    ) # fluidRow
                    
           ), # tabPanel("tears_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "tears_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Third- and fourth-degree perineal tears",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("When a woman is giving birth, the baby stretches the mother’s vagina and perineum. Occasionally, the tissues cannot stretch enough, and a tear (called a ",
                            
                            tags$a(
                              href = "https://www.rcog.org.uk/en/patients/patient-leaflets/third--or-fourth-degree-tear-during-childbirth/",
                              tags$u("‘perineal tear’ (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "occurs. The perineum is the area between a woman’s vagina and anus."
                          ),
                          
                          p("Perineal tears are classified as first- to fourth-degree, with fourth-degree tears being the most serious. First-degree tears just involve the skin of the perineum or lining of the lower vagina. Second-degree tears also involve the muscles of the perineum. Third-degree tears extend further back and also involve the muscles surrounding the anus. Fourth-degree tears extend further into the lining of the anus or rectum (lower bowel)."
                          ),
                          
                          p("Third- and fourth-degree tears are also known as obstetric anal sphincter injury (OASI). These tears require surgical repair immediately after birth. Most women recover completely following a third- or fourth-degree tear; however, some are left with persistent problems controlling their bowels (anal incontinence)."
                          ),
                          
                          p("Most tears are unexpected, and it is hard to predict which women will have a tear, although tears are more common during a woman’s first vaginal birth, if the baby is big (over 4kg/8.5 lbs birthweight), or if the second stage of labour goes on for a long time."
                          ),
                          
                          p("An ",
                            
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/assisted-birth/perineal-tears-and-episiotomy",
                              tags$u("‘episiotomy’ (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "may be offered if a woman is thought to be at risk of a tear. An episiotomy is a controlled cut made by a healthcare professional through the vaginal wall and perineum that is repaired with stitches after birth. An episiotomy does not guarantee that a tear will not happen, as the episiotomy cut may extend and become a tear. Women requiring an assisted vaginal birth (with forceps or ventouse) are at high risk of a tear so would generally be offered an episiotomy."
                          ),
                          
                          p(
                            tags$div(
                              HTML(
                                paste0(
                                  "The most recent National Maternity and Perinatal Audit (NMPA) report that includes Scottish data ",
                                  
                                  tags$a(
                                    href = "https://maternityaudit.org.uk/FilesUploaded/Ref315%20NMPA%20clinical%20report%202021_v1.1.pdf",
                                    tags$u("Clinical Report 2021 - version 1.1 (PDF on external website)"),
                                    class = "externallink",
                                    target = "_blank"
                                  ),
                                  
                                  " notes (on page 18) that ‘Rates of third- and fourth-degree tears in Scotland and Wales remain steady with minimal fluctuation in rates since the first NMPA report time point.’ However, the overall proportion of women who give birth vaginally to a singleton baby in the cephalic position between 37", tags$sup("+0"), " and 42", tags$sup("+6")
                                ), # paste0,
                                
                                "weeks of gestation who sustain a third- or fourth-degree tear is reported as higher in Scotland than in England or Wales, so we need to continue to monitor rates across Scotland in order to reduce rate and  risk."
                              ) # HTML
                            ) # div
                          ),
                          
                          p("Further information on ",
                            
                            tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland,"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " based on SMR02 data, is also available in PHS annual reports."
                            )

                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Scottish Morbidity Record (SMR02) - Maternity Inpatient and Day Case."
                          ),
                          
                          p("The data used for the ‘perineal tears’ measure come from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."
                          ),
                          
                          p(
                            
                            tags$div(
                              HTML(
                                paste0("For ‘perineal tears’, SMR02 records for episodes of care for singletons (i.e. one baby, not twins or more) born alive or stillborn between 37",  tags$sup("+0"), " to 42", tags$sup("+6"), " weeks gestation have been used. Only records with known perineal tear status are included in the denominator when calculating percentages. A birth is allocated to a quarter based on the date the woman was discharged from hospital after giving birth."
                                )
                              ) # HTML
                            ) #div
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters are likely to change slightly as additional  SMR02 records are submitted to PHS."
                          ),
                          
                          p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2021 to 31 March 2022, live births recorded on SMR02 represented 97.6% of the live births registered by law with National Records of Scotland (NRS) and stillbirths on SMR02 represented 97.2% of stillbirths registered with NRS. In addition, the recording of specific data items allowing identification of the birth of a singleton live or stillborn baby at 37-42 weeks gestation is very complete."
                          ),
                          
                          p("SMR02 birth records allow the recording of both type of birth (i.e. mode of delivery: spontaneous or assisted vaginal, caesarean, etc) and the presentation of the baby at birth prior to any operative manipulation (i.e. which part of the baby was lowest in the maternal pelvis: cephalic [‘head first’], breech, etc). The completeness and accuracy of type of birth recording is very high whereas the completeness and accuracy of presentation recording is slightly less good, in particular from some hospitals. For the period 1 April 2017 to 31 December 2022, 99.9% of records relating to a singleton live or stillbirth had a meaningful value (not unknown or missing) recorded for ‘Mode of Delivery’. When producing this measure, we have therefore relied on the ‘Mode of delivery’ (type of birth) variable alone to identify vaginal births of babies with a cephalic presentation. Records with type of birth coded as spontaneous cephalic or assisted vaginal (forceps or ventouse) have been assumed to identify vaginal births of babies with a cephalic presentation. Babies with other (breech, caesarean or unknown) types of birth recorded have been excluded. As this measure only includes births of singleton babies at 37-42 weeks gestation, this is considered a reasonable assumption. It is unlikely that these babies will have been manipulated from a non-cephalic to a cephalic presentation prior to an assisted vaginal birth, as may happen for preterm babies or second twins."
                          ),
                          
                          p("Perineal tears are classified as first- to fourth-degree, with fourth-degree tears being the most serious. On SMR02 birth records, a specific data item captures information on whether a woman had a perineal tear. This item is coded as 0 (no tear), 1-4 (first- to fourth-degree tear respectively), 8 (tear of unspecified degree), or 9 (unknown whether there was a tear or not). The completeness of tear recording is very high. For the period 1 April 2021 to 31 December 2020, 99.7% of records relating to a singleton live or stillbirth had a meaningful value (not unknown or missing) recorded for ‘Perineal Tears’. Other forms of injury to the birth canal, for example isolated high  vaginal tears, or cervical tears, are less common than perineal tears. These other injuries are recorded separately on SMR02 and have not been considered as part of this measure."
                          )
                      ) # box
                    ) # fluidRow
                    
           ) # tabPanel("tears_datasource")
    ) # tabBox ("Perineal tears")
  ) # fluidRow
) # tabItem ("perineal_tears")

# GESTATION AT BIRTH ----

gestation_at_birth <- tabItem(
  tabName = "gestation_at_birth",
  
  fluidRow(
    tabBox(title = "Gestation at birth",
           
           # The id lets us use input$tabset24 on the server to find the current tab
           id = "tabset24",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", # value = "gest_at_birth_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_birth_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p(htmlOutput("gest_at_birth_small_multiples_sub_title"
                             )
                             )
                      ),
                      
                      column(1,
                             downloadButton("gest_at_birth_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(10,
                             loading(
                               plotlyOutput("gest_at_birth_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(2,
                             br(),
                             uiOutput("gestationControl"
                             )
                      ),
                      
                      column(12, 
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_birth_overview")
           
           # Individual Board
           
           tabPanel(title = "Individual Board",  #value = "gest_at_birth_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("gest_at_birth_runcharts_title"
                             ),
                             
                             br()
                             
                      ),

                      column(10,
                             p("Percentage of singleton live births that were at the stated gestation",
                               style = "font-weight: normal;
                                         text-align: left;"
                             )
                      ),
                      
                      column(1,
                             downloadButton("gest_at_birth_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gest_at_birth_runcharts",
                                            height = "50em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12, 
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                             ),
                             
                             p("The black dots connected by a line in the charts above show the percentage of singleton live births (with known gestation; 18-44 weeks) that were at the stated gestation, for each quarter, from Jan-Mar 2017 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the average (median) percentage of singleton live births (with known gestation; 18-44 weeks) that were at the stated gestation over the period Jan-Mar 2017 to Oct-Dec 2019 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             ),
                             
                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             ),
                             
                             hr()
                             
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton live births that were at the stated gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gest_at_birth_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information is available from the PHS annual",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " report.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("gest_at_birth_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "gest_at_birth_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Gestation at birth: pre- and post-term births",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("Gestation at birth strongly influences babies’ health. Babies born preterm can have complications following their birth and the consequences of being born too early can continue to affect health and development throughout childhood and adult life. The ",
                            
                            tags$a(
                              href = "https://www.tommys.org/pregnancy-information/premature-birth/how-long-do-you-stay-in-hospital-after-birth/gestational-age-and-medical-needs",
                              tags$u("risk increases the earlier a baby is born (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "In Scotland, being born too soon is the single biggest reason babies require admission to neonatal care and the single biggest cause of death in early infancy."
                          ),
                          
                          p("Babies are also at increased risk when pregnancies extend post-term, in particular the ",
                            
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/getting-ready-for-the-birth/induced-labour",
                              tags$u("risk of stillbirth (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "increases from 42 weeks gestation onwards."
                          ),
                          
                          p("The monitoring of gestation at birth therefore enables us to understand where need for prevention of pre- and post- term birth is greatest, monitor the impact of interventions and ensure maternity and neonatal care services are provided proportionate to need."
                          ),
                          
                          p("Further information based on SMR02 data is also available from the annual ",
                            
                            tags$a(
                              href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                              tags$u("Births in Scotland"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            " report."
                          )
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Scottish Morbidity Record (SMR02) - Maternity Inpatient and Day Case."
                          ),
                          
                          p("‘Gestation at birth’ refers to the number of completed weeks pregnant a woman is when her baby is born. Babies are ‘due’ at 40 completed weeks gestation. Those born between 37 and up to 42 weeks inclusive are considered to be born ‘at term’."
                          ),
                          
                          p("Babies born at under 37 weeks (more than three weeks before their due date) are considered to be",
                            
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/after-the-birth/premature-babies",
                              tags$u("pre-term or premature (external website)"),
                              class = "externallink",
                              target = "_blank" 
                            ),
                            
                            "with those born at under 32 weeks considered to be very pre-term and those born at 32 to 36 weeks inclusive considered to be moderately pre-term. Babies born at or over 42 weeks (more than two weeks after their due date) are considered to be post-term or over-due."
                          ),
                          
                          p("The data used for the ‘gestation at birth’ measure come from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."
                          ),
                          
                          p("For ‘gestation at birth’, SMR02 records for episodes of care for singletons (i.e. one baby, not twins or more) born alive have been used. Only records with a known gestation (between 18-44 weeks inclusive) are included in the denominator when calculating percentages. Gestation at birth has been categorised as ",
                            
                            tags$ul(
                              tags$li(class= "bullet-points",
                                      "under 32 weeks (very pre-term)"),
                              tags$li(class= "bullet-points",
                                      tags$div(
                                        HTML(
                                          paste0("32", tags$sup("+0"), " to 36", tags$sup("+6"), " weeks (moderately pre-term)")
                                        )
                                      )
                              ),
                              tags$li(class= "bullet-points",
                                      "under 37 weeks (pre-term)"),
                              
                              tags$li(class= "bullet-points",
                                      tags$div(
                                        HTML(
                                          paste0("37", tags$sup("+0"), " to 41", tags$sup("+6"), " weeks (term)")
                                        )
                                      )
                              ),
                              tags$li(class= "bullet-points",
                                      tags$div(
                                        HTML(
                                          paste0("42", tags$sup("+0"), " weeks or over (post-term)")
                                        )
                                      )
                              )
                            ),
                            
                            "A birth is allocated to a quarter based on the date the woman was discharged from hospital after giving birth."
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters are likely to change slightly as additional SMR02 records are submitted to PHS."
                          ),
                          
                          p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2021 to 31 March 2021, live births recorded on SMR02 represented 97.6% of the live births registered by law with National Records of Scotland (NRS). In addition, the recording of gestation at birth is very complete. For the period 1 April 2021 to 31 March 2022, gestation was recorded on >99.9% of SMR02 records relating to singleton live births."
                          )
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("gest_at_birth_datasource")
           
    ) # tabBox("Gestation at birth")
    
  ) # fluidRow
  
) # tabItem ("gestation_at_birth")

# STILLBIRTHS AND INFANT DEATHS ----

stillbirths <- tabItem(
  tabName = "stillbirths",
  
  fluidRow(
    tabBox(title = "Stillbirths and infant deaths",
           
           # The id lets us use input$tabset25 on the server to find the current tab
           id = "tabset25",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Scotland", # value = "stillbirths_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("stillbirths_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Quarterly rates of stillbirths and infant deaths*"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("stillbirths_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("stillbirths_runcharts",
                                            height = "60em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p("* Rates are per 1,000 live births except for stillbirths and extended perinatal deaths, where the rates are per 1,000 total (live and still) births.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p(paste0("Data first published by National Records of Scotland (NRS) on ", NRS_published_date),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: National Records of Scotland ", #vital event registrations"
                               
                               tags$a(
                                 href = "https://www.nrscotland.gov.uk/statistics-and-data/births-deaths-marriages-and-life-expectancy",
                                 tags$u("Births, Deaths and Other Vital Events - Quarterly Figures (external website)"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("In mid-March 2020 Registration Offices closed due to the Covid-19 pandemic. Birth registrations were postponed. Birth registration restarted in late June 2020. Quarterly rates for stillbirths and      infant deaths, which are calculated using the number of births, have not been shown for 2020 as the effect on the quarterly number of birth registrations could make the rates misleading.  An annual rate for 2020 is shown instead."
                             ),
                             
                             p("The black dots connected by a line in the charts above show the mortality rate for each particular measure, for each quarter, from Jan-Mar 2016 onwards."
                             ),
                             
                             p("To provide a basis for identifying patterns in the data, a blue line shows the average (mean) rate over the period Jan-Mar 2016 to Oct-Dec 2019 inclusive (the period before the COVID-19 pandemic in Scotland). The blue line is dashed where the average is projected outside that time range."
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("stillbirths_overview")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "stillbirths_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Stillbirths and infant deaths",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("Monitoring rates of stillbirth, neonatal and infant mortality allows us to understand the distribution of need for prevention across different populations, review change over time and evaluate the impact of prevention efforts."
                          )
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: National Records of Scotland (NRS) vital event registrations."
                          ),
                          
                          p("The quarterly stillbirth and infant mortality rates shown have been generated using quarterly births, deaths and other vital events data provided by ", 
                            
                            tags$a(
                              href = "https://www.nrscotland.gov.uk/statistics-and-data/births-deaths-marriages-and-life-expectancy",
                              tags$u("National Records for Scotland (NRS) (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "from the statutory registration of deaths and births and using the same definitions as ",
                            tags$a(
                              href = "https://webarchive.nrscotland.gov.uk/20240926182510/https:/www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/deaths/deaths-background-information/stillbirths-and-infant-deaths",
                              tags$u("National Records for Scotland (NRS) (external website)."),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            "NRS are the primary publishers of data on stillbirth and infant death and their website includes ",
                            
                            tags$a(
                              href = "https://www.nrscotland.gov.uk/publications/vital-events-reference-tables-2023/",
                              tags$u("annual data"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " for Scotland, (section 4 of Vital Events Reference Tables: time series; by sex and cause; by age of mother and father), and ",
                            
                            tags$a(
                              href = "https://www.nrscotland.gov.uk/statistics-and-data/births-deaths-marriages-and-life-expectancy/",
                              tags$u("quarterly data"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " for Scotland, Council Areas and NHS Boards."
                          ),
                          
                          p(strong("Stillbirths"), " refer to children born after the 24th week of pregnancy (gestations of 24 weeks or longer) that did not breathe or show any signs of life. The stillbirth rate is the number of stillbirths per 1,000 total (live + still) births."
                          ),
                          
                          p(strong("Neonatal deaths"), " refer to deaths in the first four weeks of life. The neonatal death rate is the number of neonatal deaths per 1,000 live births."
                          ),
                          
                          p(strong("Extended perinatal deaths"), " refer to the sum of stillbirths and neonatal mortality (deaths within the first 4 weeks of life). The extended perinatal death rate is the number of stillbirths plus the number of neonatal deaths per 1,000 total (live + still) births."
                          ),
                          
                          p(strong("Post-neonatal deaths"), " refer to deaths occurring after the first 4 weeks but within the first year of life. The post-neonatal death rate is the number of post-neonatal deaths per 1,000 live births."
                          ),
                          
                          p(strong("Infant deaths"), " refer to all deaths in the first year of life. The infant death rate is the number of infant deaths per 1,000 live births."
                          ),
                          
                          p("Across the UK, surveillance of perinatal deaths is undertaken by MBRRACE-UK (Mothers and Babies: Reducing Risk through Audits and Confidential Enquiries across the UK). Each year MBRRACE-UK publish a ",
                            
                            tags$a(
                              href = "https://www.npeu.ox.ac.uk/mbrrace-uk/perinatal-programme",
                              tags$u("perinatal mortality report (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            
                            " that provides background information on factors that influence perinatal deaths."
                          ),
                          
                          p("Within Scotland, the ",
                            
                            tags$a(
                              href = "https://ihub.scot/improvement-programmes/scottish-patient-safety-programme-spsp/spsp-perinatal-programme/",
                              tags$u("Scottish Patient Safety Programme - Perinatal (external website)"),
                              class = "externallink",
                              target = "_blank"
                            ),

                            " focuses on care quality to improve outcomes for babies, children and their mothers. One of the key outcomes they track is stillbirths."
                          ),
                          
                          p("Deaths which have occurred in Scotland should be registered within the statutory period of 8 days from the date of occurrence, whilst births which have occurred in Scotland should be registered within the statutory period of 21 days from the date of occurrence."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which death and birth data are ", 
                            tags$a(
                              href = "https://www.nrscotland.gov.uk/statistics-and-data/births-deaths-marriages-and-life-expectancy",
                              tags$u("available from NRS."),
                              class = "externallink",
                              target = "_blank"
                            )
                          ),
                          
                          p("Deaths are allocated to the quarter in which they were registered."
                          )
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("stillbirths_datasource")
           
    ) # tabBox("Stillbirths and infant deaths")
    
  ) # fluidRow
  
) # tabItem ("stillbirths")

# APGAR5 SCORES ----

apgar_scores <- tabItem(
  tabName = "apgar_scores",
  
  fluidRow(
    tabBox(title = "Low Apgar5 scores",
           
           # The id lets us use input$tabset26 on the server to find the current tab
           id = "tabset26",
           width = 12,
           
           # Small multiples tab
           
           tabPanel(title = "Board comparison", #value = "apgar5_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("apgar5_small_multiples_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton babies born alive at 37-42 weeks gestation that had a 5-minute Apgar score of less than 7"
                             )
                      ),
                      
                      column(1,
                             downloadButton("apgar5_download_data1", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(11,
                             loading(
                               plotlyOutput("apgar5_small_multiples",
                                            height = "40em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("apgar5_overview")
           
           # Individual Board
           
           tabPanel("Individual Board", #value = "apgar5_board",
                    
                    fluidRow(
                      column(12,
                             textOutput("apgar5_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Percentage of singleton babies born alive at 37-42 weeks gestation that had a 5-minute Apgar score of less than 7"
                             )
                      ),
                      
                      column(1,
                             downloadButton("apgar5_download_data2", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("apgar5_runcharts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used run charts to present the data above. Run charts use a series of rules to help identify unusual behaviour in data and indicate patterns that merit further investigation. Read more about the rules used in the charts in the ‘How do we identify patterns in the data?’ section on the Home page."
                             ),
                             
                             p(tags$div(
                               HTML(
                                 paste0("The black dots connected by a line in the chart above show the percentage of singleton babies born alive at 37", tags$sup("+0"), " to 42", tags$sup("+6"), " weeks gestation with a known 5-minute Apgar score that had a score of <7, for each quarter from Jan-Mar 2017 onwards."
                                 )
                               ) # HTML
                             ) # div
                             ),
                             
                             p(tags$div(
                               HTML(
                                 paste0("To provide a basis for identifying patterns in the data, a blue line shows the average (median) percentage of singleton babies born alive at 37", tags$sup("+0"), " to 42", tags$sup("+6"), " weeks gestation with a known 5-minute Apgar score that had a score of <7 over the period Jan-Mar 2017 to Oct-Dec 2019 inclusive (the period before the COVID-19 pandemic in Scotland).  The blue line is dashed where the average is projected outside that time range."
                                 )
                               ) # HTML
                             ) # div
                             ),
                             
                             p("The black line becomes yellow where there are 6 or more consecutive points above or below the average and is highlighted in green where there are 5 or more consecutively increasing or decreasing points."
                             ),
                             
                             hr()
                             
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of singleton babies born alive at 37-42 weeks gestation"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("apgar5_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("apgar5_board")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "apgar5_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Low Apgar 5 scores",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("The Apgar score was developed by Dr Virginia Apgar in 1952 to measure the condition of newborn babies. Scoring allows health professionals to quickly identify babies needing resuscitation after birth. Babies are scored 0, 1, or 2 for each of their heart rate, respiratory effort, muscle tone, response to stimulation, and skin colour. Scores therefore range from 0 to 10, with higher scores indicating a better condition. Scores of 7 or over are generally interpreted as ‘reassuring’, with scores of 4-6 considered moderately low, and scores of 0-3 considered very low. The Apgar score is measured at 1 and 5 minutes after birth for all babies in Scotland."
                          ),
                          
                          p("Low Apgar scores at 5 minutes after birth are associated with a higher risk of neonatal death, neonatal morbidity, and longer-term problems with babies’ development. Babies born pre-term can have lower scores due to their overall immaturity rather than a specific problem such as lack of oxygen during birth. Due to this, the association between low Apgar scores and poor outcomes is generally stronger for babies born at term (at 37-41 weeks gestation) or post-term (at ≥42 weeks gestation), compared to those born pre-term (at <37 weeks gestation)."
                          ),
                          
                          p("Monitoring of the score is useful as a proxy measure for neonatal health needs and therefore understanding the distribution of need across different populations and changes over time."
                          ),
                          
                          p("Further information on ",
                            
                            tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland,"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " based on SMR02 data, is also available in PHS annual reports."
                            )
                          
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: Scottish Morbidity Record (SMR02) - Maternity Inpatient and Day Case."
                          ),
                          
                          p("The data used for the ‘Apgar scores’ measure come from the Scottish Morbidity Record 02 (SMR02) database. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended home births."
                          ),
                          
                          p(tags$div(
                            HTML(
                              paste0("Babies born pre-term can have lower scores due to their  overall immaturity rather than a specific problem. So, for ‘Apgar scores’, SMR02 records for episodes of care for singletons (i.e. one baby, not twins or more) born alive between 37", tags$sup("+0"), " to 42", tags$sup("+6"), " weeks gestation inclusive have been used. Only records with known Apgar score at 5 minutes following birth are included in the denominator when calculating percentages. A birth is allocated to a quarter based on the date the woman was discharged from hospital after giving birth."
                              )
                            ) # HTML
                          ) # div
                          ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters are likely to change slightly as additional SMR02 records are submitted to PHS."
                          ),
                          
                          p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2021 to 31 March 2022, live births recorded on SMR02 represented 97.6% of the live births registered by law with National Records of Scotland (NRS). In addition, the recording of specific data items allowing identification of singleton live births at 37-42 weeks gestation, and of babies’ 5-minute Apgar score, is very complete. For the period 1 April 2021 to 31 March 2022, a 5-minute Apgar score was recorded on 99% of SMR02 records relating to singleton live births at 37-42 weeks gestation."
                          ),
                          
                          p("Further information based on SMR02 data is also available from the annual",
                            
                            tags$a(
                              href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                              tags$u("Births in Scotland"),
                              class = "externallink",
                              target = "_blank"
                            ),
                            " report."
                          )
                      ) # box
                    ) # fluidRow
                    
           ) # tabPanel("apgar5_datasource")
           
    ) # tabBox("Low Apgar5 scores")
    
  ) # fluidRow
  
) # tabItem ("apgar_scores")

# MEDIAN CGA 30-32 WEEKS ----

median_cga_30_32 <- tabItem(
  tabName = "median_cga_30_32",
  
  fluidRow(
    tabBox(title = "Median corrected gestational age",
           
           # The id lets us use input$tabset27 on the server to find the current tab
           id = "tabset27",
           width = 12,
           
           # Time series chart and context chart
           
           tabPanel(title = "Scotland", #value = "median_cga_30_32_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("corrected_gestational_age_runcharts_title"
                             ),
                             
                             br()
                             
                      ),
                      
                      column(10,
                             p("Median corrected gestational age at discharge from neonatal care for babies born at 30-32 weeks gestation*"
                             )
                      ),
                      
                      column(1, 
                             downloadButton("corrected_gestational_age_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("corrected_gestational_age_runcharts", # time series not runchart
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p("* For more details about the cohort of babies included in this analysis please refer to ‘About this measure’.",
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - NeoCareIn+.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style",
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             p("We have used a time series chart to present the data above."
                             ),
                             
                             p("The black dots connected by a line in the chart above show the average (median) corrected gestation at discharge from a NICU for babies born at 30-32 weeks gestation, for each quarter, from Jan-Mar 2018 onwards."
                             ),

                             p("Due to the small number of births at this very early gestation, data are only shown at all Scotland level."
                             ),
                             
                             hr()
                             
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p("Number of babies of 30-32 weeks gestation (at birth) discharged from neonatal care"
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("corrected_gestational_age_context_charts",
                                            height = "30em"
                               )
                             ),
                             
                             br()
                             
                      ),
                      
                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - NeoCareIn+.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             )
                      )
                      
                    ) # fluidRow
                    
           ), # tabPanel("median_cga_30_32_overview")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "median_cga_30_32_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Median corrected gestational age at discharge from neonatal care", br(), "for babies of 30-32 weeks gestation",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("NEED SOME TEXT HERE"
                          ),
                          
                          p("Further information on ",
                            
                            tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland,"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " based on SMR02 data, is also available in PHS annual reports."
                            )
                          
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: NeoCare+."
                          ),
                          
                          p("NEED TO AMEND TEXT HERE"
                          ),
                          
                          p("The data used for the ‘median corrected gestational age’ measure come from the NeoCareIn+ database. A NeoCareIn+ record is submitted by neonatal units to Public Health Scotland (PHS) whenever a baby is admitted to neonatal care."
                          ),
                          
                          p("The median is calculated from the corrected gestational age at discharge for babies born at ",
                            
                            tags$ul(
                              tags$li(class= "bullet-points",
                                      tags$div(
                                        HTML(
                                          paste0("30", tags$sup("+0"), " to 32", tags$sup("+6"), " weeks")
                                        )
                                      )
                              )
                            ),
                            
                            "who were admitted to a neonatal unit. Babies who had surgery and babies who died before discharge are excluded."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which NeoCare+ records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters are likely to change slightly as additional NeoCare+ records are submitted to PHS."
                          )
                          
                      ) # box
                    ) # fluidRow
                    
           ) # tabPanel("median_cga_30_32_datasource")
           
    ) # tabBox("Median corrected gestational age at discharge from neonatal unit")
    
  ) # fluidRow
  
) # tabItem ("median_cga_30_32")

# LATE PRE-TERM AND TERM/POST-TERM ADMISSIONS TO NEONATAL BY BAPM LEVELS OF CARE ----

gestation_by_BAPM_LOC <- tabItem(
  tabName = "gestation_by_BAPM_LOC",
  
  fluidRow(
    tabBox(title = "Admissions to a neonatal unit by highest level of care",
           
           # The id lets us use input$tabset28 on the server to find the current tab
           id = "tabset28",
           width = 12,
           
           # Time series chart and context chart
           
           tabPanel(title = "Scotland", #value = "gestation_by_BAPM_LOC_overview",
                    
                    fluidRow(
                      column(12,
                             textOutput("gestation_by_BAPM_LOC_runcharts_title"
                             ),

                             br()

                      ),
                      
                      column(12,
                             uiOutput("BAPM_LOC_subgroupControl"
                             )
                      ),

                      column(10,
                             p(htmlOutput("gestation_by_BAPM_LOC_runcharts_sub_title"
                             )
                             )
                      ),

                      column(1,
                             downloadButton("gest_by_BAPM_LOC_download_data", "Download data",
                                            icon = shiny::icon("download") %>% rem_aria_label()
                             )
                      ),
                      
                      column(12,
                             loading(
                               plotlyOutput("gestation_by_BAPM_LOC_runcharts", # time series not runchart
                                            height = "50em"
                               )
                             ),

                             br()

                      ),

                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),
                      
                      column(12,
                             p("Source: Public Health Scotland - NeoCareIn+ and SMR02.",
                               class = "notes-style"
                             ),
                             
                             p("Further information on ",
                               
                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style",
                             ),
                             
                             hr()
                             
                      ),
                      
                      column(12,
                             
                             p("The levels of care shown are those defined by the ",
                               
                               tags$a(
                                 href = "https://www.bapm.org/",
                                 tags$u("British Association of Perinatal Medicine (external website)"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               
                               "(BAPM). For more details see the ‘About this measure’ tab."
                               ),
                             
                             p("We have used time series charts to present the data above."
                               ),

                             p("Due to the small number of babies admitted to neonatal care, data are only shown at all Scotland level."
                             ),
                             
                             hr()
                             
                      )
                      
                    ), # fluidRow
                    
                    fluidRow(
                      column(10,
                             p(htmlOutput("gest_by_BAPM_LOC_context_chart_sub_title")
                               )
                             ),

                      column(12,
                             loading(
                               plotlyOutput("gest_by_BAPM_LOC_context_charts",
                                            height = "30em"
                               )
                             ),

                             br()

                      ),

                      column(12,
                             p(paste0("Data last refreshed on ", pretty_refresh_date, "."),
                               class = "notes-style"
                             )
                      ),

                      column(12,
                             p("Source: Public Health Scotland - NeoCareIn+ and SMR02.",
                               class = "notes-style"
                             ),

                             p("Further information on ",

                               tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " is available in PHS annual reports.",
                               class = "notes-style"
                             )
                      )

                    ) # fluidRow
                    
           ), # tabPanel("gestation_by_BAPM_LOC_overview")
           
           # Data source and definitions etc.
           
           tabPanel(title = "About this measure", #value = "gestation_by_BAPM_LOC_datasource",
                    
                    fluidRow(
                      column(12,
                             p("Admissions to a neonatal unit by highest level of caree",
                               class = "about-this-measure-title"
                             ),
                             
                             br()
                             
                      ),
                      
                      box(title = "Why measure this?",
                          status = "primary",
                          width = 5,
                          
                          p("NEED SOME TEXT HERE"
                          ),
                          
                          p("Further information on ",
                            
                            tags$a(
                                 href = "https://publichealthscotland.scot/publications/births-in-scotland/",
                                 tags$u("Births in Scotland,"),
                                 class = "externallink",
                                 target = "_blank"
                               ),
                               " based on SMR02 data, is also available in PHS annual reports."
                            )
                          
                      ), # box
                      
                      box(width = 1,
                          solidHeader = TRUE
                      ),
                      
                      box(title = "Data source and definitions",
                          status = "primary",
                          width = 5,
                          
                          p("Data source: NeoCareIn+ (numerator) and Scottish Morbidity Record (SMR02) - Maternity Inpatient and Day Case (denominator)."
                          ),
                          
                          p("NEED TO AMEND TEXT HERE"
                          ),
                          
                          p("The data used for the ‘late pre-term and term/post-term admissions’ measure come from the NeoCareIn+ and Scottish Morbidity Record 02 (SMR02) databases. A NeoCareIn+ record is submitted by neonatal units to Public Health Scotland (PHS) whenever a baby is admitted to neonatal care. An SMR02 record is submitted by maternity hospitals to Public Health Scotland (PHS) whenever a woman is discharged from an episode of day case or inpatient maternity care. From October 2019, maternity hospitals have also been asked to submit SMR02 records following attended homebirths."
                          ),

                          p("The numerator contains a subset of the number of live born babies admitted to a neonatal unit (first admission only). These babies are categorised by their gestation at admission:",
                            
                            tags$ul(
                              tags$li(class= "bullet-points",
                                      tags$div(
                                        HTML(
                                          paste0("34", tags$sup("+0"), " to 36", tags$sup("+6"), " weeks (late pre-term)")
                                        )
                                      )
                              ),

                              tags$li(class= "bullet-points",
                                      tags$div(
                                        HTML(
                                          paste0("37", tags$sup("+0"), " to 42", tags$sup("+6"), " weeks (term and post-term)")
                                        )
                                      )
                              )
                              )
                            ),
                            
                            p("and by the highest level of care they receive during this stay in the neonatal unit:",
                            
                            tags$ul(
                              tags$li(class= "bullet-points",
                                      "Intensive care"),
                              tags$li(class= "bullet-points",
                                      "High dependency care"),
                              tags$li(class= "bullet-points",
                                      "Special care")
                                      )
                              ),

                          p("Babies are ‘due’ at 40 completed weeks gestation. Those born between 37 and up to 42 weeks inclusive are considered to be born ‘at term’.",
                            style = "font-style:italic"
                          ),
                          
                          p("Babies born at under 37 weeks (more than three weeks before their due date) are considered to be",
                            
                            tags$a(
                              href = "https://www.nhsinform.scot/ready-steady-baby/labour-and-birth/after-the-birth/premature-babies",
                              tags$u("pre-term or premature (external website)"),
                              class = "externallink",
                              target = "_blank" 
                            ),
                            
                            "with those born at under 32 weeks considered to be very pre-term and those born at 32 to 36 weeks inclusive considered to be moderately pre-term. Babies born at or over 42 weeks (more than two weeks after their due date) are considered to be post-term or over-due.",
                            style = "font-style:italic"
                          ),
                            
                          p("A birth is allocated to a quarter based on the date the woman was discharged from hospital after giving birth."
                          ),
                          
                          p("An episode in neonatal care is allocated to a quarter based on the date of admission (first admission only)."
                            ),
                          
                          p("Pregnancies of cis women (non-transgender women), trans men, and gender non-binary people are included in the data shown.  However, as national health records do not currently provide reliable data on individuals’ gender identity, data on the number of trans or non-binary people cannot be provided."
                          ),
                          
                          p("Data are shown for up to and including the most recent quarter for which NeoCare+ and SMR02 records are considered near complete. Data for the most recent quarters should be viewed as provisional. Data for all quarters will be refreshed every time the dashboard page is updated, and data for the most recent quarters are likely to change slightly as additional records are submitted to PHS."
                          ),
                          
                          p("Although there is no legal requirement to submit SMR02 records to PHS, data completeness is very high. For example, for the period 1 April 2021 to 31 March 2021, live births recorded on SMR02 represented 97.6% of the live births registered by law with National Records of Scotland (NRS). In addition, the recording of gestation at birth is very complete. For the period 1 April 2021 to 31 March 2022, gestation was recorded on >99.9% of SMR02 records relating to singleton live births."
                          )
                      ) # box
                      
                    ) # fluidRow
                    
           ) # tabPanel("gestation_by_BAPM_LOC_datasource")
           
    ) # tabBox("Late pre-term and term/post-term admissions")
    
  ) # fluidRow
  
) # tabItem ("gestation_by_BAPM_LOC")


# INFANT FEEDING ----

infant_feeding <- tabItem(
  tabName = "infant_feeding",
  fluidRow(
    div(class = "no-tabbox-title",
        p("Infant feeding"
        )
    )
  ),
  
  fluidRow(
    column(12,
           div(class = "shiny-text-output",
               p("Infant Feeding Information",
                 style = "padding: 20px 0 14px 0;"
               )
           )
    )
  ),
  
  box(solidHeader = TRUE,
      width = 12,
      
      p("Infant feeding information is available in a ",
        
        tags$a(
          href = "https://scotland.shinyapps.io/phs-health-in-the-early-years-in-scotland/",
          tags$u("Health in the Early Years in Scotland (HEYS) Dashboard"),
          class = "externallink",
          target = "_blank"
        ),
        
        "also published by Public Health Scotland"
      ),
      
      br(),
      
      p("HEYS is a sister dashboard that presents data on measures related to breastfeeding and early child development in Scotland."
      ),
      
      p("This includes: ",
        
        tags$ul(
          tags$li(class = "bullet-points",
                  strong("Infant feeding:"), " exclusive breastfeeding; overall breastfeeding; and 'ever breastfed', based on data collected at Health Visitor reviews at 10-14 days and 6-8 weeks;"
          ),
          
          tags$li(class = "bullet-points",
                  strong("Child development:"), " developmental concerns based on data collected at reviews at 13-15 months, 27-30 months and 4-5 years."
          )
          
        )
      ),
      
      p("For all measures data are presented for each Health Board and Council Area of Residence (based on home postcode)."
      ),
      
      br(),

      tags$a(
        href = "https://scotland.shinyapps.io/phs-health-in-the-early-years-in-scotland/",
        class = "externallink",
        target = "_blank",
        tags$img(src = "HEYS.png",
                 alt = "Health in the Early Years in Scotland (HEYS) Dashboard landing page",
                 title = "Typical representation of the Health in the Early Years in Scotland (HEYS) Dashboard landing page",
                 width = "90%",
                 height = "90%")
      )

  ) # box
  
) # tabItem ("infant_feeding")

# BODY ----

body <- dashboardBody(
  
  use_theme(mytheme), # <-- use the theme to change colours
  tags$head(includeCSS("www/styles.css")),
  
  tabItems(
    home,
    multi_indicator_overview,
    pregnancies_booked,
    terminations,
    gestation_at_booking,
    gestation_at_termination,
    location_of_ex_pre_term,
    inductions,
    type_of_birth,
    perineal_tears,
    gestation_at_birth,
    stillbirths,
    apgar_scores,
    median_cga_30_32,
    gestation_by_BAPM_LOC,
    infant_feeding
  ) # tabItems
  
) # dashboardBody

# ui ----

ui <- 
  secure_app( # uncomment if want password protection
  tagList( #needed for shinyjs
    #useShinyjs(),  # Include shinyjs
    tags$head(
      HTML(
        "<html lang='en'>"),
      tags$link(rel="shortcut icon",
                href="favicon_phs.ico"), # Icon for browser tab
      tags$title("Scottish Pregnancy, Births and Neonatal Dashboard"),
    ),
    # Including Google analytics
    # includeScript("google-analytics.js")),
    
    dashboardPage(

      header,
      
      sidebar,
      
      body
      
    ) # dashboardPage
  
    
   ) # tagList

) # secure_app # uncomment if want password protection

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
                             Nicename = "under 32 weeks",
                             BAPM_LOC_Subgroup_cat = "between 34 and 36 weeks (inclusive)")
  
  observeEvent(input$organisation, Selected$HBType <- input$organisation)
  
  observeEvent(input$hbname, Selected$HBName <- input$hbname)
  
  observeEvent(input$date, Selected$Date <- input$date)
  
  observeEvent(input$gestation, Selected$Gestation <- input$gestation)
  
  observeEvent(input$BAPM_LOC_subgroup_cat, Selected$BAPM_LOC_Subgroup_cat <- input$BAPM_LOC_subgroup_cat)
  
  # observeEvent(input$link_to_patterns, {
  #   updateTabsetPanel(getDefaultReactiveDomain(),
  #                     "tabset00",
  #                     "patterns")
  # })
  
  # this observeEvent sets the current tabset back to the first tabPanel when a new tabset is selected from the
  # menu - this is needed to trigger the filter selections correctly
  
  observeEvent(input$topics,

               if (input$topics %in% names(tabnames)) {

                 # updateTabsetPanel(getDefaultReactiveDomain(),
                 #                   "tabset00", # home
                 #                   "instructions") 
                 
                 # removed as this was switching back to "instructions" if you clicked on another tab too soon after opening the dashboard - should not affect the workings as filters are not dependent on the tab selected here

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset01", # multi_indicator_overview
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset10", # pregnancies_booked
                                   "Individual Board")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset11", # terminations
                                   "Individual Board")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset12", # gestation_at_booking
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset13", # gestation_at_termination
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset20", # location_of_ex_pre_term
                                   "Scotland")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset21", # inductions
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset22", # type_of_birth
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset23", # perineal_tears
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset24", # gestation_at_birth
                                   "Board comparison")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset25", # stillbirths
                                   "Scotland")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset26", # apgar_scores
                                   "Board comparison")
                 
                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset27", # median_cga_30_32
                                   "Scotland")

                 updateTabsetPanel(getDefaultReactiveDomain(),
                                   "tabset28", # gestation_by_BAPM_LOC
                                   "Scotland")

                 Selected$Tabset <- "Board comparison" # forces reset for HBname filter where there is a Board comparison tab
               }
  )
  
  observeEvent(input$tabset00, Selected$Tabset <- input$tabset00)
  observeEvent(input$tabset01, Selected$Tabset <- input$tabset01)
  observeEvent(input$tabset10, Selected$Tabset <- input$tabset10)
  observeEvent(input$tabset11, Selected$Tabset <- input$tabset11)
  observeEvent(input$tabset12, Selected$Tabset <- input$tabset12)
  observeEvent(input$tabset13, Selected$Tabset <- input$tabset13)
  observeEvent(input$tabset20, Selected$Tabset <- input$tabset20)
  observeEvent(input$tabset21, Selected$Tabset <- input$tabset21)
  observeEvent(input$tabset22, Selected$Tabset <- input$tabset22)
  observeEvent(input$tabset23, Selected$Tabset <- input$tabset23)
  observeEvent(input$tabset24, Selected$Tabset <- input$tabset24)
  observeEvent(input$tabset25, Selected$Tabset <- input$tabset25)
  observeEvent(input$tabset26, Selected$Tabset <- input$tabset26)
  observeEvent(input$tabset27, Selected$Tabset <- input$tabset27)
  observeEvent(input$tabset28, Selected$Tabset <- input$tabset28)

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
        choices = c("Scotland", "NHS Ayrshire & Arran", "NHS Borders", "NHS Dumfries & Galloway",
             "NHS Fife", "NHS Forth Valley", "NHS Grampian", "NHS Greater Glasgow & Clyde",
             "NHS Highland", "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "NHS Orkney",
             "NHS Shetland", "NHS Western Isles"),
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
        #selected = "2023",
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
  
  # select TYPEOFBIRTH - appears on Type of Birth "Board comparison" view only
  
  output$typeofbirthControl <- renderUI({ 
    radioButtons(
      inputId = "tob",
      label = "Choose type of birth",
      choiceNames = list("all caesarean", "planned caesarean", "unplanned caesarean",
                         "assisted", "spontaneous vaginal"),
      choiceValues = list("all caesarean births", "planned caesarean births",
                          "unplanned caesarean births", "assisted vaginal births",
                          "spontaneous vaginal births"),
      selected = "all caesarean births",
      inline = FALSE
    )
  })
  
  # select Gestation at Birth - appears on Gestation at Birth "Board comparison" view only
  
  output$gestationControl <- renderUI({ 
    radioButtons(
      inputId = "gestation",
      label = "Choose gestation at birth",
      choiceNames = list("under 32 weeks",
                         HTML(
                           paste0("32", tags$sup("+0"), " to 36", tags$sup("+6"), " weeks"
                           )
                         ),
                         "under 37 weeks",
                         HTML(
                           paste0("42", tags$sup("+0"), " weeks and over"
                           )
                         )
      ),
      choiceValues = list("under 32 weeks", "between 32 and 36 weeks (inclusive)", "under 37 weeks", "42 weeks and over (inclusive)"
      ),
      selected = "under 32 weeks",
      inline = FALSE
    )
  })
  
  # select BAPM_LOC_gestation
  
  output$BAPM_LOC_subgroupControl <- renderUI({
    radioButtons(
      inputId = "BAPM_LOC_subgroup_cat",
      label = "Select gestation group",
      choiceNames = list("late pre-term", "term and post-term"),
      choiceValues = list("between 34 and 36 weeks (inclusive)", "between 37 and 42 weeks (inclusive)"),
      #selected = "late pre-term",
      inline = TRUE
    )
  })

  # output$mytext <- renderText({ # for testing
  #   paste0("Topic = ", input$topics) 
  # })

  # observeEvent(input$link_to_patterns, print(input$link_to_patterns))
  # observeEvent(input$topics, print(paste0("Topic = ", input$topics)))
  # observe(print(paste0("Selected Tabset = ", Selected$Tabset)))
  # observe(print(paste0("Measure_cat = ", Selected$Measure_cat)))
  # observe(print(paste0("Home: ", input$tabset00)))
  # observe(print(paste0("MIO: ", input$tabset01)))
  # observe(print(paste0("Pregnancies booked: ", input$tabset10)))
  # observe(print(paste0("Terminations: ", input$tabset11)))
  # observe(print(paste0("Gestation at booking: ", input$tabset12)))
  # observe(print(paste0("Gestation at termination: ", input$tabset13)))
  # observe(print(paste0("Location of extremely pre-term births: ", input$tabset20)))
  # observe(print(paste0("Induction of labour: ", input$tabset21)))
  # observe(print(paste0("Type of birth: ", input$tabset22)))
  # observe(print(paste0("Perineal tears: ", input$tabset23)))
  # observe(print(paste0("Gestation at birth: ", input$tabset24)))
  # observe(print(paste0("Stillbirths: ", input$tabset25)))
  # observe(print(paste0("Apgar scores: ", input$tabset26)))
  # observe(print(paste0("BAPM: ", Selected$BAPM_LOC_Subgroup_cat)))
  
  # this section tells the app where to find the code for each tab
  
  source("Multi indicator overview/Multi indicator overview chart.R", local = TRUE)
  
  source("Multi indicator overview/Multi indicator overview table.R", local = TRUE)
  
  source("Multi indicator overview/Multi indicator overview download data.R", local = TRUE)
  
  source("Antenatal booking/Antenatal bookings runcharts.R", local = TRUE)
  
  source("Terminations/Terminations runcharts.R", local = TRUE)
  
  source("Antenatal booking/Average gestation at booking small multiples.R", local = TRUE)
  
  source("Antenatal booking/Average gestation at booking runcharts.R", local = TRUE)
  
  source("Antenatal booking/Average gestation at booking download data.R", local = TRUE)
  
  source("Terminations/Average gestation at termination small multiples.R", local = TRUE)
  
  source("Terminations/Average gestation at termination runcharts.R", local = TRUE)
  
  source("Terminations/Average gestation at termination download data.R", local = TRUE)
  
  source("Extremely preterm/Extremely preterm control chart.R", local = TRUE) 
  
  source("Extremely preterm/Extremely preterm context chart.R", local = TRUE) 
  
  source("Extremely preterm/Extremely preterm download data.R", local = TRUE)
  
  source("Inductions/Inductions small multiples.R", local = TRUE)
  
  source("Inductions/Inductions runcharts.R", local = TRUE)
  
  source("Inductions/Inductions context charts.R", local = TRUE)
  
  source("Inductions/Inductions download data.R", local = TRUE)
  
  source("Type of birth/Type of birth small multiples.R", local = TRUE)
  
  source("Type of birth/Type of birth runcharts.R", local = TRUE)
  
  source("Type of birth/Type of birth context charts.R", local = TRUE)
  
  source("Type of birth/Type of birth download data.R", local = TRUE)
  
  source("Tears/Tears small multiples.R", local = TRUE)
  
  source("Tears/Tears runcharts.R", local = TRUE)
  
  source("Tears/Tears context charts.R", local = TRUE)
  
  source("Tears/Tears download data.R", local = TRUE)
  
  source("Gestation at birth/Gestation at birth small multiples.R", local = TRUE)
  
  source("Gestation at birth/Gestation at birth runcharts.R", local = TRUE)
  
  source("Gestation at birth/Gestation at birth context charts.R", local = TRUE)
  
  source("Gestation at birth/Gestation at birth download data.R", local = TRUE)
  
  source("Stillbirths and infant deaths/Stillbirths runcharts.R", local = TRUE)
  
  source("Apgar5/Apgar5 small multiples.R", local = TRUE)
  
  source("Apgar5/Apgar5 runcharts.R", local = TRUE)
  
  source("Apgar5/Apgar5 context charts.R", local = TRUE)
  
  source("Apgar5/Apgar5 download data.R", local = TRUE)
  
 # source("Neonatal/Median corrected gestational age at discharge runcharts.R", local = TRUE)

 # source("Neonatal/Median corrected gestational age at discharge context charts.R", local = TRUE)

 # source("Neonatal/Median corrected gestational age at discharge download data.R", local = TRUE)

 # source("Neonatal/Gestation by BAPM level of care runcharts.R", local = TRUE)

 # source("Neonatal/Gestation by BAPM level of care context charts.R", local = TRUE)

 # source("Neonatal/Gestation by BAPM level of care download data.R", local = TRUE)
  
  source("Version.R", local = TRUE)
  
  source("Footnotes.R", local = TRUE)
  
}

shinyApp(ui, server)  
