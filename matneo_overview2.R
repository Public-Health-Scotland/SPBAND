# MatNeo Data Hub Intro

output$matneo_data_hub_intro <- renderUI({
  
  tabPanel(title = "A MatNeo Data Hub for Scotland ",
           value = "matneo_info",
           
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
  ) #tabPanel
  
#   fluidRow(
# 
#     box(solidHeader = TRUE,
#         width = 12,
#         
#         br(),
#         
#         column(12,
#                h3(strong("A MatNeo Data Hub for Scotland"
#                )
#                ),
#                
#                br(),
#                
#                p("One recommendation of ‘The Best Start – A Five Year Forward Plan for Maternity and Neonatal Care in Scotland’ was to establish a Maternity and Neonatal Data Hub for Scotland.  
# 
# A MatNeo Data Hub has been created within Public Health Scotland (PHS), with support from the Scottish Perinatal Network, Healthcare Improvement Scotland, Scottish Government, and National Records of Scotland. The MatNeo Data Hub is now part of the Early Years and Young People Programme in PHS 
# 
# Developmental work since 2019 includes: 
# 
# Establish additional new all-Scotland maternity data sets 
# 
# Antenatal Booking Collection (ABC; including an expansion to collect further variables on mother’s social circumstances, health and behaviours, and antenatal scans) 
# 
# Mother, Birth and Baby (MoBBa; to sit alongside SMR02 and collect data on mother’s health, details of birth and information on baby’s health and treatment 
# 
# Miscarriage (to provide enhanced awareness of numbers of women experiencing miscarriages, numbers of miscarriages and numbers of recurrent miscarriages) 
# 
# Routine collection of data on specialist neonatal care (NeoCareIn+) 
# 
# Data displays showing maternity and neonatal CORE measures (including the Scottish Pregnancy, Births and Neonatal Data (SPBAND) Dashboard)"
#                )
#                )
#     ) # box 
#   ) # fluidRow
})
               