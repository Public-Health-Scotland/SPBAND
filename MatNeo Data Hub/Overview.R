# MatNeo Data Hub Overview

output$hub_overview <- renderUI({
  
  fluidRow(
    
    box(solidHeader = TRUE,
        width = 12,
        
        p("One recommendation of ",
          
          tags$a(
            href = "https://www.gov.scot/publications/best-start-five-year-forward-plan-maternity-neonatal-care-scotland/",
            tags$u("The Best Start: five year plan for maternity and neonatal care"),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),
          
          "was to establish a Maternity and Neonatal Data Hub for Scotland."
        ),
        
        p("The MatNeo Data Hub was created within ",
          
          tags$a(
            href = "https://www.publichealthscotland.scot/",
            tags$u("Public Health Scotland"),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),
          
          " (PHS), with support from the ",
          
          tags$a(
            href = "https://perinatalnetwork.scot/",
            tags$u("Scottish Perinatal Network,"),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),
          
          tags$a(
            href = "https://www.healthcareimprovementscotland.scot/",
            tags$u("Healthcare Improvement Scotland,"),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),
          
          tags$a(
            href = "https://www.gov.scot/policies/maternal-and-child-health/improving-maternal-and-neonatal-care/",
            tags$u("Scottish Government"),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),
          
          " and",
          
          tags$a(
            href = "https://www.nrscotland.gov.uk/statistics-and-data",
            tags$u("National Records of Scotland."),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),
          
          " The Hub is now part of the ",
          
          tags$a(
            href = "https://www.publichealthscotland.scot/population-health/early-years-and-young-people/",
            tags$u("Early Years and Young People Programme"),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),
          
          " in PHS."
        ),
        
        p("Developmental work since 2019 includes:"
        ),
        
        tags$ul(
          
          tags$li(class = "bullet-points", 
                  
                  p("Establish additional new all-Scotland ", strong("maternity data sets")
                  )
          ),
          
        #), # tags$ul,
        
        tags$ul(
          
          #column(1),
          
          tags$li(class = "open-bullet-points", 
                  p(strong("Antenatal Booking Collection"), 
                    " -  known as ABC; including an expansion to collect further variables on mother’s social circumstances, health and behaviours, and antenatal scans"
                  )
          ),
          
          #column(1),
          
          tags$li(class = "open-bullet-points",
                  p(strong("Mother, Birth and Baby"),
                    " - known as MoBBa; to sit alongside SMR02 and collect data on mother’s health, details of birth and information on baby’s health and treatment"
                  )
          ),
          
          #column(1),
          
          tags$li(class = "open-bullet-points",
                  p(strong("Miscarriage"),
                    " - to provide enhanced awareness of numbers of women experiencing miscarriages, numbers of miscarriages and numbers of recurrent miscarriages"
                  )
          )
          
        ), # tags$ul
        
        #tags$ul(
          
          tags$li(class = "bullet-points",
                  p("Routine collection of data on ",
                    strong("specialist neonatal care"),
                    "(NeoCareIn+)"
                  )
          ),
          
          tags$li(class = "bullet-points",
                  strong("Data displays"),
                  " showing maternity and neonatal CORE measures, including the ",
                  
                  tags$a(
                    href = "https://scotland.shinyapps.io/phs-pregnancy-births-neonatal/",
                    tags$u("Scottish Pregnancy, Births and Neonatal Data (SPBAND) Dashboard"),
                    class = "externallink",
                    target = "_blank", 
                    rel = "noopener noreferrer"
                  )
          )
        ) # tags$ul
        
    ) # box
    
  ) # fluidRow
  
})
