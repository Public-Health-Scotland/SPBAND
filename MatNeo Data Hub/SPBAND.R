# MatNeo Data Hub SPBAND

output$hub_spband <- renderUI({
  
  fluidRow(
    
    box(solidHeader = TRUE,
        width = 12,
        
        p("As part of our COVID-19 response, Public Health Scotland created a data dashboard showing wider impacts of COVID-19. ",
          strong("‘Pregnancy’"),
          " and ",
          strong("‘Births and babies’"),
          " sections of the Wider Impacts dashboard were updated each month until Sep 2023. Those two sections were replaced in October 2023 by a new ",
          
          tags$a(
            href = "https://scotland.shinyapps.io/phs-pregnancy-births-neonatal/",
            tags$u("Scottish Pregnancy, Births and Neonatal Data (SPBAND) Dashboard"),
            class = "externallink",
            target = "_blank", 
            rel = "noopener noreferrer"
          ),

        "(this dashboard). The measures on SPBAND are refreshed quarterly (in January, April, July and October each year)."
        ),
        
        p("SPBAND includes the following topics:"
        ),
        
        tags$ul(
          
          tags$li(class = "bullet-points",
                  strong("Pregnancy:"),
                  "number of pregnancies booked; average gestation at booking; number of terminations; average gestation at termination"
          ),
          
          tags$li(class = "bullet-points",
                  strong("Births and Babies:"),
                  "location of extremely pre-term births; induction of labour; type of birth; third- and fourth-degree perineal tears; pre- and post-term births; stillbirths and infant deaths; Apgar scores"
          )
        ), # tags$ul
        
        p("SPBAND offers three ways to view data: time series charts for individual measures and individual Health Boards; small multiple time series charts, to allow comparison (for a particular measure) across Health Board areas, and a multi-indicator overview that displays multiple measures simultaneously, allowing comparison across Health Board areas."
        )
        
    ) # box
    
  ) # fluidRow
  
})
