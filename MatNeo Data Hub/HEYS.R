# MatNeo Data Hub HEYS

output$hub_heys <- renderUI({
  
  fluidRow(
    
    box(solidHeader = TRUE,
        width = 12,
        
        p("The ",
          
          tags$a(
            href = "https://scotland.shinyapps.io/phs-health-in-the-early-years-in-scotland/",
            tags$u("Health in the Early Years in Scotland (HEYS)"),
            class = "externallink",
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          
          "dashboard provides information on infant feeding (and child development), and is updated quarterly. A link to HEYS is available from the ",
          strong("‘Infant feeding’"),
          " menu item on SPBAND."
        )
        
    ) # box
    
  ) # fluidRow
  
})
