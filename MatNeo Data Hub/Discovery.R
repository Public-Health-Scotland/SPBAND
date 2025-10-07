# MatNeo Data Hub Discovery

output$hub_discovery <- renderUI({
  
  fluidRow(
    
    box(solidHeader = TRUE,
        width = 12,
        
        p("Another Public Health Scotland dashboard that includes information on maternity care in Scotland is the ",
          tags$a(
            href = "https://www.nssdiscovery.scot.nhs.uk/",
            tags$u("Discovery dashboard."),
            class = "externallink",
            target = "_blank",
            rel = "noopener noreferrer"
          ),
          
          "You will need to register for a user name and password for this one."
        )
        
    ) # box
    
  ) # fluidRow
  
})
