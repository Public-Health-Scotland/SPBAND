# Topics Index

output$hub_topics_index <- output$hub_topics_index2 <- renderUI({
  
  fluidRow(
    
    box(solidHeader = TRUE,
        width = 12,
        
        p("The Topics Index catalogues individual maternity and neonatal measures already available, including those on the ",
          
          tags$a(
            href = "https://scotland.shinyapps.io/phs-pregnancy-births-neonatal/",
            tags$u("Scottish Pregnancy, Births and Neonatal Data (SPBAND) Dashboard,"),
            class = "externallink",
            target = "_blank"
          ),
          
          "the ",
          
          tags$a(
            href = "https://scotland.shinyapps.io/phs-health-in-the-early-years-in-scotland/",
            tags$u("Health in the Early Years in Scotland (HEYS) dashboard,"),
            class = "externallink",
            target = "_blank"
          ),
          
          tags$a(
            href = "https://publichealthscotland.scot/services/discovery/overview/what-is-discovery/",
            tags$u("Discovery,"),
            class = "externallink",
            target = "_blank"
          ),
          
          tags$a(
            href = "https://maternityaudit.org.uk/",
            tags$u("NMPA,"),
            class = "externallink",
            target = "_blank"
          ),
          
          tags$a(
            href = "https://www.rcpch.ac.uk/work-we-do/quality-improvement-patient-safety/national-neonatal-audit-programme",
            tags$u("NNAP,"),
            class = "externallink",
            target = "_blank"
          ),
          
          tags$a(
            href = "https://www.nrscotland.gov.uk/statistics-and-data",
            tags$u("National Records of Scotland"),
            class = "externallink",
            target = "_blank"
          ),
          
          "and",
          
          tags$a(
            href = "https://publichealthscotland.scot/population-health/early-years-and-young-people/maternity-and-births/",
            tags$u("PHS"),
            class = "externallink",
            target = "_blank"
          ),
          
          "websites."
        ),
        
        p("The Topics Index also includes a list of CORE maternity measures."
        ),
        
        p("The Topics index is available at ", 
          
          tags$a(
            href = "https://docs.google.com/spreadsheets/d/1iAcRF8gc1-k7341JygofiSUmsvmKJ_OxUPyE07XVTPU/edit#gid=277533606",
            tags$u("MatNeo Data Hub Topics Index."),
            class = "externallink",
            target = "_blank"
          )
        ),
        
        p("We appreciate the Google Sheets platform may not be available to all our users and are working to develop a more sophisticated accessible platform for the catalogue. In the meantime, if you cannot access the hyperlink above and would like to view the Topics Index, please contact the ",
          tags$a(href = "mailto:phs.matneodatahub@phs.scot", tags$u("Hub Programme Team")),
          "and we will seek alternative ways of sharing the index with you."
        )
        
  ) # box
  
  ) # fluidRow

})
