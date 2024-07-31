# builds Version table

# `Version` <- c("1.0", "1.1", "1.2", "1.3", "1.4", "1.5")
# `Date` <- c("3 Oct 2023", "9 Nov 2023", "15 Feb 2024", "2 Apr 2024", "2 Jul 2024", "1 Oct 2024")
# `Change` <- c("First public release of SPBAND.",
#               "Amended Home - How to use this dashboard.",
#               "Updated links and standardised titles, labels and metadata.",
#               "Corrected the medians and shifts for NHS Forth Valley and NHS Tayside in the ‘Gestation at booking’ measure;
#               replaced CSV download files with accessible Excel download files;
#               updated links and standardised titles, labels, legends and metadata.",
#               "Added aggregated values for the Island Boards in the ‘Gestation at termination’ measure - these Boards are now also represented on the Multi Indicator Overview for this measure;
#               revised the y-axis scales for the Island Boards in the small multiple charts (where possible) to make the mainland Boards' variation easier to see;
#               removed the 'dots' from the monthly small multiple charts (i.e. the ‘Gestation at booking’ and ‘Gestation at termination’ measures); added notes describing the issue with NHS Borders planned and unplanned caesarean birth rates.",
#               "Added a post-pandemic median to the ‘Gestation at booking’ measure - this spans the two-year period July 2022 to June 2024. Shifts have been recalculated accordingly. "
# )

`Version` <- c("1.5", "1.4", "1.3", "1.2", "1.41", "1.0")
`Date` <- c("1 Oct 2024", "2 Jul 2024", "2 Apr 2024", "15 Feb 2024", "9 Nov 2023", "3 Oct 2023")
`Change` <- c("Added a post-pandemic median to the ‘Gestation at booking’ measure - this spans the two-year period July 2022 to June 2024. Shifts have been recalculated accordingly.",
              "Added aggregated values for the Island Boards in the ‘Gestation at termination’ measure - these Boards are now also represented on the Multi Indicator Overview for this measure;
              revised the y-axis scales for the Island Boards in the small multiple charts (where possible) to make the mainland Boards' variation easier to see;
              removed the 'dots' from the monthly small multiple charts (i.e. the ‘Gestation at booking’ and ‘Gestation at termination’ measures); added notes describing the issue with NHS Borders planned and unplanned caesarean birth rates.",
              "Corrected the medians and shifts for NHS Forth Valley and NHS Tayside in the ‘Gestation at booking’ measure;
              replaced CSV download files with accessible Excel download files;
              updated links and standardised titles, labels, legends and metadata.",
              "Updated links and standardised titles, labels and metadata.",
              "Amended Home - How to use this dashboard.",
              "First public release of SPBAND."
              )

version_info <- tibble(`Version`, `Date`, `Change`)

output$version_tbl <- renderTable(version_info, 
                                  striped = TRUE,
                                  bordered = TRUE)

output$version_panel <- renderUI({
  
  fluidRow(

    box(solidHeader = TRUE,
        width = 12,
        
        br(),
        
        column(12,
               
               p("This dashboard is scheduled to be updated each quarter, generally by the first Tuesday in the months of January, April, July and October."
               ),
               
               p("Data are shown for up to and including the most recent period for which records are considered near complete. Data for the most recent period should be viewed as provisional. Data for the whole time period shown will be refreshed every time the dashboard updated, and data for the most recent periods are likely to change slightly as additional or updated records are added."
               ),
               
               br(),
               
               p(paste0("The data were last refreshed on ", pretty_refresh_date, ".")
               ),
               
               br(),
               
               p("Small changes to the format of the dashboard are noted with new version numbers as detailed below:"),
               
               br(),
               
               tableOutput('version_tbl')
        )
        
    ) # box
    
  ) # fluidRow

})
