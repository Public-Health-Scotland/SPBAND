#   # footnote which deals with correction to Gestation at Booking for FV and Tayside - retired as of October 2024 release
#   
#   url <- a("phs.matneodatahub@phs.scot", href="mailto:phs.matneodatahub@phs.scot")
#   
#   # footnote text for Forth Valley and Tayside
# 
# gest_at_booking_correction_text <-
# 
#   "* In early 2024 we detected errors in the way that revised medians for average gestation were calculated for NHS Forth Valley and NHS Tayside after changes in the process for recording booking. These errors, and separate errors affecting calculations of shifts relative to these medians, were corrected in the April 2024 release. This issue did not impact the majority of Boards for this measure nor any other measure's runcharts. For further details contact "
#   
#   output$gest_at_booking_footnote <- renderUI({
#     tagList(gest_at_booking_correction_text, url)
#   })
#   
#   observeEvent(input$hbname,
#                
#                toggleElement(id = "gest_at_booking_footnote", 
#                              condition = input$hbname %in% c("NHS Forth Valley", "NHS Tayside"))
#   )
#   
  # footnote for gestation at termination runcharts (when Island Boards are selected)

  output$gest_at_termination_runcharts_footnote1 <- renderText({
    
      if(input$hbname %in% island_names) {
        "* Values shown for the Island Boards (NHS Orkney, NHS Shetland and NHS Western Isles) for ‘Average gestation at termination’ are based on the data for those three Boards combined."
      }
    
    })
  
 # footnote for Type of Birth - Board comparison - Borders caesarean anomalies
  
  observeEvent(input$tob,
  
  output$Borders_caesarean_footnote1 <- renderText({
    if(grepl("planned", input$tob)) {
      "* Data for NHS Borders for planned and unplanned caesarean births show some unusual patterns from April 2022 to date. We have been liaising with NHS Borders and believe this to be a recording issue rather than a true reflection of the numbers. We are working with the Board to try to further understand and rectify the issue."
    }
  })
  )
  
  # footnote for Type of Birth - Individual Board - Borders caesarean anomalies

  output$Borders_caesarean_footnote2 <- output$Borders_caesarean_footnote3 <- renderText({
      if(input$hbname == "NHS Borders") {
        "* Data for NHS Borders for planned and unplanned caesarean births show some unusual patterns from April 2022 to date. We have been liaising with NHS Borders and believe this to be a recording issue rather than a true reflection of the numbers. We are working with the Board to try to further understand and rectify the issue."
      }
    })
