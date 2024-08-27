# footnote text for Forth Valley and Tayside revised medians in Gestation at booking measure

gest_at_booking_revised_median_text <-  # was gest_at_booking_correction_text

  tags$li(class= "bullet-points",
          "A green line shows a revised overall average (median) of the mean gestation at booking during a period after changes were made to the process for recording booking. The green line is dashed where the revised average is projected outside that time range."
  )

  output$gest_at_booking_footnote <- renderUI({
    tagList(gest_at_booking_revised_median_text)
  })

  observeEvent(input$hbname,

               toggleElement(id = "gest_at_booking_footnote",
                             condition = input$hbname %in% c("NHS Forth Valley", "NHS Tayside"))
  )

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
