# a) data ----

gest_at_booking_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- gest_at_booking_data %>%
    filter(HBTYPE == Selected$HBType) %>%
    set_variable_labels(
      MEASURE = "Average gestation at booking",
      MEDIAN = " average gestation to end Feb 2020",
      EXTENDED = " projected average gestation from Mar 2020 to end Jul 2020"
    ) %>% 
    mutate(mytext = paste0(HBNAME,
                           "<br>",
                           "Month: ", 
                           format(DATE, "%b %Y"),
                           "<br>",
                           var_label(MEASURE),
                           ": ",
                           format(MEASURE,
                                  digits = 1,
                                  nsmall = 1),
                           " weeks")
    )


  if (is.null(data()))
  {
    return()
  }
  
  else {
    data
  }
})

# b) chart ---- 

output$gest_at_booking_small_multiples <- renderPlotly({

creates_overview_charts_without_median(
  plotdata = gest_at_booking_small_multiples_data(),
  yaxislabel = "Average gestation at booking (weeks)"
  )
  })

# c) chart title ----

output$gest_at_booking_small_multiples_title <- renderText({
  # paste0("Average (mean) gestation at booking by Board of ",
  paste0("Board of ",
         str_to_sentence(input$organisation)
  )
})

