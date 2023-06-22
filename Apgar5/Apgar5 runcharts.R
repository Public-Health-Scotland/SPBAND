# a) data ----

apgar5_runchart_data <- reactive({
  # selects data
  
  #req(input$period)
  
data <- apgar5_data %>%
  filter(HBNAME == Selected$HBName &
           PERIOD == "Q" &
           HBTYPE == Selected$HBType) %>%
  set_variable_labels(
    NUM = "Number of births that have a 5 minute Apgar score of <7: ",
    DEN = "Total number of births: ",
    MEASURE = "percentage of births (%)",
    MEDIAN = " average to Oct-Dec 2019",
    EXTENDED = " projected average from Jan-Mar 2020"
    #orig_trend = "Trends: 5 or more consistently increasing or decreasing points",
    #orig_shift = "Shifts: 6 or more consecutive points above or below average"
  ) %>% 
  mutate(mytext = paste0("Quarter: ", 
                         QUARTER_LABEL,
                         "<br>",
                         var_label(NUM), NUM, "<br>",
                         var_label(DEN), DEN, "<br>",
                         "Percentage of births",
                         ": ",
                         format(MEASURE,
                                digits = 1,
                                nsmall = 1),
                         "%")
         #DATE = QUARTER_LABEL
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

output$apgar5_runcharts <- renderPlotly({
  creates_runcharts(plotdata = apgar5_runchart_data()
                    )
})

# c) chart title ---- 

output$apgar5_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ", 
         input$hbname
  )
  
})