# a) data ----

apgar5_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- apgar5_data %>%
    filter(HBTYPE == Selected$HBType & PERIOD == "Q") %>%
    set_variable_labels(
      MEASURE = "Percentage of singleton live births at 37-42 weeks gestation that have a 5 minute Apgar score of <7 (%)",
      MEDIAN = " average to Oct-Dec 2019",
      EXTENDED = " projected average from Jan-Mar 2020"
    ) %>% 
  mutate(mytext = paste0(HBNAME,
                           "<br>",
                         "Quarter: ", 
                         QUARTER_LABEL,
                         "<br>",
                         "Percentage of births",
                         ": ",
                         format(MEASURE,
                                digits = 1,
                                nsmall = 1),
                         "%"),
        DATE = QUARTER_LABEL
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

output$apgar5_small_multiples <- renderPlotly({

creates_overview_charts_without_median(plotdata = apgar5_small_multiples_data()
                                       )
  
})

# c) chart title ----

output$apgar5_small_multiples_title <- renderText({
  #paste0("Percentage of singleton live births at 37-42 weeks gestation that have a 5 minute Apgar score of <7 by Board of ",
  paste0("Board of ", str_to_sentence(input$organisation))
})
