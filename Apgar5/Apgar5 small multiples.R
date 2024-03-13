# a) data ----

apgar5_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- apgar5_data %>%
    filter(hbtype == Selected$HBType & period == "Q") %>%
    # set_variable_labels(
    #   measure_value = "Percentage of singleton live births at 37-42 weeks gestation that had a 5 minute Apgar score of <7 (%)",
    #   median = " average to Oct-Dec 2019",
    #   extended = " projected average from Jan-Mar 2020"
    # ) %>% 
  mutate(mytext = paste0(hbname,
                           "<br>",
                         "Quarter: ", 
                         quarter_label,
                         "<br>",
                         "Percentage of babies", # babies have apgar scores not births
                         ": ",
                         format(measure_value,
                                digits = 1,
                                nsmall = 2),
                         "%"),
        date = quarter_label
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
  paste0("Board of ", str_to_sentence(input$organisation))
})
