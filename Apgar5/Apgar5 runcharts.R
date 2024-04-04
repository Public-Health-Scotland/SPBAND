# a) data ----

apgar5_runchart_data <- reactive({
  # selects data
  
  #req(input$period)
  
data <- apgar5_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType) %>%
  set_variable_labels(
    num = "Number of babies that had a 5 minute Apgar score of <7: ",  # babies have apgar scores not births
    den = "Total number of babies: ",
    measure_value = "percentage of babies (%)",
    median = " average to Oct-Dec 2019",
    extended = " projected average from Jan-Mar 2020"
  ) %>% 
  mutate(mytext = paste0("Quarter: ", 
                         quarter_label,
                         "<br>",
                         var_label(num),
                         prettyNum(num, big.mark = ","),
                         "<br>",
                         var_label(den),
                         prettyNum(den, big.mark = ","),
                         "<br>",
                         "Percentage of babies",
                         ": ",
                         format(measure_value,
                                digits = 1,
                                nsmall = 2),
                         "%")
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
