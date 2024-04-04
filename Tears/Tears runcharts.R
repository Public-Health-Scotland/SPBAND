# a) data ----

tears_runchart_data <- reactive({
  # selects data
  
  #req(input$period)
  
data <- tears_data %>%
  filter(hbname == Selected$HBName &
           period == "Q" &
           hbtype == Selected$HBType) %>%
  set_variable_labels(
    num = "Number of women who had a third or fourth degree perineal tear: ",
    den = "Total number of women: ",
    measure_value = "percentage of women (%)",
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
                         "Percentage of women",
                         ": ",
                         format(measure_value,
                                digits = 1,
                                nsmall = 1),
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

output$tears_runcharts <- renderPlotly({
  creates_runcharts(plotdata = tears_runchart_data()
                    )
})

# c) chart title ----

output$tears_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ", 
         input$hbname
  )
  
})
