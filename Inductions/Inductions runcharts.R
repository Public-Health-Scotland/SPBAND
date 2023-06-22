# a) data ----

inductions_runchart_data <- reactive({
  # selects data
  
  #req(input$period)
  
data <- inductions_data %>%
  filter(HBNAME == Selected$HBName &
           PERIOD == "Q" &
           HBTYPE == Selected$HBType) %>%
  set_variable_labels(
    NUM = "Number of births following induction: ",
    DEN = "Total number of births: ",
    MEASURE = "percentage of births following induction (%)",
    MEDIAN = " average to Oct-Dec 2019",
    EXTENDED = " projected average from Jan-Mar 2020"
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

output$inductions_runcharts <- renderPlotly({
  creates_runcharts(plotdata = inductions_runchart_data()
                    )
})

# c) chart title ----

output$inductions_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ", 
         input$hbname
  )
  
})