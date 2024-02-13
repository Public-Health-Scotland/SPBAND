# a) data ----

inductions_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- inductions_data %>%
    filter(hbtype == Selected$HBType & period == "Q") %>%
    set_variable_labels(
      measure_value = "Percentage of births that followed induction (%)",
      median = " average to Oct-Dec 2019",
      extended = " projected average from Jan-Mar 2020"
    ) %>% 
  mutate(mytext = paste0(hbname,
                           "<br>",
                         "Quarter: ", 
                         quarter_label,
                         "<br>",
                         "Percentage of births",
                         ": ",
                         format(measure_value,
                                digits = 1,
                                nsmall = 1),
                         "%"),
        date = quarter_label
  )

})

# b) chart ---- 

output$inductions_small_multiples <- renderPlotly({

creates_overview_charts_without_median(
  plotdata = inductions_small_multiples_data()
  )
})

# c) chart title ----

output$inductions_small_multiples_title <- renderText({
  # paste0("Percentage of singleton live births at 37-42 weeks gestation following induction of labour by Board of ",
  paste0("Board of ", str_to_sentence(input$organisation)
  )
})
