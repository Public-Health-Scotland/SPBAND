# a) data ----

tears_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- tears_data %>%
    filter(hbtype == Selected$HBType & period == "Q") %>%
    set_variable_labels(
      measure = "Percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear (%)",
      median = " average to Oct-Dec 2019",
      extended = " projected average from Jan-Mar 2020"
    ) %>% 
  mutate(mytext = paste0(hbname,
                           "<br>",
                         "Quarter: ", 
                         quarter_label,
                         "<br>",
                         "Percentage of women",
                         ": ",
                         format(measure,
                                digits = 1,
                                nsmall = 1),
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

output$tears_small_multiples <- renderPlotly({

creates_overview_charts_without_median(
  plotdata = tears_small_multiples_data()
  ) 
})

# c) chart title ----

output$tears_small_multiples_title <- renderText({
  #paste0("Percentage of women giving birth vaginally to a singleton live or stillborn baby with a cephalic presentation between 37-42 weeks gestation who have a third or fourth degree perineal tear by Board of ",
  paste0("Board of ", str_to_sentence(input$organisation)
  )
})
