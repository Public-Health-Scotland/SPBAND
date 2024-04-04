# a) data ----

gest_at_termination_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  Selected$HBName_terminations <- if_else(input$hbname %in% island_names,
                                          "NHS Orkney, NHS Shetland and NHS Western Isles*",
                                          input$hbname)
  
  data <- gest_at_termination_data %>%
    filter(hbname == Selected$HBName_terminations &
             hbtype == Selected$HBType) %>%
    set_variable_labels(
      measure_value = "Average gestation at termination",
      median = " average gestation to end Feb 2020",
      extended = " projected average gestation from Mar 2020"
    ) %>% 
    mutate(mytext = paste0("Month: ", 
                           format(date, "%b %Y"),
                           "<br>",
                           var_label(measure_value),
                           ": ",
                           format(measure_value,
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

output$gest_at_termination_runcharts <- renderPlotly({
  
  # if (input$hbname %in% island_names){
  #   
  #   plot_nodata(height = 450,
  #               text_nodata = paste("This chart is not shown due to small numbers.", "<br>",
  #               "Data for the Island Boards are included in the Scotland total.")
  #               )
  #   
  #   } else {

creates_runcharts(plotdata = gest_at_termination_runchart_data(),
                  yaxislabel = "Average gestation at termination (weeks)") %>% 
  layout(xaxis = list(range = 
                        range(gest_at_termination_runchart_data()$date) + c(months(-1), months(1)))
         )
    #}
})

# c) chart title ----

output$gest_at_terminaton_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         Selected$HBName_terminations
  )
})