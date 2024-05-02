# a) data ----

apgar5_small_multiples_data <- reactive({
  # selects data
  
  req(input$organisation)
  
  data <- apgar5_data %>%
    filter(hbtype == Selected$HBType) %>%
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
           date = quarter_label,
           hbgroup = factor(if_else(hbname %in% island_names, "island", "mainland"),
                            levels = c("mainland", "island"), ordered = TRUE)
    ) %>% 
    group_by(hbgroup, hbtype) %>% 
    mutate(y_max = max(measure_value)
    ) %>%
    ungroup()

  
  
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

subplot_mainland_island_small_multiples(
  plotdata = apgar5_small_multiples_data()
  )
})

# c) chart title ----

output$apgar5_small_multiples_title <- renderText({
  paste0("Board of ", str_to_sentence(input$organisation))
})
