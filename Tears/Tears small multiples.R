# a) data ----

tears_small_multiples_data <- reactive({
  # selects data
  
  req(input$organisation)
  
  data <- tears_data %>%
    filter(hbtype == Selected$HBType) %>%
    mutate(mytext = paste0(hbname,
                           "<br>",
                           "Quarter: ", 
                           quarter_label,
                           "<br>",
                           "Percentage of women",
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
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

output$tears_small_multiples <- renderPlotly({

subplot_mainland_island_small_multiples(
  plotdata = tears_small_multiples_data()
  ) 
})

# c) chart title ----

output$tears_small_multiples_title <- renderText({
  paste0("Board of ", str_to_sentence(input$organisation)
  )
})
