# a) data ----

inductions_small_multiples_data <- reactive({
  # selects data
  
  req(input$organisation)
  
  data <- inductions_data %>%
    filter(hbtype == Selected$HBType) %>%
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

output$inductions_small_multiples <- renderPlotly({

subplot_mainland_island_small_multiples(
  plotdata = inductions_small_multiples_data()
  )
})

# c) chart title ----

output$inductions_small_multiples_title <- renderText({
  paste0("Board of ", str_to_sentence(input$organisation)
  )
})
