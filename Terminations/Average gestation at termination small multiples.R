# a) data ----

gest_at_termination_small_multiples_data <- reactive({
  # selects data
  
  req(input$organisation)
  
  data <- gest_at_termination_data %>%
    filter(hbtype == Selected$HBType) %>%
    mutate(hbname2 = factor(hbname, 
                            levels = HBnames_alternative), # includes grouped Island Boards),
           mytext = paste0(hbname,
                           "<br>",
                           "Month: ", 
                           format(date, "%b %Y"),
                           "<br>",
                           "Average gestation at termination",
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           " weeks"
           ),
           hbgroup = factor(if_else(hbname %in% island_names_alternative, "island", "mainland"),
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

output$gest_at_termination_small_multiples_mainland <- renderPlotly({
  subplot_mainland_island_small_multiples(
  plotdata = gest_at_termination_small_multiples_data()
  )$mainland
})

output$gest_at_termination_small_multiples_island <- renderPlotly({
  subplot_mainland_island_small_multiples(
  plotdata = gest_at_termination_small_multiples_data()
  )$island
})

# c) chart title ----

output$gest_at_termination_small_multiples_title <- renderText({
  # paste0("Average gestation at termination by Board of ",
  paste0("Board of ",
         str_to_sentence(input$organisation)
  )
})
