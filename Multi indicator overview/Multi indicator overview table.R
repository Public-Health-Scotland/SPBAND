# Multi indicator overview - table ----

# a) data ----

multi_indicator_table_data_scot <- reactive({

  # selects data

  req(input$date)
  req(input$hbname)
  req(input$organisation)
  
  data <- filter(annual_dataframe,
                 date == Selected$Date &
                   hbname == "Scotland" &
                   hbtype == Selected$HBType
                 ) %>%
    arrange(key_measure_ref) %>%
    mutate(label = sub("Percentage", "%", key_measure_label)) %>% 
    mutate(label = factor(label, levels = as.character(unique(label)))) %>% # updates the factor levels
    pivot_wider(names_from = hbname, values_from = measure, values_fill = 0) %>%
    ungroup() %>% 
    rename(SCOT_MEASURE = Scotland) %>% 
    select(period, hbtype, key_measure_ref, indicator, label, suffix, date, SCOT_MEASURE)
  
  if (is.null(data()))
  {
    return()
  }

  else {
    data
  }
})

multi_indicator_table_data_hb<- reactive({
  
  # selects data
  
  req(input$date)
  req(input$hbname)
  req(input$organisation)
  req(multi_indicator_table_data_scot())
  
  data <- filter(annual_dataframe,
                 date == Selected$Date &
                   hbname == Selected$HBName &
                   hbtype == Selected$HBType
  ) %>%
    arrange(key_measure_ref) %>%
    mutate(label = sub("Percentage", "%", key_measure_label)) %>% 
    mutate(label = factor(label, levels = as.character(unique(label)))) %>% # updates the factor levels
    ungroup() %>% 
    left_join(., multi_indicator_table_data_scot()) %>% # joins Scotland data
    select(label, measure, SCOT_MEASURE, suffix)
  
  if (is.null(data()))
  {
    return()
  }
  
  else {
    data
  }
})

# b) data table

output$mytable <- 
  renderDT({datatable(multi_indicator_table_data_hb(),
                      options = list(dom = 't'),
                      selection = "single",
                      rownames = FALSE,
                      colnames = c("", Selected$HBName, "Scotland", "")) %>%
      formatRound(
        c('measure', 'SCOT_MEASURE'), 1) 
  })

# c) title

output$multi_indicator_table_title <- renderText({
  paste0("Core Maternity Indicators, by Board of ",
         str_to_sentence(input$organisation),
         ", for ",
         if_else(input$date %like% "/", "financial year ",
                 "calendar year "),
         input$date)
})
