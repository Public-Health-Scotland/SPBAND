# Multi indicator overview - table ----

# a) data ----

multi_indicator_table_data_scot <- reactive({

  # selects data

  req(input$date)
  req(input$hbname)
  req(input$organisation)
  
  data <- filter(annual_dataframe,
                 DATE == Selected$Date &
                   HBNAME == "Scotland" &
                   HBTYPE == Selected$HBType
                 ) %>%
    arrange(KEY_INDICATOR_REF) %>%
    mutate(LABEL = sub("Percentage", "%", KEY_INDICATOR_LABEL)) %>% 
    mutate(LABEL = factor(LABEL, levels = as.character(unique(LABEL)))) %>% # updates the factor levels
    pivot_wider(names_from = HBNAME, values_from = MEASURE, values_fill = 0) %>%
    ungroup() %>% 
    rename(SCOT_MEASURE = Scotland) %>% 
    select(PERIOD, HBTYPE, KEY_INDICATOR_REF, INDICATOR, LABEL, SUFFIX, DATE, SCOT_MEASURE)
  
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
                 DATE == Selected$Date &
                   HBNAME == Selected$HBName &
                   HBTYPE == Selected$HBType
  ) %>%
    arrange(KEY_INDICATOR_REF) %>%
    mutate(LABEL = sub("Percentage", "%", KEY_INDICATOR_LABEL)) %>% 
    mutate(LABEL = factor(LABEL, levels = as.character(unique(LABEL)))) %>% # updates the factor levels
    ungroup() %>% 
    left_join(., multi_indicator_table_data_scot()) %>% # joins Scotland data
    select(LABEL, MEASURE, SCOT_MEASURE, SUFFIX)
  
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
  DT::renderDT({datatable(multi_indicator_table_data_hb(),
                          options = list(dom = 't'),
                          selection = "single",
                          rownames = FALSE,
                          colnames = c("", Selected$HBName, "Scotland", "")) %>%
      formatRound(
        c('MEASURE', 'SCOT_MEASURE'), 1) 
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
