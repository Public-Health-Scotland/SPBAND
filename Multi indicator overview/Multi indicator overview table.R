# Multi measure overview - table ----

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
    pivot_wider(names_from = hbname, values_from = measure_value, values_fill = 0) %>%
    ungroup() %>% 
    rename(SCOT_MEASURE = Scotland) %>% 
    select(period, hbtype, key_measure_ref, measure, label, suffix, date, SCOT_MEASURE)
  
  if (is.null(data()))
  {
    return()
  }

  else {
    data
  }
})

multi_indicator_table_data_hb <- reactive({
  
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
    select(label, measure_value, SCOT_MEASURE, suffix)

  if (is.null(data()))
  {
    return()
  }
  
  else {
    data
  }
})

# b) data table

# pull header names from the table
header.names <- reactive ({
  c("", if_else(input$hbname %in% island_names,
                paste0(Selected$HBName, "*"),
                Selected$HBName),
    "Scotland", "")
 })

# the container parameter allows us to design the header of the table using CSS

my.container <- reactive({
  withTags(
    table(
      style(type = "text/css", header.style),
      thead(
        tr(
          lapply(header.names(), th) #, style = "text-align: center; border-right-width: 1px; border-right-style: solid; border-right-color: white; border-bottom-width: 1px; border-bottom-style: solid; border-bottom-color: white")
          )
        )
      )
    )
})

output$mytable <- 
  renderDT({
    my.table <- datatable(
      multi_indicator_table_data_hb(),
      container = my.container(),
      options = my.options,
      caption = htmltools::tags$caption("View measures by Board: select a Board in the filter to compare against Scotland",
                  style = "color: #3F3685; margin-bottom: 0px;"
                  ),
      rownames = FALSE, # do not treat table row names as separate column
      width = '100%', # ensure table remains within the dimensions of the container
      height = '100%' # ensure table remains within the dimensions of the container
    )
    
    # create specific table formatting customizations for table (round numbers to 1 d.p., font)
    
    my.table <- 
      formatStyle(
        my.table,
        columns = colnames(multi_indicator_table_data_hb()),
        color = "#3F3685",
        fontFamily = "Arial",
        fontSize = "16px")
    
    my.table <- formatRound(
      my.table,
      columns = c("measure_value", "SCOT_MEASURE"), 1)
  })

# c) title

output$multi_indicator_table_title <- renderText({
  paste0("Core measures, by Board of ",
         str_to_sentence(input$organisation),
         ", for ",
         if_else(input$date %like% "/", "financial year ",
                 "calendar year "),
         input$date)
})
