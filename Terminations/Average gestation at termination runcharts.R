# a) data ----

gest_at_termination_runchart_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- gest_at_termination_data %>%
    filter(HBNAME == Selected$HBName &
             HBTYPE == Selected$HBType) %>%
    set_variable_labels(
      MEASURE = "Average gestation at termination",
      MEDIAN = " average gestation to end Feb 2020",
      EXTENDED = " projected average gestation from Mar 2020"
    ) %>% 
    mutate(HBNAME2 = factor(HBNAME2, 
                            levels = c("NHS Ayrshire & Arran", "NHS Borders",
                                       "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley",
                                       "NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Highland",
                                       "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "Scotland")),
           mytext = paste0("Month: ", 
                           format(DATE, "%b %Y"),
                           "<br>",
                           var_label(MEASURE),
                           ": ",
                           format(MEASURE,
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
  
  if (input$hbname %in% island_names){
    
    plot_nodata(height = 450,
                text_nodata = paste("This chart is not shown due to small numbers.", "<br>",
                "Data for the Island Boards are included in the Scotland total.")
                )
    
    } else {

creates_runcharts(plotdata = gest_at_termination_runchart_data(),
                  yaxislabel = "Average gestation at termination (weeks)") %>% 
  layout(xaxis = list(range = 
                        range(gest_at_termination_runchart_data()$DATE) + c(months(-1), months(1)))
         )
    }
})

# c) chart title ----

output$gest_at_terminaton_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         input$hbname
  )
})
