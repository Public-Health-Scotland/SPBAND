# a) data ----

gest_at_termination_runchart_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- gest_at_termination_data %>%
    filter(hbname == Selected$HBName &
             hbtype == Selected$HBType) %>%
    set_variable_labels(
      measure = "Average gestation at termination",
      median = " average gestation to end Feb 2020",
      extended = " projected average gestation from Mar 2020"
    ) %>% 
    mutate(hbname2 = factor(hbname2, 
                            levels = c("NHS Ayrshire & Arran", "NHS Borders",
                                       "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley",
                                       "NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Highland",
                                       "NHS Lanarkshire", "NHS Lothian", "NHS Tayside", "Scotland")),
           mytext = paste0("Month: ", 
                           format(date, "%b %Y"),
                           "<br>",
                           var_label(measure),
                           ": ",
                           format(measure,
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
                        range(gest_at_termination_runchart_data()$date) + c(months(-1), months(1)))
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
