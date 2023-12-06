# a) data ----

gest_at_termination_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- gest_at_termination_data %>%
    filter(hbtype == Selected$HBType) %>%
    set_variable_labels(
      measure = "Average gestation at termination",
      median = " average gestation to end Feb 2020",
      extended = " projected average gestation from Mar 2020 to end Jul 2020"
    ) %>% 
    mutate(hbname2 = factor(hbname, 
                            levels = c("Scotland", "NHS Ayrshire & Arran", "NHS Borders",
                                       "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley",
                                       "NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Highland",
                                       "NHS Lanarkshire", "NHS Lothian", "NHS Tayside")),
           mytext = paste0(hbname,
                           "<br>",
                           "Month: ", 
                           format(date, "%b %Y"),
                           "<br>",
                           var_label(measure),
                           ": ",
                           format(measure,
                                  digits = 1,
                                  nsmall = 1),
                           " weeks"
           )
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

output$gest_at_termination_small_multiples <- renderPlotly({
  
  validate(
    need(nrow(gest_at_termination_small_multiples_data()) > 0,
         message = "Data not shown for Island Boards due to small numbers.")
  )

creates_overview_charts_without_median(
  plotdata = gest_at_termination_small_multiples_data(),
  yaxislabel = "Average gestation at termination (weeks)"
)
})

# c) chart title ----

output$gest_at_termination_small_multiples_title <- renderText({
  # paste0("Average gestation at termination by Board of ",
  paste0("Board of ",
         str_to_sentence(input$organisation)
  )
})
