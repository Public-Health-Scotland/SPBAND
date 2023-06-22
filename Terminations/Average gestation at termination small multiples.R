# a) data ----

gest_at_termination_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- gest_at_termination_data %>%
    filter(HBTYPE == Selected$HBType) %>%
    set_variable_labels(
      MEASURE = "Average gestation at termination",
      MEDIAN = " average gestation to end Feb 2020",
      EXTENDED = " projected average gestation from Mar 2020 to end Jul 2020"
    ) %>% 
    mutate(HBNAME2 = factor(HBNAME2, 
                            levels = c("Scotland", "NHS Ayrshire & Arran", "NHS Borders",
                                       "NHS Dumfries & Galloway", "NHS Fife", "NHS Forth Valley",
                                       "NHS Grampian", "NHS Greater Glasgow & Clyde", "NHS Highland",
                                       "NHS Lanarkshire", "NHS Lothian", "NHS Tayside")),
           mytext = paste0(HBNAME,
                           "<br>",
                           "Month: ", 
                           format(DATE, "%b %Y"),
                           "<br>",
                           var_label(MEASURE),
                           ": ",
                           format(MEASURE,
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
