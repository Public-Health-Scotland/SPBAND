# a) data ----

gest_at_booking_runchart_data <- reactive({
  # selects data
  
  #req(input$period)

  data <- gest_at_booking_data %>%
    filter(HBNAME == Selected$HBName &
             HBTYPE == Selected$HBType) %>%
    set_variable_labels(
      MEASURE = "Average gestation at booking",
      MEDIAN = "average gestation to end Feb 2020",
      EXTENDED = 
        case_when(Selected$HBName == "NHS Forth Valley" ~ 
                    paste0("projected average gestation from Mar 2020", "<br>", "to end Feb 2021"),
                  Selected$HBName == "NHS Tayside" ~ 
                    paste0("projected average gestation from Mar 2020", "<br>", "to end Jul 2020"),
                  TRUE ~ "projected average gestation from Mar 2020")
    ) %>% 
    mutate(mytext = paste0("Month: ", 
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

output$gest_at_booking_runcharts <- renderPlotly({

gest_booking <- creates_runcharts(plotdata = gest_at_booking_runchart_data(),
                  yaxislabel = "Average gestation at booking (weeks)"
                  )
})

# c) chart title ----

output$gest_at_booking_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         input$hbname
  )
})


