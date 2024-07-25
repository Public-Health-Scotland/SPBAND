# a) data ----

gest_at_booking_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)

  data <- gest_at_booking_data %>%
    filter(hbname == Selected$HBName &
             hbtype == Selected$HBType) %>%
    set_variable_labels(
      measure_value = "Average gestation at booking",
      median = "average gestation to end Feb 2020",
      extended_median = 
        case_when(Selected$HBName == "NHS Forth Valley" ~ 
                    paste0("projected average gestation from Mar 2020", "<br>", "to end Feb 2021"),
                  Selected$HBName == "NHS Tayside" ~ 
                    paste0("projected average gestation from Mar 2020", "<br>", "to end Jul 2020"),
                  TRUE ~ 
                    paste0("projected average gestation from Mar 2020", "<br>", "to end Jun 2022")
                  ),
      post_pandemic_median = paste0("average gestation from Jul 2022", "<br>", "to end Jun 2024"),
      extended_post_pandemic_median = "projected average gestation from Jul 2024" 
    ) %>% 
    mutate(mytext = paste0("Month: ", 
                           format(date, "%b %Y"),
                           "<br>",
                           var_label(measure_value),
                           ": ",
                           format(measure_value,
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
  if_else(input$hbname %in% c("NHS Forth Valley", "NHS Tayside"),
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         input$hbname,
         "*"),
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ",
         input$hbname)
  )
  })
