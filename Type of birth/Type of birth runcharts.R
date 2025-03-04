# a) data ----

type_of_birth_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  data <- type_of_birth_data %>%
    filter(hbname == Selected$HBName &
             period == "Q" &
             hbtype == Selected$HBType) %>%
    mutate(num_label = if_else(measure_cat == "all caesarean births",
                               paste0("Number of caesarean births: "),
                               paste0("Number of ", measure_cat, ": ")),
           measure_label = paste0("Percentage of births that were ", measure_cat, " (%)")
    ) %>% 
    set_variable_labels(
      measure_value = "Percentage of births (%)",
      den = "Total number of births: ",
      pre_pandemic_median = "average to Oct-Dec 2019",
      extended_pre_pandemic_median = "projected average from Jan-Mar 2020") %>% 
    mutate(mytext = paste0("Quarter: ",
                           quarter_label,
                           "<br>",
                           num_label,
                           prettyNum(num, big.mark = ","),
                           "<br>",
                           var_label(den),
                           prettyNum(den, big.mark = ","),
                           "<br>",
                           "Percentage of births: ", # not MEASURE_LABEL - too long
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           "%")
    )
  
  if (is.null(data)) {
    return()
  } else {
    data
  }
})

# b) chart ----

output$type_of_birth_runcharts <- renderPlotly({
  
  type_of_birth_runchart_data() %>% 
    group_by(.$measure_cat) %>%
    group_map(~
                creates_runcharts(plotdata = .
                ) %>% 
                layout(
                  yaxis = list(range = c(0, y_max_type_of_birth * 1.05))
                ) # forces y axis to same value on all charts
    ) %>% 
    subplot(nrows = 2,
            margin = c(0.03, 0.03, 0.15, 0.15),
            shareX = FALSE,
            shareY = FALSE,
            titleY = TRUE) %>%  
    layout(legend = 
             list(x = 0.7,
                  y = 0)
           )
  })

# c) chart title ----

output$type_of_birth_runcharts_title <- renderText({
  
  # if_else(input$hbname == "NHS Borders",
  #         paste0("Board of ",
  #                str_to_sentence(input$organisation),
  #                ": ",
  #                input$hbname,
  #                "*"),
          paste0("Board of ",
                 str_to_sentence(input$organisation),
                 ": ",
                 input$hbname)
  #)
})

