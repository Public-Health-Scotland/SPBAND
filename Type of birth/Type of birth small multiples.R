# a) data ----

# map input$tob to the values in measure_cat to enable filtering of the data frame

observeEvent(input$tob, Selected$Measure_cat <- input$tob)

type_of_birth_small_multiples_data <- reactive({
  # selects data
  
  req(input$organisation)
  
  data <- type_of_birth_data %>%
    filter(hbtype == Selected$HBType &
             measure_cat == Selected$Measure_cat) %>%
    # set_variable_labels(
    # measure_value = paste0("Percentage of births that were ",
    #                  Selected$Measure_cat, " (%)"),
    # median = " average to Oct-Dec 2019",
    # extended = " projected average from Jan-Mar 2020") %>% 
    mutate(mytext = paste0(hbname,
                           ": ",
                           measure_cat,
                           "<br>",
                           "Quarter: ", 
                           quarter_label,
                           "<br>",
                           "Percentage of births",
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           "%"),
           date = quarter_label,
           hbgroup = factor(if_else(hbname %in% island_names, "island", "mainland"),
                            levels = c("mainland", "island"), ordered = TRUE)
    ) %>% 
    group_by(hbgroup, hbtype) %>% 
    mutate(y_max = max(measure_value)
           # y_max = round_any(y_max, 10, f = ceiling),
           # hbname2 = factor(hbname, levels = HBnames)
    ) %>%
    ungroup()
  
  
  if (is.null(data))
  {
    return()
  }
  
  else {
    data
  }
  
})


# b) chart ---- 

output$type_of_birth_small_multiples <- renderPlotly({

subplot_mainland_island_small_multiples(
  plotdata = type_of_birth_small_multiples_data()
  )
})

# c) chart title ----

output$type_of_birth_small_multiples_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation)
  )
})

output$type_of_birth_small_multiples_sub_title <- renderText({

  case_match(Selected$Measure_cat,
         "assisted vaginal births" ~ HTML(paste0("Percentage of singleton live births at any gestation that were ",
                                    input$tob, 
                                    " (includes forceps, ventouse and vaginal breech births)")),
         "all caesarean births" ~ HTML("Percentage of singleton live births at any gestation that were
         caesarean births"),
         .default = HTML(paste0("Percentage of singleton live births at any gestation that were ",
                           input$tob))
         )
  })

