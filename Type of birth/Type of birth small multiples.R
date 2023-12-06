# a) data ----

# map input$tob to the values in measure_cat to enable filtering of the data frame

observeEvent(input$tob, Selected$Measure_cat <- input$tob)

type_of_birth_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- type_of_birth_data %>%
  filter(hbtype == Selected$HBType &
           period == "Q" &
           measure_cat == Selected$Measure_cat) %>%
    set_variable_labels(
    measure_value = paste0("Percentage of births that were ",
                     Selected$Measure_cat, " (%)"),
    median = " average to Oct-Dec 2019",
    extended = " projected average from Jan-Mar 2020") %>% 
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
         date = quarter_label
  )
  
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

creates_overview_charts_without_median(
  plotdata = type_of_birth_small_multiples_data()
  )
})

# c) chart title ----

output$type_of_birth_small_multiples_title <- renderText({
  #paste0("Percentage of singleton live births following ", input$tob, " by Board of ", 
  #paste0("Percentage of singleton live babies ", input$tob, " by Board of ",
  paste0("Board of ",
         str_to_sentence(input$organisation)
  )
})

output$type_of_birth_small_multiples_sub_title <- renderText({

  case_match(Selected$Measure_cat,
         "assisted births" ~ HTML(paste0("Percentage of singleton live births at any gestation that were ",
                                    input$tob, 
                                    " (includes forceps, ventouse and vaginal breech births)")),
         "all caesarean births" ~ HTML("Percentage of singleton live births at any gestation that were
         caesarean births"),
         .default = HTML(paste0("Percentage of singleton live births at any gestation that were ",
                           input$tob))
         )
  })

