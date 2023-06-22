# a) data ----

# map input$tob to the values in INDICATOR_CAT to enable filtering of the data frame

observeEvent(input$tob, Selected$Indicator_cat <- input$tob)

type_of_birth_small_multiples_data <- reactive({
  # selects data
  
  #req(input$period)
  
  data <- type_of_birth_data %>%
  filter(HBTYPE == Selected$HBType &
           PERIOD == "Q" &
           INDICATOR_CAT == Selected$Indicator_cat) %>%
    set_variable_labels(
    MEASURE = paste0("Percentage of births that were ",
                     Selected$Indicator_cat, " (%)"),
    MEDIAN = " average to Oct-Dec 2019",
    EXTENDED = " projected average from Jan-Mar 2020") %>% 
    mutate(mytext = paste0(HBNAME,
                           ": ",
                           INDICATOR_CAT,
                           "<br>",
                           "Quarter: ", 
                           QUARTER_LABEL,
                           "<br>",
                           "Percentage of births",
                           ": ",
                           format(MEASURE,
                                  digits = 1,
                                  nsmall = 1),
                         "%"),
         DATE = QUARTER_LABEL
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
  
  case_match(Selected$Indicator_cat,
         "assisted births" ~ paste0("Percentage of singleton live births that were ", input$tob, 
                             " (includes forceps, ventouse and vaginal breech births)"),
         "all caesarean births" ~ "Percentage of singleton live births that were caesarean births",
         .default = paste0("Percentage of singleton live births that were ", input$tob)
         )
  })

