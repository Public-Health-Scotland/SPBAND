# a) data ----

# map input$gestation to visually nicer gestation group names for title

observeEvent(input$gestation, Selected$Nicename <- case_when(
  input$gestation == "under 32 weeks" ~ "under 32 weeks",
  input$gestation == ">= 32 and <= 36 weeks" ~ paste0("32", "<sup>+0</sup>",
                                                         " to 36", "<sup>+6</sup>", " weeks"),
  input$gestation == "under 37 weeks" ~ "under 37 weeks",
  input$gestation == ">= 42 weeks" ~ paste0("42", "<sup>+0</sup>", " weeks and over")
  )
)

gest_at_birth_small_multiples_data <- reactive({
  # selects data

  #req(input$period)

  data <- gest_at_birth_data %>%
  filter(HBTYPE == Selected$HBType &
           PERIOD == "Q" &
           INDICATOR_CAT == Selected$Gestation) %>%
    set_variable_labels(
    MEASURE = paste0("Percentage of births at ",
                     HTML(Selected$Nicename), " (%)"),
    MEDIAN = " average to Oct-Dec 2019",
    EXTENDED = " projected average from Jan-Mar 2020"
    ) %>% 
    mutate(mytext = paste0(HBNAME,
                           ": ",
                           Selected$Nicename,
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

output$gest_at_birth_small_multiples <- renderPlotly({

creates_overview_charts_without_median(
  plotdata = gest_at_birth_small_multiples_data()
  )
})

# c) chart title ----

output$gest_at_birth_small_multiples_title <- renderText({
  #paste0("Percentage of singleton live births following ", input$tob, " by Board of ",
  #paste0("Percentage of singleton live babies ", input$tob, " by Board of ",
  paste0("Board of ",
         str_to_sentence(input$organisation)
  )
})

output$gest_at_birth_small_multiples_sub_title <- renderText({
  HTML(paste0("Percentage of singleton live babies born at ", Selected$Nicename))
})

