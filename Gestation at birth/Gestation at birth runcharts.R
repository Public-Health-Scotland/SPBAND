# a) data ----

gest_at_birthplotListNames = c("under 32 weeks", "between 32 and 36 weeks (inclusive)", 
                               "under 37 weeks", "42 weeks and over (inclusive)")

y_max_gestation_at_birth <- reactiveVal(0) # initialise y_max_gestation_at_birth

gest_at_birth_runchart_data <- reactive({
  # selects data
  
  req(input$hbname)
  
  data <- gest_at_birth_data %>%
    filter(hbname == Selected$HBName &
             period == "Q" &
             hbtype == Selected$HBType &
             measure_cat %in% gest_at_birthplotListNames
    ) %>%
    droplevels() %>%
    mutate(num_label = paste0("Number of births that were at ", measure_cat_label, ": "),
           measure_label = paste0("Percentage of births that were at ", measure_cat_label, " (%)"),
           measure_cat_label =
             str_replace_all(measure_cat_label, c(" gestation" = "", " gestation " = ""))
    ) %>%
    set_variable_labels(
      measure_value = "Percentage of births (%)",
      den = "Total number of births with a known gestation: ",
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
                                  digits = 2,
                                  nsmall = 2),
                           "%")
    )
  
  data$measure_cat = factor(data$measure_cat, levels = gest_at_birthplotListNames)
  
  new_max <- max(data$measure_value) # local maximum measure_value
  
  observeEvent(new_max, {   # update maximum measure_value when Board changes
    y_max_gestation_at_birth(new_max)
  }
  )
  
  if (is.null(data)) {
    print("no data")
    return()
  } else {
    data
  }
})

# b) chart ----
  
output$gest_at_birth_runcharts <- renderPlotly({
  
  runcharts <- 
  
  gest_at_birth_runchart_data() %>% 
    group_by(.$measure_cat) %>%
    group_map(~
                creates_runcharts(plotdata = .
                ) %>% 
                layout(
                  yaxis = list(range = c(0, y_max_gestation_at_birth() * 1.05))
                ) # forces y axis to same value on all charts
    ) %>% 
    subplot(nrows = 2,
            margin = c(0.04, 0.04, 0.15, 0.15),
            shareX = FALSE,
            shareY = FALSE,
            titleY = TRUE) %>% 
    layout(legend =
             list(x = 1.05,
                  y = 0.94)
    )
  
  # Add dynamic alt text using htmlwidgets::onRender
  
  runcharts <- htmlwidgets::onRender(runcharts, "
      function(el, x) {
        el.setAttribute('aria-label', 'Run charts showing the percentage of singleton live births at under 32 weeks, 32+0 to 36+6 weeks, under 37 weeks and 42+0 weeks and over, for each quarter, from Jan-Mar 2017 onwards');
      }
      ")
  
  return(runcharts)
})
  
# c) chart title ----

output$gest_at_birth_runcharts_title <- renderText({
  paste0("Board of ",
         str_to_sentence(input$organisation),
         ": ", 
         input$hbname
  )
  
})
