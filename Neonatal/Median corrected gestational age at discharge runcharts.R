# a) data ----

corrected_gest_age_at_discharge_runchart_data <- ({
  # selects data
  
  data <- corrected_gest_at_discharge_data %>% 
    #filter(measure_cat == "median corrected gestational age") %>% 
    set_variable_labels(
      num = "Number of babies discharged to home or foster care",
      measure_value = "Median corrected gestational age at discharge",
      #pre_pandemic_median = " average to Oct-Dec 2019",
      #extended_pre_pandemic_median = " projected average from Jan-Mar 2020",
      post_pandemic_median = paste0("average from Jul-Sep 2022", "<br>", "to Apr-Jun 2025"),
      extended_post_pandemic_median = "projected average from Jul-Sep 2025"
    ) %>% 
    mutate(mytext = paste0("Quarter: ",
                           date_label,
                           "<br>",
                           var_label(num),
                           ": ",
                           num,
                           "<br>",
                           var_label(measure_value),
                           ": ",
                           format(measure_value,
                                  digits = 1,
                                  nsmall = 1),
                           " weeks"),
           trend = NA, # to prevent this line being plotted
           shift = NA # ditto
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

output$corrected_gest_age_at_discharge_runcharts <- renderPlotly({
  
  creates_runcharts(plotdata = corrected_gest_age_at_discharge_runchart_data,
                    yaxislabel = "Median corrected gestational age at discharge (weeks)"
  )
  
})

# c) chart title ----

output$corrected_gestational_age_at_discharge_runcharts_title <- renderText({
  "Scotland"
})
