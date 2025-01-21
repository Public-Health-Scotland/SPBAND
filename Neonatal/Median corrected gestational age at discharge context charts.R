# a) data ----

corrected_gestational_age_context_data <- ({ 
  # selects data
  
  data <- babies_30_32_discharged_from_neocare_data %>% 
    filter(measure_cat != "median corrected gestational age") %>% 
    rename(den = measure_value) %>% # so function works
    set_variable_labels(
      den = "Number of babies discharged from neonatal care", # to pass into function
    ) %>% 
    mutate(mytext1 = "",
           mytext2 = paste0("Quarter: ",
                            date_label,
                            "<br>",
                            var_label(den), ": ", prettyNum(den, big.mark = ","))
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

output$corrected_gestational_age_context_charts <- renderPlotly({
  
  creates_context_charts(plotdata = corrected_gestational_age_context_data,
                         yaxislabel = "Number of babies"
                    ) %>% 
    layout(showlegend = TRUE)
})
